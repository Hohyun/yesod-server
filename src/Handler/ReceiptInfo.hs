{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Handler.ReceiptInfo where

import Import
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import qualified Prelude as P    -- ^ head, tail
import qualified Data.List as DL -- ^ isInfixOf
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Text as T  -- ^ unpack


getReceiptInfoR :: Text -> Handler Value
getReceiptInfoR paymentid = do
  info <- liftIO $ getReceiptTrxInfo $ T.unpack paymentid
  returnJson info

data RcptTrx = RcptTrx
    { trxid :: String
    , receipts :: [Receipt]
    , receivables :: [Receivable]
    } deriving (Show, Eq)

instance ToJSON RcptTrx where
    toJSON RcptTrx {..} = object
        [ "trxid" .= trxid
        , "receipts" .= receipts
        , "receivables" .= receivables
        ]
        
data Receipt = Receipt
    { journal :: Journal
    , bankstmts :: [Bankstmt]
    } deriving (Show, Eq)

instance ToJSON Receipt where
    toJSON Receipt {..} = object
        [ "journal" .= journal
        , "bankstmts" .= bankstmts
        ]

data Journal = Journal
    { sq :: Int
    , paymentid :: String
    , merchantid :: String
    , pay_date :: Day
    , checking :: Double
    , ccfee :: Double
    , disc :: Double
    , ar_payment_clearing :: Double
    , onoff_bridge :: Double
    } deriving (Show, Generic, FromRow, Eq)

instance ToJSON Journal where
    toJSON Journal {..} = object
        [ "sq" .= sq
        , "paymentid" .= paymentid
        , "merchantid" .= merchantid
        , "pay_date" .= pay_date
        , "checking" .= checking
        , "ccfee" .= ccfee
        , "disc" .= disc
        , "ar_payment_clearing" .= ar_payment_clearing
        , "onoff_bridge" .= onoff_bridge
        ]

data Receivable = Receivable
    { customer :: String
    , acctdate :: Day
    , ccy :: String
    , ccar :: Double
    , cccm :: Double
    , ccar_applied :: Double
    , cccm_applied :: Double
    } deriving (Show, Generic, FromRow, Eq)

instance ToJSON Receivable where
    toJSON Receivable {..} = object
        [ "customer" .= customer
        , "acctdate" .= acctdate
        , "ccy" .= ccy
        , "ccar" .= ccar
        , "cccm" .= cccm
        , "ccar_applied" .= ccar_applied
        , "cccm_applied" .= cccm_applied
        ]

data Payment = Payment
    { date :: Day
    , payamt :: Double
    } deriving (Show, Generic, FromRow, Eq)

instance ToJSON Payment where
    toJSON Payment {..} = object
        [ "date" .= date
        , "payamt" .= payamt
        ]
        
data Bankstmt = Bankstmt
    { accountno :: String
    , trxdate :: Day
    , dramt :: Double
    } deriving (Show, Generic, FromRow, Eq)

instance ToJSON Bankstmt where
    toJSON Bankstmt {..} = object
        [ "accountno" .= accountno
        , "trxdate" .= trxdate
        , "dramt" .= dramt
        ]

aprsPG :: ConnectInfo
aprsPG = defaultConnectInfo
  { connectHost = "10.90.65.61"
  , connectDatabase = "aprs"
  , connectUser = "postgres"
  , connectPassword = "LJ2008*"
  }

extractBankStmts :: (Num a, Eq a) => a -> [a] -> [a] -> [a]
extractBankStmts r rs bs =
    case concat [matchNto1 xs bs | xs <- allCombs r rs] of
        rr1@(_:_) -> rr1
        [] ->
            case concat [matchNto2 xs bs | xs <- allCombs r rs] of
                rr2@(_:_) -> rr2
                [] -> []

-- merged case: N payment = 1 bankstmt
matchNto1 :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNto1 rs = filter (== sum rs)

-- split case 2: sum(N payment) = 2 bankstmts (only first 3 bankstmts considered)
matchNto2 :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNto2 r xs
    | not (null matched) = P.head matched
    | otherwise = []
  where
    matched = filter (\x -> P.head x + P.last x == sum r) (combination 2 xs)

-- make combination: nCr
combination :: Num a => Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination n (x:xs) = map (x :) (combination (n - 1) xs) ++ combination n xs

-- combinations having concerned amt:  checking -> all checkings --> [all combinations include concerned checking amt]
allCombs :: (Num a, Eq a) => a -> [a] -> [[a]]
allCombs 0 _ = []
allCombs _ [] = []
allCombs r rs = filter (elem r) $ cs
  where
    cnt = length rs
    cs = concat $ [combination n rs | n <- [1 .. cnt]]

receivableInfo :: Connection -> String -> IO [Receivable]
receivableInfo conn pid =
  query conn [sql|
        with inv as (
	  select distinct code, salesdate, ccy
	  from inv_hdr ih join erp_code ec on ih.gateway = ec.gateway and ih.settleco = ec.settleco 
	  where reference in (
	        select distinct reference from compare_settled cs 
	        where reference is not null and paymentid = ?)
        ), base as (               
	  select e.customer, e.acctdate, e.ccy, 
	         case when e.salerfnd = 'sale' then amount else 0 end as sale,
		 case when e.salerfnd = 'refund' then amount else 0 end as refund	   
	  from erp_inv e join inv v on e.customer = v.code and e.acctdate = v.salesdate and e.ccy = v.ccy
        ), issued as (
          select customer, acctdate, ccy, sum(sale) ccar, sum(refund) cccm 
          from base 
          group by customer, acctdate, ccy
        ), applied as (
          select customer, acctdate, ccy, sum(ccar) as ccar_applied, sum(cccm) as cccm_applied 
	  from erp_applied
	  where customer = (select distinct customer from issued) and
                acctdate in (select distinct acctdate from issued)
	  group by customer, acctdate, ccy
        )
        select i.customer, i.acctdate, i.ccy, i.ccar :: float8, i.cccm :: float8, 
	       coalesce(a.ccar_applied, 0) :: float8 ccar_applied,
               coalesce(a.cccm_applied, 0) :: float8 cccm_applied
        from issued i left join applied a on i.customer = a.customer and i.acctdate = a.acctdate and i.ccy = a.ccy
        order by i.customer, i.acctdate
     |] $ (Only pid)

journalInfo :: Connection -> String -> IO [Journal]
journalInfo conn pid =
  query conn [sql| with base as (
	  select paymentid, date, merchantid, account, sum(amount) amount 
	  from ledger where paymentid = ?
	  group by  paymentid, date, merchantid, account
	  having sum(amount) != 0
        ), payment as (
	  select paymentid, date, merchantid, amount checking
	  from base where account = 'assets:checking'
        ), fee as ( 
	  select paymentid, date, amount ccfee
	  from base where account = 'expenses:ccfee' 
        ), disc as (
	  select paymentid, date, amount disc
	  from base where account = 'expenses:salesdisc'
        ), clear as ( 
	  select paymentid, date, amount ar_payment_clearing 
	  from base where account = 'clearing:ar_payment' 
        ), bridge as ( 
	  select paymentid, date, amount onoff_bridge 
	  from base where account = 'bridge:onoff' 
        ) 
        select row_number() over(order by p.paymentid, p.date) :: int8 sq, p.paymentid,
          p.merchantid, p.date pay_date,
          checking :: float8, ccfee :: float8, coalesce(disc, 0) :: float8 disc,
          coalesce(ar_payment_clearing,0) :: float8 ar_payment_clearing,
          coalesce(onoff_bridge,0) :: float8 onoff_bridge
        from payment p join fee f on p.paymentid = f.paymentid and p.date = f.date 
               left join disc   d on p.paymentid = d.paymentid and p.date = d.date
               left join clear  c on p.paymentid = c.paymentid and p.date = c.date
               left join bridge b on p.paymentid = b.paymentid and p.date = b.date
        order by p.date, p.checking desc
|] $ (Only pid)

bankstmtInfoBSP :: Connection -> Journal -> IO [Bankstmt]
bankstmtInfoBSP conn j =
    query
        conn
        [sql|
        select '630-006859-038' accountno, date trxdate, amount :: float8 dramt
        from ccsettle
        where gateway = 'BSP' and settleco = substring(?, 1, 2) and
          date >= ? and amount = ?
        |] $
    (pid, min_date, search_amt)
  where
    pid = paymentid j
    min_date = pay_date j
    search_amt = checking j
     
bankstmtInfoLJSP :: Connection -> Journal -> IO [Bankstmt]
bankstmtInfoLJSP conn j = do
    let dt = pay_date j
    let merchant = merchantid j
    let keyamt = checking j
    ps <-
        query
            conn
            [sql|
          with base as (
            select paymentid, date, merchantid, account, sum(amount) amount
            from ledger
            where account = 'assets:checking' and date = ? and merchantid = ?
            group by paymentid, date, merchantid, account
            having sum(amount) != 0
          )
          select date, amount :: float8
          from base
       |] $
        (dt, merchant) :: IO [Payment]
    bs <-
        query
            conn
            [sql|
          select accountno, trxdate, dramt :: float8
          from bankstmt
          where trxdate = ? and merchantid = ?
       |] $
        (dt, merchant) :: IO [Bankstmt]
    let ps' = map payamt ps :: [Double]
    let bs' = map dramt bs :: [Double]
    let matched = extractBankStmts keyamt ps' bs' :: [Double]
    return $ filter (\x -> (dramt x) `elem` matched) bs

relevantBankstmts :: Connection -> Journal -> IO [Bankstmt]
relevantBankstmts conn j
    | DL.isInfixOf "BSP" $ paymentid j = bankstmtInfoBSP conn j
    | otherwise = bankstmtInfoLJSP conn j

receiptInfo :: Connection -> Journal -> IO Receipt
receiptInfo conn j = do
    bs <- relevantBankstmts conn j
    return Receipt {journal = j, bankstmts = bs}

getReceiptTrxInfo :: String -> IO RcptTrx
getReceiptTrxInfo pid = do
    conn <- PG.connect aprsPG
    rcvbl <- receivableInfo conn pid
    js <- journalInfo conn pid
    ri <- mapM (\x -> receiptInfo conn x) js
    return RcptTrx {trxid = pid, receipts = ri, receivables = rcvbl}

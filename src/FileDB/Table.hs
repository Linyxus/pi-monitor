module FileDB.Table
  ( createTable
  , DataRecord (..)
  , getAll
  , getByPK
  , save
  , (.>>)
  , (.==)
  , (.>)
  , (.<)
  , (.>=)
  , (.<=)
  , (.--)
  , DataOrder (..)
  , insert
  , DataTable
  )
where

import Control.Concurrent
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Map as M
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Data.Sort (sortBy)

class (FromJSON a, ToJSON a) => DataRecord a where
  primaryKey :: Maybe (a -> Int)
  primaryKey = Nothing

data DataContainer a k = DataList [a] | DataMap (M.Map k a)

data DataTable a = DataTable { records :: MVar (DataContainer a Int)
                             , filePath :: String }

createContainer :: (DataRecord a) => [a] -> DataContainer a Int
createContainer ds = case primaryKey of
                       Nothing -> DataList ds
                       Just p -> let pk = p <$> ds
                                     l = zip pk ds
                                 in DataMap $ M.fromList l

createTable :: (DataRecord a) => String -> [a] -> IO (DataTable a)
createTable path defaults = do
  fileExt <- doesFileExist path
  case fileExt of
    False -> do
      container <- newEmptyMVar :: IO (MVar (DataContainer a Int))
      putMVar container $ createContainer defaults
      return $ DataTable container path
    True -> do
      ds <- decode <$> BS.readFile path
      case ds of
        Nothing -> error "Failed to decode file!"
        Just dataList -> do
          container <- newEmptyMVar
          putMVar container $ createContainer dataList
          return $ DataTable container path

getAll :: (DataRecord a) => DataTable a -> IO [a]
getAll (DataTable ds _) = do
  records <- takeMVar ds
  putMVar ds records
  case records of
    DataList vals -> return vals
    DataMap m -> return . map (\(_, x) -> x) . M.toList $ m

getByPK :: (DataRecord a) => DataTable a -> Int -> IO (Maybe a)
getByPK (DataTable records _) pk = do
  ds <- takeMVar records
  putMVar records ds
  case ds of
    DataList vals -> error "No primary key is given."
    DataMap m -> return $ M.lookup pk m

infixl 1 .>>
(.>>) :: Monad m => m a -> (a -> b) -> m b
xs .>> f = xs >>= (return . f)

mkFilterWithOp :: (b -> b -> Bool) -> (a -> b) -> b -> [a] -> [a]
mkFilterWithOp op f v = filter $ (flip op) v . f

infixl 4 .==
(.==) :: (Eq b) => (a -> b) -> b -> [a] -> [a]
f .== v = mkFilterWithOp (==) f v

infixl 4 .>
(.>) :: (Ord b) => (a -> b) -> b -> [a] -> [a]
f .> v = mkFilterWithOp (>) f v

infixl 4 .>=
(.>=) :: (Ord b) => (a -> b) -> b -> [a] -> [a]
f .>= v = mkFilterWithOp (>=) f v

infixl 4 .<
(.<) :: (Ord b) => (a -> b) -> b -> [a] -> [a]
f .< v = mkFilterWithOp (<) f v

infixl 4 .<=
(.<=) :: (Ord b) => (a -> b) -> b -> [a] -> [a]
f .<= v = mkFilterWithOp (<=) f v

invOrd :: Ordering -> Ordering
invOrd LT = GT
invOrd GT = LT
invOrd EQ = EQ

compare' x y = invOrd . compare x $ y

data DataOrder = Asc | Desc

infixl 4 .--
(.--) :: (Ord b) => (a -> b) -> DataOrder -> [a] -> [a]
f .-- order = sortBy (\x y -> comp (f x) (f y))
  where comp = case order of
                 Asc -> compare
                 Desc -> compare'

save :: (DataRecord a) => DataTable a -> IO ()
save table@(DataTable records path) = (fmap encode . getAll $ table) >>= BS.writeFile path

insert :: (DataRecord a) => a -> DataTable a -> IO ()
insert val (DataTable container _) = do
  ds <- takeMVar container
  let ds' = case ds of
        DataList vals -> DataList $ val : vals
        DataMap m -> DataMap $ M.insert (fromMaybe (-1) $ primaryKey <*> Just val) val m
  putMVar container ds'

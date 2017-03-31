{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO hiding (ascii,latin1,utf8)

import Control.Monad.Trans.Resource
import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as HTTP
import Codec.Phaser.Core
import Codec.Phaser.Common
import Codec.Phaser.UTF8
import Codec.Phaser.ByteString

import Web.Html
import Web.JSON

data Recipe =
  Shaped
    (Maybe String) (Maybe String) (Maybe String)
    (Maybe String) (Maybe String) (Maybe String)
    (Maybe String) (Maybe String) (Maybe String)
  | Unshaped [String]

needsTable (Shaped _ _ Nothing _ _ Nothing Nothing Nothing Nothing) = False
needsTable (Shaped Nothing _ _ Nothing _ _ Nothing Nothing Nothing) = False
needsTable (Shaped Nothing Nothing Nothing _ _ Nothing _ _ Nothing) = False
needsTable (Shaped Nothing Nothing Nothing Nothing _ _ Nothing _ _) = False
needsTable (Shaped _ _ _ _ _ _ _ _ _) = True
needsTable (Unshaped (_:_:_:_:_:_)) = True
needsTable (Unshaped _) = False

phaserToConduit p s = stream p s ((fmap . fmap) (:[]) C.await) (C.yield)

craftURL category =
  "http://minecraft.gamepedia.com\
  \/api.php?action=parse&format=json&prop=text|modules|jsconfigvars&page=" ++
  escape category
 where
  escape [] = []
  escape (' ':r) = '+' : escape r
  escape (a:r) = a : escape r

firstpage = "http://minecraft.gamepedia.com/Crafting"

pageP = html <|> (jsonDocument >>= \j -> case j of
  JSONObject p -> case lookup "parse" p of
    Just (JSONObject p') -> case lookup "text" p' of
      Just (JSONObject t) -> case lookup "*" t of
        Just (JSONString s) -> fromAutomaton $ starve $ run (toAutomaton html) s
        _ -> fail "document.parse.text.* should be a string"
      _ -> fail "document.parse.text should be an object"
    _ -> fail "document.parse should be an object"
  _ -> fail "Expecting document root to be an object"
 )

recipes = do
  search (\h ->
    tagName h == Just "table" && getAttribute "data-description" h == Just "Crafting recipes")
  search (\h -> tagName h == Just "tr")
  fmap catMaybes $ foreach $ do
    name <- scratch $ do
      search $ \h -> tagName h == Just "th"
      getTextS
    inList <- scratch $ do
      search $ \h -> tagName h == Just "td" && getAttributes h == []
      replace (\h -> tagName h == Just "br") " "
      h <- Web.Html.get
      return $ case h of
        [] -> []
        (a:_) -> getText a
    search $ \h -> case getAttribute "class" h of
      Nothing -> False
      Just c -> "mcui-Crafting_Table" `elem` words c
    rowHasRecipe <- scratch $ or <$> foreach (return True)
    if rowHasRecipe
      then do
        shapeless <- scratch $ do
          search $ \h -> case getAttribute "class" h of
            Nothing -> False
            Just c -> "mcui-icons" `elem` words c
          search $ \h -> case getAttribute "class" h of
            Nothing -> False
            Just c -> "mcui-shapeless" `elem` words c
          or <$> foreach (return True)
        search $ \h -> case getAttribute "class" h of
          Nothing -> False
          Just c -> "mcui-input" `elem` words c
        search $ \h -> case getAttribute "class" h of
          Nothing -> False
          Just c -> "invslot" `elem` words c
        inGrid <- foreach $ do
          search $ \h -> case getAttribute "class" h of
            Nothing -> False
            Just c -> "invslot-item" `elem` words c
          pf <- Web.Html.get
          return $ foldr (<|>) Nothing $ map (getAttribute "title") pf
        return $ Just (name, inList, if shapeless
          then Unshaped (catMaybes inGrid)
          else let
            [i1,i2,i3,i4,i5,i6,i7,i8,i9] = inGrid
            in Shaped i1 i2 i3 i4 i5 i6 i7 i8 i9
         )
      else return Nothing

subPages = do
  search $ \h -> case getAttribute "class" h of
    Nothing -> False
    Just c -> "load-page" `elem` words c
  fmap catMaybes $ foreach $ getAttributeS "data-page"

main = do
  childThreads <- newTVarIO (0 :: Integer)
  writerThreads <- newTVarIO (0 :: Integer)
  windDown <- newTVarIO False
  nextItem <- newTVarIO Nothing
  nextLog <- newTVarIO Nothing
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  clh <- openFile "handcraft.txt" WriteMode
  let
    safeFork' cv a = do
      atomically $ modifyTVar' cv (+ 1)
      forkIO $ a <* atomically (modifyTVar' cv (subtract 1))
    safeFork = safeFork' childThreads
    writeLoop :: Handle -> TVar (Maybe String) -> IO ()
    writeLoop h s = do
      msg <- atomically $ do
        m <- readTVar s
        case m of
          Just _ -> writeTVar s Nothing >> return m
          Nothing -> do
            rc <- readTVar windDown
            if rc then return Nothing else retry
      case msg of
        Nothing -> return ()
        Just m -> hPutStrLn h m >> writeLoop h s
    mainloop cv = atomically $ do
      rc <- readTVar cv
      when (rc /= 0) retry
    writeTo s m = atomically $ do
      em <- readTVar s
      case em of
        Nothing -> writeTVar s $ Just m
        Just _ -> retry
    writeLog = writeTo nextLog
    writeItem = writeTo nextItem
    
    processPage url = case HTTP.parseRequest url of
      Nothing -> writeLog $ "Unable to parse URL: " ++ url
      Just rq -> do
        writeLog $ "Fetching " ++ url
        let p = unpackBS >># utf8_stream >># trackPosition >># pageP
        rspb <- runResourceT $ do
          rsp <- HTTP.http rq manager
          HTTP.responseBody rsp C.$$+-
           C.mapOutputMaybe (const Nothing) (phaserToConduit (Position 1 1) p)
        case rspb of
          Left e -> writeLog $ intercalate "\n" $
            ("Unable to parse document at " ++ url) :
            concatMap (\(p,el) -> show p : map ("  " ++) el) e
          Right html -> do
            writeLog $ "Processing " ++ url
            safeFork $ forM_ (snd $ runSelector subPages html)
              (safeFork . processPage . craftURL)
            forM_ (snd $ runSelector recipes html) $ \(name,input,recipe) -> do
              writeLog $ "Recipe: " ++ name ++ " <- " ++ input
              if needsTable recipe
                then return ()
                else writeItem $ name ++ " <- " ++ input
  
  safeFork' writerThreads $ writeLoop stderr nextLog
  safeFork' writerThreads $ writeLoop clh nextItem
  safeFork $ processPage firstpage
  mainloop childThreads
  atomically $ writeTVar windDown True
  mainloop writerThreads
  hClose clh

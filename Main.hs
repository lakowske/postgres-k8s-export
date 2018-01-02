module Main where

import Database.PostgreSQL.Simple
import Control.Exception
import System.Process
import System.Environment

data Shell a = Shell a deriving (Show)

instance Functor Shell where
  fmap f (Shell a) = Shell (f a)

instance Applicative Shell where
  pure = Shell
  (Shell f) <*> shell = fmap f shell

instance Monad Shell where
  return x = Shell x
  Shell x >>= f = f x

fromLiteral :: [(String, String)] -> String -> String
fromLiteral (x:xs) line = fromLiteral xs (line ++ " --from-literal="++(fst x)++"="++(snd x))
fromLiteral [] line = line

createConfigMap :: String -> [(String, String)] -> Shell String
createConfigMap name literals = Shell $ fromLiteral literals ("kubectl create configmap " ++ name)

deleteConfigMap name = Shell ("kubectl delete configmap "++name)

createSecret :: String -> [(String, String)] -> Shell String
createSecret name literals = Shell $ fromLiteral literals ("kubectl create secret generic " ++ name)

deleteSecret name = Shell ("kubectl delete secret "++name)

toProc :: Shell String -> CreateProcess
toProc (Shell a) = shell a

readShell :: Shell String -> IO String
readShell s = (readCreateProcess $ toProc s) ""

getNodePort release = Shell ("kubectl get service --namespace default " ++ release ++ "-postgresql -o jsonpath=\"{.spec.ports[0].nodePort}\"")
getPort release = Shell ("kubectl get service --namespace default " ++ release ++ "-postgresql -o jsonpath=\"{.spec.ports[0].port}\"")
getUser release = Shell ("kubectl get secret --namespace default " ++ release ++ "-imagine-db-chart -o jsonpath=\"{.data.postgres-user}\" | base64 --decode;")
getPassword release = Shell ("kubectl get secret --namespace default " ++ release ++ "-postgresql -o jsonpath=\"{.data.postgres-password}\" | base64 --decode;")

connInfoFromRelease :: String -> IO ConnectInfo
connInfoFromRelease release = do
  db <- lookupEnv "DATABASE"  
  port <- (readShell $ getPort release)
  user <- (readShell $ getUser release)
  pw <- (readShell $ getPassword release)
  return defaultConnectInfo {
    connectDatabase =  (maybe "postgres" id db),
    connectUser = user,
    connectHost = (release ++ "-postgresql"),
    connectPort = read port,
    connectPassword = pw
  }

main = do
  release <- lookupEnv "RELEASE_NAME"
  secretName <- lookupEnv "SECRET_NAME"
  configMapName <- lookupEnv "CONFIGMAP_NAME"
  
  case release of
    Just name -> do
      putStrLn $ "exporting " ++ name
      info <- connInfoFromRelease name
      putStrLn $ "password " ++ (connectPassword info)
      putStrLn $ "user " ++ (connectUser info)
      putStrLn $ "host " ++ (connectHost info)
      putStrLn $ "port " ++ (show (connectPort info))
      putStrLn $ "database " ++ (connectDatabase info)
      
      re <- (try $ readShell $ deleteSecret (maybe "db-secret" id secretName)) :: IO (Either SomeException String)
      readShell $ createSecret (maybe "db-secret" id secretName)
        [("password", connectPassword info)]

      res <- (try $ readShell $ deleteConfigMap (maybe "db-config" id configMapName)) :: IO (Either SomeException String)
        
      readShell $ createConfigMap (maybe "db-config" id configMapName)
        [("username", connectUser info),
         ("host", connectHost info),
         ("port", (show $ connectPort info)),
         ("database", connectDatabase info)]

      return ()

    Nothing -> putStrLn $ "please supply a RELEASE_NAME env variable"

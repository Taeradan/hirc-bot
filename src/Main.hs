{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.List
import           Network
import           Network.IRC.Base
import           Network.IRC.Commands
import           Network.IRC.Parser
import           System.Exit
import           System.IO
import           Text.Printf

-- | Pour les tests, on va emmerder le monde sur le chan de Teleragno :)
server = "irc.teleragno.fr" :: B.ByteString
port   = 6667
chan   = "#bistro"          :: B.ByteString
nickname   = "haskell-bot"  :: B.ByteString

-- | The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- | Fonction principale, se connecte au serveur IRC et lance la boucle d'écoute
main :: IO ()
main = bracket connect disconnect loop
    where
        disconnect = hClose . socket
        loop = runReaderT run

-- | Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
        handle <- connectTo (B.unpack server) (PortNumber (fromIntegral port))
        hSetBuffering handle NoBuffering
        return (Bot handle)
            where notify = bracket_ pre post
                  pre    = printf "Connecting to %s ... " (B.unpack server) >> hFlush stdout
                  post   = putStrLn "done."

-- | We're in the Net monad now, so we've connected successfully
--   Join a channel, and start processing commands
run :: Net ()
run = do
        write $ nick nickname
        write $ user nickname (B.pack "0") (B.pack "*") (B.pack "Haskell IRC Bot")
        write $ joinChan chan
        handle <- asks socket
        listen handle

-- | Boucle d'écoute du serveur.
--   Reçoit les commandes entrantes et lance le traitement.
listen :: Handle -> Net ()
listen handle = forever $ do
        command <- init `fmap` io (hGetLine handle)
        io (putStrLn command)
        let message = processIrcCommand command
        maybe (return ()) write message

-- | Fonction qui évalue une commande IRC
processIrcCommand :: String -> Maybe Message
processIrcCommand x
    | "PING :" `isPrefixOf` x            = Just $ pong server
    | ("PRIVMSG " ++ B.unpack chan) `isInfixOf` x = processUserCommand (clean x)
    | otherwise                          = Nothing
        where
               clean = tail . dropWhile ( /= ':') . tail

-- | Fonction qui évalue une commande d'un utilisateur
processUserCommand :: String -> Maybe Message
processUserCommand x
    | x == "!quit"                     = Just $ quit . Just . B.pack $ "Exiting"
    | "!id " `isPrefixOf` x            = Just $ privmsg chan (B.pack (drop 4 x))
    | "coin" `isInfixOf` map toLower x = Just $ privmsg chan (B.pack "PAN !")
    | otherwise                        = Nothing

write :: Message -> Net()
write message = do
        handle <- asks socket
        let string = B.unpack . encode $ message
        io $ hPutStrLn handle string
        io $ putStrLn string

-- Convenience.
io :: IO a -> Net a
io = liftIO

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Reader
import           Control.Exception
import           Data.List
import           Network
import           System.Exit
import           System.IO
import           Text.Printf

-- | Pour les tests, on va emmerder le monde sur le chan de Teleragno :)
server = "irc.teleragno.fr"
port   = 6667
chan   = "#bistro"
nick   = "haskell-bot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- | Fonction principale, se connecte au serveur IRC et lance la boucle d'écoute
main :: IO ()
main = bracket connect disconnect loop
    where
        disconnect = hClose . socket
        loop = runReaderT run

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
        handle <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering handle NoBuffering
        return (Bot handle)
            where notify = bracket_ pre post
                  pre    = printf "Connecting to %s ... " server >> hFlush stdout
                  post   = putStrLn "done."

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
        write "NICK" nick
        write "USER" (nick ++ " 0 * :tutorial bot")
        write "JOIN" chan
        handle <- asks socket
        listen handle

{- |
    Boucle d'écoute du serveur.
    Reçoit les commandes entrantes et lance le traitement.
-}
listen :: Handle -> Net ()
listen handle = forever $ do
        command <- init `fmap` io (hGetLine handle)
        io (putStrLn command)
        processIrcCommand command

-- | Fonction qui évalue une commande IRC
processIrcCommand :: String -> Net ()
processIrcCommand x
    | "PING :" `isPrefixOf` x = write "PONG" (':' : drop 6 x)
    | otherwise               = processUserCommand (clean x)
        where
               clean = drop 1 . dropWhile ( /= ':') . drop 1

processUserCommand :: String -> Net ()
processUserCommand x
    | x == "!quit"          = write "QUIT" ":Exiting" >> io exitSuccess
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | otherwise             = return () -- ignore everything else

-- | Fonction qui envoie un message sur le chan
privmsg :: String -> Net ()
privmsg string = write "PRIVMSG" (chan ++ " :" ++ string)

-- | Fonction de base qui envoie au serveur une commande IRC
write :: String -> String -> Net ()
write s t = do
        handle <- asks socket
        io $ hPrintf handle "%s %s\r\n" s t
        io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO

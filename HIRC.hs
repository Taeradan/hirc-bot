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
        loop st    = runReaderT run st

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
        handle <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering handle NoBuffering
        return (Bot handle)
       where notify a = bracket_
                (printf "Connecting to %s ... " server >> hFlush stdout)
                (putStrLn "done.")
                a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
        write "NICK" nick
        write "USER" (nick ++ " 0 * :tutorial bot")
        write "JOIN" chan
        asks socket >>= listen

{-|
    Boucle d'écoute du serveur.
    Reçoit les commandes entrantes et décide de quoi en faire.
    Les choix sont :
    * Répondre à un PING
    * Lancer le traitement des commandes
-}
listen :: Handle -> Net ()
listen handle = forever $ do
        command <- init `fmap` io (hGetLine handle)
        io (putStrLn command)
        if ping command then pong command else eval (clean command)
            where
               clean     = drop 1 . dropWhile ( /= ':') . drop 1
               ping x    = "PING :" `isPrefixOf` x
               pong x    = write "PONG" (':' : drop 6 x)

-- | Fonction qui évalue une commande IRC
eval :: String -> Net ()
eval    "!quit"                = write "QUIT" ":Exiting" >> io (exitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval   _                       = return () -- ignore everything else

-- | Fonction qui envoie un message sur le chan
privmsg :: String -> Net ()
privmsg string = write "PRIVMSG" (chan ++ " :" ++ string)

-- | Fonction de base qui envoie au serveur une commande IRC
write :: String -> String -> Net ()
write s t = do
        handle<- asks socket
        io $ hPrintf handle "%s %s\r\n" s t
        io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO

import           Control.Monad
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

-- | Fonction principale, se connecte au serveur IRC et lance la boucle d'écoute
main :: IO ()
main = do
        handle <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering handle NoBuffering
        write handle "NICK" nick
        write handle "USER" (nick ++ " 0 * :tutorial bot")
        write handle "JOIN" chan
        listen handle

{-|
    Boucle d'écoute du serveur.
    Reçoit les commandes entrantes et décide de quoi en faire.
    Les choix sont :
    * Répondre à un PING
    * Lancer le traitement des commandes
-}
listen :: Handle -> IO ()
listen handle = forever $ do
        line <- hGetLine handle
        let command = init line
        if ping command then pong command else eval handle (clean command)
        putStrLn command
            where
               clean     = drop 1 . dropWhile ( /= ':') . drop 1
               ping x    = "PING :" `isPrefixOf` x
               pong x    = write handle "PONG" (':' : drop 6 x)

-- | Fonction qui évalue une commande IRC
eval :: Handle -> String -> IO ()
eval handle    "!quit"                = write handle "QUIT" ":Exiting" >> exitSuccess
eval handle x | "!id " `isPrefixOf` x = privmsg handle (drop 4 x)
eval _   _                       = return () -- ignore everything else

-- | Fonction qui envoie un message sur le chan
privmsg :: Handle -> String -> IO ()
privmsg handle string = write handle "PRIVMSG" (chan ++ " :" ++ string)

-- | Fonction de base qui envoie au serveur une commande IRC
write :: Handle -> String -> String -> IO ()
write handle s t = do
        hPrintf handle "%s %s\r\n" s t
        printf    "> %s %s\n" s t


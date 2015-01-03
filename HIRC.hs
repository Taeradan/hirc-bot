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
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        write h "NICK" nick
        write h "USER" (nick ++ " 0 * :tutorial bot")
        write h "JOIN" chan
        listen h

{-|
    Boucle d'écoute du serveur.
    Reçoit les commandes entrantes et décide de quoi en faire.
    Les choix sont :
    * Répondre à un PING
    * Lancer le traitement des commandes
-}
listen :: Handle -> IO ()
listen h = forever $ do
        t <- hGetLine h
        let s = init t
        if ping s then pong s else eval h (clean s)
        putStrLn s
            where
               clean     = drop 1 . dropWhile ( /= ':') . drop 1
               ping x    = "PING :" `isPrefixOf` x
               pong x    = write h "PONG" (':' : drop 6 x)

-- | Fonction qui évalue une commande IRC
eval :: Handle -> String -> IO ()
eval h    "!quit"                = write h "QUIT" ":Exiting" >> exitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _   _                       = return () -- ignore everything else

-- | Fonction qui envoie un message sur le chan
privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

-- | Fonction de base qui envoie au serveur une commande IRC
write :: Handle -> String -> String -> IO ()
write h s t = do
        hPrintf h "%s %s\r\n" s t
        printf    "> %s %s\n" s t


import Data.List (nub)
import qualified Data.Map as Map

type Passphrase = [String]

readPassphrases :: String -> [Passphrase]
readPassphrases s = map words $ lines s

valid :: Passphrase -> Bool
valid p = (nub p) == p

strictValid :: Passphrase -> Bool
strictValid p = nub counts == counts where
    counts = map count p
    count word = Map.fromListWith (+) (zip word [1,1..])

main = do
    contents <- readFile "passphrases.txt"
    let passphrases = readPassphrases contents
    print $ length (filter valid passphrases)
    print $ length (filter strictValid passphrases)


import Data.Int
import Tiny

magic :: [Char]
magic = "4f5545745caf3200"

quineComputer :: Computer
quineComputer = initialize (Program . read $ "0x" ++ magic) []

quineTest :: Bool
quineTest = (magic ==) . showOutput . run $ quineComputer

main :: IO ()
main = printOut $ quineComputer
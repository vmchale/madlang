import           Distribution.CommandLine
import           Distribution.Simple

main :: IO ()
main = setManpath >>
    writeManpages "man/madlang.1" "madlang.1" >>
    writeBashCompletions "madlang" >>
    defaultMain

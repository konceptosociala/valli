module Utils (
  clearScreen, fgcolor, bgcolor, bold, 
  enterAltScreen, exitAltScreen
) where
  
import App
import GHC.IO.Handle
import GHC.IO.Handle.FD

data Color = 
  Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightBlack | BrightRed | BrightGreen | BrightYellow
  | BrightBlue | BrightMagenta | BrightCyan | BrightWhite
  | Rgb Int Int Int
  deriving (Show, Eq)

reset :: String
reset = "\ESC[0m"

fgcolor :: Color -> String -> String
fgcolor c s = code ++ s ++ reset where
  code = case c of
    Black         -> "\ESC[30m"
    Red           -> "\ESC[31m"
    Green         -> "\ESC[32m"
    Yellow        -> "\ESC[33m"
    Blue          -> "\ESC[34m"
    Magenta       -> "\ESC[35m"
    Cyan          -> "\ESC[36m"
    White         -> "\ESC[37m"
    BrightBlack   -> "\ESC[90m"
    BrightRed     -> "\ESC[91m"
    BrightGreen   -> "\ESC[92m"
    BrightYellow  -> "\ESC[93m"
    BrightBlue    -> "\ESC[94m"
    BrightMagenta -> "\ESC[95m"
    BrightCyan    -> "\ESC[96m"
    BrightWhite   -> "\ESC[97m"
    Rgb r g b    -> "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

bgcolor :: Color -> String -> String
bgcolor c s = show code ++ s ++ reset where
  code = case c of
    Black         -> "\ESC[40m"
    Red           -> "\ESC[41m"
    Green         -> "\ESC[42m"
    Yellow        -> "\ESC[43m"
    Blue          -> "\ESC[44m"
    Magenta       -> "\ESC[45m"
    Cyan          -> "\ESC[46m"
    White         -> "\ESC[47m"
    BrightBlack   -> "\ESC[100m"
    BrightRed     -> "\ESC[101m"
    BrightGreen   -> "\ESC[102m"
    BrightYellow  -> "\ESC[103m"
    BrightBlue    -> "\ESC[104m"
    BrightMagenta -> "\ESC[105m"
    BrightCyan    -> "\ESC[106m"
    BrightWhite   -> "\ESC[107m"
    Rgb r g b    -> "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ reset

clearScreen :: AppM e s ()
clearScreen = liftIO $ putStr "\ESC[2J\ESC[H"

enterAltScreen :: AppM e s ()
enterAltScreen = do
  liftIO $ do 
    putStr "\ESC[?1049h"
    hFlush stdout

exitAltScreen :: AppM e s ()
exitAltScreen = do
  liftIO $ do 
    putStr "\ESC[?1049l"
    hFlush stdout
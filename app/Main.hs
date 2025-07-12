module Main where

import App
import Utils

newtype Env = Env {
  colored :: Bool
}

newtype AppState = AppState {
  counter :: Int
}

app :: AppM Env AppState ()
app = do
  enterAltScreen
  appLoop

appLoop :: AppM Env AppState ()
appLoop = do
  -- Acquire data
  state <- get
  env <- ask

  clearScreen
  liftIO $ putStrLn $ 
    if colored env
    then bold (fgcolor BrightRed "Counter: ") ++ fgcolor BrightGreen (show (counter state))
    else "Counter: " ++ show (counter state)

  char <- liftIO getChar
  case char of
    '+' -> modify (const AppState { counter = counter state + 1 }) >> appLoop
    '-' -> modify (const AppState { counter = counter state - 1 }) >> appLoop
    'q' -> do
      exitAltScreen
      return ()
    _ -> appLoop

main :: IO ()
main = do
  let env = Env {
    colored = True
  }

  let initialState = AppState {
    counter = 0
  }

  (_, finalState) <- runAppM app env initialState

  putStrLn $ "Final state: "++show (counter finalState)
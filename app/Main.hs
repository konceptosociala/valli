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
  state <- get
  env <- ask

  clearScreen
  liftIO $ putStrLn $ "Counter: " ++ show (counter state)
  char <- liftIO getChar
  case char of
    '+' -> modify (const AppState { counter = counter state + 1 }) >> app
    '-' -> modify (const AppState { counter = counter state - 1 }) >> app
    'q' -> return ()
    _ -> app

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
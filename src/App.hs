module App (
  AppM,
  liftIO, runAppM, get, put, modify, ask
) where

-- Application monad
-- `e` is an environment
-- `s is a mutable state
newtype AppM e s a = AppM {
  -- Run app with the environment and the state
  -- and return result with updated state
  runAppM :: e -> s -> IO (a, s)
}

-- Implement functor for AppM
-- so you can apply pure functions to
-- values inside it
instance Functor (AppM e s) where
  fmap :: (a -> b) -> AppM e s a -> AppM e s b
  fmap f (AppM runApp) = AppM $ \env state -> do
    -- run app and acquire new state
    (a, newState) <- runApp env state
    -- Return updated result and state wrapped in IO
    pure (f a, newState)

-- Implement applicative functor for AppM
-- so you can apply pure functions with N>1 
-- arguments to it
instance Applicative (AppM e s) where
  -- pure function:
  -- * doesn't touch environment (pure doesn't rely on input)
  -- * doesn't change state
  pure :: a -> AppM e s a
  pure a = AppM $ \_ state -> pure (a, state)

  -- applicative functor
  (<*>) :: AppM e s (a -> b) -> AppM e s a -> AppM e s b
  (<*>) (AppM mf) (AppM ma) = AppM $ \env state0 -> do
    -- extract monadic function
    (f, state1) <- mf env state0
    -- extract monadic value
    (a, state2) <- ma env state1
    -- apply pure function
    pure (f a, state2)

-- Implement monad for AppM
-- so you can apply function a -> m b
-- to it
instance Monad (AppM e s) where
  (>>=) :: AppM e s a -> (a -> AppM e s b) -> AppM e s b
  (>>=) (AppM f) g = AppM $ \env state0 -> do
    -- extract monadic value
    (a, state1) <- f env state0
    -- run app
    runAppM (g a) env state1

-- function to perform IO operation 
-- inside AppM monad context
liftIO :: IO a -> AppM e s a
liftIO io = AppM $ \_ state -> do
  -- get pure value a from IO action
  a <- io
  -- apply pure function to this value
  pure (a, state)

-- get current state without modifying it
get :: AppM e s s
get = AppM $ \_ state -> pure (state, state)

-- put state to app
put :: s -> AppM e s ()
put newState = AppM $ \_ _ -> pure ((), newState)

-- get state and modify it
modify :: (s -> s) -> AppM e s ()
modify f = do
  state <- get
  put (f state)

-- get application environment
ask :: AppM e s e
ask = AppM $ curry pure
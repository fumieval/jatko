{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Jatko.Syntax.Offside where
import Control.Applicative
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Data.Char
import Control.Monad.Reader.Class as Class
import Control.Monad.State.Class as Class
import Control.Monad.Writer.Class as Class
import Control.Monad.State.Strict

runOffside :: Monad m => Int -> Offside m a -> m a
runOffside n (Offside m) = evalStateT m n

-- | This is a parser transformer you can use to disable the automatic trailing
-- newline (but not whitespace-in-general) consumption of a Token parser.
newtype Offside m a = Offside { unOffside :: StateT Int m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,CharParsing)

currentIndent :: Monad m => Offside m Int
currentIndent = Offside get

withIndent :: Monad m => Offside m a -> Offside m a
withIndent (Offside m) = Offside $ do
  n <- get
  put (n + 1)
  a <- m
  put n
  return a


instance (MonadPlus m, Parsing m) => Parsing (Offside m) where
  try (Offside m) = Offside $ try m
  {-# INLINE try #-}
  Offside m <?> l = Offside $ m <?> l
  {-# INLINE (<?>) #-}
  unexpected = Offside . unexpected
  {-# INLINE unexpected #-}
  eof = Offside eof
  {-# INLINE eof #-}
  notFollowedBy (Offside m) = Offside $ notFollowedBy m
  {-# INLINE notFollowedBy #-}

instance MonadTrans Offside where
  lift = Offside . lift
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (Offside m) where
  get = lift Class.get
  {-# INLINE get #-}
  put = lift . Class.put
  {-# INLINE put #-}

instance MonadReader e m => MonadReader e (Offside m) where
  ask = lift Class.ask
  {-# INLINE ask #-}
  local f = Offside . Class.local f . unOffside
  {-# INLINE local #-}

instance MonadWriter e m => MonadWriter e (Offside m) where
  tell = lift . Class.tell
  {-# INLINE tell #-}
  listen = Offside . Class.listen . unOffside
  {-# INLINE listen #-}
  pass = Offside . Class.pass . unOffside
  {-# INLINE pass #-}

blockSome :: (MonadPlus m, CharParsing m) => Offside m a -> Offside m [a]
blockSome m = some $ do
  n <- currentIndent
  replicateM_ n space <?> "indentation"
  skipMany (satisfy $ \c -> c /= '\n' && isSpace c)
  a <- m
  _ <- newline
  return a

instance (MonadPlus m, TokenParsing m) => TokenParsing (Offside m) where
  nesting (Offside m) = Offside (nesting m)
  {-# INLINE nesting #-}
  someSpace = do
    skipMany (satisfy $ \c -> c /= '\n' && isSpace c)
    (<|> pure ()) $ try $ do
      _ <- some newline
      n <- currentIndent
      replicateM_ n space <?> "indentation"
      ss <- many (satisfy $ \c -> c /= '\n' && isSpace c)
      Offside $ put $ n + length ss + 1
  {-# INLINE someSpace #-}
  semi      = Offside semi
  {-# INLINE semi #-}
  highlight h (Offside m) = Offside (highlight h m)
  {-# INLINE highlight #-}

  token p = do
    n <- currentIndent
    r <- p
    someSpace
    Offside $ put n
    return r

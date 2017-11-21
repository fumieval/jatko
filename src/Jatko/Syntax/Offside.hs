{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Jatko.Syntax.Offside where
import Control.Applicative
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Data.Char
import Control.Monad.Reader as Class
import Control.Monad.State.Class as Class
import Control.Monad.Writer.Class as Class

runOffside :: Offside m a -> m a
runOffside (Offside m) = runReaderT m 0

-- | This is a parser transformer you can use to disable the automatic trailing
-- newline (but not whitespace-in-general) consumption of a Token parser.
newtype Offside m a = Offside { unOffside :: ReaderT Int m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,CharParsing)

currentIndent :: Monad m => Offside m Int
currentIndent = Offside ask

withIndent :: Monad m => Offside m a -> Offside m a
withIndent (Offside m) = Offside $ local (+1) m

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
  local f = Offside . mapReaderT (Class.local f) . unOffside
  {-# INLINE local #-}

instance MonadWriter e m => MonadWriter e (Offside m) where
  tell = lift . Class.tell
  {-# INLINE tell #-}
  listen = Offside . Class.listen . unOffside
  {-# INLINE listen #-}
  pass = Offside . Class.pass . unOffside
  {-# INLINE pass #-}

instance (MonadPlus m, TokenParsing m) => TokenParsing (Offside m) where
  nesting (Offside m) = Offside (nesting m)
  {-# INLINE nesting #-}
  someSpace = do
    n <- currentIndent
    replicateM_ n space
    skipMany (satisfy $ \c -> c /= '\n' && isSpace c)
  {-# INLINE someSpace #-}
  semi      = Offside semi
  {-# INLINE semi #-}
  highlight h (Offside m) = Offside (highlight h m)
  {-# INLINE highlight #-}

  token p = someSpace *> p

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.Lisp
  ( moduleToLisp
  ) where

import Control.Applicative
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class

import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

data DialectSpecifics = DialectSpecifics {
}

data Lisp = Lisp

-- |
-- Generate code in the simplified Lisp intermediate representation for all declarations in a
-- module.
--
moduleToLisp :: forall m. (Applicative m, Monad m, MonadReader Options m, MonadSupply m)
           => Module Ann -> Maybe Lisp -> DialectSpecifics -> m [Lisp]
moduleToLisp (Module coms mn imps exps foreigns decls) foreign dialect = undefined

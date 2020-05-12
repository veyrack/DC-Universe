module Exception where

import Control.Exception

data CustomException = InvalidMapException
                    | Autreexception
                    deriving(Show)
instance Exception CustomException
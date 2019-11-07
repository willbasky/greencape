module Util
       ( makeClassWithLenses
       ) where


--
-- Template Haskell
import Language.Haskell.TH
import Lens.Micro.Platform
-- import Language.Haskell.TH.Datatype


makeClassWithLenses :: Name -> DecsQ
makeClassWithLenses = makeLensesWith
  (classyRules
     & lensField .~ (\_ _ n -> [TopName (mkName ('_':nameBase n))])
     & lensClass .~ (\n -> Just ( mkName (nameBase n ++ "Lenses")
                                , mkName ('_':nameBase n))))



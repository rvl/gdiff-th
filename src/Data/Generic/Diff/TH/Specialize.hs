module Data.Generic.Diff.TH.Specialize where
import Language.Haskell.TH
import Data.Generics.Uniplate.Data (childrenBi, transformBi, transformBiM)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.List  (nub)
import Language.Haskell.TH.Ppr (split)
import Data.Traversable (traverse)
import Control.Arrow (first)
import Language.Haskell.TH.ExpandSyns (expandSyns)

specialize :: Name -> Q [(Type, Dec)]
specialize n = nub <$> evalStateT (specializeChildDecs [] n) []

type Context = StateT [([Type], Name)] Q

--This is the main recursive function
specializeChildDecs :: [Type] -> Name -> Context [(Type, Dec)]
specializeChildDecs args n = do
	--this is where I recurse
    let go x = case x of
          a@(AppT _ _) -> uncurry specializeChildDecs . collectArgs $ a
          ConT conName -> specializeChildDecs [] conName
          TupleT 0     -> fmap (maybeToList . fmap (ConT $ mkName "()",))
                            (reifyDecOnce [] $ mkName "()")
          _            -> return []

    mdec <- reifyDecOnce args n
    case mdec of
        Just dec -> do
            let sdec = substTypes args dec
            children <- fmap concat . sequence $ [go t | t <- childrenBi sdec]
            return $ (foldl AppT (ConT n) args, sdec) : children
        Nothing  -> return []



--Only look up the [Type] Name combos once
--I also expand the type synonym here
reifyDecOnce :: [Type]-> Name -> Context (Maybe Dec)
reifyDecOnce ts n = do
    env <- get
    if (ts, n) `elem` env
        then return Nothing
        else do
          modify ((ts, n):)
          lift $ traverse expandTypes =<< reifyDec n

reifyDec :: Name -> Q (Maybe Dec)
reifyDec n = do
    info <- reify n
    case info of
        TyConI dec -> return $ Just dec
        _          -> return Nothing

substTypes :: [Type] -> Dec -> Dec
substTypes ts dec = result where
   tyvars = map getTyName . getTyVars $ dec
   m = zip tyvars ts

   go x = case x of
     oldT@(VarT n) -> fromMaybe oldT (lookup n m)
     t -> t

   result = transformBi go dec
--   result = transformBi (subst' $ map (second Just) m) dec

expandTypes :: Dec -> Q Dec
expandTypes = transformBiM expandSyns


------------------------------------------------------------------------------
--                                Utils                                     --
------------------------------------------------------------------------------

getTyVars :: Dec -> [TyVarBndr]
getTyVars x = case x of
    DataD _ _ tys _ _ -> tys
    NewtypeD _ _ tys _ _ -> tys
    TySynD _ tys _ -> tys
    ClassD _ _ tys _ _ -> tys
    FamilyD _ _ tys _ -> tys
    _ -> []

getTyName :: TyVarBndr -> Name
getTyName x = case x of
    PlainTV n    -> n
    KindedTV n _ -> n

getTypeName :: Type -> Name
getTypeName x = case x of
        ConT n -> n
        TupleT c          -> mkName $ "(" ++ replicate (c - 1) ',' ++ ")"
        UnboxedTupleT c   -> mkName $ "(" ++ replicate c ',' ++ ")"
        ListT             -> ''[]
        _ -> error $ show x ++ " is not a ConT"

collectArgs :: Type -> ([Type], Name)
collectArgs = swap . first getTypeName . split

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

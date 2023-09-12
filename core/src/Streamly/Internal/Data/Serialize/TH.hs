{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize.TH
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Serialize.TH
    ( deriveSerialize
    , Config(..)
    , defaultConfig
    , deriveSerializeWith
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.List (foldl')
import Data.Word (Word16, Word32, Word64, Word8)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Streamly.Internal.Data.Serialize.Type

import Streamly.Internal.Data.Unbox.TH
    ( DataCon(..)
    , DataType(..)
    , appsT
    , plainInstanceD
    , reifyDataType
    )

import Streamly.Internal.Data.Serialize.TH.Bottom
import Streamly.Internal.Data.Serialize.TH.Common

--------------------------------------------------------------------------------
-- Domain specific helpers
--------------------------------------------------------------------------------

exprGetSize :: Q Exp -> (Int, Type) -> Q Exp
exprGetSize acc (i, _) = [|size $(acc) $(varE (mkFieldName i))|]

getTagSize :: Int -> Int
getTagSize numConstructors
    | numConstructors == 1 = 0
    | fromIntegral (maxBound :: Word8) >= numConstructors = 1
    | fromIntegral (maxBound :: Word16) >= numConstructors = 2
    | fromIntegral (maxBound :: Word32) >= numConstructors = 4
    | fromIntegral (maxBound :: Word64) >= numConstructors = 8
    | otherwise = error "Too many constructors"

getTagType :: Int -> Name
getTagType numConstructors
    | numConstructors == 1 = error "No tag for 1 constructor"
    | fromIntegral (maxBound :: Word8) >= numConstructors = ''Word8
    | fromIntegral (maxBound :: Word16) >= numConstructors = ''Word16
    | fromIntegral (maxBound :: Word32) >= numConstructors = ''Word32
    | fromIntegral (maxBound :: Word64) >= numConstructors = ''Word64
    | otherwise = error "Too many constructors"

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

getNameBaseLen :: Name -> Word8
getNameBaseLen cname =
    let x = length (nameBase cname)
     in if x > 63
        then error "Max Constructor Len: 63 characters"
        else fromIntegral x

conEncLen :: Name -> Word8
conEncLen cname = getNameBaseLen cname + 1

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

mkSizeOfExpr :: Bool -> TypeOfType -> Q Exp
mkSizeOfExpr True tyOfTy =
    case tyOfTy of
        UnitType cname ->
            lamE
                [varP _acc, wildP]
                [|$(varE _acc) + $(litIntegral (conEncLen cname))|]
        TheType con ->
            lamE
                [varP _acc, varP _x]
                (caseE (varE _x) [matchCons (varE _acc) con])
        MultiType constructors -> sizeOfHeadDt constructors

    where

    sizeOfFields acc fields =
        foldl' exprGetSize acc $ zip [0..] fields

    matchCons acc (SimpleDataCon cname fields) =
        let a = litIntegral (conEncLen cname)
            b = sizeOfFields acc (map snd fields)
            expr = [|$(a) + $(b)|]
         in matchConstructor cname (length fields) expr

    sizeOfHeadDt cons =
        let acc = [|$(varE _acc)|]
         in lamE
                [varP _acc, varP _x]
                (caseE (varE _x) (fmap (matchCons acc) cons))

mkSizeOfExpr False tyOfTy =
    case tyOfTy of
        UnitType _ -> lamE [varP _acc, wildP] [|$(varE _acc) + 1|]
        TheType con ->
            lamE
                [varP _acc, varP _x]
                (caseE (varE _x) [matchCons (varE _acc) con])
        MultiType constructors -> sizeOfHeadDt constructors

    where

    tagSizeExp numConstructors =
        litE (IntegerL (fromIntegral (getTagSize numConstructors)))

    -- XXX fields of the same type can be folded together, will reduce the code
    -- size when there are many fields of the same type.
    -- XXX const size fields can be calculated statically.
    -- XXX This can result in large compilation times due to nesting when there
    -- are many constructors. We can create a list and sum the list at run time
    -- to avoid that depending on the number of constructors. Or using a let
    -- statement for each case may help?
    -- appE (varE 'sum) (listE (acc : map (exprGetSize (litE (IntegerL 0))) (zip [0..] fields)))
    sizeOfFields acc fields =
        foldl' exprGetSize acc $ zip [0..] fields

    matchCons acc (SimpleDataCon cname fields) =
        let expr = sizeOfFields acc (map snd fields)
         in matchConstructor cname (length fields) expr

    -- XXX We fix VarSize for simplicity. Should be changed later.
    sizeOfHeadDt cons =
        let numCons = length cons
            acc = [|$(varE _acc) + $(tagSizeExp numCons)|]
         in lamE
                [varP _acc, varP _x]
                (caseE (varE _x) (fmap (matchCons acc) cons))

mkSizeDec :: Config -> Type -> [DataCon] -> Q [Dec]
mkSizeDec (Config {..}) headTy cons = do
    -- INLINE on sizeOf actually worsens some benchmarks, and improves none
    sizeOfMethod <- mkSizeOfExpr constructorTagAsString (typeOfType headTy cons)
    pure
        [ PragmaD (InlineP 'size inlineSize FunLike AllPhases)
        , FunD 'size [Clause [] (NormalB sizeOfMethod) []]
        ]

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

mkDeserializeExpr :: Bool -> Type -> TypeOfType -> Q Exp
mkDeserializeExpr True headTy tyOfTy =
    case tyOfTy of
        UnitType cname -> deserializeConsExpr [SimpleDataCon cname []]
        TheType con -> deserializeConsExpr [con]
        MultiType cons -> deserializeConsExpr cons

  where

    deserializeConsExpr cons = do
        conLen <- newName "conLen"
        off1 <- newName "off1"
        [|do ($(varP off1), $(varP conLen) :: Word8) <-
                 deserialize
                     $(varE _initialOffset)
                     $(varE _arr)
                     $(varE _endOffset)
             $(multiIfE (map (guardCon conLen off1) cons ++ [catchAll]))|]

    catchAll =
        normalGE
            [|True|]
            [|error
               ("Found invalid tag while peeking (" ++
                   $(lift (pprint headTy)) ++ ")")|]

    guardCon conLen off con@(SimpleDataCon cname _) = do
        let lenCname = getNameBaseLen cname
            tag = map c2w (nameBase cname)
        normalGE
            [|($(litIntegral lenCname) == $(varE conLen))
                   && $(xorCmp tag off _arr)|]
            [|let $(varP (makeI 0)) = $(varE off) + $(litIntegral lenCname)
               in $(mkDeserializeExprOne con)|]

mkDeserializeExpr False headTy tyOfTy =
    case tyOfTy of
        -- Unit constructor
        UnitType cname ->
            [|pure ($(varE _initialOffset) + 1, $(conE cname))|]
        -- Product type
        TheType con ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (mkDeserializeExprOne con)
        -- Sum type
        MultiType cons -> do
            let lenCons = length cons
                tagType = getTagType lenCons
            doE
                [ bindS
                      (tupP [varP (mkName "i0"), varP _tag])
                      [|deserialize $(varE _initialOffset) $(varE _arr) $(varE _endOffset)|]
                , noBindS
                      (caseE
                           (sigE (varE _tag) (conT tagType))
                           (map peekMatch (zip [0 ..] cons) ++ [peekErr]))
                ]
  where
    peekMatch (i, con) =
        match (litP (IntegerL i)) (normalB (mkDeserializeExprOne con)) []
    peekErr =
        match
            wildP
            (normalB
                -- XXX Print the tag
                 [|error
                       ("Found invalid tag while peeking (" ++
                        $(lift (pprint headTy)) ++ ")")|])
            []

mkDeserializeDec :: Config -> Type -> [DataCon] -> Q [Dec]
mkDeserializeDec (Config {..}) headTy cons = do
    peekMethod <-
        mkDeserializeExpr constructorTagAsString headTy (typeOfType headTy cons)
    pure
        [ PragmaD (InlineP 'deserialize inlineDeserialize FunLike AllPhases)
        , FunD
              'deserialize
              [ Clause
                    (if isUnitType cons && not constructorTagAsString
                         then [VarP _initialOffset, WildP, WildP]
                         else [VarP _initialOffset, VarP _arr, VarP _endOffset])
                    (NormalB peekMethod)
                    []
              ]
        ]

--------------------------------------------------------------------------------
-- Poke
--------------------------------------------------------------------------------

mkSerializeExprTag :: Name -> Int -> Q Exp
mkSerializeExprTag tagType tagVal =
    [|serialize
          $(varE _initialOffset)
          $(varE _arr)
          $((sigE (litE (IntegerL (fromIntegral tagVal))) (conT tagType)))|]

mkSerializeExpr :: Bool -> TypeOfType -> Q Exp
mkSerializeExpr True tyOfTy =
    case tyOfTy of
        -- Unit type
        UnitType cname ->
            caseE
                (varE _val)
                [serializeDataCon (SimpleDataCon cname [])]
        -- Product type
        (TheType con) ->
            caseE
                (varE _val)
                [serializeDataCon con]
        -- Sum type
        (MultiType cons) ->
            caseE
                (varE _val)
                (map serializeDataCon cons)

    where

    serializeDataCon (SimpleDataCon cname fields) = do
        let tagLen8 = getNameBaseLen cname
            conEnc = tagLen8 : map c2w (nameBase cname)
        matchConstructor
            cname
            (length fields)
            (doE [ bindS
                       (varP (mkName "i0"))
                       (serializeW8List _initialOffset _arr conEnc)
                 , noBindS (mkSerializeExprFields fields)
                 ])

mkSerializeExpr False tyOfTy =
    case tyOfTy of
        -- Unit type
        UnitType _ -> [|pure ($(varE _initialOffset) + 1)|]
        -- Product type
        (TheType (SimpleDataCon cname fields)) ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (caseE
                     (varE _val)
                     [ matchConstructor
                           cname
                           (length fields)
                           (mkSerializeExprFields fields)
                     ])
        -- Sum type
        (MultiType cons) -> do
            let lenCons = length cons
                tagType = getTagType lenCons
            caseE
                (varE _val)
                (map (\(tagVal, (SimpleDataCon cname fields)) ->
                          matchConstructor
                              cname
                              (length fields)
                              (doE [ bindS
                                         (varP (mkName "i0"))
                                         (mkSerializeExprTag tagType tagVal)
                                   , noBindS (mkSerializeExprFields fields)
                                   ]))
                     (zip [0 ..] cons))

mkSerializeDec :: Config -> Type -> [DataCon] -> Q [Dec]
mkSerializeDec (Config {..}) headTy cons = do
    pokeMethod <-
        mkSerializeExpr constructorTagAsString (typeOfType headTy cons)
    pure
        [ PragmaD (InlineP 'serialize inlineSerialize FunLike AllPhases)
        , FunD
              'serialize
              [ Clause
                    (if isUnitType cons && not constructorTagAsString
                         then [VarP _initialOffset, WildP, WildP]
                         else [VarP _initialOffset, VarP _arr, VarP _val])
                    (NormalB pokeMethod)
                    []
              ]
        ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | A general function to derive Serialize instances where you can control
-- which Constructors of the datatype to consider and what the Context for the
-- 'Serialize' instance would be.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b
--     = CDTConstructor1
--     | CDTConstructor2 Bool
--     | CDTConstructor3 Bool b
--     deriving (Show, Eq)
-- @
--
-- Usage:
-- @
-- $(deriveSerializeInternal
--       defaultConfig
--       [AppT (ConT ''Serialize) (VarT (mkName "b"))]
--       (AppT
--            (AppT (ConT ''CustomDataType) (VarT (mkName "a")))
--            (VarT (mkName "b")))
--       [ DataCon 'CDTConstructor1 [] [] []
--       , DataCon 'CDTConstructor2 [] [] [(Nothing, (ConT ''Bool))]
--       , DataCon
--             'CDTConstructor3
--             []
--             []
--             [(Nothing, (ConT ''Bool)), (Nothing, (VarT (mkName "b")))]
--       ])
-- @
deriveSerializeInternal ::
       Config -> Cxt -> Type -> [DataCon] -> Q [Dec]
deriveSerializeInternal conf preds headTy cons = do
    sizeDec <- mkSizeDec conf headTy cons
    peekDec <- mkDeserializeDec conf headTy cons
    pokeDec <- mkSerializeDec conf headTy cons
    let methods = concat [sizeDec, peekDec, pokeDec]
    return [plainInstanceD preds (AppT (ConT ''Serialize) headTy) methods]

-- | Similar to 'deriveSerialize,' but take a 'Config' to control how
-- the instance is generated.
--
-- Usage: @$(deriveSerializeWith config ''CustomDataType)@
deriveSerializeWith :: Config -> Name -> Q [Dec]
deriveSerializeWith conf@(Config {..}) name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (filterOutVars (dtTvs dt))
        headTy = appsT (ConT name) (map substituteVar (dtTvs dt))
        cons = dtCons dt
    deriveSerializeInternal conf preds headTy cons

    where
    allUnconstrainedTypeVars =
        unconstrained ++ map fst specializations
    filterOutVars vs =
        map mkName
            $ filter (not . flip elem allUnconstrainedTypeVars)
            $ map nameBase vs
    substituteVar v =
        case lookup (nameBase v) specializations of
            Nothing -> VarT v
            Just ty -> ty

    unboxPred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Serialize) ty
#else
        ClassP ''Serialize [ty]
#endif

-- | Template haskell helper to create instances of 'Serialize' automatically.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b c = ...
-- @
--
-- Usage: @$(deriveSerialize ''CustomDataType)@
--
-- Note: All type variables automatcally get an "Serialize" constraint.
-- The derived code will look like the following,
-- @
-- instance (Serialize a, Serialize b, Serialize c) => Serialize (CustomDataType a b c) where
-- ...
-- @
--
-- To control which type variables don't get the Serialize constraint, use
-- 'deriveSerializeWith'.
--
-- >>> import qualified Streamly.Internal.Data.Serialize.TH as Serialize
-- >>> deriveSerialize = Serialize.deriveSerializeWith Serialize.defaultConfig
deriveSerialize :: Name -> Q [Dec]
deriveSerialize name = deriveSerializeWith defaultConfig name

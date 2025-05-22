{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Biparser.Combinators where

import Biparser.Internal
import Control.Lens hiding (cons, from, iso, to, uncons)
import qualified Data.List as List
import Data.Maybe
import PErr
import Utils
import Prelude hiding (print)

-- Derived combinators

-- | @eof@ expects the end-of-file.
eof :: Biparser () x x
eof = notFollowedBy_ $ \_ -> do
  s <- use _1
  guardError (not $ null s) ""

-- | Combine a munch and validate on the result.
munch' :: (Char -> Bool) -> (String -> Bool) -> Biparser () () String
munch' munchF validF = munch munchF >#>> validate validF

-- | Transform the input/output using an isomorphism.
iso :: (x -> y) -> (y -> x) -> Biparser () x y
iso f g = partialIso (Right . f) (Right . g)

-- | @constant x p@ outputs @x@ when parsing. The printer consumes @x@ but fails if @p x@ is @False@.
constant :: x -> (x -> Bool) -> Biparser () () x
constant x p = partialIso to from
  where
    to = const $ Right x
    from x' = if p x' then Right () else Left "constant.print: unexpected value"

-- | @constant' x@ outputs @x@ when parsing, and consumes it when printing, but fails if the input is not equal to @x@.
constant' :: (Eq x) => x -> Biparser () () x
constant' x = constant x (== x)

-- | @known y p@ consumes @y@ when parsing, but fails if @p y@ is @False@. The printer always outputs @y@.
known :: y -> (y -> Bool) -> Biparser () y ()
known y p = partialIso to from
  where
    to y' = if p y' then Right () else Left "known.parse: unexpected value"
    from () = Right y

-- | @known' y@ consumes @y@ when parsing, but fails if its input is not equal to @y@. The printer always outputs @y@.
known' :: (Eq y) => y -> Biparser () y ()
known' y = known y (== y)

infixl 6 >#>>, >>#>

-- | @f >#>> g@ is @f >>> g@, but the unit complement of @f@ is discarded, and the complement of @g@ is kept.
(>#>>) :: Biparser () x y -> Biparser c y z -> Biparser c x z
f >#>> g = mapC unitL' unitL (f >>> g)

-- | @f >>#> g@ is @f >>> g@, but the unit complement of @g@ is discarded, and the complement of @f@ is kept.
(>>#>) :: Biparser c x y -> Biparser () y z -> Biparser c x z
f >>#> g = mapC unitR' unitR (f >>> g)

-- | Make the complement optional by mapping it into @Right@.
relaxCr :: Biparser c2 x y -> Biparser (Either c1 c2) x y
relaxCr p = mapC swapEither swapEither (relaxCl p)

-- | Wrap the complement in @Maybe@.
relaxC :: Biparser c x y -> Biparser (Maybe c) x y
relaxC = mapC leftToMaybe maybeToLeft . relaxCl

-- | @validate p@ validates that the input satisfies a predicate @p@.
validate :: (x -> Bool) -> Biparser () x x
validate p = partialIso check check
  where
    check x
      | p x = Right x
      | otherwise = Left "validate: invalid value"

-- | Consume one item from the input which satisfies the predicate.
satisfy :: (Char -> Bool) -> Biparser () () Char
satisfy p = item >#>> validate p

-- | @second p@ executes @p@ in the second component of the input/output tuples.
second :: Biparser c y1 y2 -> Biparser c (x, y1) (x, y2)
second p = swapIso >#>> first p >>#> swapIso
  where
    swapIso = iso swap swap

infixl 7 ***, *#**, **#*, <<*>>, *>>, <<*, <<#*>>, <<*#>>, #*>>, *#>>, <<*#, <<#*

-- | @p *** q@ executes @p@ in the first component and @q@ in the second component, in that order.
(***) :: Biparser c1 x1 y1 -> Biparser c2 x2 y2 -> Biparser (c1, c2) (x1, x2) (y1, y2)
p *** q = first p >>> second q

(*#**) :: Biparser () x1 y1 -> Biparser c x2 y2 -> Biparser c (x1, x2) (y1, y2)
p *#** q = mapC unitL' unitL (p *** q)

(**#*) :: Biparser c x1 y1 -> Biparser () x2 y2 -> Biparser c (x1, x2) (y1, y2)
p **#* q = mapC unitR' unitR (p *** q)

(<<*>>) :: Biparser c1 () y1 -> Biparser c2 () y2 -> Biparser (c1, c2) () (y1, y2)
p <<*>> g = dupl >#>> (p *** g)
  where
    dupl = iso (\() -> ((), ())) (\((), ()) -> ())

(*>>) :: Biparser c1 () () -> Biparser c2 x y -> Biparser (c1, c2) x y
p *>> q = iso unitL unitL' >#>> (p *** q) >>#> iso unitL' unitL

(<<*) :: Biparser c1 x y -> Biparser c2 () () -> Biparser (c1, c2) x y
p <<* q = iso unitR unitR' >#>> (p *** q) >>#> iso unitR' unitR

(<<#*>>) :: Biparser () () x -> Biparser c2 () y -> Biparser c2 () (x, y)
p <<#*>> q = mapC unitL' unitL (p <<*>> q)

(<<*#>>) :: Biparser c1 () x -> Biparser () () y -> Biparser c1 () (x, y)
p <<*#>> q = mapC unitR' unitR (p <<*>> q)

(#*>>) :: Biparser () () () -> Biparser c2 x y -> Biparser c2 x y
p #*>> q = mapC unitL' unitL (p *>> q)

(*#>>) :: Biparser c1 () () -> Biparser () x y -> Biparser c1 x y
p *#>> q = mapC unitR' unitR (p *>> q)

(<<*#) :: Biparser c1 x y -> Biparser () () () -> Biparser c1 x y
p <<*# q = mapC unitR' unitR (p <<* q)

(<<#*) :: Biparser () x y -> Biparser c2 () () -> Biparser c2 x y
p <<#* q = mapC unitL' unitL (p <<* q)

-- | Consume character @c@ from the input, or fail if the next character doesn't match it.
char :: Char -> Biparser () () ()
char c = item >#>> known' c

-- | Same as @char@ but also produces the character in the output.
char' :: Char -> Biparser () () Char
char' c = char c >#>> constant' c

-- | Consume the specified string from the input.
string :: String -> Biparser () () ()
string = foldr (\c r -> char c #*>> r) identity

-- | Same as @string@ but also produces the string in the output.
string' :: String -> Biparser () () String
string' s = string s >#>> constant' s

infixl 5 <++>

-- | Biased choice where the branches can have a different complement.
-- - ParsePrint requires ParsePrintConsistent
-- - PrintParse requires PrintParseConsistent
(<++>) :: Biparser c1 x y -> Biparser c2 x y -> Biparser (Either c1 c2) x y
p <++> q = relaxCl p <+> relaxCr q

-- | @many p@ parses zero or more occurrences of @p@.
many :: Biparser c () x -> Biparser [Maybe c] () [x]
-- Note the "notFollowedBy (parse p)": it is necessary to guarantee round-trip properties,
-- since the two branches would otherwise accept common input.
many p = some p <+> notFollowedBy (parse p) #*>> mapC (\() -> []) (\_ -> ()) mkNil

-- | @some p@ parses one or more occurrences of @p@.
some :: Biparser c () x -> Biparser [Maybe c] () [x]
some p = mapC cons uncons ((relaxC p <<*>> many p) >>#> mkCons)
  where
    cons :: (Maybe c, [Maybe c]) -> [Maybe c]
    cons (Nothing, _) = [Nothing]
    cons (Just c, cs) = Just c : cs
    uncons :: [Maybe c] -> (Maybe c, [Maybe c])
    uncons [] = (Nothing, [])
    uncons (c : cs) = (c, cs)

-- | Like @many p@, but discards the unit complements produces by the @p@s.
many' :: Biparser () () x -> Biparser () () [x]
many' p = some' p <+> notFollowedBy (parse p) #*>> mkNil

-- | Like @some p@, but discards the unit complements produces by the @p@s.
some' :: Biparser () () x -> Biparser () () [x]
some' p = (p <<#*>> many' p) >>#> mkCons

-- | Like @many' p@ but uses limited lookahead to guarantee consistency. That means @p@ may not use the position.
many'_ :: Biparser () () x -> Biparser () () [x]
many'_ p = some'_ p <+> notFollowedBy_ (parse p) #*>> mkNil

-- | Like @some' p@ but uses limited lookahead to guarantee consistency. That means @p@ may not use the position.
some'_ :: Biparser () () x -> Biparser () () [x]
some'_ p = (p <<#*>> many'_ p) >>#> mkCons

-- | Combinator for making constructor biparsers using a name and a prism.
mk :: String -> Prism' y x -> Biparser () x y
mk cName p = partialIso (Right . review p) (maybe (Left msg) Right . preview p)
  where
    msg :: PErr
    msg = Error ("mk" ++ cName ++ ": expecting constructor " ++ cName)

prNil :: Prism' [x] ()
prNil = prism' (const []) (\l -> if null l then Just () else Nothing)

prCons :: Prism' [x] (x, [x])
prCons = prism' (uncurry (:)) List.uncons

mkNil :: Biparser () () [x]
mkNil = mk "Nil" prNil

mkNil'' :: Biparser () () [x]
mkNil'' = partialIso wrap unwrap
  where
    wrap :: () -> Either PErr [x]
    wrap () = Right []
    unwrap :: [x] -> Either PErr ()
    unwrap [] = Right ()
    unwrap _ = Left "mkNil.unwrap: got nonempty list"

mkCons :: Biparser () (x, [x]) [x]
mkCons = mk "Cons" prCons

mkCons'' :: Biparser () (x, [x]) [x]
mkCons'' = partialIso wrap unwrap
  where
    wrap :: (x, [x]) -> Either PErr [x]
    wrap (x, xs) = Right (x : xs)
    unwrap :: [x] -> Either PErr (x, [x])
    unwrap [] = Left "mkCons.unwrap: got empty list"
    unwrap (x : xs) = Right (x, xs)

-- | @sepBy0 p sep@ parses zero or more occurrences of @p@, separated by @sep@.
sepBy0 :: Biparser c () x -> Biparser csep () () -> Biparser [(Maybe c, Maybe csep)] () [x]
sepBy0 p sep = sepBy1 p sep <+> notFollowedBy (parse p) #*>> mapC (\() -> []) (\_ -> ()) mkNil

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
sepBy1 :: Biparser c () x -> Biparser csep () () -> Biparser [(Maybe c, Maybe csep)] () [x]
sepBy1 p sep = mapC wrap unwrap (relaxC p <<*>> rest) >>#> mkCons
  where
    rest = relaxC sep *>> sepBy1 p sep <+> notFollowedBy (parse (sep <<*>> p)) #*>> empty
    empty = mapC (\_ -> (Nothing, [])) (\_ -> ()) (constant [] null)
    wrap (c, (csep, cs)) = (c, csep) : cs
    unwrap [] = (Nothing, (Nothing, []))
    unwrap ((c, csep) : cs) = (c, (csep, cs))

-- | @sepBy2 p sep@ parses two or more occurrences of @p@, separated by @sep@.
sepBy2 :: Biparser c () x -> Biparser csep () () -> Biparser [(Maybe c, Maybe csep)] () [x]
sepBy2 p sep = mapC wrap unwrap ((relaxC p <<* relaxC sep) <<*>> sepBy1 p sep) >>#> mkCons
  where
    wrap (a, []) = [a]
    wrap (a, as) = a : as
    unwrap [] = ((Nothing, Nothing), [])
    unwrap (a : as) = (a, as)

-- | Like @sepBy1 p sep@ but ignores the unit complements of @sep@.
sepBy1' :: Biparser c () x -> Biparser () () () -> Biparser [Maybe c] () [x]
sepBy1' p sep = mapC f g $ sepBy1 p sep
  where
    f = map fst
    g = map (,Just ())

-- | Like @sepBy1 p sep@ but ignores the unit complements of @p@ and @sep@.
sepBy1'' :: Biparser () () x -> Biparser () () () -> Biparser () () [x]
sepBy1'' p sep = mapC f g $ align h $ sepBy1' p sep
  where
    -- This combinator is special...
    -- Technically, 'g . f == id' is not satisfied for mapC here, and *cannot* be satisfied
    -- because f is not injective. But this is fixed by also considering the align:
    -- Since it is followed by 'align h', consider what happens when printing with...
    -- - Nothing: then align does nothing and sepBy1' is run with complement Nothing also.
    -- - Just c: then align ignores c completely and replaces it using h.
    -- In both cases, g is ignored completely, and h can recover the correct complement despite
    -- the fact that f is not injective, because all the information required to recover it
    -- is available in the input xs.
    f = const ()
    g = error "sepBy1'' never uses the given complement because the align transforms it"
    -- AlignConsistent is satisfied because 'length xs' is always how many 'Just ()'s were parsed.
    -- So all the information to recover the complement is present in the output. This is not usually
    -- the case, but for this combinator it is.
    h xs _ = replicate (length xs) (Just ())

-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([], Nothing)
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([()], Nothing)
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([(),()], Nothing)
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([(),(),()], Nothing)
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([], Just ())
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([()], Just ())
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([(),()], Just ())
-- >>> testPrint (sepBy1'' (char 'a') (char 'b')) ([(),(),()], Just ())
-- Left mkCons: expecting constructor Cons
-- Right ((),("a",[],Ln 1, Col 2))
-- Right ((),("aba",[],Ln 1, Col 4))
-- Right ((),("ababa",[],Ln 1, Col 6))
-- Left mkCons: expecting constructor Cons
-- Right ((),("a",[],Ln 1, Col 2))
-- Right ((),("aba",[],Ln 1, Col 4))
-- Right ((),("ababa",[],Ln 1, Col 6))

-- | @between open close p@ parses @p@ between @open@ and @close@.
between :: Biparser cOpen () () -> Biparser cClose () () -> Biparser c x y -> Biparser (c, (cOpen, cClose)) x y
between open close p = mapC f g $ open *>> p <<* close
  where
    f ((cOpen, c), cClose) = (c, (cOpen, cClose))
    g (c, (cOpen, cClose)) = ((cOpen, c), cClose)

-- | @optional p@ parses @p@ or Nothing.
optional :: Biparser c () x -> Biparser (Maybe c) () (Maybe x)
optional p =
  mapC leftToMaybe maybeToLeft $
    (p >>#> mkJust) </> constant Nothing isNothing

prJust :: Prism' (Maybe x) x
prJust = prism' Just id

mkJust :: Biparser () x (Maybe x)
mkJust = mk "Just" prJust

mkJust'' :: Biparser () x (Maybe x)
mkJust'' = partialIso f g
  where
    f = Right . Just
    g Nothing = Left "mkJust got Nothing"
    g (Just x) = Right x

-- | @option y p@ parses @p@ or defaults to @y@ otherwise.
option :: (Eq y) => y -> Biparser () () y -> Biparser () () y
option y p = p <+> constant' y

-- | @opt p@ parses @p@ or defaults to @mempty@ otherwise.
opt :: (Eq y, Monoid y) => Biparser () () y -> Biparser () () y
opt = option mempty

module System.Terminal.Emulator.Attrs where

import Control.Lens
import Data.Bits
import Data.Word (Word32)
import qualified System.Console.ANSI.Types as SGR

-- | Attrs:
--
-- @
--     00000000 00000000 000000000000uuii
--     ^^ fg ^^ ^^ bg ^^ ^^^^^^^^^^^^^^^^
--
--     ii : ConsoleIntensity (00 = Normal, 01 = Bold, 10 = Faint)
--     uu : Underlining (00 = NoUnderline, 01 = SingleUnderline, 10 = DoubleUnderline)
-- @
type Attrs = Word32

blankAttrs :: Attrs
blankAttrs = 0
{-# INLINE blankAttrs #-}

type Cell = (Char, Attrs)

cellChar :: Lens' Cell Char
cellChar = lens fst (\(_, attrs) c -> (c, attrs))
{-# INLINE cellChar #-}

cellAttrs :: Lens' Cell Attrs
cellAttrs = lens snd (\(char, _) attrs -> (char, attrs))
{-# INLINE cellAttrs #-}

attrsFg :: Lens' Attrs (Maybe (SGR.ColorIntensity, SGR.Color))
attrsFg = lens getter setter
  where
    getter :: Attrs -> Maybe (SGR.ColorIntensity, SGR.Color)
    getter attrs = intToColor (shiftR attrs 24 .&. 0x000000FF)
    setter :: Attrs -> Maybe (SGR.ColorIntensity, SGR.Color) -> Attrs
    setter attrs color = (attrs .&. 0x00FFFFFF) .|. shiftL (colorToInt color) 24
    {-# INLINE getter #-}
    {-# INLINE setter #-}
{-# INLINE attrsFg #-}

attrsBg :: Lens' Attrs (Maybe (SGR.ColorIntensity, SGR.Color))
attrsBg = lens getter setter
  where
    getter :: Attrs -> Maybe (SGR.ColorIntensity, SGR.Color)
    getter attrs = intToColor (shiftR attrs 16 .&. 0x000000FF)
    setter :: Attrs -> Maybe (SGR.ColorIntensity, SGR.Color) -> Attrs
    setter attrs color = (attrs .&. 0xFF00FFFF) .|. shiftL (colorToInt color) 16
    {-# INLINE getter #-}
    {-# INLINE setter #-}
{-# INLINE attrsBg #-}

attrsIntensity :: Lens' Attrs SGR.ConsoleIntensity
attrsIntensity = lens getter setter
  where
    getter :: Attrs -> SGR.ConsoleIntensity
    getter attrs
      | attrs .&. 0x00000003 == 0 = SGR.NormalIntensity
      | attrs .&. 0x00000003 == 1 = SGR.BoldIntensity
      | otherwise = SGR.FaintIntensity
    setter :: Attrs -> SGR.ConsoleIntensity -> Attrs
    setter attrs intensity = ((attrs .&. 0xFFFFFFFC) .|. consoleIntensityToInt intensity)
    {-# INLINE getter #-}
    {-# INLINE setter #-}
{-# INLINE attrsIntensity #-}

attrsUnderline :: Lens' Attrs SGR.Underlining
attrsUnderline = lens getter setter
  where
    getter :: Attrs -> SGR.Underlining
    getter attrs
      | (shiftR attrs 2) .&. 0x00000003 == 0 = SGR.NoUnderline
      | (shiftR attrs 2) .&. 0x00000003 == 1 = SGR.SingleUnderline
      | otherwise = SGR.DoubleUnderline
    setter :: Attrs -> SGR.Underlining -> Attrs
    setter attrs underlining = ((attrs .&. 0xFFFFFFF3) .|. shiftL (underliningToInt underlining) 2)
    {-# INLINE getter #-}
    {-# INLINE setter #-}
{-# INLINE attrsUnderline #-}

intToColor :: Word32 -> Maybe (SGR.ColorIntensity, SGR.Color)
intToColor 0 = Nothing
intToColor 1 = Just (SGR.Dull, SGR.Black)
intToColor 2 = Just (SGR.Dull, SGR.Red)
intToColor 3 = Just (SGR.Dull, SGR.Green)
intToColor 4 = Just (SGR.Dull, SGR.Yellow)
intToColor 5 = Just (SGR.Dull, SGR.Blue)
intToColor 6 = Just (SGR.Dull, SGR.Magenta)
intToColor 7 = Just (SGR.Dull, SGR.Cyan)
intToColor 8 = Just (SGR.Dull, SGR.White)
intToColor 9 = Just (SGR.Vivid, SGR.Black)
intToColor 10 = Just (SGR.Vivid, SGR.Red)
intToColor 11 = Just (SGR.Vivid, SGR.Green)
intToColor 12 = Just (SGR.Vivid, SGR.Yellow)
intToColor 13 = Just (SGR.Vivid, SGR.Blue)
intToColor 14 = Just (SGR.Vivid, SGR.Magenta)
intToColor 15 = Just (SGR.Vivid, SGR.Cyan)
intToColor 16 = Just (SGR.Vivid, SGR.White)
intToColor i = error $ "intToColor: invalid int: " <> show i
{-# INLINE intToColor #-}

colorToInt :: Maybe (SGR.ColorIntensity, SGR.Color) -> Word32
colorToInt Nothing = 0
colorToInt (Just (SGR.Dull, SGR.Black)) = 1
colorToInt (Just (SGR.Dull, SGR.Red)) = 2
colorToInt (Just (SGR.Dull, SGR.Green)) = 3
colorToInt (Just (SGR.Dull, SGR.Yellow)) = 4
colorToInt (Just (SGR.Dull, SGR.Blue)) = 5
colorToInt (Just (SGR.Dull, SGR.Magenta)) = 6
colorToInt (Just (SGR.Dull, SGR.Cyan)) = 7
colorToInt (Just (SGR.Dull, SGR.White)) = 8
colorToInt (Just (SGR.Vivid, SGR.Black)) = 9
colorToInt (Just (SGR.Vivid, SGR.Red)) = 10
colorToInt (Just (SGR.Vivid, SGR.Green)) = 11
colorToInt (Just (SGR.Vivid, SGR.Yellow)) = 12
colorToInt (Just (SGR.Vivid, SGR.Blue)) = 13
colorToInt (Just (SGR.Vivid, SGR.Magenta)) = 14
colorToInt (Just (SGR.Vivid, SGR.Cyan)) = 15
colorToInt (Just (SGR.Vivid, SGR.White)) = 16
{-# INLINE colorToInt #-}

consoleIntensityToInt :: SGR.ConsoleIntensity -> Word32
consoleIntensityToInt SGR.NormalIntensity = 0
consoleIntensityToInt SGR.BoldIntensity = 1
consoleIntensityToInt SGR.FaintIntensity = 2
{-# INLINE consoleIntensityToInt #-}

underliningToInt :: SGR.Underlining -> Word32
underliningToInt SGR.NoUnderline = 0
underliningToInt SGR.SingleUnderline = 1
underliningToInt SGR.DoubleUnderline = 2
{-# INLINE underliningToInt #-}

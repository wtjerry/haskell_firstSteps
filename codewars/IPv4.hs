module IPv4 where
import Data.Word  (Word32)
import Data.Bits
import Data.List

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP = concat . (intersperse ".") . (map show) . to8BitParts

to8BitParts :: Word32 -> [Word32]
to8BitParts w = map (to8Bit w) [3,2,1,0]

to8Bit w i = extractLast8Bits $ shiftR w (i*8)
    where extractLast8Bits = (.&.) (2^8-1)

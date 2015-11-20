module Css.Color where

import Prelude

import Data.Foldable (intercalate)

import Css.Property
import Css.String

data Color = Rgba Int Int Int Int
           | Hsla Number Number Number Number
           | Other Value

instance isStringColor :: IsString Color where
  fromString = Other <<< fromString

instance valColor :: Val Color where
  value (Rgba r g b 255) = Value <<< fromString $ "rgb(" <> intercalate ", " [show r, show g, show b] <> ")"
  value (Rgba r g b a) = Value <<< fromString $ "rgba(" <> intercalate ", " [show r, show g, show b, show a] <> ")"
  value (Hsla h s l 1.0) = Value <<< fromString $ "hsl(" <> intercalate ", " [show h, show s, show l] <> ")"
  value (Hsla h s l a) = Value <<< fromString $ "hsla(" <> intercalate ", " [show h, show s, show l, show a] <> ")"
  value (Other v) = v

rgba :: Int -> Int -> Int -> Int -> Color
rgba = Rgba

rgb :: Int -> Int -> Int -> Color
rgb r g b = Rgba r g b 255

clamp :: Int -> Int
clamp n = if n < 0 then 0 else if n > 255 then 255 else n

aliceblue :: Color
aliceblue = rgb 240 248 255

antiquewhite :: Color
antiquewhite = rgb 250 235 215

aqua :: Color
aqua = rgb 0 255 255

aquamarine :: Color
aquamarine = rgb 127 255 212

azure :: Color
azure = rgb 240 255 255

beige :: Color
beige = rgb 245 245 220

bisque :: Color
bisque = rgb 255 228 196

black :: Color
black = rgb 0   0   0

blanchedalmond :: Color
blanchedalmond = rgb 255 235 205

blue :: Color
blue = rgb 0   0 255

blueviolet :: Color
blueviolet = rgb 138  43 226

brown :: Color
brown = rgb 165  42  42

burlywood :: Color
burlywood = rgb 222 184 135

cadetblue :: Color
cadetblue = rgb 95 158 160

chartreuse :: Color
chartreuse = rgb 127 255   0

chocolate :: Color
chocolate = rgb 210 105  30

coral :: Color
coral = rgb 255 127  80

cornflowerblue :: Color
cornflowerblue = rgb 100 149 237

cornsilk :: Color
cornsilk = rgb 255 248 220

crimson :: Color
crimson = rgb 220  20  60

cyan :: Color
cyan = rgb 0 255 255

darkblue :: Color
darkblue = rgb 0   0 139

darkcyan :: Color
darkcyan = rgb 0 139 139

darkgoldenrod :: Color
darkgoldenrod = rgb 184 134  11

darkgray :: Color
darkgray = rgb 169 169 169

darkgreen :: Color
darkgreen = rgb 0 100   0

darkgrey :: Color
darkgrey = rgb 169 169 169

darkkhaki :: Color
darkkhaki = rgb 189 183 107

darkmagenta :: Color
darkmagenta = rgb 139   0 139

darkolivegreen :: Color
darkolivegreen = rgb 85 107  47

darkorange :: Color
darkorange = rgb 255 140   0

darkorchid :: Color
darkorchid = rgb 153  50 204

darkred :: Color
darkred = rgb 139   0   0

darksalmon :: Color
darksalmon = rgb 233 150 122

darkseagreen :: Color
darkseagreen = rgb 143 188 143

darkslateblue :: Color
darkslateblue = rgb 72  61 139

darkslategray :: Color
darkslategray = rgb 47  79  79

darkslategrey :: Color
darkslategrey = rgb 47  79  79

darkturquoise :: Color
darkturquoise = rgb 0 206 209

darkviolet :: Color
darkviolet = rgb 148   0 211

deeppink :: Color
deeppink = rgb 255  20 147

deepskyblue :: Color
deepskyblue = rgb 0 191 255

dimgray :: Color
dimgray = rgb 105 105 105

dimgrey :: Color
dimgrey = rgb 105 105 105

dodgerblue :: Color
dodgerblue = rgb 30 144 255

firebrick :: Color
firebrick = rgb 178  34  34

floralwhite :: Color
floralwhite = rgb 255 250 240

forestgreen :: Color
forestgreen = rgb 34  139  34

fuchsia :: Color
fuchsia = rgb 255   0 255

gainsboro :: Color
gainsboro = rgb 220 220 220

ghostwhite :: Color
ghostwhite = rgb 248 248 255

gold :: Color
gold = rgb 255 215   0

goldenrod :: Color
goldenrod = rgb 218 165  32

gray :: Color
gray = rgb 128 128 128

green :: Color
green = rgb 0 128   0

greenyellow :: Color
greenyellow = rgb 173 255  47

grey :: Color
grey = rgb 128 128 128

honeydew :: Color
honeydew = rgb 240 255 240

hotpink :: Color
hotpink = rgb 255 105 180

indianred :: Color
indianred = rgb 205  92  92

indigo :: Color
indigo = rgb 75    0 130

ivory :: Color
ivory = rgb 255 255 240

khaki :: Color
khaki = rgb 240 230 140

lavender :: Color
lavender = rgb 230 230 250

lavenderblush :: Color
lavenderblush = rgb 255 240 245

lawngreen :: Color
lawngreen = rgb 124 252   0

lemonchiffon :: Color
lemonchiffon = rgb 255 250 205

lightblue :: Color
lightblue = rgb 173 216 230

lightcoral :: Color
lightcoral = rgb 240 128 128

lightcyan :: Color
lightcyan = rgb 224 255 255

lightgoldenrodyellow :: Color
lightgoldenrodyellow = rgb 250 250 210

lightgray :: Color
lightgray = rgb 211 211 211

lightgreen :: Color
lightgreen = rgb 144 238 144

lightgrey :: Color
lightgrey = rgb 211 211 211

lightpink :: Color
lightpink = rgb 255 182 193

lightsalmon :: Color
lightsalmon = rgb 255 160 122

lightseagreen :: Color
lightseagreen = rgb 32 178 170

lightskyblue :: Color
lightskyblue = rgb 135 206 250

lightslategray :: Color
lightslategray = rgb 119 136 153

lightslategrey :: Color
lightslategrey = rgb 119 136 153

lightsteelblue :: Color
lightsteelblue = rgb 176 196 222

lightyellow :: Color
lightyellow = rgb 255 255 224

lime :: Color
lime = rgb 0 255   0

limegreen :: Color
limegreen = rgb 50 205  50

linen :: Color
linen = rgb 250 240 230

magenta :: Color
magenta = rgb 255   0 255

maroon :: Color
maroon = rgb 128   0   0

mediumaquamarine :: Color
mediumaquamarine = rgb 102 205 170

mediumblue :: Color
mediumblue = rgb 0   0 205

mediumorchid :: Color
mediumorchid = rgb 186  85 211

mediumpurple :: Color
mediumpurple = rgb 147 112 219

mediumseagreen :: Color
mediumseagreen = rgb 60 179 113

mediumslateblue :: Color
mediumslateblue = rgb 123 104 238

mediumspringgreen :: Color
mediumspringgreen = rgb 0 250 154

mediumturquoise :: Color
mediumturquoise = rgb 72 209 204

mediumvioletred :: Color
mediumvioletred = rgb 199  21 133

midnightblue :: Color
midnightblue = rgb 25  25 112

mintcream :: Color
mintcream = rgb 245 255 250

mistyrose :: Color
mistyrose = rgb 255 228 225

moccasin :: Color
moccasin = rgb 255 228 181

navajowhite :: Color
navajowhite = rgb 255 222 173

navy :: Color
navy = rgb 0   0 128

oldlace :: Color
oldlace = rgb 253 245 230

olive :: Color
olive = rgb 128 128   0

olivedrab :: Color
olivedrab = rgb 107 142  35

orange :: Color
orange = rgb 255 165   0

orangered :: Color
orangered = rgb 255 69    0

orchid :: Color
orchid = rgb 218 112 214

palegoldenrod :: Color
palegoldenrod = rgb 238 232 170

palegreen :: Color
palegreen = rgb 152 251 152

paleturquoise :: Color
paleturquoise = rgb 175 238 238

palevioletred :: Color
palevioletred = rgb 219 112 147

papayawhip :: Color
papayawhip = rgb 255 239 213

peachpuff :: Color
peachpuff = rgb 255 218 185

peru :: Color
peru = rgb 205 133  63

pink :: Color
pink = rgb 255 192 203

plum :: Color
plum = rgb 221 160 221

powderblue :: Color
powderblue = rgb 176 224 230

purple :: Color
purple = rgb 128   0 128

red :: Color
red = rgb 255   0   0

rosybrown :: Color
rosybrown = rgb 188 143 143

royalblue :: Color
royalblue = rgb 65 105 225

saddlebrown :: Color
saddlebrown = rgb 139  69  19

salmon :: Color
salmon = rgb 250 128 114

sandybrown :: Color
sandybrown = rgb 244 164  96

seagreen :: Color
seagreen = rgb 46 139  87

seashell :: Color
seashell = rgb 255 245 238

sienna :: Color
sienna = rgb 160  82  45

silver :: Color
silver = rgb 192 192 192

skyblue :: Color
skyblue = rgb 135 206 235

slateblue :: Color
slateblue = rgb 106  90 205

slategray :: Color
slategray = rgb 112 128 144

slategrey :: Color
slategrey = rgb 112 128 144

snow :: Color
snow = rgb 255 250 250

springgreen :: Color
springgreen = rgb 0 255 127

steelblue :: Color
steelblue = rgb 70 130 180

tan :: Color
tan = rgb 210 180 140

teal :: Color
teal = rgb 0 128 128

thistle :: Color
thistle = rgb 216 191 216

tomato :: Color
tomato = rgb 255  99  71

turquoise :: Color
turquoise = rgb 64 224 208

violet :: Color
violet = rgb 238 130 238

wheat :: Color
wheat = rgb 245 222 179

white :: Color
white = rgb 255 255 255

whitesmoke :: Color
whitesmoke = rgb 245 245 245

yellow :: Color
yellow = rgb 255 255   0

yellowgreen :: Color
yellowgreen = rgb 154 205  50

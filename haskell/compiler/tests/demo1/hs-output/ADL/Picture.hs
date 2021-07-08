{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Picture(
    Circle(..),
    Picture(..),
    Rectangle(..),
    Translated(..),
    mkCircle,
    mkRectangle,
    mkTranslated,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Circle = Circle
    { circle_radius :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkCircle :: Prelude.Double -> Circle
mkCircle radius = Circle radius

jbCircle :: JsonBinding Circle
jbCircle = JsonBinding
    { atype_ = "picture.Circle"
    , jsonGen_ = genObject
        [ genField "radius" circle_radius
        ]
    , jsonParser_ = Circle
        <$> parseField "radius"
    }

data Picture
    = Picture_circle Circle
    | Picture_rectangle Rectangle
    | Picture_composed [Picture]
    | Picture_translated (Translated Picture)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

jbPicture :: JsonBinding Picture
jbPicture = JsonBinding
    { atype_ = "picture.Picture"
    
    , jsonGen_ = genUnion (\jv -> case jv of
        Picture_circle v -> genUnionValue_ jbCircle "circle" v
        Picture_rectangle v -> genUnionValue_ jbRectangle "rectangle" v
        Picture_composed v -> genUnionValue_ (jbVector jbPicture) "composed" v
        Picture_translated v -> genUnionValue_ (jbTranslated jbPicture) "translated" v
        )
    
    , jsonParser_ = parseUnion $ \disc -> case disc of
        "circle" ->  parseUnionValue_ jbCircle  Picture_circle
        "rectangle" ->  parseUnionValue_  jbRectangle Picture_rectangle
        "composed" ->  parseUnionValue_ (jbVector jbPicture) Picture_composed
        "translated" ->  parseUnionValue_ (jbTranslated jbPicture) Picture_translated
        _ -> parseFail "expected a discriminator for Picture (circle,rectangle,composed,translated)"
    }

data Rectangle = Rectangle
    { rectangle_width :: Prelude.Double
    , rectangle_height :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkRectangle :: Prelude.Double -> Prelude.Double -> Rectangle
mkRectangle width height = Rectangle width height

jbRectangle :: JsonBinding Rectangle
jbRectangle = JsonBinding
    { atype_ = "picture.Rectangle"
    , jsonGen_ = genObject
          [ genField_ jbDouble "width" rectangle_width
          , genField_ jbDouble "height" rectangle_height
          ]
      
    , jsonParser_ = Rectangle
          <$> parseField_ jbDouble "width"
          <*> parseField_ jbDouble "height"
    }
--
data Translated t = Translated
    { translated_xoffset :: Prelude.Double
    , translated_yoffset :: Prelude.Double
    , translated_object :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkTranslated :: t -> Translated t
mkTranslated object = Translated 0 0 object

jbTranslated :: JsonBinding a -> JsonBinding (Translated a)
jbTranslated jbA = JsonBinding
    { atype_ = T.concat
        [ "picture.Translated"
        , "<", atype_ jbA
        , ">" ]
    
    , jsonGen_ = genObject
        [ genField_ jbDouble "xoffset" translated_xoffset
        , genField_ jbDouble "yoffset" translated_yoffset
        , genField_ jbA "object" translated_object
        ]
    
    , jsonParser_ = Translated
        <$> parseFieldDef_ jbDouble "xoffset" 0
        <*> parseFieldDef_ jbDouble "yoffset" 0
        <*> parseField_ jbA "object"
    }

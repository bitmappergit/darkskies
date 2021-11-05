module Style where

import Data.Array

import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Types as FT

button :: forall a b. FHE.ToNode a b F.Html => Array (FT.NodeData b) -> a -> F.Html b
button attr body = FHE.button (FHA.class' "pure-button" : attr) body


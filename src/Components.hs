-- | Components, data types and type synonyms used in the game.
-- By convention, data types which are instantiated as components begin with C
-- (e.g. CPosition, CPlayer etc.).

{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Components ( 
  -- * World
  initWorld,
  World(..),
  -- * Components
  CAnimation(..),
  CConfig(..),
  CEnemy(..), 
  CExit(..), 
  CFloor(..),
  CFood(..),
  CFoodPoints(..),
  CHealth(..),
  CInnerWall(..),
  CInnerWallPic(..),
  CLevel(..),
  CNutrition(..),
  COuterWall(..),
  CPictureBundle(..),
  CPlayer(..),
  CPosition(..),
  CTime(..),
  -- * Other Data Types
  Enemy(..),
  Floor(..),
  Food(..),
  InnerWall(..),
  Nutrition(..),
  OuterWall(..),
  -- * Type Synonyms
  EnemyComponents,
  FoodComponents,
  InnerWallComponents,
  Position,
  System',
  -- * Sprite dimensions
  spriteWidth,
  spriteHeight
)
where 

import Apecs

import qualified Apecs.Gloss          as AG
import qualified Data.Map             as Map
import qualified Linear               as L
import qualified System.Random        as R


----------------------------------------------------------------------------------------------
-----------------------                SPRITE DIMENSIONS               -----------------------
----------------------------------------------------------------------------------------------
-- | Width and height of sprites in the sheet Scavengers_SpriteSheet.png
spriteWidth, spriteHeight :: Int
spriteWidth  = 32 
spriteHeight = 32
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                  TYPE SYNONYMS                 -----------------------
----------------------------------------------------------------------------------------------
type Position = L.V2 Int

type EnemyComponents     = (CPosition, CEnemy, CAnimation, CHealth)
type FoodComponents      = (CPosition, CFood, CNutrition)
type InnerWallComponents = (CPosition, CInnerWall, CInnerWallPic, CHealth)
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                OTHER DATA TYPES                -----------------------
----------------------------------------------------------------------------------------------
data Enemy = Vampire | Zombie deriving (Bounded, Enum, Eq, Ord, Show)

data Floor  = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 deriving (Bounded, Enum, Eq, Ord, Show) 

data Food = Fruit | Soda deriving (Bounded, Enum, Eq, Ord, Show)

data InnerWall = IW1 | IW2 | IW3 | IW4 | IW5 | IW6 | IW7 deriving (Bounded, Enum, Eq, Ord, Show)

data OuterWall = OW1 | OW2 | OW3 deriving (Bounded, Enum, Eq, Ord, Show) 

data Nutrition = Nutrition {soda :: Int, fruit :: Int} deriving (Show)
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   COMPONENTS                   -----------------------
----------------------------------------------------------------------------------------------
-- CAnimation
-- | Representation of an animation using sprites.
data CAnimation = CAnimation 
  { period   :: Float        -- ^ Time (in seconds) to pass until sprite changes.
  , sprites  :: [AG.Picture] -- ^ The sprites comprising the animation.
  , index    :: Int          -- ^ Index of the current sprite. Usually initialised to 0 and kept between 0 and length sprites - 1.
  , isFinite :: Bool         -- ^ Is the animation finite (play once and done) or infinite (play forever).
  }

instance Component CAnimation where type Storage CAnimation = Map CAnimation

-- CConfig
data CConfig = CConfig 
  { bottomLeft         :: (Int, Int)
  , topRight           :: (Int, Int)
  , cellWidth          :: Int 
  , cellHeight         :: Int
  , exitPosition       :: CPosition
  , startPosition      :: CPosition
  , foodPointsPosition :: CPosition
  , foodRange          :: (Int, Int)
  , innerWallsRange    :: (Int, Int)
  , foodPoints         :: Int
  , innerWallHealth    :: CHealth
  , nutrition          :: Nutrition
  }

instance Semigroup CConfig where (<>) _ c = c

instance Monoid CConfig where
  mempty = CConfig
    { bottomLeft         = (-1, -1)
    , topRight           = (8, 8)
    , cellWidth          = spriteWidth 
    , cellHeight         = spriteHeight
    , exitPosition       = CPosition $ L.V2 7 7 
    , startPosition      = CPosition $ L.V2 0 0
    , foodPointsPosition = CPosition $ L.V2 2 (-1)
    , foodRange          = (1, 5)
    , innerWallsRange    = (5, 9)
    , foodPoints         = 100
    , innerWallHealth    = CHealth 4 1
    , nutrition          = Nutrition {soda = 20, fruit = 10}
    }

instance Component CConfig where type Storage CConfig = Global CConfig 

-- CEnemy
newtype CEnemy = CEnemy Enemy deriving (Show)
instance Component CEnemy where type Storage CEnemy = Map CEnemy 

-- CExit
data CExit = CExit deriving (Show)
instance Component CExit where type Storage CExit = Unique CExit 

-- CFloor
newtype CFloor = CFloor Floor deriving (Show)
instance Component CFloor where type Storage CFloor = Map CFloor

-- CFood
newtype CFood = CFood Food deriving (Show)
instance Component CFood where type Storage CFood = Map CFood

-- CFoodPoints
newtype CFoodPoints = CFoodPoints Int deriving (Show, Num)
instance Semigroup CFoodPoints where (<>) = (+)
instance Monoid CFoodPoints where mempty = 0
instance Component CFoodPoints where type Storage CFoodPoints = Global CFoodPoints

-- CHealth
data CHealth = CHealth 
  { hp     :: Int -- ^ Hit points.
  , damage :: Int -- ^ Damage inflicted to the object when hit by the player.
  }
  deriving (Show)

instance Component CHealth where type Storage CHealth = Map CHealth

-- CInnerWall
newtype CInnerWall = CInnerWall InnerWall deriving (Show) 
instance Component CInnerWall where type Storage CInnerWall = Map CInnerWall

-- CInnerWallPic
data CInnerWallPic = CInnerWallPic 
  { intact  :: AG.Picture
  , damaged :: AG.Picture 
  } 
  deriving (Show) 

instance Component CInnerWallPic where type Storage CInnerWallPic = Map CInnerWallPic

-- CLevel
newtype CLevel = CLevel Int deriving (Show, Num)
instance Semigroup CLevel where (<>) = (+)
instance Monoid CLevel where mempty = 0
instance Component CLevel where type Storage CLevel = Global CLevel 

-- CNutrition
newtype CNutrition = CNutrition Int deriving (Show)
instance Component CNutrition where type Storage CNutrition = Map CNutrition

-- COuterWall
newtype COuterWall = COuterWall OuterWall deriving (Show) 
instance Component COuterWall where type Storage COuterWall = Map COuterWall

-- CPictureBundle
data CPictureBundle = CPictureBundle 
  { damagedInnerWallPics :: Map.Map InnerWall AG.Picture 
  , exitPic              :: AG.Picture
  , floorPics            :: Map.Map Floor AG.Picture
  , fruitPic             :: AG.Picture
  , intactInnerWallPics  :: Map.Map InnerWall AG.Picture 
  , outerWallPics        :: Map.Map OuterWall AG.Picture
  , playerAttackPics     :: [AG.Picture]
  , playerIdlePics       :: [AG.Picture]
  , sodaPic              :: AG.Picture
  , vampireIdlePics      :: [AG.Picture]
  , zombieIdlePics       :: [AG.Picture]
  }

instance Semigroup CPictureBundle where (<>) _ p = p

instance Monoid CPictureBundle where
  mempty = CPictureBundle
    { damagedInnerWallPics = mempty
    , exitPic              = mempty
    , floorPics            = mempty
    , fruitPic             = mempty
    , intactInnerWallPics  = mempty 
    , outerWallPics        = mempty
    , playerAttackPics     = mempty
    , playerIdlePics       = mempty
    , sodaPic              = mempty
    , vampireIdlePics      = mempty
    , zombieIdlePics       = mempty
    }

instance Component CPictureBundle where type Storage CPictureBundle = Global CPictureBundle 

-- CPlayer
data CPlayer = CPlayer deriving (Show) 
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

-- CPosition
newtype CPosition = CPosition Position 
  deriving (Eq, Show)
  deriving Num via Position

instance Component CPosition where type Storage CPosition = Map CPosition

-- CTime
newtype CTime = CTime Float deriving (Show, Num)
instance Semigroup CTime where (<>) = (+)
instance Monoid CTime where mempty = 0
instance Component CTime where type Storage CTime = Global CTime
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                      WORLD                     -----------------------
----------------------------------------------------------------------------------------------
makeWorld "World"
  [ ''CAnimation
  , ''CConfig
  , ''CFloor
  , ''CFood
  , ''CFoodPoints
  , ''CEnemy
  , ''CExit
  , ''CHealth
  , ''CInnerWall
  , ''CInnerWallPic
  , ''CLevel
  , ''CNutrition
  , ''COuterWall
  , ''CPictureBundle
  , ''CPlayer
  , ''CPosition
  , ''CTime
  , ''AG.Camera ]

type System' a = System World a 
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                RANDOM INSTANCES                -----------------------
----------------------------------------------------------------------------------------------
instance R.Random Enemy where
  randomR (lo, hi) g = let (a, g') = R.randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')
  random = R.randomR (minBound, maxBound)

instance R.Random Floor where
  randomR (lo, hi) g = let (a, g') = R.randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')
  random = R.randomR (minBound, maxBound)

instance R.Random Food where
  randomR (lo, hi) g = let (a, g') = R.randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')
  random = R.randomR (minBound, maxBound)

instance R.Random InnerWall where
  randomR (lo, hi) g = let (a, g') = R.randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')
  random = R.randomR (minBound, maxBound)

instance R.Random OuterWall where
  randomR (lo, hi) g = let (a, g') = R.randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')
  random = R.randomR (minBound, maxBound)
----------------------------------------------------------------------------------------------
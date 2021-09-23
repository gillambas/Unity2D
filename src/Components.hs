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
  CBoardPicture(..),
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
  CPointsChange(..),
  CPosition(..),
  CScreen(..),
  CSkipMove(..),
  CTime(..),
  -- * Other Data Types
  Enemy(..),
  Floor(..),
  Food(..),
  InnerWall(..),
  Nutrition(..),
  OuterWall(..),
  Screen(..),
  -- * Type Synonyms
  EnemyComponents,
  ExitComponents,
  FloorComponents,
  FoodComponents,
  InnerWallComponents,
  OuterWallComponents,
  PlayerComponents,
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

type EnemyComponents     = (CEnemy, CPosition, CAnimation, CHealth)
type ExitComponents      = (CExit, CPosition)
type FloorComponents     = (CFloor, CPosition)
type FoodComponents      = (CFood, CPosition, CNutrition)
type InnerWallComponents = (CInnerWall, CPosition, CInnerWallPic, CHealth)
type OuterWallComponents = (COuterWall, CPosition)
type PlayerComponents    = (CPlayer, CPosition, CAnimation)
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

data Screen = LevelIntro | Game | GameOver deriving (Eq, Show)
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

-- CBoardPicture 
newtype CBoardPicture = CBoardPicture AG.Picture deriving (Monoid, Semigroup, Show)
instance Component CBoardPicture where type Storage CBoardPicture = Global CBoardPicture

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
  , vampireHealth      :: CHealth
  , zombieHealth       :: CHealth 
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
    , innerWallHealth    = CHealth 4 0 1
    , vampireHealth      = CHealth 4 10 1
    , zombieHealth       = CHealth 4 20 1
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
  { hp            :: Int -- ^ Hit points.
  , damageInflict :: Int -- ^ Damage inflicted to the player when hit by the object.
  , damageReceive :: Int -- ^ Damage inflicted to the object when hit by the player.
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
newtype CLevel = CLevel Int deriving (Enum, Num)
instance Show CLevel where show (CLevel l) = "Day " <> show l
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
  { damagedInnerWallPics  :: Map.Map InnerWall AG.Picture 
  , exitPic               :: AG.Picture
  , floorPics             :: Map.Map Floor AG.Picture
  , fruitPic              :: AG.Picture
  , intactInnerWallPics   :: Map.Map InnerWall AG.Picture 
  , outerWallPics         :: Map.Map OuterWall AG.Picture
  , playerAttackPics      :: [AG.Picture]
  , playerHurtPics        :: [AG.Picture]
  , playerIdlePics        :: [AG.Picture]
  , sodaPic               :: AG.Picture
  , vampireAttackPics     :: [AG.Picture]
  , vampireIdlePics       :: [AG.Picture]
  , zombieAttackPics      :: [AG.Picture]
  , zombieIdlePics        :: [AG.Picture]
  }

instance Semigroup CPictureBundle where
  pb1 <> pb2 = CPictureBundle
    { damagedInnerWallPics = damagedInnerWallPics pb1 <> damagedInnerWallPics pb2
    , exitPic              = exitPic              pb1 <> exitPic              pb2
    , floorPics            = floorPics            pb1 <> floorPics            pb2
    , fruitPic             = fruitPic             pb1 <> fruitPic             pb2
    , intactInnerWallPics  = intactInnerWallPics  pb1 <> intactInnerWallPics  pb2
    , outerWallPics        = outerWallPics        pb1 <> outerWallPics        pb2
    , playerAttackPics     = playerAttackPics     pb1 <> playerAttackPics     pb2
    , playerHurtPics       = playerHurtPics       pb1 <> playerHurtPics       pb2
    , playerIdlePics       = playerIdlePics       pb1 <> playerIdlePics       pb2
    , sodaPic              = sodaPic              pb1 <> sodaPic              pb2
    , vampireAttackPics    = vampireAttackPics    pb1 <> vampireAttackPics    pb2
    , vampireIdlePics      = vampireIdlePics      pb1 <> vampireIdlePics      pb2
    , zombieAttackPics     = zombieAttackPics     pb1 <> zombieAttackPics     pb2
    , zombieIdlePics       = zombieIdlePics       pb1 <> zombieIdlePics       pb2
    }

instance Monoid CPictureBundle where
  mempty = CPictureBundle
    { damagedInnerWallPics = mempty
    , exitPic              = mempty
    , floorPics            = mempty
    , fruitPic             = mempty
    , intactInnerWallPics  = mempty 
    , outerWallPics        = mempty
    , playerAttackPics     = mempty
    , playerHurtPics       = mempty
    , playerIdlePics       = mempty
    , sodaPic              = mempty
    , vampireAttackPics    = mempty
    , vampireIdlePics      = mempty
    , zombieAttackPics     = mempty
    , zombieIdlePics       = mempty
    }

instance Component CPictureBundle where type Storage CPictureBundle = Global CPictureBundle 

-- CPlayer
data CPlayer = CPlayer deriving (Show) 
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

-- CPointsChange
newtype CPointsChange = CPointsChange String deriving (Monoid, Semigroup, Show)
instance Component CPointsChange where type Storage CPointsChange = Global CPointsChange 

-- CPosition
newtype CPosition = CPosition Position 
  deriving (Eq, Show)
  deriving Num via Position -- WARNING: fromInteger CPosition doesn't work!

instance Component CPosition where type Storage CPosition = Map CPosition

-- CScreen 
newtype CScreen = CScreen Screen deriving (Show)
instance Semigroup CScreen where (<>) _ s = s
instance Monoid CScreen where mempty = CScreen LevelIntro 
instance Component CScreen where type Storage CScreen = Global CScreen

-- CSkipMove
newtype CSkipMove = CSkipMove Bool deriving (Show)
instance Semigroup CSkipMove where (<>) (CSkipMove s1) (CSkipMove s2) = CSkipMove (s1 && s2)
instance Monoid CSkipMove where mempty = CSkipMove True
instance Component CSkipMove where type Storage CSkipMove = Global CSkipMove 

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
  , ''CBoardPicture
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
  , ''CPointsChange
  , ''CPosition
  , ''CScreen
  , ''CSkipMove
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
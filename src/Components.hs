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
  CPlayer(..),
  CPosition(..),
  CTime(..),
  -- * Other Data Types
  Config(..),
  Enemy(..),
  Floor(..),
  Food(..),
  InnerWall(..),
  Nutrition(..),
  OuterWall(..),
  PictureBundle(..),
  -- * Type Synonyms
  EnemyComponents,
  FoodComponents,
  InnerWallComponents,
  Position,
  System'
)
where 

import Apecs

import qualified Apecs.Gloss   as AG
import qualified Data.Map      as Map
import qualified Linear        as L
import qualified System.Random as R


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

data PictureBundle = PictureBundle 
  { damagedInnerWallPics :: Map.Map InnerWall AG.Picture 
  , exitPic              :: AG.Picture
  , floorPics            :: Map.Map Floor AG.Picture
  , fruitPic             :: AG.Picture
  , intactInnerWallPics  :: Map.Map InnerWall AG.Picture 
  , outerWallPics        :: Map.Map OuterWall AG.Picture
  , playerIdlePics       :: [AG.Picture]
  , sodaPic              :: AG.Picture
  , vampireIdlePics      :: [AG.Picture]
  , zombieIdlePics       :: [AG.Picture]
  }

data Config = Config 
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
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   COMPONENTS                   -----------------------
----------------------------------------------------------------------------------------------
newtype CPosition = CPosition Position 
  deriving (Eq, Show)
  deriving Num via Position

instance Component CPosition where type Storage CPosition = Map CPosition


newtype CEnemy = CEnemy Enemy deriving (Show)
instance Component CEnemy where type Storage CEnemy = Map CEnemy 

data CExit = CExit deriving (Show)
instance Component CExit where type Storage CExit = Unique CExit 

newtype CFloor = CFloor Floor deriving (Show)
instance Component CFloor where type Storage CFloor = Map CFloor

newtype CFood = CFood Food deriving (Show)
instance Component CFood where type Storage CFood = Map CFood

newtype CInnerWall = CInnerWall InnerWall deriving (Show) 
instance Component CInnerWall where type Storage CInnerWall = Map CInnerWall

newtype CLevel = CLevel Int deriving (Show)
instance Semigroup CLevel where (<>) (CLevel l1) (CLevel l2) = CLevel (l1+l2)
instance Monoid CLevel where mempty = CLevel 0
instance Component CLevel where type Storage CLevel = Global CLevel 

newtype COuterWall = COuterWall OuterWall deriving (Show) 
instance Component COuterWall where type Storage COuterWall = Map COuterWall

data CPlayer = CPlayer deriving (Show) 
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

newtype CTime = CTime Float deriving (Show, Num)
instance Semigroup CTime where (<>) = (+)
instance Monoid CTime where mempty = 0
instance Component CTime where type Storage CTime = Global CTime

newtype CFoodPoints = CFoodPoints Int deriving (Show, Num)
instance Semigroup CFoodPoints where (<>) = (+)
instance Monoid CFoodPoints where mempty = 0
instance Component CFoodPoints where type Storage CFoodPoints = Global CFoodPoints

data CHealth = CHealth 
  { hp     :: Int -- ^ Hit points.
  , damage :: Int -- ^ Damage inflicted to the object when hit by the player.
  }
  deriving (Show)

instance Component CHealth where type Storage CHealth = Map CHealth

-- | Representation of an animation using sprites.
data CAnimation = CAnimation 
  { period  :: Float        -- ^ Time (in seconds) to pass until sprite changes.
  , sprites :: [AG.Picture] -- ^ The sprites comprising the animation.
  , index   :: Int          -- ^ Index of the current sprite. Usually initialised to 0 and kept between 0 and length sprites - 1.
  }

instance Component CAnimation where type Storage CAnimation = Map CAnimation


data CInnerWallPic = CInnerWallPic 
  { intact  :: AG.Picture
  , damaged :: AG.Picture 
  } 
  deriving (Show) 

instance Component CInnerWallPic where type Storage CInnerWallPic = Map CInnerWallPic


newtype CNutrition = CNutrition Int deriving (Show)
instance Component CNutrition where type Storage CNutrition = Map CNutrition
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                      WORLD                     -----------------------
----------------------------------------------------------------------------------------------
makeWorld "World" [ ''CPosition, ''CEnemy, ''CExit, ''CFloor, ''CFood, ''CFoodPoints, ''CHealth
                  , ''CInnerWall, ''CInnerWallPic, ''CLevel, ''CNutrition, ''COuterWall, ''CPlayer
                  , ''CAnimation, ''CTime, ''AG.Camera]

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
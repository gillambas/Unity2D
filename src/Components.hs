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
  CGraphics(..),
  CHealth(..),
  CInnerWall(..),
  CInnerWallPic(..),
  CLevel(..),
  CNutrition(..),
  COuterWall(..),
  CPlayer(..),
  CPointsChange(..),
  CPosition(..),
  CScreen(..),
  CSkipMove(..),
  CSwitchInput(..),
  CTime(..),
  -- * Other Data Types
  Enemy(..),
  Floor(..),
  Food(..),
  InnerWall(..),
  Nutrition(..),
  OuterWall(..),
  Screen(..),
  SwitchInput(..),
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

import qualified Apecs.Gloss                    as AG
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Data.Map                       as Map
import qualified Device.Nintendo.Switch         as NS
import qualified Graphics.Text.TrueType         as TT
import qualified Linear                         as L
import qualified System.Random                  as R


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

data SwitchInput
  = Up
  | Down
  | Left
  | Right
  | Attack
  | Restart
  | Exit
  | None
  deriving (Eq, Show)
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

-- CGraphics
data CGraphics = CGraphics 
  { damagedInnerWallPics  :: Map.Map InnerWall AG.Picture 
  , exitPic               :: AG.Picture 
  , floorPics             :: Map.Map Floor AG.Picture
  , font                  :: TT.Font
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

instance Semigroup CGraphics where
  g1 <> g2 = CGraphics
    { damagedInnerWallPics = damagedInnerWallPics g1 <> damagedInnerWallPics g2
    , exitPic              = exitPic              g1 <> exitPic              g2
    , floorPics            = floorPics            g1 <> floorPics            g2
    , font                 = font g2 -- TODO: Think
    , fruitPic             = fruitPic             g1 <> fruitPic             g2
    , intactInnerWallPics  = intactInnerWallPics  g1 <> intactInnerWallPics  g2
    , outerWallPics        = outerWallPics        g1 <> outerWallPics        g2
    , playerAttackPics     = playerAttackPics     g1 <> playerAttackPics     g2
    , playerHurtPics       = playerHurtPics       g1 <> playerHurtPics       g2
    , playerIdlePics       = playerIdlePics       g1 <> playerIdlePics       g2
    , sodaPic              = sodaPic              g1 <> sodaPic              g2
    , vampireAttackPics    = vampireAttackPics    g1 <> vampireAttackPics    g2
    , vampireIdlePics      = vampireIdlePics      g1 <> vampireIdlePics      g2
    , zombieAttackPics     = zombieAttackPics     g1 <> zombieAttackPics     g2
    , zombieIdlePics       = zombieIdlePics       g1 <> zombieIdlePics       g2
    }

instance Monoid CGraphics where
  mempty = CGraphics
    { damagedInnerWallPics = mempty
    , exitPic              = mempty
    , floorPics            = mempty
    , font                 = undefined  -- TODO: Think
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

instance Component CGraphics where type Storage CGraphics = Global CGraphics 

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

-- CSwitchInput
newtype CSwitchInput = CSwitchInput (Maybe (TBQ.TBQueue NS.Input))
instance Semigroup CSwitchInput where (<>) _ si = si
instance Monoid CSwitchInput where mempty = CSwitchInput Nothing
instance Component CSwitchInput where type Storage CSwitchInput = Global CSwitchInput

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
  , ''CGraphics
  , ''CPlayer
  , ''CPointsChange
  , ''CPosition
  , ''CScreen
  , ''CSkipMove
  , ''CSwitchInput
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
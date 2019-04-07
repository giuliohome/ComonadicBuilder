{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import Control.Comonad (Comonad (..), (=>>))
import Data.Semigroup (Any (..))
import Data.Text (Text)
import Text.Pretty.Simple (pPrint)

import Lib


data Settings = Settings
    { settingsHasLibrary :: Any
    , settingsGitHub     :: Any
    , settingsTravis     :: Any
    } deriving (Show)

instance Semigroup Settings where
    Settings a1 b1 c1 <> Settings a2 b2 c2 =
        Settings (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid Settings where
    mempty = Settings mempty mempty mempty

data Project = Project
    { projectName       :: !Text
    , projectHasLibrary :: !Bool
    , projectGitHub     :: !Bool
    , projectTravis     :: !Bool
    } deriving (Show)

type ProjectBuilder = Settings -> Project

buildProject :: Text -> ProjectBuilder
buildProject projectName Settings{..} = Project
    { projectHasLibrary = getAny settingsHasLibrary
    , projectGitHub     = getAny settingsGitHub
    , projectTravis     = getAny settingsTravis
    , ..
    }

{-
builder =>> f
        :: Settings -> Project
    = flip extend builder f  -- (=>>) definition
    = extend f builder       -- flip definition
    = fmap f (duplicate builder)  -- default implementation of extend
    = fmap f (\settings1 -> builder . mappend settings1)  -- duplicate for arrow
    = f . (\settings1 -> builder . mappend settings1)  -- Functor instance for arrow
    = f . (\settings1 settings2 -> builder $ settings1 <> settings2)  -- eta-expanding internal lambda
    = \settings -> f $ (\settings1 settings2 -> builder $ settings1 <> settings2) settings  -- eta-expanding outer lambda
    = \settings -> f $ \settings2 -> builder $ settings <> settings2  -- partially applying lambda
-}
append :: ProjectBuilder -> (ProjectBuilder -> Project) -> ProjectBuilder
append = (=>>)

{-
buildProject "foo" =>> hasLibraryB
    :: Settings -> Project
    = \settings -> hasLibraryB $ \settings2 -> buildProject "foo" $ settings <> settings2
    = \settings -> (\settings2 -> buildProject "foo" $ settings <> settings2) (mempty { settingsHasLibrary = Any True })
    = \settings -> buildProject "foo" $ settings <> mempty { settingsHasLibrary = Any True }
-}

hasLibraryB :: ProjectBuilder -> Project
hasLibraryB builder = builder $ mempty { settingsHasLibrary = Any True }

gitHubB :: ProjectBuilder -> Project
gitHubB builder = builder $ mempty { settingsGitHub = Any True }

alwaysTravisB :: ProjectBuilder -> Project
alwaysTravisB builder = builder $ mempty { settingsTravis = Any True }

travisB :: ProjectBuilder -> Project
travisB builder =
  let project = extract builder
  in project { projectTravis = projectGitHub project }




main :: IO ()
main = do
    -- plain
    pPrint $ extract $ buildProject "minimal-project"
    pPrint $ extract $ buildProject "only-library" =>> hasLibraryB
    pPrint $ extract $ buildProject "library-github" =>> hasLibraryB =>> gitHubB

    -- dependent: 1 level
    pPrint $ extract $ buildProject "travis" =>> travisB
    pPrint $ extract $ buildProject "always-travis" =>> alwaysTravisB
    pPrint $ extract $ buildProject "github-travis" =>> gitHubB =>> travisB
    pPrint $ extract $ buildProject "travis-github" =>> travisB =>> gitHubB
    someFunc

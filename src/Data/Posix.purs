module Data.Posix where

import Prelude (Show, show, (<>), Ord, compare, Eq, eq)
import Data.Function (on)

-- | A process ID.
newtype Pid = Pid Int

runPid :: Pid -> Int
runPid (Pid x) = x

instance showPid :: Show Pid where
  show (Pid pid) = "(Pid " <> show pid <> ")"

instance eqPid :: Eq Pid where
  eq = eq `on` runPid

instance ordPid :: Ord Pid where
  compare = compare `on` runPid

-- | A group ID (for a process or a file).
newtype Gid = Gid Int

runGid :: Gid -> Int
runGid (Gid x) = x

instance showGid :: Show Gid where
  show (Gid gid) = "(Gid " <> show gid <> ")"

instance eqGid :: Eq Gid where
  eq = eq `on` runGid

instance ordGid :: Ord Gid where
  compare = compare `on` runGid

-- | A user ID (for a process or a file).
newtype Uid = Uid Int

runUid :: Uid -> Int
runUid (Uid x) = x

instance showUid :: Show Uid where
  show (Uid uid) = "(Uid " <> show uid <> ")"

instance eqUid :: Eq Uid where
  eq = eq `on` runUid

instance ordUid :: Ord Uid where
  compare = compare `on` runUid

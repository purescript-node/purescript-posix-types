module Data.Posix where

import Prelude (Show, show, (<>))
import Data.Function (on)

data Signal
 = SIGABRT
 | SIGALRM
 | SIGBUS
 | SIGCHLD
 | SIGCLD
 | SIGCONT
 | SIGEMT
 | SIGFPE
 | SIGHUP
 | SIGILL
 | SIGINFO
 | SIGINT
 | SIGIO
 | SIGIOT
 | SIGKILL
 | SIGLOST
 | SIGPIPE
 | SIGPOLL
 | SIGPROF
 | SIGPWR
 | SIGQUIT
 | SIGSEGV
 | SIGSTKFLT
 | SIGSTOP
 | SIGSYS
 | SIGTERM
 | SIGTRAP
 | SIGTSTP
 | SIGTTIN
 | SIGTTOU
 | SIGUNUSED
 | SIGURG
 | SIGUSR1
 | SIGUSR2
 | SIGVTALRM
 | SIGWINCH
 | SIGXCPU
 | SIGXFSZ

-- | Convert a Signal to a String. Suitable for Node.js APIs.
toString :: Signal -> String
toString s = case s of
 SIGABRT   -> "SIGABRT"
 SIGALRM   -> "SIGALRM"
 SIGBUS    -> "SIGBUS"
 SIGCHLD   -> "SIGCHLD"
 SIGCLD    -> "SIGCLD"
 SIGCONT   -> "SIGCONT"
 SIGEMT    -> "SIGEMT"
 SIGFPE    -> "SIGFPE"
 SIGHUP    -> "SIGHUP"
 SIGILL    -> "SIGILL"
 SIGINFO   -> "SIGINFO"
 SIGINT    -> "SIGINT"
 SIGIO     -> "SIGIO"
 SIGIOT    -> "SIGIOT"
 SIGKILL   -> "SIGKILL"
 SIGLOST   -> "SIGLOST"
 SIGPIPE   -> "SIGPIPE"
 SIGPOLL   -> "SIGPOLL"
 SIGPROF   -> "SIGPROF"
 SIGPWR    -> "SIGPWR"
 SIGQUIT   -> "SIGQUIT"
 SIGSEGV   -> "SIGSEGV"
 SIGSTKFLT -> "SIGSTKFLT"
 SIGSTOP   -> "SIGSTOP"
 SIGSYS    -> "SIGSYS"
 SIGTERM   -> "SIGTERM"
 SIGTRAP   -> "SIGTRAP"
 SIGTSTP   -> "SIGTSTP"
 SIGTTIN   -> "SIGTTIN"
 SIGTTOU   -> "SIGTTOU"
 SIGUNUSED -> "SIGUNUSED"
 SIGURG    -> "SIGURG"
 SIGUSR1   -> "SIGUSR1"
 SIGUSR2   -> "SIGUSR2"
 SIGVTALRM -> "SIGVTALRM"
 SIGWINCH  -> "SIGWINCH"
 SIGXCPU   -> "SIGXCPU"
 SIGXFSZ   -> "SIGXFSZ"

instance showSignal :: Show Signal where
  show = toString

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

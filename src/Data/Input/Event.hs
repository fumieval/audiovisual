{-# LANGUAGE DeriveDataTypeable #-}
module Data.Input.Event where
import Data.Typeable
import Data.Char
import Linear

data Chatter a = Up a | Down a deriving (Show, Eq, Ord, Read, Typeable)

data MouseEvent = Button (Chatter Int)
  | Cursor (V2 Float)
  | Scroll (V2 Float)
  deriving (Show, Eq, Ord, Read, Typeable)

data Gamepad = Gamepad Int String deriving (Show, Eq, Ord, Read, Typeable)

data GamepadEvent = PadButton Gamepad (Chatter Int)
  | PadAxis Gamepad Int Float
  | PadConnection (Chatter Gamepad)
  deriving (Show, Eq, Ord, Read, Typeable)

data Key =
      KeyUnknown
    | KeySpace
    | KeyApostrophe
    | KeyComma
    | KeyMinus
    | KeyPeriod
    | KeySlash
    | Key0
    | Key1
    | Key2
    | Key3
    | Key4
    | Key5
    | Key6
    | Key7
    | Key8
    | Key9
    | KeySemicolon
    | KeyEqual
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | KeyLeftBracket
    | KeyBackslash
    | KeyRightBracket
    | KeyGraveAccent
    | KeyWorld1
    | KeyWorld2
    | KeyEscape
    | KeyEnter
    | KeyTab
    | KeyBackspace
    | KeyInsert
    | KeyDelete
    | KeyRight
    | KeyLeft
    | KeyDown
    | KeyUp
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | KeyCapsLock
    | KeyScrollLock
    | KeyNumLock
    | KeyPrintScreen
    | KeyPause
    | KeyF1
    | KeyF2
    | KeyF3
    | KeyF4
    | KeyF5
    | KeyF6
    | KeyF7
    | KeyF8
    | KeyF9
    | KeyF10
    | KeyF11
    | KeyF12
    | KeyF13
    | KeyF14
    | KeyF15
    | KeyF16
    | KeyF17
    | KeyF18
    | KeyF19
    | KeyF20
    | KeyF21
    | KeyF22
    | KeyF23
    | KeyF24
    | KeyF25
    | KeyPad0
    | KeyPad1
    | KeyPad2
    | KeyPad3
    | KeyPad4
    | KeyPad5
    | KeyPad6
    | KeyPad7
    | KeyPad8
    | KeyPad9
    | KeyPadDecimal
    | KeyPadDivide
    | KeyPadMultiply
    | KeyPadSubtract
    | KeyPadAdd
    | KeyPadEnter
    | KeyPadEqual
    | KeyLeftShift
    | KeyLeftControl
    | KeyLeftAlt
    | KeyLeftSuper
    | KeyRightShift
    | KeyRightControl
    | KeyRightAlt
    | KeyRightSuper
    | KeyMenu
    deriving (Enum, Eq, Ord, Read, Show, Typeable, Bounded)

charToKey :: Char -> Key
charToKey ch
    | isAlpha ch = toEnum $ fromEnum KeyA + fromEnum ch - fromEnum 'A'
    | isDigit ch = toEnum $ fromEnum Key0 + fromEnum ch - fromEnum '0'
charToKey '-' = KeyMinus
charToKey ',' = KeyComma
charToKey '.' = KeyPeriod
charToKey '/' = KeySlash
charToKey ' ' = KeySpace
charToKey '\'' = KeyApostrophe
charToKey '\\' = KeyBackslash
charToKey '=' = KeyEqual
charToKey ';' = KeySemicolon
charToKey '[' = KeyLeftBracket
charToKey ']' = KeyRightBracket
charToKey '`' = KeyGraveAccent
charToKey '\n' = KeyEnter
charToKey '\r' = KeyEnter
charToKey '\t' = KeyTab
charToKey _ = KeyUnknown

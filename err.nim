
type
  ErrCode* = enum
    errOk
    errSymNotFound
    errWordNotBound
    fatalSymtableOutOfSpace

proc raiseScriptError*(code: ErrCode) {.noinline.} =
  raise newException(Exception, "script error: " & $code)

type
  ErrCode* = enum
    errOk
    errMethodNotAllowed
    errSymNotFound
    errWordNotBound
    fatalSymtableOutOfSpace

proc raiseScriptError*(code: ErrCode) {.noinline.} =
  raise newException(Exception, "script error: " & $code)
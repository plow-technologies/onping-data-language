# Commands

* [assert](#assert)
* [densitygraph](#densitygraph)
* [exit](#exit)
* [getallkeys](#getallkeys)
* [gettimebounds](#gettimebounds)
* [keyinfo](#keyinfo)
* [memoryusage](#memoryusage)
* [newarray](#newarray)
* [print](#print)
* [removekey](#removekey)
* [setarray](#setarray)
* [settimebounds](#settimebounds)
* [sync](#sync)
* [truncate](#truncate)


## assert

```
!assert [ <Bool expression> ]
```

First, it evaluates the given boolean expression. If the result
is `true`, no further action is performed. However, if the result
is `false`, `assert` will throw an `AssertionFailed` error. This
is useful if you want to stop the execution if certain condition
is not fulfilled.


## densitygraph

```
!densitygraph [ <Text expression> ] [ <Key expression> ]
```

_This command has not been documented yet._

## exit

```
!exit
```

Interrupt the execution of the script by sending an `Exit` error.

## getallkeys

```
!getallkeys <identifier>
```

_This command has not been documented yet._

## gettimebounds

```
!gettimebounds <identifier> <identifier>
```

_This command has not been documented yet._

## keyinfo

```
!keyinfo [ <Key expression> ] <identifier> <identifier> <identifier>
```

_This command has not been documented yet._

## memoryusage

```
!memoryusage <identifier>
```

_This command has not been documented yet._

## newarray

```
!newarray <identifier> [ <Int expression> ]
```

_This command has not been documented yet._

## print

```
!print [ <Value expression> ]
```

Display a value on the screen.

## removekey

```
!removekey [ <Key expression> ]
```

_This command has not been documented yet._

## setarray

```
!setarray <identifier> [ <Int expression> ] [ <Value expression> ]
```

_This command has not been documented yet._

## settimebounds

```
!settimebounds [ <Int expression> ] [ <Int expression> ]
```

_This command has not been documented yet._

## sync

```
!sync
```

_This command has not been documented yet._

## truncate

```
!truncate [ <Key expression> ] [ <Int expression> ]
```

_This command has not been documented yet._



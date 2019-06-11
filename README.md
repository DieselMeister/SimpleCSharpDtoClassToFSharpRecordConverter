# A very very simple C# dto class to F# record converter

on the left side, insert the c# dto class and click "translate" and it should produce a F# record.

Client is made with the awesome F# Fable Elmish stuff. Also thanks to the SAFE-Stack Team.
The Server is other than in the SAFE-Template an Azure Function.

## Known issues:

* You have to start the azure function manually.
* very different boilerplate inside the C# dto class can cause a parsing error.

## Please be aware, that this is only a Test Project to learn a little of FParsec. I am no expert in parsing and it is hard to understand and harder to write elegant code. My code is far from elegant or even readable ... at least the "domain logic" XD

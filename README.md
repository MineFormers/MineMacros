# MineMacros
Scala Macros for use with Minecraft

## Features
- Full support for constructors, fields and methods of any class
- Compile time safety
  - No need for fully qualified names
  - Resolving names at compile time (no "NoSuchFieldException"s etc. at runtime)
  - Type checking against everything during compilation
- Automatic mapping of names
  - No user-facing SRG names during development
  - Mappings are always correct
  - Use of the user-specified mappings from a project's build.gradle through GradleStart
- Perfect for use with ASM
  - No classes are loaded during runtime at all
  - Automatic conversion during runtime: Let the library handle names instead of hardcoding the alternatives
  - Handy methods to retrieve ASM descriptors and types
  
## Known issues / ToDo list
- Currently unable to resolve private members of Java classes
  - Makes the library effectively useless
  - Cause unknown, public members in Java classes and all members of Scala types are accessible
  - Possibly solvable by using "normal" reflection during compile time
- The library is a memory hog
  - Eclipse seems to constantly increase its memory usage
  - Presumably caused by loading all mappings into memory at once
  - Solutions are appreciated ;)

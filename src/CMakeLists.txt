# SPDX-License-Identifier: MIT

cmake_minimum_required (VERSION 3.19)

find_package(Boost REQUIRED)
find_package(utf8cpp CONFIG REQUIRED)
find_path(RANG_INCLUDE_DIRS "rang.hpp")
find_package(fmt CONFIG REQUIRED)

add_executable(schemepp Interpreter.cpp Parser.cpp REPL.cpp BuiltinValue/Number.cpp BuiltinValue/Boolean.cpp Error.cpp BuiltinValue/Character.cpp BuiltinValue/ByteVector.cpp BuiltinValue/String.cpp Scope.cpp BuiltinValue/Vector.cpp BuiltinValue/List.cpp BuiltinProcedure/Arithmetic.cpp Interface/BuiltinLibraries.hpp BuiltinProcedure/RTTI.cpp BuiltinProcedure/Write.cpp Value.cpp BuiltinProcedure/String.cpp BuiltinValue/Symbol.cpp  Interface/EvaluateContext.hpp BuiltinProcedure/Math.cpp BuiltinProcedure/File.cpp BuiltinProcedure/Complex.cpp BuiltinProcedure/Read.cpp BuiltinProcedure/Char.cpp BuiltinProcedure/ProcessContext.cpp BuiltinProcedure/Container.cpp "BuiltinValue/Pair.cpp")

target_include_directories(schemepp PRIVATE ${RANG_INCLUDE_DIRS})
target_include_directories(schemepp PRIVATE .)

target_link_libraries(schemepp PRIVATE Boost::boost)
target_link_libraries(schemepp PRIVATE utf8cpp)
target_link_libraries(schemepp PRIVATE fmt::fmt)

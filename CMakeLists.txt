# SPDX-License-Identifier: MIT

cmake_minimum_required (VERSION 3.19)
enable_testing()

project("schemepp")
set(CMAKE_CXX_STANDARD 17)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
	add_compile_definitions("UNICODE")
	add_compile_definitions("_UNICODE")
	add_compile_options("/utf-8")
	if(CMAKE_BUILD_TYPE MATCHES Debug)
		add_compile_options("/Od" "/Oy-")
	else()
		add_compile_options("/O2" "/Ob2" "/Oi" "/Ot" "/Oy" "/GT" "/GL" "/Qpar")
		add_link_options("/LTCG:incremental" "/OPT:REF" "/OPT:ICF")
	endif()
elseif(CMAKE_COMPILER_IS_GNUCXX)
	if(CMAKE_BUILD_TYPE MATCHES Debug)
		add_compile_options("-fsanitize=address" "-O0" "-fno-omit-frame-pointer")
	else()
		add_compile_options("-Ofast")
	endif()
endif()
if(CMAKE_BUILD_TYPE MATCHES Debug)
	add_compile_definitions("SCHEMEPP_DEBUG")
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory("src")
add_subdirectory("test")

install(FILES LICENSE DESTINATION ./)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/lib DESTINATION ./)
install(DIRECTORY ${CMAKE_BINARY_DIR}/bin DESTINATION ./)

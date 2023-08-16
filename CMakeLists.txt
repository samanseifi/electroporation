cmake_minimum_required(VERSION 3.19)
project(electroporation Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-traceback;-warn>")
endif()

add_executable(electroporation main.f90 src/electricfield_mod.f90 src/solver_mod.f90 src/util_mod.f90 src/variables_mod.f90)

if(NOT CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  file(GENERATE OUTPUT .gitignore CONTENT "*")
endif()

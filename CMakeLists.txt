cmake_minimum_required(VERSION 3.19)
project(electroporation Fortran)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/bin)

enable_language(Fortran)
add_executable(electroporation main.f90 src/electricfield_mod.f90 src/solver_mod.f90 src/util_mod.f90 src/variables_mod.f90)
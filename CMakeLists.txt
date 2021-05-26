cmake_minimum_required(VERSION 3.9)

project(
  "mstore"
  LANGUAGES "Fortran"
  VERSION "0.2.0"
  DESCRIPTION "Modular computation tool chain"
)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# Collect source of the project
set(srcs)
add_subdirectory("src")

# Fetch external dependency
include(FetchContent)
FetchContent_Declare(
  mctc-lib
  GIT_REPOSITORY https://github.com/grimme-lab/mctc-lib.git
  GIT_TAG main
)
FetchContent_GetProperties(mctc-lib)
if(NOT mctc-lib_POPULATED)
  FetchContent_Populate(mctc-lib)
endif()
set(${_package_upper}_SOURCE_DIR "${${_package_lower}_SOURCE_DIR}")
set(${_package_upper}_BINARY_DIR "${${_package_lower}_BINARY_DIR}")
add_subdirectory(${mctc-lib_SOURCE_DIR} ${mctc-lib_BINARY_DIR})

# MCTC library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)

target_link_libraries(${PROJECT_NAME}-lib PUBLIC mctc-lib-lib)

set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include"
)

target_include_directories(
  "${PROJECT_NAME}-lib"
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

add_subdirectory(app)

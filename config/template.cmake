@PACKAGE_INIT@

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if(NOT TARGET "@PROJECT_NAME@::@PROJECT_NAME@")
  include("${CMAKE_CURRENT_LIST_DIR}/@PROJECT_NAME@-targets.cmake")

  include(CMakeFindDependencyMacro)

  if(NOT TARGET "mctc-lib::mctc-lib")
    find_dependency("mctc-lib")
  endif()
endif()

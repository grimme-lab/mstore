# This file is part of mstore.
# SPDX-Identifier: Apache-2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

if(TARGET "mctc-lib::mctc-lib")
  set(mctc-lib_FOUND TRUE)
  set(MCTC_FOUND TRUE)
else()
  find_package(PkgConfig QUIET)
  pkg_check_modules(MCTC QUIET mctc-lib)
  if(MCTC_FOUND)
    message(STATUS "Found mctc-lib via pkg-config")
    set(mctc-lib_FOUND ${MCTC_FOUND})

    add_library("mctc-lib::mctc-lib" INTERFACE IMPORTED)
    target_link_libraries("mctc-lib::mctc-lib" INTERFACE ${MCTC_LINK_LIBRARIES})
    target_include_directories("mctc-lib::mctc-lib" INTERFACE ${MCTC_INCLUDE_DIRS})
  else()
    if(EXISTS "${PROJECT_SOURCE_DIR}/subprojects/mctc-lib/CMakeLists.txt")
      message(STATUS "Using mctc-lib subproject")
      add_subdirectory("${PROJECT_SOURCE_DIR}/subprojects/mctc-lib" EXCLUDE_FROM_ALL)
    else()
      set("mctc-lib-url" "https://github.com/grimme-lab/mctc-lib")
      message(STATUS "Retrieving mctc-lib from ${mctc-lib-url}")
      include(FetchContent)
      FetchContent_Declare(
        "mctc-lib"
        GIT_REPOSITORY "${mctc-lib-url}"
        GIT_TAG "HEAD"
      )
      FetchContent_MakeAvailable("mctc-lib")
    endif()
    add_library("mctc-lib::mctc-lib" INTERFACE IMPORTED)
    target_link_libraries("mctc-lib::mctc-lib" INTERFACE "mctc-lib")
  endif()
endif()

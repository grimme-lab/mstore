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

add_subdirectory("data")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/amino20x4.f90"
  "${dir}/but14diol.f90"
  "${dir}/heavy28.f90"
  "${dir}/ice10.f90"
  "${dir}/il16.f90"
  "${dir}/mb16_43.f90"
  "${dir}/upu23.f90"
  "${dir}/x23.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)

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

project(
  'mstore',
  'c',
  version: '0.2.0',
  license: 'Apache-2.0',
  meson_version: '>=0.57.2',
  default_options: [
    'buildtype=debugoptimized',
  ],
)
install = true

mstore_dep = dependency(
  'mstore',
  version: '>=@0@'.format(meson.project_version()),
  fallback: ['mstore', 'mstore_dep'],
  default_options: [
    'default_library=static',
    'api=true',
    'python=false',
  ],
)
mstore_header = files('include'/'_mstore.h')

subdir('mstore')

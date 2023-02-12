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
  'fortran',
  version: '0.2.0',
  license: 'Apache-2.0',
  meson_version: '>=0.57.2',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ],
)
install = not (meson.is_subproject() and get_option('default_library') == 'static')
has_cc = add_languages('c', required: get_option('api') or get_option('python'), native: false)

# General configuration information
lib_deps = []
inc_dirs = []
subdir('config')

# Collect source of the project
srcs = []
subdir('src')

# Library target
mstore_lib = library(
  meson.project_name(),
  sources: srcs,
  version: meson.project_version(),
  dependencies: lib_deps,
  include_directories: inc_dirs,
  install: install,
)

# Export dependency for other projects and test suite
mstore_inc = [include_directories('include'), mstore_lib.private_dir_include()]
mstore_dep = declare_dependency(
  link_with: mstore_lib,
  include_directories: mstore_inc,
  dependencies: lib_deps,
  variables: {'includedir': meson.current_source_dir() / 'include'},
)

# Add executable targets
subdir('app')

# Package the license files
mstore_lic = files(
  'LICENSE',
)

mstore_header = files(
  'include/mstore.h',
)

if install
  # Distribute the license files in share/licenses/<name>
  install_data(
    mstore_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )

  install_headers(
    mstore_header,
  )
  install_subdir(
    'include/mstore',
    install_dir: get_option('includedir'),
  )

  module_id = meson.project_name() / fc_id + '-' + fc.version()
  meson.add_install_script(
    find_program(files('config'/'install-mod.py')),
    get_option('includedir') / module_id,
  )

  pkg = import('pkgconfig')
  pkg.generate(
    mstore_lib,
    description: 'Molecular structure store for testing',
    subdirs: ['', module_id],
  )
endif

if get_option('python')
  subdir('python/mstore')
endif

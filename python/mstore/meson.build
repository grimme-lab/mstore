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

cc = meson.get_compiler('c')

pymod = import('python')
python = pymod.find_installation(
  get_option('python_version'),
  modules: [
    'cffi',
  ],
)
python_dep = python.dependency(required: true)

# Python's CFFI is horrible in working with preprocessor statements,
# therefore, we have to preprocess the header before passing it to the ffibuilder
mstore_pp = configure_file(
  command: [
    cc,
    '-I@0@'.format(
      mstore_dep.get_variable(
        pkgconfig: 'includedir',
        cmake: 'mstore_INCLUDE_DIRS',
        internal: 'includedir',
      ).split().get(0)
    ),
    '-DMSTORE_CFFI',
    '-E',
    '@INPUT@',
  ],
  input: mstore_header,
  output: '_libmstore.h',
  capture: true,
)

# This is the actual out-of-line API processing of the ffibuilder
mstore_cffi_srcs = configure_file(
  command: [python, files('..'/'ffi-builder.py'), '@INPUT@', '@BASENAME@'],
  input: mstore_pp,
  output: '@BASENAME@.c',
)

# Actual generation of the Python extension, since the shared_module does not work
# well with dependency objects, we will trick it by linking a whole static lib
mstore_pyext = python.extension_module(
  '_libmstore',
  link_whole: static_library(
    '_libmstore',
    mstore_cffi_srcs,
    dependencies: [mstore_dep, python_dep],
  ),
  dependencies: [mstore_dep, python_dep],
  install: install,
  subdir: 'mstore',
)

if install
  python.install_sources(
    files(
      '__init__.py',
      'interface.py',
      'library.py',
    ),
    subdir: 'mstore',
  )
endif

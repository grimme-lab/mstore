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

os = host_machine.system()
fc = meson.get_compiler('fortran')
fc_id = fc.get_id()

if fc_id == 'gcc'
  add_project_arguments(
    '-ffree-line-length-none',
    '-fbacktrace',
    language: 'fortran',
  )
elif fc_id == 'intel'
  add_project_arguments(
    '-traceback',
    language: 'fortran',
  )
elif fc_id == 'pgi'
  add_project_arguments(
    '-Mbackslash',
    '-Mallocatable=03',
    '-traceback',
    language: 'fortran',
  )
endif

if get_option('openmp')
  omp_dep = dependency('openmp')
  lib_deps += omp_dep
endif

# Create the tool chain library as subproject
mctc_dep = dependency('mctc-lib', required: false)
if not mctc_dep.found()
  mctc_prj = subproject(
    'mctc-lib',
    version: '>=0.2',
    default_options: [
      'default_library=static',
    ],
  )
  mctc_dep = mctc_prj.get_variable('mctc_dep')

  if install
    install_data(
      mctc_prj.get_variable('mctc_lic'),
      install_dir: get_option('datadir')/'licenses'/meson.project_name()/'mctc-lib'
    )
  endif
endif
lib_deps += mctc_dep

# Molecular structure store for testing

[![Apache-2.0](https://img.shields.io/github/license/grimme-lab/mstore)](LICENSE)
[![Release](https://img.shields.io/github/v/release/grimme-lab/mstore)](https://github.com/grimme-lab/mstore/releases/latest)
[![CI](https://github.com/grimme-lab/mstore/workflows/CI/badge.svg)](https://github.com/grimme-lab/mstore/actions)


## Installation

To build this project from the source code in this repository you need to have
a Fortran compiler supporting Fortran 2008 and one of the supported build systems:
- [meson](https://mesonbuild.com) version 0.53 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer
- [cmake](https://cmake.org) version 3.14 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- [fpm](https://github.com/fortran-lang/fpm) version 0.2.0 or newer

Supported for this project are GCC and Intel compilers.


### Building with meson

Setup a build with

```
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable.
To compile the project run

```
meson compile -C _build
```

To include ``mstore`` in your project add the following wrap file to your subprojects directory:

```ini
[wrap-git]
directory = mstore
url = https://github.com/grimme-lab/mstore
revision = head
```

You can retrieve the dependency from the wrap fallback with

```meson
mstore_dep = dependency('mstore', ['mstore', 'mstore_dep'])
```

and add it as dependency to your targets.


### Building with CMake

Setup a new build with

```
cmake -B _build -G Ninja
```

You can select the Fortran compiler by the `FC` environment variable.
To compile the project run

```
cmake --build _build
```

To include ``mstore`` in your CMake project retrieve it using the ``FetchContent`` module:

```cmake
if(NOT TARGET mstore)
  set("mstore-url" "https://github.com/grimme-lab/mstore")
  message(STATUS "Retrieving mctc-lib from ${mstore-url}")
  include(FetchContent)
  FetchContent_Declare(
    "mstore"
    GIT_REPOSITORY "${mstore-url}"
    GIT_TAG "HEAD"
  )
  FetchContent_MakeAvailable("mstore")
endif()
```

And link against the ``"mstore"`` interface library.

```cmake
target_link_libraries("${PROJECT_NAME}-lib" PUBLIC "mstore")
```


### Building with fpm

Invoke fpm in the project root with

```
fpm build
```

You can access the ``mstore-info`` and ``mstore-fortranize`` programs using the run subcommand

```
fpm run mstore-info
```

To use ``mstore`` for testing include it as development dependency in your package manifest

```toml
[dev-dependencies]
mstore.git = "https://github.com/grimme-lab/mstore"
```


## Example

To use this project in your testsuite just invoke the ``get_structure`` routine of the ``mstore`` module:

```f90
use mctc_io
use mstore
type(structure_type) :: mol

call get_structure(mol, "MB16-43", "01")
```

The ``get_structure`` routine loads a geometry, here *01*, of a  benchmark set, here *MB16-43*, into a ``structure_type``.
Currently available benchmark sets are

- *Amino20x4*
- *But14diol*
- *Heavy28*
- *ICE10*
- *IL16*
- *MB16-43*
- *UPU23*
- *X23*

For the detailed record names of the benchmarks look up the respective benchmark entry.


## License

Licensed under the Apache License, Version 2.0 (the “License”);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an *“as is” basis*,
*without warranties or conditions of any kind*, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in this project by you, as defined in the
Apache-2.0 license, shall be licensed as above, without any additional
terms or conditions.

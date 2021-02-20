# Molecular structure store for testing

[![Apache-2.0](https://img.shields.io/github/license/grimme-lab/mstore)](LICENSE)
[![Release](https://img.shields.io/github/v/release/grimme-lab/mstore)](https://github.com/grimme-lab/mstore/releases/latest)
[![CI](https://github.com/grimme-lab/mstore/workflows/CI/badge.svg)](https://github.com/grimme-lab/mstore/actions)


## Installation

To build this project from the source code in this repository you need to have
- a Fortran compiler supporting Fortran 2008
- [meson](https://mesonbuild.com) version 0.53 or newer
- a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer

Setup a build with

```
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable, currently this project supports GCC and Intel compilers.
To compile the project run

```
meson compile -C _build
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

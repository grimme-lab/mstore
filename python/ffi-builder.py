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
"""
FFI builder module for mstore for usage from meson and from setup.py.

Since meson has the full knowledge about the build, it will handle
the generation of the C definitions in the meson.build file rather
than in the FFI builder. This allows to correctly keep track of
dependencies and updates in the build process.

For setup.py we have to do the preprocessing ourselves here, this
requires us to use the C compiler to preprocess the header file
of mstore because the CFFI C parser cannot handle certain C
preprocessor constructs. Also, we cannot rely on an external build
system fixing dependencies for us and therefore have to find those
ourselves using pkg-config.
"""

import os
import cffi

library = "mstore"
version = "0.2.0"

include_header = '#include "' + library + '.h"'
prefix_var = library.upper() + "_PREFIX"
if prefix_var not in os.environ:
    prefix_var = "CONDA_PREFIX"

if __name__ == "__main__":
    import sys

    kwargs = dict(libraries=[library])

    header_file = sys.argv[1]
    module_name = sys.argv[2]

    with open(header_file, encoding="utf-8") as f:
        cdefs = f.read()
else:
    import subprocess

    try:
        import pkgconfig  # type: ignore

        if not pkgconfig.exists(library):
            raise ModuleNotFoundError(f"Unable to find pkg-config package '{library}'")
        if pkgconfig.installed(library, f"< {version}"):
            raise ModuleNotFoundError(
                f"Installed '{library}' version is too old, {version} or newer is required"
            )

        kwargs = pkgconfig.parse(library)
        cflags = pkgconfig.cflags(library).split()

    except ModuleNotFoundError:
        kwargs = dict(libraries=[library])
        cflags = []
        if prefix_var in os.environ:
            prefix = os.environ[prefix_var]
            kwargs.update(
                include_dirs=[os.path.join(prefix, "include")],
                library_dirs=[os.path.join(prefix, "lib")],
                runtime_library_dirs=[os.path.join(prefix, "lib")],
            )
            cflags.append("-I" + os.path.join(prefix, "include"))

    cc = os.environ["CC"] if "CC" in os.environ else "cc"

    module_name = library + "._lib" + library

    p = subprocess.Popen(
        [cc, *cflags, "-DMSTORE_CFFI", "-E", "-"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    out, err = p.communicate(include_header.encode())

    cdefs = out.decode()

ffibuilder = cffi.FFI()
ffibuilder.set_source(module_name, include_header, **kwargs)
ffibuilder.cdef(cdefs)

if __name__ == "__main__":
    ffibuilder.distutils_extension(".")

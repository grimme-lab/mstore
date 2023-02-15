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
Thin wrapper around CFFI extension module mstore.

This module mainly acts as a guard for importing the libmstore extension and
also provides some FFI based wappers for memory handling.
"""

import functools
import numpy as np

try:
    from ._libmstore import ffi, lib  # type: ignore
except ImportError as e:
    raise ImportError("mstore C extension unimportable, cannot use C-API") from e


def get_version() -> tuple:
    """Return the current API version from mstore.
    For easy usage in C the API version is provided as

    10000 * major + 100 * minor + patch

    For Python we want something that looks like a semantic version again.
    """
    version = lib.mstore_get_version()
    return (
        version // 10000,
        version % 10000 // 100,
        version % 100,
    )


def _delete_error(error) -> None:
    """Delete a mstore error handler object"""
    ptr = ffi.new("mstore_error *")
    ptr[0] = error
    lib.mstore_delete_error(ptr)


def new_error():
    """Create new mstore error handler object"""
    return ffi.gc(lib.mstore_new_error(), _delete_error)


def error_check(func):
    """Handle errors for library functions that require an error handle"""

    @functools.wraps(func)
    def handle_error(*args, **kwargs):
        """Run function and than compare context"""
        _err = new_error()
        value = func(_err, *args, **kwargs)
        if lib.mstore_check_error(_err):
            _message = ffi.new("char[]", 512)
            lib.mstore_get_error(_err, _message, ffi.NULL)
            raise RuntimeError(ffi.string(_message).decode())
        return value

    return handle_error


def _delete_structure(mol) -> None:
    """Delete molecular structure data"""
    ptr = ffi.new("mstore_structure *")
    ptr[0] = mol
    lib.mstore_delete_structure(ptr)


def new_structure(natoms, numbers, positions, charge, uhf, lattice, periodic):
    """Create new molecular structure data"""
    return ffi.gc(
        error_check(lib.mstore_new_structure)(
            natoms,
            numbers,
            positions,
            charge,
            uhf,
            lattice,
            periodic,
        ),
        _delete_structure,
    )


def get_structure_from_name(collection, record):
    """Get a molecular structure from the mstore database."""
    cmol = ffi.gc(
        error_check(lib.mstore_get_structure)(
            collection,
            record,
        ),
        _delete_structure,
    )
    
    # the following is probably a little bit pedestrian, but it works...
    _natoms = ffi.new("int *")
    error_check(lib.mstore_get_structure_number_of_atoms)(cmol, _natoms)

    _numbers = np.zeros((_natoms[0],), dtype="i4")
    error_check(lib.mstore_get_structure_numbers)(
        cmol, ffi.cast("int *", _numbers.ctypes.data)
    )

    _positions = np.zeros((_natoms[0], 3))
    error_check(lib.mstore_get_structure_positions)(
        cmol, ffi.cast("double *", _positions.ctypes.data)
    )

    _charge = ffi.new("double *")
    error_check(lib.mstore_get_structure_charge)(cmol, _charge)

    _uhf = ffi.new("int *")
    error_check(lib.mstore_get_structure_uhf)(cmol, _uhf)

    return _numbers, _positions, _charge[0], _uhf[0]


update_structure_geometry = error_check(lib.mstore_update_structure_geometry)

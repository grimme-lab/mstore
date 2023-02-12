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
Definition of the basic interface to library for most further integration in
other Python frameworks. The classes defined here allow a more Pythonic usage
of the library in actual workflows than the low-level access provided in the
CFFI generated wrappers.
"""

from __future__ import annotations

import numpy as np

from . import library


class Structure:
    """
    .. Molecular structure data

    Represents a wrapped structure object in ``mstore``.
    The molecular structure data object has a fixed number of atoms
    and immutable atomic identifiers.

    Example
    -------
    >>> from mstore.interface import Structure
    >>> import numpy as np
    >>> mol = Structure(
    ...     positions=np.array([
    ...         [+0.00000000000000, +0.00000000000000, -0.73578586109551],
    ...         [+1.44183152868459, +0.00000000000000, +0.36789293054775],
    ...         [-1.44183152868459, +0.00000000000000, +0.36789293054775],
    ...     ]),
    ...     numbers = np.array([8, 1, 1]),
    ... )
    ...
    >>> len(mol)
    3

    Raises
    ------
    ValueError
        on invalid input, like incorrect shape / type of the passed arrays
    """

    _mol = library.ffi.NULL
    """`cdata` object of molecular structure."""

    numbers: np.ndarray
    """Atomic numbers of the system."""

    positions: np.ndarray
    """Atomic coordinates of the system (nat, 3)."""

    charge: float | None
    """Total charge of the system."""

    def __init__(
        self,
        numbers: np.ndarray,
        positions: np.ndarray,
        charge: float | None = None,
        uhf: int | None = None,
        lattice: np.ndarray | None = None,
        periodic: np.ndarray | None = None,
    ):
        """
        Create new molecular structure data from arrays. The returned object has
        immutable atomic species and boundary condition, also the total number
        of atoms cannot be changed.

        Raises
        ------
        ValueError
            on invalid input, like incorrect shape / type of the passed arrays
        """
        if positions.size % 3 != 0:
            raise ValueError("Expected tripels of cartesian coordinates")

        if 3 * numbers.size != positions.size:
            raise ValueError("Dimension missmatch between numbers and positions")

        self._natoms = len(numbers)
        self.numbers = np.ascontiguousarray(numbers, dtype="i4")
        self.positions = np.ascontiguousarray(positions, dtype="float")
        self.charge = charge
        self.uhf = uhf

        if lattice is not None:
            if lattice.size != 9:
                raise ValueError("Invalid lattice provided")
            _lattice = np.ascontiguousarray(lattice, dtype="float")
        else:
            _lattice = None

        if periodic is not None:
            if periodic.size != 3:
                raise ValueError("Invalid periodicity provided")
            _periodic = np.ascontiguousarray(periodic, dtype="bool")
        else:
            _periodic = None

        self._mol = library.new_structure(
            self._natoms,
            _cast("int*", self.numbers),
            _cast("double*", self.positions),
            _ref("double", charge),
            _ref("int", uhf),
            _cast("double*", _lattice),
            _cast("bool*", _periodic),
        )

    def __len__(self):
        return self._natoms

    def update(
        self,
        positions: np.ndarray,
        lattice: np.ndarray | None = None,
    ) -> None:
        """Update coordinates and lattice parameters, both provided in
        atomic units (Bohr).
        The lattice update is optional also for periodic structures.

        Generally, only the cartesian coordinates and the lattice parameters
        can be updated, every other modification, regarding total charge,
        total spin, boundary condition, atomic types or number of atoms
        requires the complete reconstruction of the object.

        Raises
        ------
        ValueError
            on invalid input, like incorrect shape / type of the passed arrays
        """

        if 3 * len(self) != positions.size:
            raise ValueError("Dimension missmatch for positions")
        _positions = np.ascontiguousarray(positions, dtype="float")

        if lattice is not None:
            if lattice.size != 9:
                raise ValueError("Invalid lattice provided")
            _lattice = np.ascontiguousarray(lattice, dtype="float")
        else:
            _lattice = None

        library.update_structure_geometry(
            self._mol,
            _cast("double*", _positions),
            _cast("double*", _lattice),
        )


def get_structure(collection: str, record: str) -> Structure:
    """
    Get a molecular structure from the `mstore` database using the
    name of a `collection` and the `record` with the former.

    Parameters
    ----------
    collection : str
        Name of the collection.
    record : str
        Name of the record in the collection.

    Returns
    -------
    Structure
        Molecular structure object.
    """
    # names in Fortran are case sensitive
    COLLECTIONS = {
        "mb16-43": "MB16-43",
        "amino20x4": "Amino20x4",
        "but14diol": "But14diol",
        "heavy28": "Heavy28",
        "ice10": "ICE10",
        "il16": "IL16",
        "upu23": "UPU23",
        "x23": "X23",
    }

    return Structure(
        *library.get_structure_from_name(
            _ref("char[]", COLLECTIONS.get(collection.casefold(), collection)),
            _ref("char[]", record),
        )
    )


def _cast(ctype: str, array: None | np.ndarray):
    """Cast a numpy array to an FFI pointer"""
    return (
        library.ffi.NULL
        if array is None
        else library.ffi.cast(ctype, array.ctypes.data)
    )


def _ref(ctype: str, value: str | float | None):
    """Create a reference to a value"""
    if value is None:
        return library.ffi.NULL

    if isinstance(value, str) and ctype == "char[]":
        return library.ffi.new(ctype, value.encode("ascii"))

    ref = library.ffi.new(ctype + "*")
    ref[0] = value
    return ref

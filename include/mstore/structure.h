/* This file is part of mstore.
 * SPDX-Identifier: Apache-2.0
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
**/

/**
 * @file mstore/structure.h
 * @brief
 * The structure data #mstore_structure is used to represent the system of
 * interest in the library.
 *
 * It contains immutable system specific information like the number of atoms,
 * the unique atom groups and the boundary conditions as well as mutable
 * geometry data like cartesian coordinates and lattice parameters.
 *
 * To change immutable parameters of the #mstore_structure data the object
 * should be reinstantiated as well as all dependent objects, like 
 * #mstore_calculator or #mstore_result instances.
 */

#pragma once

#include "mstore/macros.h"
#include "mstore/error.h"

/*
 * Molecular structure data class
**/

/// Molecular structure data class.
///
/// The structure data is used to represent the system of interest in the library.
/// It contains immutable system specific information like the number of atoms,
/// the unique atom groups and the boundary conditions as well as mutable geometry
/// data like cartesian coordinates and lattice parameters.
typedef struct _mstore_structure* mstore_structure;

/// Create new molecular structure data
///
/// @param error: Handle for error messages
/// @param natoms: Number of atoms
/// @param numbers: Atomic numbers for each atom, shape [natoms]
/// @param positions: Cartesian coordinates in Bohr for each atom, shape [natoms][3]
/// @param charge: Total charge of the system, (optional)
/// @param uhf: Number of unpaired electrons, (optional)
/// @param lattice: Lattice parameters in Bohr, shape [3][3], (optional)
/// @param periodic: Periodic dimensions, shape [3], (optional)
/// @return New molecular structure data
MSTORE_API_ENTRY mstore_structure MSTORE_API_CALL
mstore_new_structure(mstore_error error,
                     const int natoms,
                     const int* numbers,
                     const double* positions,
                     const double* charge,
                     const int* uhf,
                     const double* lattice,
                     const bool* periodic);

/// Delete molecular structure data
///
/// @param mol: Molecular structure data
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_delete_structure(mstore_structure* mol);

/// Update coordinates and lattice parameters (quantities in Bohr)
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure data
/// @param positions: Cartesian coordinates in Bohr for each atom, shape [natoms][3]
/// @param lattice: Lattice parameters in Bohr, shape [3][3], (optional)
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_update_structure_geometry(mstore_error error,
                                 mstore_structure mol,
                                 const double* positions,
                                 const double* lattice);

/// Update total charge in structure object
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure data
/// @param charge: Total charge of the system
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_update_structure_charge(mstore_error error,
                               mstore_structure mol,
                               const double* charge);

/// Update number of unpaired electrons in structure object
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure data
/// @param uhf: Number of unpaired electrons
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_update_structure_uhf(mstore_error error,
                            mstore_structure mol,
                            const int* uhf);

/// Obtain a structure from the name of the collection and record
///
/// @param error: Handle for error messages
/// @param collection: Name of the collection the structure is in
/// @param record: Name of the structure
/// @return Molecular structure data
MSTORE_API_ENTRY mstore_structure MSTORE_API_CALL
mstore_get_structure(mstore_error error,
                     const char collection[],
                     const char record[]);

/// Retrieve number of atoms from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param nat: Number of atoms
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_number_of_atoms(mstore_error error,
                                     mstore_structure mol,
                                     const int* nat);

/// Retrieve atoms from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param numbers: Atomic numbers
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_numbers(mstore_error error,
                             mstore_structure mol,
                             const int* numbers);

/// Retrieve positions from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param positions: Atomic coordinates (3, nat)
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_positions(mstore_error error,
                               mstore_structure mol,
                               const double* positions);

/// Retrieve total charge from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param charge: Total charge of the system
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_charge(mstore_error error,
                            mstore_structure mol,
                            const double* charge);

/// Retrieve spin from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param uhf: Spin of the system
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_uhf(mstore_error error,
                         mstore_structure mol,
                         const int* uhf);

/// Retrieve lattice parameters from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param lattice: Atomic coordinates (3, nat)
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_lattice(mstore_error error,
                             mstore_structure mol,
                             const double* lattice);

/// Retrieve periodic directions from molecular structure
///
/// @param error: Handle for error messages
/// @param mol: Molecular structure
/// @param periodic: Atomic coordinates (3, nat)
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_structure_periodic(mstore_error error,
                              mstore_structure mol,
                              const bool* periodic);
! This file is part of mstore.
! SPDX-Identifier: Apache-2.0
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> @file mstore/api/structure.f90
!> Provides API exports for the #mstore_structure handle.

!> API export for working with molecular structure data objects
module mstore_api_structure
   use, intrinsic :: iso_c_binding
   use mctc_env, only : wp, error_type, fatal_error
   use mctc_io, only : structure_type, new
   use mstore_api_error, only : vp_error
   use mstore_api_version, only : namespace
   use mstore_api_utils, only : c_f_character, wrap_to_central_cell
   use mstore, only : get_record
   implicit none
   private

   public :: vp_structure
   public :: new_structure_api, delete_structure_api, update_structure_geometry_api
   public :: update_structure_charge_api, update_structure_uhf_api

   public :: get_structure_api
   public :: get_structure_number_of_atoms_api, get_structure_numbers_api
   public :: get_structure_positions_api, get_structure_charge_api

   !> Void pointer to molecular structure data
   type :: vp_structure
      !> Actual payload
      type(structure_type) :: ptr
   end type vp_structure


   logical, parameter :: debug = .false.


contains

!> Create new molecular structure data (quantities in Bohr)
function new_structure_api(verror, natoms, numbers, positions, c_charge, c_uhf, &
      & c_lattice, c_periodic) result(vmol) &
      & bind(C, name=namespace//"new_structure")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   integer(c_int), value, intent(in) :: natoms
   integer(c_int), intent(in) :: numbers(natoms)
   real(c_double), intent(in) :: positions(3, natoms)
   real(c_double), intent(in), optional :: c_charge
   real(wp), allocatable :: charge
   integer(c_int), intent(in), optional :: c_uhf
   integer, allocatable :: uhf
   real(c_double), intent(in), optional :: c_lattice(3, 3)
   real(wp), allocatable :: lattice(:, :)
   logical(c_bool), intent(in), optional :: c_periodic(3)
   logical, allocatable :: periodic(:)
   type(vp_structure), pointer :: mol
   type(c_ptr) :: vmol

   if (debug) print '("[Info]",1x, a)', "new_structure"

   vmol = c_null_ptr

   if (.not.c_associated(verror)) return
   call c_f_pointer(verror, error)

   if (present(c_lattice)) then
      allocate(lattice(3, 3))
      lattice(:, :) = c_lattice
   end if
   if (present(c_periodic)) then
      allocate(periodic(3))
      periodic(:) = c_periodic
   end if
   if (present(c_charge)) then
      charge = c_charge
   end if
   if (present(c_uhf)) then
      uhf = c_uhf
   end if

   allocate(mol)
   call new(mol%ptr, numbers, positions, lattice=lattice, periodic=periodic, &
      charge=charge, uhf=uhf)
   vmol = c_loc(mol)

   call wrap_to_central_cell(mol%ptr%xyz, mol%ptr%lattice, mol%ptr%periodic)

   call verify_structure(error%ptr, mol%ptr)

end function new_structure_api


!> Delete molecular structure data
subroutine delete_structure_api(vmol) &
      & bind(C, name=namespace//"delete_structure")
   type(c_ptr), intent(inout) :: vmol
   type(vp_structure), pointer :: mol

   if (debug) print '("[Info]",1x, a)', "delete_structure"

   if (c_associated(vmol)) then
      call c_f_pointer(vmol, mol)

      deallocate(mol)
      vmol = c_null_ptr
   end if

end subroutine delete_structure_api


!> Update coordinates and lattice parameters (quantities in Bohr)
subroutine update_structure_geometry_api(verror, vmol, positions, lattice) &
      & bind(C, name=namespace//"update_structure_geometry")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   real(c_double), intent(in) :: positions(3, *)
   real(c_double), intent(in), optional :: lattice(3, 3)

   if (debug) print '("[Info]",1x, a)', "update_structure"

   if (.not.c_associated(verror)) return
   call c_f_pointer(verror, error)

   if (.not.c_associated(vmol)) then
      call fatal_error(error%ptr, "Molecular structure data is missing")
      return
   end if
   call c_f_pointer(vmol, mol)

   if (mol%ptr%nat <= 0 .or. mol%ptr%nid <= 0 .or. .not.allocated(mol%ptr%num) &
      & .or. .not.allocated(mol%ptr%id) .or. .not.allocated(mol%ptr%xyz)) then
      call fatal_error(error%ptr, "Invalid molecular structure data provided")
      return
   end if

   mol%ptr%xyz(:, :) = positions(:3, :mol%ptr%nat)
   if (present(lattice)) then
      mol%ptr%lattice(:, :) = lattice(:3, :3)
   end if

   call wrap_to_central_cell(mol%ptr%xyz, mol%ptr%lattice, mol%ptr%periodic)

   call verify_structure(error%ptr, mol%ptr)

end subroutine update_structure_geometry_api


!> Update coordinates and lattice parameters (quantities in Bohr)
subroutine update_structure_charge_api(verror, vmol, c_charge) &
      & bind(C, name=namespace//"update_structure_charge")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   real(c_double), intent(in), optional :: c_charge

   if (debug) print '("[Info]",1x, a)', "update_structure_charge"

   if (.not.c_associated(verror)) return
   call c_f_pointer(verror, error)

   if (.not.c_associated(vmol)) then
      call fatal_error(error%ptr, "Molecular structure data is missing")
      return
   end if
   call c_f_pointer(vmol, mol)

   if (mol%ptr%nat <= 0 .or. mol%ptr%nid <= 0 .or. .not.allocated(mol%ptr%num) &
      & .or. .not.allocated(mol%ptr%id) .or. .not.allocated(mol%ptr%xyz)) then
      call fatal_error(error%ptr, "Invalid molecular structure data provided")
      return
   end if

   if (present(c_charge)) then
      mol%ptr%charge = c_charge
   end if

end subroutine update_structure_charge_api


!> Update coordinates and lattice parameters (quantities in Bohr)
subroutine update_structure_uhf_api(verror, vmol, c_uhf) &
      & bind(C, name=namespace//"update_structure_uhf")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   integer(c_int), intent(in), optional :: c_uhf

   if (debug) print '("[Info]",1x, a)', "update_structure_uhf"

   if (.not.c_associated(verror)) return
   call c_f_pointer(verror, error)

   if (.not.c_associated(vmol)) then
      call fatal_error(error%ptr, "Molecular structure data is missing")
      return
   end if
   call c_f_pointer(vmol, mol)

   if (mol%ptr%nat <= 0 .or. mol%ptr%nid <= 0 .or. .not.allocated(mol%ptr%num) &
      & .or. .not.allocated(mol%ptr%id) .or. .not.allocated(mol%ptr%xyz)) then
      call fatal_error(error%ptr, "Invalid molecular structure data provided")
      return
   end if

   if (present(c_uhf)) then
      mol%ptr%uhf = c_uhf
   end if

end subroutine update_structure_uhf_api


!> Obtain a structure from the name of the collection and record
function get_structure_api(verror, c_collection, c_record) result(vmol) &
      & bind(C, name=namespace//"get_structure")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   character(kind=c_char), intent(in) :: c_collection(*)
   character(len=:), allocatable :: collection
   character(kind=c_char), intent(in) :: c_record(*)
   character(len=:), allocatable :: record
   
   type(vp_structure), pointer :: mol
   type(c_ptr) :: vmol

   if (debug) print '("[Info]",1x, a)', "get_structure"

   vmol = c_null_ptr

   if (.not.c_associated(verror)) return
   call c_f_pointer(verror, error)

   call c_f_character(c_collection, collection)
   call c_f_character(c_record, record)

   allocate(mol)
   call get_record(mol%ptr, collection, record, error%ptr)
   
   if (allocated(error%ptr)) then
      deallocate(mol)
   else
      vmol = c_loc(mol)
   end if
end function get_structure_api


!> Helper function to get Fortran structure object from C object
subroutine get_molecule(verror, vmol, error, mol, ok)
   type(c_ptr), intent(in) :: verror
   type(c_ptr), intent(in) :: vmol
   type(vp_error), pointer, intent(out) :: error
   type(vp_structure), pointer, intent(out) :: mol
   logical, intent(out) :: ok

   ok = .false.
   if (.not.c_associated(verror)) return
   call c_f_pointer(verror, error)

   if (.not.c_associated(vmol)) then
      call fatal_error(error%ptr, "Result container is missing")
      return
   end if
   call c_f_pointer(vmol, mol)
   ok = .true.
end subroutine get_molecule


subroutine get_structure_number_of_atoms_api(verror, vmol, natoms) &
      & bind(C, name=namespace//"get_structure_number_of_atoms")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   integer(c_int), intent(out) :: natoms
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_number_of_atoms"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   if (mol%ptr%nat == 0) then
      call fatal_error(error%ptr, "Molecular structure does not contain any atoms")
      return
   end if

   natoms = mol%ptr%nat
end subroutine get_structure_number_of_atoms_api


subroutine get_structure_numbers_api(verror, vmol, numbers) &
      & bind(C, name=namespace//"get_structure_numbers")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   integer(c_int), intent(out) :: numbers(*)
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_numbers"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   if (.not.allocated(mol%ptr%num)) then
      call fatal_error(error%ptr, "Molecular structure does not contain numbers")
      return
   end if

   numbers(:mol%ptr%nat) = mol%ptr%num(mol%ptr%id)
end subroutine get_structure_numbers_api


subroutine get_structure_positions_api(verror, vmol, positions) &
      & bind(C, name=namespace//"get_structure_positions")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   real(c_double), intent(out) :: positions(*)
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_positions"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   if (.not.allocated(mol%ptr%xyz)) then
      call fatal_error(error%ptr, "Molecular structure does not contain positions")
      return
   end if

   positions(:size(mol%ptr%xyz)) = reshape(mol%ptr%xyz, [size(mol%ptr%xyz)])
end subroutine get_structure_positions_api


subroutine get_structure_charge_api(verror, vmol, charge) &
      & bind(C, name=namespace//"get_structure_charge")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   real(c_double), intent(out) :: charge
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_charge"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   charge = mol%ptr%charge
end subroutine get_structure_charge_api


subroutine get_structure_uhf_api(verror, vmol, uhf) &
      & bind(C, name=namespace//"get_structure_uhf")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   integer(c_int), intent(out) :: uhf
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_uhf"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   uhf = mol%ptr%uhf
end subroutine get_structure_uhf_api


subroutine get_structure_lattice_api(verror, vmol, lattice) &
      & bind(C, name=namespace//"get_structure_lattice")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   real(c_double), intent(out) :: lattice(*)
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_lattice"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   if (.not.allocated(mol%ptr%lattice)) then
      call fatal_error(error%ptr, "Molecular structure does not contain lattice")
      return
   end if

   lattice(:size(mol%ptr%lattice)) = reshape(mol%ptr%lattice, [size(mol%ptr%lattice)])
end subroutine get_structure_lattice_api


subroutine get_structure_periodic_api(verror, vmol, periodic) &
      & bind(C, name=namespace//"get_structure_periodic")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   logical(c_bool), intent(out) :: periodic(*)
   logical :: ok

   if (debug) print '("[Info]", 1x, a)', "get_structure_periodic"

   call get_molecule(verror, vmol, error, mol, ok)
   if (.not.ok) return

   if (.not.allocated(mol%ptr%periodic)) then
      call fatal_error(error%ptr, "Molecular structure does not contain periodic")
      return
   end if

   periodic(:size(mol%ptr%periodic)) = mol%ptr%periodic
end subroutine get_structure_periodic_api


!> Cold fusion check
subroutine verify_structure(error, mol)
   type(error_type), allocatable, intent(out) :: error
   type(structure_type), intent(in) :: mol
   integer :: iat, jat, stat
   stat = 0
   do iat = 1, mol%nat
      do jat = 1, iat - 1
         if (norm2(mol%xyz(:, jat) - mol%xyz(:, iat)) < 1.0e-9_wp) stat = stat + 1
      end do
   end do
   if (stat > 0) then
      call fatal_error(error, "Too close interatomic distances found")
   end if
end subroutine verify_structure

end module mstore_api_structure

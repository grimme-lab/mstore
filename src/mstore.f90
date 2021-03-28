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

module mstore
   use, intrinsic :: iso_fortran_env, only : error_unit
   use mctc_env, only : error_type, fatal_error
   use mctc_io_structure, only : structure_type
   use mstore_data_collection, only : collection_type, new_collection, select_collection
   use mstore_data_record, only : select_record
   use mstore_data_store, only : store_type, new_store
   use mstore_amino20x4, only : get_amino20x4_records
   use mstore_but14diol, only : get_but14diol_records
   use mstore_heavy28, only : get_heavy28_records
   use mstore_ice10, only : get_ice10_records
   use mstore_il16, only : get_il16_records
   use mstore_mb16_43, only : get_mb16_43_records
   use mstore_upu23, only : get_upu23_records
   use mstore_x23, only : get_x23_records
   implicit none
   private

   public :: get_structure
   public :: list_collections, list_records

   type(store_type), allocatable :: store

contains

subroutine check_mstore
   !$omp critical (mstore_load)
   if (.not.allocated(store)) store = new_store("mstore", get_mstore_collections)
   !$omp end critical (mstore_load)
end subroutine check_mstore

subroutine list_collections(unit, error)
   integer, intent(in) :: unit
   type(error_type), allocatable, intent(out) :: error
   integer :: is

   call check_mstore

   do is = 1, size(store%collections)
      write(unit, '(a)') store%collections(is)%id
   end do

end subroutine list_collections

subroutine list_records(unit, collection, error)
   integer, intent(in) :: unit
   character(len=*), intent(in) :: collection
   type(error_type), allocatable, intent(out) :: error
   integer :: is, ir

   call check_mstore

   is = select_collection(store%collections, collection)

   if (is < 1) then
      call fatal_error(error, "Requested collection '"//collection//"' not available")
      return
   end if

   do ir = 1, size(store%collections(is)%records)
      write(unit, '(a)') store%collections(is)%records(ir)%id
   end do

end subroutine list_records

subroutine get_record(mol, collection, record, error)
   type(structure_type), intent(out) :: mol
   character(len=*), intent(in) :: collection
   character(len=*), intent(in) :: record
   type(error_type), allocatable :: error
   integer :: is, ir

   call check_mstore

   is = select_collection(store%collections, collection)

   if (is < 1) then
      call fatal_error(error, "Requested collection '"//collection//"' not available")
      return
   end if

   ir = select_record(store%collections(is)%records, record)

   if (ir < 1) then
      call fatal_error(error, "Requested record '"//record//"' not available in '"//&
         & collection//"' collection")
      return
   end if

   call store%collections(is)%records(ir)%get(mol)

end subroutine get_record

subroutine get_structure(self, collection, record)
   type(structure_type), intent(out) :: self
   character(len=*), intent(in) :: collection
   character(len=*), intent(in) :: record
   type(error_type), allocatable :: error

   call get_record(self, collection, record, error)

   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

end subroutine get_structure

subroutine get_mstore_collections(collections)
   type(collection_type), allocatable, intent(out) :: collections(:)

   collections = [ &
      new_collection("Amino20x4", get_amino20x4_records), &
      new_collection("But14diol", get_but14diol_records), &
      new_collection("Heavy28", get_heavy28_records), &
      new_collection("ICE10", get_ice10_records), &
      new_collection("IL16", get_il16_records), &
      new_collection("MB16-43", get_mb16_43_records), &
      new_collection("UPU23", get_upu23_records), &
      new_collection("X23", get_x23_records) &
      ]

end subroutine get_mstore_collections

end module mstore

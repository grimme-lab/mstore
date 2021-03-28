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

module mstore_data_record
   use mctc_io, only : structure_type
   implicit none
   private

   public :: record_type, new_record, select_record

   abstract interface
      subroutine get_structure_interface(self)
         import :: structure_type
         type(structure_type), intent(out) :: self
      end subroutine get_structure_interface
   end interface

   !> Data base record of a structure type
   type :: record_type

      !> Identifier for this record
      character(len=:), allocatable :: id

      !> Getter for the actual structure
      procedure(get_structure_interface), pointer, nopass :: get => null()
   end type record_type

contains


!> Register a new record
function new_record(id, getter) result(new)

   !> Identifier for this record
   character(len=*), intent(in) :: id

   !> Getter for the actual structure
   procedure(get_structure_interface) :: getter

   !> New structure record
   type(record_type) :: new

   new%id = id
   new%get => getter
end function new_record


!> Select a test suite from all available suites
function select_record(records, id) result(pos)

   !> Name identifying the record
   character(len=*), intent(in) :: id

   !> Available structure records
   type(record_type) :: records(:)

   !> Selected test suite
   integer :: pos

   integer :: it

   pos = 0
   do it = 1, size(records)
      if (id == records(it)%id) then
         pos = it
         exit
      end if
   end do

end function select_record


end module mstore_data_record

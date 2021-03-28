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

module mstore_data_collection
   use mstore_data_record, only : record_type
   implicit none
   private

   public :: collection_type, new_collection, select_collection

   abstract interface
      subroutine get_record_interface(records)
         import :: record_type
         type(record_type), allocatable, intent(out) :: records(:)
      end subroutine get_record_interface
   end interface

   !> Data base collection of a record type
   type :: collection_type

      !> Identifier for this collection
      character(len=:), allocatable :: id

      !> Collection of records
      type(record_type), allocatable :: records(:)

      !> Actual getter for the record data
      procedure(get_record_interface), pointer, nopass :: get => null()

   end type collection_type

contains


!> Register a new collection
function new_collection(id, getter) result(new)

   !> Identifier for this collection
   character(len=*), intent(in) :: id

   !> Getter for the actual record
   procedure(get_record_interface) :: getter

   !> New record collection
   type(collection_type) :: new

   new%id = id
   new%get => getter
   call getter(new%records)

end function new_collection


!> Select a test suite from all available suites
function select_collection(collections, id) result(pos)

   !> Name identifying the collection
   character(len=*), intent(in) :: id

   !> Available record collections
   type(collection_type) :: collections(:)

   !> Selected test suite
   integer :: pos

   integer :: it

   pos = 0
   do it = 1, size(collections)
      if (id == collections(it)%id) then
         pos = it
         exit
      end if
   end do

end function select_collection


end module mstore_data_collection

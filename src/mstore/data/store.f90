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

module mstore_data_store
   use mctc_env_error, only : error_type, fatal_error
   use mstore_data_collection, only : collection_type
   use mstore_data_record, only : record_type
   implicit none
   private

   public :: store_type, new_store

   abstract interface
      subroutine get_collection_interface(collections)
         import :: collection_type
         type(collection_type), allocatable, intent(out) :: collections(:)
      end subroutine get_collection_interface
   end interface

   type :: store_type

      !> Identifier for this store
      character(len=:), allocatable :: id

      !> Cached collections
      type(collection_type), allocatable :: collections(:)

      !> Actual getter for the collection data
      procedure(get_collection_interface), pointer, nopass :: get => null()

   contains

      procedure :: get_record

   end type store_type


contains


!> Register a new store
function new_store(id, getter) result(new)

   !> Identifier for this store
   character(len=*), intent(in) :: id

   !> Getter for the actual record
   procedure(get_collection_interface) :: getter

   !> New record store
   type(store_type) :: new

   new%id = id
   new%get => getter
   call getter(new%collections)

end function new_store


subroutine get_record(self, set, id, error)
   class(store_type), intent(in) :: self
   character(len=*), intent(in) :: set
   character(len=*), intent(in) :: id
   type(error_type), allocatable, intent(out) :: error

end subroutine get_record


end module mstore_data_store

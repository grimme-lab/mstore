! This file is part of mstore.
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
   use mctc_io_structure, only : structure_type
   use mstore_mb16_43, only : get_structure_mb16_43
   use mstore_upu23, only : get_structure_upu23
   use mstore_x23, only : get_structure_x23
   implicit none
   private

   public :: get_structure

contains

subroutine get_structure(self, set, id)
   type(structure_type), intent(out) :: self
   character(len=*), intent(in) :: set
   character(len=*), intent(in) :: id

   select case(set)
   case default; error stop "Unknown set"
   case("MB16-43"); call get_structure_mb16_43(self, id)
   case("UPU23"); call get_structure_upu23(self, id)
   case("X23"); call get_structure_x23(self, id)
   end select

end subroutine get_structure

end module mstore

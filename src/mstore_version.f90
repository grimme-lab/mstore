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

module mstore_version
   implicit none
   private

   public :: mstore_version_string, mstore_version_compact
   public :: get_mstore_version


   !> String representation of the mstore version
   character(len=*), parameter :: mstore_version_string = "0.2.0"

   !> Numeric representation of the mstore version
   integer, parameter :: mstore_version_compact(3) = [0, 2, 0]


contains


!> Getter function to retrieve mstore version
subroutine get_mstore_version(major, minor, patch, string)

   !> Major version number of the mstore version
   integer, intent(out), optional :: major

   !> Minor version number of the mstore version
   integer, intent(out), optional :: minor

   !> Patch version number of the mstore version
   integer, intent(out), optional :: patch

   !> String representation of the mstore version
   character(len=:), allocatable, intent(out), optional :: string

   if (present(major)) then
      major = mstore_version_compact(1)
   end if
   if (present(minor)) then
      minor = mstore_version_compact(2)
   end if
   if (present(patch)) then
      patch = mstore_version_compact(3)
   end if
   if (present(string)) then
      string = mstore_version_string
   end if

end subroutine get_mstore_version


end module mstore_version

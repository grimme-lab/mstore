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

!> @dir mstore/api
!> Provides C API bindings for mstore library

!> @file mstore/api/version.f90
!> Provides a stable version query with #mstore_get_version.

!> API export for the version information of the library
module mstore_api_version
   use, intrinsic :: iso_c_binding
   use mstore_version, only : get_mstore_version
   implicit none
   private

   public :: get_version_api, namespace

   character(len=*), parameter :: namespace = "mstore_"

contains


!> Obtain library version as major * 10000 + minor + 100 + patch
function get_version_api() result(version) &
      & bind(C, name=namespace//"get_version")
   integer(c_int) :: version
   integer :: major, minor, patch

   call get_mstore_version(major, minor, patch)
   version = 10000_c_int * major + 100_c_int * minor + patch

end function get_version_api


end module mstore_api_version
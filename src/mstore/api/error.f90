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

!> @file mstore/api/error.f90
!> Provides API exports for the #mstore_error handle.

!> API export for error handling
module mstore_api_error
   use, intrinsic :: iso_c_binding
   use mctc_env, only : error_type, fatal_error
   use mstore_api_version, only : namespace
   use mstore_api_utils, only : f_c_character
   implicit none
   private

   public :: vp_error
   public :: new_error_api, check_error_api, clear_error_api, get_error_api, delete_error_api


   !> Void pointer to error handle
   type :: vp_error
      !> Actual payload
      type(error_type), allocatable :: ptr
   end type vp_error


   logical, parameter :: debug = .false.


contains


!> Create new error handle object
function new_error_api() &
      & result(verror) &
      & bind(C, name=namespace//"new_error")
   type(vp_error), pointer :: error
   type(c_ptr) :: verror

   if (debug) print '("[Info]", 1x, a)', "new_error"

   allocate(error)
   verror = c_loc(error)

end function new_error_api


!> Delete error handle object
subroutine delete_error_api(verror) &
      & bind(C, name=namespace//"delete_error")
   type(c_ptr), intent(inout) :: verror
   type(vp_error), pointer :: error

   if (debug) print '("[Info]", 1x, a)', "delete_error"

   if (c_associated(verror)) then
      call c_f_pointer(verror, error)

      deallocate(error)
      verror = c_null_ptr
   end if

end subroutine delete_error_api


!> Check error handle status
function check_error_api(verror) result(status) &
      & bind(C, name=namespace//"check_error")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   integer(c_int) :: status

   if (debug) print '("[Info]", 1x, a)', "check_error"

   if (c_associated(verror)) then
      call c_f_pointer(verror, error)

      if (allocated(error%ptr)) then
         status = 1
      else
         status = 0
      end if
   else
      status = 2
   end if

end function check_error_api


!> Clear error from handle
subroutine clear_error_api(verror) &
      & bind(C, name=namespace//"clear_error")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error

   if (debug) print '("[Info]", 1x, a)', "clear_error"

   if (c_associated(verror)) then
      call c_f_pointer(verror, error)

      if (allocated(error%ptr)) then
         deallocate(error%ptr)
      end if
   end if

end subroutine clear_error_api


!> Get error message from error handle
subroutine get_error_api(verror, charptr, buffersize) &
      & bind(C, name=namespace//"get_error")
   type(c_ptr), value :: verror
   type(vp_error), pointer :: error
   character(kind=c_char), intent(inout) :: charptr(*)
   integer(c_int), intent(in), optional :: buffersize
   integer :: max_length

   if (debug) print '("[Info]", 1x, a)', "get_error"

   if (c_associated(verror)) then
      call c_f_pointer(verror, error)

      if (present(buffersize)) then
         max_length = buffersize
      else
         max_length = huge(max_length) - 2
      end if

      if (allocated(error%ptr)) then
         call f_c_character(error%ptr%message, charptr, max_length)
      end if
   end if

end subroutine get_error_api


end module mstore_api_error

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

!> @file mstore/api/utils.f90
!> Common utilities for all API exports

!> Provides utilities used for all API export modules
module mstore_api_utils
   use, intrinsic :: iso_c_binding
   use mctc_env, only : wp
   use mctc_io_math, only : matinv_3x3
   implicit none
   private

   public :: f_c_character, c_f_character, strlen, wrap_to_central_cell


   interface
      function strlen(str) result(len) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: str
         integer(c_int) :: len
      end function strlen
   end interface

contains

subroutine wrap_to_central_cell(xyz, lattice, periodic)
   real(wp), intent(inout) :: xyz(:, :)
   real(wp), intent(in) :: lattice(:, :)
   logical, intent(in) :: periodic(:)
   real(wp) :: invlat(3, 3), vec(3)
   integer :: iat, idir

   if (.not.any(periodic)) return

   invlat = matinv_3x3(lattice)
   do iat = 1, size(xyz, 2)
      vec(:) = matmul(invlat, xyz(:, iat))
      vec(:) = shift_back_abc(vec)
      xyz(:, iat) = matmul(lattice, vec)
   end do

end subroutine wrap_to_central_cell

elemental function shift_back_abc(in) result(out)
   !> fractional coordinate in (-∞,+∞)
   real(wp),intent(in) :: in
   !> fractional coordinate in [0,1)
   real(wp) :: out
   real(wp),parameter :: p_pbc_eps = 1.0e-14_wp
   out = in
   if(in < (0.0_wp - p_pbc_eps)) &
      out = in + real(ceiling(-in),wp)
   if(in > (1.0_wp + p_pbc_eps)) &
      out = in - real(floor  ( in),wp)
   if (abs(in - 1.0_wp) < p_pbc_eps) &
      out = in - 1.0_wp
end function shift_back_abc

subroutine f_c_character(rhs, lhs, len)
   character(kind=c_char), intent(out) :: lhs(*)
   character(len=*), intent(in) :: rhs
   integer, intent(in) :: len
   integer :: length
   length = min(len-1, len_trim(rhs))

   lhs(1:length) = transfer(rhs(1:length), lhs(1:length))
   lhs(length+1:length+1) = c_null_char

end subroutine f_c_character

subroutine c_f_character(rhs, lhs)
   character(kind=c_char), intent(in) :: rhs(*)
   character(len=:), allocatable, intent(out) :: lhs

   integer :: ii

   do ii = 1, huge(ii) - 1
      if (rhs(ii) == c_null_char) then
         exit
      end if
   end do
   allocate(character(len=ii-1) :: lhs)
   lhs = transfer(rhs(1:ii-1), lhs)

end subroutine c_f_character

end module mstore_api_utils

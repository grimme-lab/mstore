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

module fortranize
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_fortran


contains


subroutine write_fortran(mol, unit, name)
   integer, intent(in) :: unit
   character(len=*), intent(in) :: name
   type(structure_type), intent(in) :: mol
   character(len=*), parameter :: ffmt = '(3x, a, 1x, "::", 1x, a:, 1x, "=", 1x, g0)'
   integer :: iat
   logical :: has_lattice

   write(unit, '(a, *(1x, a))') &
      & "subroutine", name // "(self)"
   write(unit, ffmt) "type(structure_type), intent(out)", "self"
   write(unit, ffmt) "integer, parameter", "nat", mol%nat
   write(unit, ffmt) "character(len=*), parameter", "sym(nat)", "[character(len=4)::&"
   write(unit, '(6x, "&")', advance='no')
   do iat = 1, mol%nat
      write(unit, "(1x, '""', a, '""')", advance='no') trim(mol%sym(mol%id(iat)))
      if (iat /= mol%nat) write(unit, '(",")', advance='no')
      if (modulo(iat, 14) == 0 .and. iat /= mol%nat) then
         write(unit, '(1x, "&", /, 6x, "&")', advance="no")
      end if
   end do
   write(unit, '("]")')
   write(unit, ffmt) "real(wp), parameter", "xyz(3, nat)", "reshape([&"
   do iat = 1, mol%nat - 1
      write(unit, '(6x, "&", 3(f18.14, "_wp,"), 1x, "&")') mol%xyz(:, iat)
   end do
   write(unit, '(6x, "&", 2(f18.14, "_wp,"), f18.14, "_wp],&")') mol%xyz(:, mol%nat)
   write(unit, '(6x, "&", 1x, a)') "shape(xyz))"
   if (allocated(mol%lattice)) then
      has_lattice = any(abs(mol%lattice) > epsilon(mol%lattice))
   else
      has_lattice = .false.
   end if
   if (has_lattice) then
      write(unit, ffmt) "real(wp), parameter", "lattice(3, 3)", "reshape([&"
      do iat = 1, size(mol%lattice, 2) - 1
         write(unit, '(6x, "&", 3(f18.14, "_wp,"), 1x, "&")') mol%lattice(:, iat)
      end do
      write(unit, '(6x, "&", 2(f18.14, "_wp,"), f18.14, "_wp],&")') &
         & mol%lattice(:, size(mol%lattice, 2))
      write(unit, '(6x, "&", 1x, a)') "shape(lattice))"
   end if
   if (abs(mol%charge) > epsilon(mol%charge)) then
      write(unit, ffmt) "real(wp), parameter", "charge", nint(mol%charge)
   end if
   if (mol%uhf /= 0) then
      write(unit, ffmt) "integer, parameter", "uhf", mol%uhf
   end if
   write(unit, '(3x, a, 1x, a)', advance='no') "call", "new(self, sym, xyz"
   if (has_lattice) then
      write(unit, '(",", 1x, a)', advance='no') "lattice=lattice"
   end if
   if (abs(mol%charge) > epsilon(mol%charge)) then
      write(unit, '(",", 1x, a)', advance='no') "charge=charge"
   end if
   if (mol%uhf /= 0) then
      write(unit, '(",", 1x, a)', advance='no') "uhf=uhf"
   end if
   write(unit, '(")")')
   write(unit, '(a, *(1x, a))') &
      & "end", "subroutine", name

end subroutine write_fortran


end module fortranize

program main_fortranize
   use, intrinsic :: iso_fortran_env, only : output_unit, error_unit, input_unit
   use fortranize
   use mctc_env, only : error_type, fatal_error, wp
   use mctc_io, only : read_structure, structure_type, filetype, get_filetype
   use mctc_version, only : get_mctc_version
   use mctc_io_symbols, only : to_symbol
   use mstore
   implicit none
   character(len=*), parameter :: prog_name = "mstore-fortranize"

   character(len=:), allocatable :: input, output, output_name
   integer, allocatable :: input_format
   type(structure_type) :: mol
   type(error_type), allocatable :: error
   integer :: idx, unit, stat, tmp
   logical :: exist

   call get_arguments(input, input_format, output, output_name, error)
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

   if (input == "-") then
      if (.not.allocated(input_format)) input_format = filetype%xyz
      call read_structure(mol, input_unit, input_format, error)
   else
      call read_structure(mol, input, error, input_format)
   end if
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

   idx = scan(input, '\/', back=.true.)
   if (idx > 0) then
      inquire(file=input(:idx)//".CHRG", exist=exist)
      if (exist) then
         open(file=input(:idx)//".CHRG", newunit=unit)
         read(unit, *, iostat=stat) tmp
         if (stat == 0) then
            mol%charge = real(tmp, wp)
         end if
         close(unit)
      end if
      inquire(file=input(:idx)//".UHF", exist=exist)
      if (exist) then
         open(file=input(:idx)//".UHF", newunit=unit)
         read(unit, *, iostat=stat) tmp
         if (stat == 0) then
            mol%uhf = tmp
         end if
         close(unit)
      end if
   end if

   mol%sym = to_symbol(mol%num)

   call write_fortran(mol, output_unit, output_name)


contains


subroutine help(unit)
   integer, intent(in) :: unit

   write(unit, '(a, *(1x, a))') &
      "Usage: "//prog_name//" [options] <input> [output]"

   write(unit, '(a)') &
      "", &
      "Read structure from input file and writes it to output file.", &
      "The format is determined by the file extension or the format hint", &
      ""

   write(unit, '(2x, a, t25, a)') &
      "-i, --input <format>", "Hint for the format of the input file", &
      "-n, --name <format>", "Name of the generated subroutine", &
      "--version", "Print program version and exit", &
      "--help", "Show this help message"

   write(unit, '(a)')

end subroutine help


subroutine version(unit)
   integer, intent(in) :: unit
   character(len=:), allocatable :: version_string

   call get_mctc_version(string=version_string)
   write(unit, '(a, *(1x, a))') &
      & prog_name, "version", version_string

end subroutine version


!> Obtain the command line argument at a given index
subroutine get_argument(idx, arg)

   !> Index of command line argument, range [0:command_argument_count()]
   integer, intent(in) :: idx

   !> Command line argument
   character(len=:), allocatable, intent(out) :: arg

   integer :: length, stat

   call get_command_argument(idx, length=length, status=stat)
   if (stat /= 0) then
      return
   endif

   allocate(character(len=length) :: arg, stat=stat)
   if (stat /= 0) then
      return
   endif

   if (length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= 0) then
         deallocate(arg)
         return
      end if
   end if

end subroutine get_argument


subroutine get_arguments(input, input_format, output, output_name, &
      & error)

   !> Input file name
   character(len=:), allocatable :: input

   !> Input file format
   integer, allocatable, intent(out) :: input_format

   !> Output file name
   character(len=:), allocatable :: output

   !> Name of the generated subroutine
   character(len=:), allocatable :: output_name

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: iarg, narg
   character(len=:), allocatable :: arg

   iarg = 0
   narg = command_argument_count()
   do while(iarg < narg)
      iarg = iarg + 1
      call get_argument(iarg, arg)
      select case(arg)
      case("--help")
         call help(output_unit)
         stop
      case("--version")
         call version(output_unit)
         stop
      case default
         if (.not.allocated(input)) then
            call move_alloc(arg, input)
            cycle
         end if
         if (.not.allocated(output)) then
            call move_alloc(arg, output)
            cycle
         end if
         call fatal_error(error, "Too many positional arguments present")
         exit
      case("-i", "--input")
         iarg = iarg + 1
         call get_argument(iarg, arg)
         if (.not.allocated(arg)) then
            call fatal_error(error, "Missing argument for input format")
            exit
         end if
         input_format = get_filetype("."//arg)
      case("-n", "--name")
         iarg = iarg + 1
         call get_argument(iarg, arg)
         if (.not.allocated(arg)) then
            call fatal_error(error, "Missing argument for output format")
            exit
         end if
         call move_alloc(arg, output_name)
      end select
   end do

   if (.not.allocated(output)) then
      output = "-"
   end if

   if (.not.(allocated(input).and.(allocated(output_name)))) then
      if (.not.allocated(error)) then
         call help(output_unit)
         error stop
      end if
   end if

end subroutine get_arguments


end program main_fortranize

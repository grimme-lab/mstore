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

program main_info
   use, intrinsic :: iso_fortran_env, only : output_unit, error_unit, input_unit
   use mctc_env, only : error_type, fatal_error, wp
   use mctc_io, only : write_structure, structure_type, filetype, get_filetype
   use mctc_version, only : get_mctc_version
   use mctc_io_symbols, only : to_symbol
   use mstore
   implicit none
   character(len=*), parameter :: prog_name = "mstore-info"

   character(len=:), allocatable :: output, collection, record
   integer, allocatable :: output_format
   type(structure_type) :: mol
   type(error_type), allocatable :: error
   integer :: idx, unit, stat, tmp
   logical :: exist

   call get_arguments(collection, record, output, output_format, error)
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

   if (allocated(collection)) then
      if (allocated(record)) then
         call get_structure(mol, collection, record)
         if (output == "-") then
            if (.not.allocated(output_format)) output_format = filetype%xyz
            call write_structure(mol, output_unit, output_format, error)
         else
            call write_structure(mol, output, error, output_format)
         end if
      else
         call list_records(output_unit, collection, error)
      end if
   else
      call list_collections(output_unit, error)
   end if
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

contains


subroutine help(unit)
   integer, intent(in) :: unit

   write(unit, '(a, *(1x, a))') &
      "Usage: "//prog_name//" [options] <collection> <record>"

   write(unit, '(a)') &
      "", &
      "Show information on selected collections and records.", &
      ""

   write(unit, '(2x, a, t25, a)') &
      "-o, --output <file>", "Name of the output file (default: stdout)", &
      "-f, --format <format>", "Hint for the format of the output file", &
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


subroutine get_arguments(collection, record, output, output_format, &
      & error)

   !> Input file name
   character(len=:), allocatable :: collection

   !> Input file name
   character(len=:), allocatable :: record

   !> Output file name
   character(len=:), allocatable :: output

   !> Output file format
   integer, allocatable, intent(out) :: output_format

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
         if (.not.allocated(collection)) then
            call move_alloc(arg, collection)
            cycle
         end if
         if (.not.allocated(record)) then
            call move_alloc(arg, record)
            cycle
         end if
         call fatal_error(error, "Too many positional arguments present")
         exit
      case("-o", "--output")
         iarg = iarg + 1
         call get_argument(iarg, arg)
         if (.not.allocated(arg)) then
            call fatal_error(error, "Missing argument for output file")
            exit
         end if
         call move_alloc(arg, output)
      case("-f", "--format")
         iarg = iarg + 1
         call get_argument(iarg, arg)
         if (.not.allocated(arg)) then
            call fatal_error(error, "Missing argument for output format")
            exit
         end if
         output_format = get_filetype("."//arg)
      end select
   end do

   if (.not.allocated(output)) then
      output = "-"
   end if

end subroutine get_arguments


end program main_info

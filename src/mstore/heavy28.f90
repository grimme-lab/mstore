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

module mstore_heavy28
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type, new
   use mstore_data_record, only : record_type, new_record, select_record
   implicit none
   private

   public :: get_heavy28_records

contains

subroutine get_heavy28_records(records)
   type(record_type), allocatable, intent(out) :: records(:)

   records = [ &
      new_record("bih3", bih3), &
      new_record("bih3_2", bih3_2), &
      new_record("bih3_h2o", bih3_h2o), &
      new_record("bih3_h2s", bih3_h2s), &
      new_record("bih3_hbr", bih3_hbr), &
      new_record("bih3_hcl", bih3_hcl), &
      new_record("bih3_hi", bih3_hi), &
      new_record("bih3_nh3", bih3_nh3), &
      new_record("h2o", h2o), &
      new_record("h2s", h2s), &
      new_record("hbr", hbr), &
      new_record("hcl", hcl), &
      new_record("hi", hi), &
      new_record("nh3", nh3), &
      new_record("pbh4", pbh4), &
      new_record("pbh4_2", pbh4_2), &
      new_record("pbh4_bih3", pbh4_bih3), &
      new_record("pbh4_h2o", pbh4_h2o), &
      new_record("pbh4_hbr", pbh4_hbr), &
      new_record("pbh4_hcl", pbh4_hcl), &
      new_record("pbh4_hi", pbh4_hi), &
      new_record("pbh4_teh2", pbh4_teh2), &
      new_record("sbh3", sbh3), &
      new_record("sbh3_2", sbh3_2), &
      new_record("sbh3_h2o", sbh3_h2o), &
      new_record("sbh3_h2s", sbh3_h2s), &
      new_record("sbh3_hbr", sbh3_hbr), &
      new_record("sbh3_hcl", sbh3_hcl), &
      new_record("sbh3_hi", sbh3_hi), &
      new_record("sbh3_nh3", sbh3_nh3), &
      new_record("teh2", teh2), &
      new_record("teh2_2", teh2_2), &
      new_record("teh2_h2o", teh2_h2o), &
      new_record("teh2_h2s", teh2_h2s), &
      new_record("teh2_hbr", teh2_hbr), &
      new_record("teh2_hcl", teh2_hcl), &
      new_record("teh2_hi", teh2_hi), &
      new_record("teh2_nh3", teh2_nh3) &
      ]

end subroutine get_heavy28_records

subroutine bih3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000025000_wp,  1.44604431150000_wp,  0.00000000000000_wp, &
      &  2.71450823725000_wp, -0.48201477050000_wp,  0.00000000000000_wp, &
      & -1.35725411875000_wp, -0.48201477050000_wp,  2.35083236000000_wp, &
      & -1.35725411875000_wp, -0.48201477050000_wp, -2.35083236000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3

subroutine bih3_2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "Bi", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  4.26281176400000_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp, -4.26281176400000_wp,  0.00000000000000_wp, &
      &  1.68276509000000_wp,  5.92387335100000_wp,  2.34906168700000_wp, &
      &  2.35689460100000_wp,  1.90344729100000_wp,  0.00000000000000_wp, &
      &  1.68276509000000_wp,  5.92387335100000_wp, -2.34906168700000_wp, &
      & -1.68276509000000_wp, -5.92387335100000_wp, -2.34906168700000_wp, &
      & -2.35689460100000_wp, -1.90344729100000_wp,  0.00000000000000_wp, &
      & -1.68276509000000_wp, -5.92387335100000_wp,  2.34906168700000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_2

subroutine bih3_h2o(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H", "O", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  2.70614727971429_wp, -0.73014395900000_wp, -0.69036711671429_wp, &
      &  5.67841036271429_wp,  0.60810896400000_wp,  0.09000467928571_wp, &
      &  1.43294439371429_wp,  2.33796224600000_wp, -0.96639183371429_wp, &
      &  1.91266645371429_wp, -0.76828429900000_wp,  2.54467937228571_wp, &
      & -3.40979026128571_wp, -0.73707547400000_wp, -0.80828034971429_wp, &
      & -3.60944926128571_wp,  0.96154252700000_wp, -0.20757236171429_wp, &
      & -4.71092896728571_wp, -1.67211000500000_wp,  0.03792761028571_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_h2o

subroutine bih3_h2s(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H", "H", "S", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.14127382614286_wp, -3.15875555200000_wp,  0.00000000000000_wp, &
      &  0.18884193785714_wp, -5.11489407200000_wp,  2.34879145600000_wp, &
      &  0.18884193785714_wp, -5.11489407200000_wp, -2.34879145600000_wp, &
      &  1.63080680385714_wp, -1.30582063700000_wp,  0.00000000000000_wp, &
      & -1.09715061314286_wp,  6.63076207200000_wp,  0.00000000000000_wp, &
      & -1.14127382614286_wp,  4.11170819400000_wp,  0.00000000000000_wp, &
      &  1.37120758585714_wp,  3.95189406700000_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_h2s

subroutine bih3_hbr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H", "Br", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.95020604133333_wp, &
      & -1.35818764617185_wp,  2.35245000938202_wp,  3.87450268494762_wp, &
      & -1.35818764617185_wp, -2.35245000938202_wp,  3.87450268494762_wp, &
      &  2.71637529234369_wp,  0.00000000000000_wp,  3.87450268494762_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -5.44915591466667_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -8.12455816166667_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_hbr

subroutine bih3_hcl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H", "Cl", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.88974425766667_wp, &
      & -1.35716529906067_wp,  2.35067925224248_wp,  3.81962691954393_wp, &
      & -1.35716529906067_wp, -2.35067925224248_wp,  3.81962691954393_wp, &
      &  2.71433059812133_wp,  0.00000000000000_wp,  3.81962691954393_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -5.47171735333333_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -7.87690768133333_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_hcl

subroutine bih3_hi(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H", "I", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.99666526933331_wp, &
      & -1.36004787302497_wp,  2.35567201680523_wp,  3.91235249623195_wp, &
      & -1.36004787302497_wp, -2.35567201680523_wp,  3.91235249623195_wp, &
      &  2.72009574604993_wp,  0.00000000000000_wp,  3.91235249623195_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -5.36098665366667_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -8.37273556166667_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_hi

subroutine bih3_nh3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bi", "H", "H", "H", "N", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  3.37965918862498_wp, &
      & -2.70602581502430_wp,  0.00000000000000_wp,  5.33757376878306_wp, &
      &  1.35301290751215_wp, -2.34348709910754_wp,  5.33757376878306_wp, &
      &  1.35301290751215_wp,  2.34348709910754_wp,  5.33757376878306_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -4.30897068337498_wp, &
      & -0.88409085304459_wp, -1.53129027598015_wp, -5.02780367370401_wp, &
      & -0.88409085304459_wp,  1.53129027598015_wp, -5.02780367370401_wp, &
      &  1.76818170608919_wp,  0.00000000000000_wp, -5.02780367370401_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine bih3_nh3

subroutine h2o(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 3
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.74114171466667_wp, &
      & -1.42882182100000_wp,  0.00000000000000_wp, -0.37057085733333_wp, &
      &  1.42882182100000_wp,  0.00000000000000_wp, -0.37057085733333_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine h2o

subroutine h2s(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 3
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "H", "S", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.81433348200000_wp,  0.00000000000000_wp, -0.58175214633333_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.16350429266667_wp, &
      &  1.81433348200000_wp,  0.00000000000000_wp, -0.58175214633333_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine h2s

subroutine hbr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Br", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.33574998200000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -1.33574998200000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine hbr

subroutine hcl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "H", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -1.20132148850000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.20132148850000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine hcl

subroutine hi(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "I", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.50219610250000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -1.50219610250000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine hi

subroutine nh3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "N", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.00000047225000_wp,  0.53525827275000_wp,  0.00000000000000_wp, &
      &  1.76922336075000_wp, -0.17841942425000_wp,  0.00000000000000_wp, &
      & -0.88461144425000_wp, -0.17841942425000_wp,  1.53219172300000_wp, &
      & -0.88461144425000_wp, -0.17841942425000_wp, -1.53219172300000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine nh3

subroutine pbh4(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 5
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  1.87923423200000_wp, -1.87923423200000_wp,  1.87923423200000_wp, &
      & -1.87923423200000_wp,  1.87923423200000_wp,  1.87923423200000_wp, &
      &  1.87923423200000_wp,  1.87923423200000_wp, -1.87923423200000_wp, &
      & -1.87923423200000_wp, -1.87923423200000_wp, -1.87923423200000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4

subroutine pbh4_2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 10
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "Pb", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  4.21919121900000_wp, &
      &  1.87580965603846_wp,  1.87580965603846_wp,  2.33146833800000_wp, &
      & -1.87580965603846_wp, -1.87580965603846_wp,  2.33146833800000_wp, &
      &  1.87931327185140_wp, -1.87931327185140_wp,  6.10058162800000_wp, &
      & -1.87931327185140_wp,  1.87931327185140_wp,  6.10058162800000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -4.21919121900000_wp, &
      &  1.87931327185140_wp,  1.87931327185140_wp, -6.10058162800000_wp, &
      & -1.87931327185140_wp, -1.87931327185140_wp, -6.10058162800000_wp, &
      &  1.87580965603846_wp, -1.87580965603846_wp, -2.33146833800000_wp, &
      & -1.87580965603846_wp,  1.87580965603846_wp, -2.33146833800000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_2

subroutine pbh4_bih3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 9
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "Bi", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.00000020988889_wp, -4.98043478877778_wp,  0.00000000000000_wp, &
      &  3.06964045311111_wp, -6.06324400177778_wp,  0.00000000000000_wp, &
      & -1.53482054188889_wp, -6.06324400177778_wp, -2.65838526500000_wp, &
      & -1.53482054188889_wp, -6.06324400177778_wp,  2.65838526500000_wp, &
      & -0.00000020988889_wp, -1.72196703577778_wp,  0.00000000000000_wp, &
      & -0.00000020988889_wp,  4.77334244722222_wp,  0.00000000000000_wp, &
      &  1.35700257511111_wp,  6.70626379422222_wp, -2.35039772300000_wp, &
      & -2.71400388988889_wp,  6.70626379422222_wp,  0.00000000000000_wp, &
      &  1.35700257511111_wp,  6.70626379422222_wp,  2.35039772300000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_bih3

subroutine pbh4_h2o(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "O", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.01059498500000_wp, -3.45677158287500_wp,  0.00000000000000_wp, &
      &  1.47158891900000_wp, -4.65115021887500_wp,  2.64634393100000_wp, &
      & -3.10915034600000_wp, -4.46870095387500_wp,  0.00000000000000_wp, &
      &  0.11497352800000_wp, -0.21464995987500_wp,  0.00000000000000_wp, &
      &  1.47158891900000_wp, -4.65115021887500_wp, -2.64634393100000_wp, &
      & -0.01059498500000_wp,  5.07312083612500_wp,  0.00000000000000_wp, &
      &  1.46390907200000_wp,  6.12465130412500_wp,  0.00000000000000_wp, &
      & -1.39172012200000_wp,  6.24465079412500_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_h2o

subroutine pbh4_hbr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "H", "Br"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.61626435300000_wp, &
      &  0.00000000000000_wp,  2.65037471700000_wp,  0.72467698200000_wp, &
      &  0.00000000000000_wp, -2.65037471700000_wp,  0.72467698200000_wp, &
      &  2.65821141000000_wp,  0.00000000000000_wp,  4.49379594100000_wp, &
      & -2.65821141000000_wp,  0.00000000000000_wp,  4.49379594100000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -7.86336419500000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -5.18984600400000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_hbr

subroutine pbh4_hcl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "H", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.53581007785714_wp, &
      &  0.00000000000000_wp,  2.65440928200000_wp,  0.65256773785714_wp, &
      &  0.00000000000000_wp, -2.65440928200000_wp,  0.65256773785714_wp, &
      &  2.65563193400000_wp,  0.00000000000000_wp,  4.41918658985714_wp, &
      & -2.65563193400000_wp,  0.00000000000000_wp,  4.41918658985714_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -7.54169989614286_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -5.13761883714286_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_hcl

subroutine pbh4_hi(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "H", "I"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.67020793057143_wp, &
      &  0.00000000000000_wp,  2.64413862100000_wp,  0.76559467957143_wp, &
      &  0.00000000000000_wp, -2.64413862100000_wp,  0.76559467957143_wp, &
      &  2.66197007500000_wp,  0.00000000000000_wp,  4.53945496057143_wp, &
      & -2.66197007500000_wp,  0.00000000000000_wp,  4.53945496057143_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -8.14419880442857_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -5.13610840642857_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_hi

subroutine pbh4_teh2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pb", "H", "H", "H", "H", "Te", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00607452450000_wp, -4.06569893412500_wp,  0.00000000000000_wp, &
      & -0.10304014450000_wp, -0.81095583012500_wp,  0.00000000000000_wp, &
      & -1.48984957750000_wp, -5.20124851012500_wp,  2.65857423800000_wp, &
      & -1.48984957750000_wp, -5.20124851012500_wp, -2.65857423800000_wp, &
      &  3.11171824650000_wp, -5.04151186112500_wp,  0.00000000000000_wp, &
      &  0.00607452450000_wp,  5.31621802187500_wp,  0.00000000000000_wp, &
      &  2.16097631450000_wp,  7.52891897087500_wp,  0.00000000000000_wp, &
      & -2.20210431050000_wp,  7.47552665287500_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine pbh4_teh2

subroutine sbh3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.34100578249999_wp, &
      & -1.31377720592552_wp,  2.27552887048887_wp, -0.44700182284130_wp, &
      & -1.31377720592552_wp, -2.27552887048887_wp, -0.44700182284130_wp, &
      &  2.62755441185103_wp,  0.00000000000000_wp, -0.44700182284130_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3

subroutine sbh3_2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "H", "H", "Sb", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00454880637500_wp, -3.81421727025000_wp,  0.00000000000000_wp, &
      & -1.59734545962500_wp, -5.35433450325000_wp,  2.27494852300000_wp, &
      & -1.59734545962500_wp, -5.35433450325000_wp, -2.27494852300000_wp, &
      & -2.13288057662500_wp, -1.45353754725000_wp,  0.00000000000000_wp, &
      &  0.00454880637500_wp,  4.19773526975000_wp,  0.00000000000000_wp, &
      &  1.57640587837500_wp,  2.62264109775000_wp,  2.27169063600000_wp, &
      &  2.16566212637500_wp,  6.53340635875000_wp,  0.00000000000000_wp, &
      &  1.57640587837500_wp,  2.62264109775000_wp, -2.27169063600000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_2

subroutine sbh3_h2o(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "H", "H", "O", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.15005963128571_wp, -2.20606747071429_wp,  0.00000000000000_wp, &
      &  1.74880372428571_wp, -3.70808215971429_wp,  2.28916493200000_wp, &
      &  1.74880372428571_wp, -3.70808215971429_wp, -2.28916493200000_wp, &
      & -2.15290596971429_wp, -4.38802635771429_wp,  0.00000000000000_wp, &
      &  0.15005963128571_wp,  5.03041900128571_wp,  0.00000000000000_wp, &
      & -0.13071018671429_wp,  3.23614118028571_wp,  0.00000000000000_wp, &
      & -1.51411055471429_wp,  5.74369796628571_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_h2o

subroutine sbh3_h2s(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "H", "H", "H", "S", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.33839323285714_wp, -2.52035427985714_wp,  0.00000000000000_wp, &
      &  0.87126950614286_wp, -4.36564525285714_wp,  2.28275498100000_wp, &
      &  0.87126950614286_wp, -4.36564525285714_wp, -2.28275498100000_wp, &
      & -3.07540243785714_wp, -4.12867550385714_wp,  0.00000000000000_wp, &
      &  2.16321847414286_wp,  6.15526246914286_wp,  0.00000000000000_wp, &
      & -0.33839323285714_wp,  5.87053934414286_wp,  0.00000000000000_wp, &
      & -0.15356858285714_wp,  3.35451847614286_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_h2s

subroutine sbh3_hbr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "Br", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.35066684916667_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -3.97970781883333_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -6.66851389283333_wp, &
      &  1.32248125976823_wp,  2.29060473397628_wp,  3.09918492559686_wp, &
      &  1.32248125976823_wp, -2.29060473397628_wp,  3.09918492559686_wp, &
      & -2.64496251953647_wp,  0.00000000000000_wp,  3.09918492559686_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_hbr

subroutine sbh3_hcl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "Cl", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000031483333_wp,  1.28744197166667_wp,  0.00000000000000_wp, &
      &  0.00000031483333_wp, -3.98081740333333_wp,  0.00000000000000_wp, &
      &  0.00000031483333_wp, -6.40021847133333_wp,  0.00000000000000_wp, &
      &  2.64666171983333_wp,  3.03119796766667_wp,  0.00000000000000_wp, &
      & -1.32333133216667_wp,  3.03119796766667_wp,  2.29207700000000_wp, &
      & -1.32333133216667_wp,  3.03119796766667_wp, -2.29207700000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_hcl

subroutine sbh3_hi(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "I", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000016667_wp, -1.42567794700000_wp,  0.00000000000000_wp, &
      &  0.00000000016667_wp,  3.97476587000000_wp,  0.00000000000000_wp, &
      &  0.00000000016667_wp,  6.99621096200000_wp,  0.00000000000000_wp, &
      &  2.64219220316667_wp, -3.18176629500000_wp,  0.00000000000000_wp, &
      & -1.32109610183333_wp, -3.18176629500000_wp, -2.28820495100000_wp, &
      & -1.32109610183333_wp, -3.18176629500000_wp,  2.28820495100000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_hi

subroutine sbh3_nh3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sb", "H", "H", "H", "N", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.68230919025000_wp, -3.19147153425000_wp,  0.00000000000000_wp, &
      & -1.29333697175000_wp, -2.22973706525000_wp,  2.29162913500000_wp, &
      & -0.64852466875000_wp, -6.10530239925000_wp,  0.00000000000000_wp, &
      & -1.29333697175000_wp, -2.22973706525000_wp, -2.29162913500000_wp, &
      &  0.68230919025000_wp,  2.90523615575000_wp,  0.00000000000000_wp, &
      & -0.25923733575000_wp,  3.54500666875000_wp,  1.53299485600000_wp, &
      &  2.38905490325000_wp,  3.76099857075000_wp,  0.00000000000000_wp, &
      & -0.25923733575000_wp,  3.54500666875000_wp, -1.53299485600000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine sbh3_nh3

subroutine teh2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 3
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.45423737600000_wp, &
      & -2.18332515900000_wp,  0.00000000000000_wp, -0.72711868800000_wp, &
      &  2.18332515900000_wp,  0.00000000000000_wp, -0.72711868800000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2

subroutine teh2_2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "Te", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.41120815450000_wp, -5.75194009533333_wp,  0.00000000000000_wp, &
      &  0.50339843750000_wp, -2.66515016933333_wp,  0.00000000000000_wp, &
      & -2.67393204050000_wp, -5.65490266633333_wp,  0.00000000000000_wp, &
      &  0.41120815450000_wp,  3.24942383966667_wp,  0.00000000000000_wp, &
      &  2.84530184250000_wp,  5.14731155666667_wp,  0.00000000000000_wp, &
      & -1.49718454850000_wp,  5.67525753466667_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_2

subroutine teh2_h2o(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "O", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  3.06177877366667_wp, &
      & -2.16806751100000_wp,  0.00000000000000_wp,  5.26622161966667_wp, &
      &  2.16806751100000_wp,  0.00000000000000_wp,  5.26622161966667_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -3.78942658433333_wp, &
      & -1.42882560100000_wp,  0.00000000000000_wp, -4.90239771433333_wp, &
      &  1.42882560100000_wp,  0.00000000000000_wp, -4.90239771433333_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_h2o

subroutine teh2_h2s(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "H", "S", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  3.73252756466667_wp, &
      & -2.17927169600000_wp,  0.00000000000000_wp,  5.92094931466667_wp, &
      &  2.17927169600000_wp,  0.00000000000000_wp,  5.92094931466667_wp, &
      &  1.81461883100000_wp,  0.00000000000000_wp, -5.77343289433333_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -4.02756040533333_wp, &
      & -1.81461883100000_wp,  0.00000000000000_wp, -5.77343289433333_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_h2s

subroutine teh2_hbr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 5
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "H", "Br"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  1.29827085740000_wp, -2.62307762440000_wp,  0.00000000000000_wp, &
      & -1.64102217660000_wp, -1.68628753940000_wp,  0.00000000000000_wp, &
      &  0.36525455440000_wp, -5.57002593840000_wp,  0.00000000000000_wp, &
      & -1.32077409260000_wp,  5.20634628160000_wp,  0.00000000000000_wp, &
      &  1.29827085740000_wp,  4.67304482060000_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_hbr

subroutine teh2_hcl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 5
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "H", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.29261084180000_wp, -2.35304787460000_wp,  0.00000000000000_wp, &
      & -2.64645542520000_wp, -1.42105580360000_wp,  0.00000000000000_wp, &
      & -0.61927076620000_wp, -5.30503230860000_wp,  0.00000000000000_wp, &
      &  2.68050450780000_wp,  4.39973110440000_wp,  0.00000000000000_wp, &
      &  0.29261084180000_wp,  4.67940488240000_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_hcl

subroutine teh2_hi(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 5
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "H", "I"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.07306285420000_wp,  4.08587331840000_wp,  0.00000000000000_wp, &
      & -0.19753834880000_wp,  1.00796888240000_wp,  0.00000000000000_wp, &
      & -3.00095063580000_wp,  4.36140292640000_wp,  0.00000000000000_wp, &
      &  3.05236327620000_wp, -4.53081232560000_wp,  0.00000000000000_wp, &
      &  0.07306285420000_wp, -4.92443280160000_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_hi

subroutine teh2_nh3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Te", "H", "H", "N", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.52537001157143_wp, -3.44216936657143_wp,  0.00000000000000_wp, &
      &  0.41227423042857_wp, -6.40364967357143_wp,  0.00000000000000_wp, &
      &  2.41231864442857_wp, -2.51988859757143_wp,  0.00000000000000_wp, &
      & -0.52537001157143_wp,  2.55819631242857_wp,  0.00000000000000_wp, &
      &  0.28980675842857_wp,  3.34893136642857_wp,  1.53494883300000_wp, &
      &  0.28980675842857_wp,  3.34893136642857_wp, -1.53494883300000_wp, &
      & -2.35346636857143_wp,  3.10964859242857_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine teh2_nh3

end module mstore_heavy28

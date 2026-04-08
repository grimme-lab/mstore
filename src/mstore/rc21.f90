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

module mstore_rc21
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type, new
   use mstore_data_record, only : record_type, new_record
   implicit none
   private

   public :: get_rc21_records

contains

subroutine get_rc21_records(records)
   type(record_type), allocatable, intent(out) :: records(:)

   records = [ &
      new_record('1e', case_1e), &
      new_record('1p1', case_1p1), &
      new_record('1p2', case_1p2), &
      new_record('1p3', case_1p3), &
      new_record('2e', case_2e), &
      new_record('2p1', case_2p1), &
      new_record('2p2', case_2p2), &
      new_record('2p3', case_2p3), &
      new_record('3e', case_3e), &
      new_record('3int1', case_3int1), &
      new_record('3int2', case_3int2), &
      new_record('3int3', case_3int3), &
      new_record('3p1', case_3p1), &
      new_record('3p2', case_3p2), &
      new_record('3p3', case_3p3), &
      new_record('4e', case_4e), &
      new_record('4p', case_4p), &
      new_record('5e', case_5e), &
      new_record('5p', case_5p), &
      new_record('6e', case_6e), &
      new_record('6int', case_6int), &
      new_record('6p1', case_6p1), &
      new_record('6p3', case_6p3), &
      new_record('7e', case_7e), &
      new_record('7p1', case_7p1), &
      new_record('7p2', case_7p2), &
      new_record('7p3', case_7p3), &
      new_record('7p4', case_7p4), &
      new_record('7p5', case_7p5), &
      new_record('7p6', case_7p6), &
      new_record('8e', case_8e), &
      new_record('8p1', case_8p1), &
      new_record('9e', case_9e), &
      new_record('9p1', case_9p1), &
      new_record('9p2', case_9p2), &
      new_record('10e', case_10e), &
      new_record('10p1', case_10p1), &
      new_record('10p2', case_10p2), &
      new_record('ethylene', ethylene), &
      new_record('me', me), &
      new_record('pr', pr) &
      ]

end subroutine get_rc21_records

subroutine case_1e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 13
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "O", "C", "C", "H", "H", "H", &
      & "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.58295498445968_wp,  2.30269536917342_wp, -1.59715741018400_wp, &
      & -1.71698399940721_wp,  0.71684626914122_wp,  0.74122258788340_wp, &
      & -2.97603179886793_wp,  1.26510174063636_wp,  2.53106478937141_wp, &
      & -0.17119293599471_wp, -1.76510434906387_wp,  0.97413945958394_wp, &
      &  2.59393956829216_wp, -1.29054196065502_wp,  0.41984763590252_wp, &
      & -1.49493248605293_wp,  1.05934061141064_wp, -3.23855671064587_wp, &
      &  0.19287811028741_wp,  3.36161644684620_wp, -1.46072938791077_wp, &
      & -3.17219293036598_wp,  3.60356526459885_wp, -1.68269813126042_wp, &
      & -1.09560103985732_wp, -2.96756259872180_wp, -0.44396564361870_wp, &
      & -0.50258041291808_wp, -2.55436245243696_wp,  2.84544964438677_wp, &
      &  2.92453902941813_wp, -0.59850384175008_wp, -1.49007305530221_wp, &
      &  3.54474261088849_wp, -3.11954779138403_wp,  0.61175990716338_wp, &
      &  3.45637126903760_wp, -0.01354270779498_wp,  1.78969631463054_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_1e

subroutine case_1p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 9
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "O", "H", "H", "H", "H", &
      & "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.86956574920373_wp, -0.54898631365118_wp,  0.00000000000000_wp, &
      &  1.04962214274265_wp, -0.55941668259431_wp,  0.00000000000000_wp, &
      &  2.03925943480543_wp,  1.94016875456910_wp,  0.00000000000000_wp, &
      &  2.74955211850356_wp,  3.91940623258556_wp,  0.00000000000000_wp, &
      & -2.45917069625086_wp, -2.52135609913528_wp,  0.00000000000000_wp, &
      & -2.60788656818841_wp,  0.36955994136415_wp, -1.68626942454858_wp, &
      & -2.60788656818841_wp,  0.36955994136415_wp,  1.68626942454858_wp, &
      &  1.85303794288987_wp, -1.48446788725110_wp, -1.67933620217379_wp, &
      &  1.85303794288987_wp, -1.48446788725110_wp,  1.67933620217379_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_1p1

subroutine case_1p2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "O", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.00000000000000_wp,  0.00000000000000_wp, -0.91741967769512_wp, &
      & -0.00000000000000_wp,  0.00000000000000_wp,  1.76206042266875_wp, &
      & -0.00000000000000_wp,  0.00000000000000_wp,  3.86117784866960_wp, &
      &  0.98495097216293_wp, -1.70598512675056_wp, -1.56860619788108_wp, &
      &  0.98495097216293_wp,  1.70598512675056_wp, -1.56860619788108_wp, &
      & -1.96990194432587_wp,  0.00000000000000_wp, -1.56860619788108_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_1p2

subroutine case_1p3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.14960709138213_wp, -0.20997094245747_wp,  0.00000000000000_wp, &
      &  1.61665492374662_wp,  0.18464577834559_wp,  0.00000000000000_wp, &
      &  2.64004481582223_wp,  0.45908383478268_wp, -1.74946353563795_wp, &
      &  2.64004481582223_wp,  0.45908383478268_wp,  1.74946353563795_wp, &
      & -1.77969524374972_wp, -1.24307948866890_wp,  1.67398185609075_wp, &
      & -1.77969524374972_wp, -1.24307948866890_wp, -1.67398185609075_wp, &
      & -2.18774697650946_wp,  1.59331647188426_wp,  0.00000000000000_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine case_1p3

subroutine case_2e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 15
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "O", "C", "C", "H", "H", "H", &
      & "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -4.53230351574667_wp,  0.14226266640036_wp, -0.70037740808230_wp, &
      & -2.19505723520187_wp, -0.68655939272625_wp,  0.70165380628451_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -0.62343944686179_wp, &
      &  2.19505723520187_wp,  0.68655939272625_wp,  0.70165380628451_wp, &
      &  4.53230351574667_wp, -0.14226266640036_wp, -0.70037740808230_wp, &
      & -6.17135882786360_wp, -0.63231862399523_wp,  0.28076075411191_wp, &
      & -4.53854058800169_wp, -0.54839539609276_wp, -2.64088743337947_wp, &
      & -4.66580068151468_wp,  2.19999844449340_wp, -0.70278371877702_wp, &
      & -2.09106070517088_wp, -2.78945387790466_wp,  0.70008386146542_wp, &
      & -2.05327770338964_wp, -0.07433823351140_wp,  2.67326986180784_wp, &
      &  2.05327770338964_wp,  0.07433823351140_wp,  2.67326986180784_wp, &
      &  2.09106070517088_wp,  2.78945387790466_wp,  0.70008386146542_wp, &
      &  4.66580068151468_wp, -2.19999844449340_wp, -0.70278371877702_wp, &
      &  6.17135882786360_wp,  0.63231862399523_wp,  0.28076075411191_wp, &
      &  4.53854058800169_wp,  0.54839539609276_wp, -2.64088743337947_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_2e

subroutine case_2p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 11
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "O", "C", "H", "H", "H", "H", &
      & "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.21072825907985_wp, -0.32175325797339_wp,  1.73609351133946_wp, &
      &  0.55630493175146_wp, -0.12701622408906_wp,  1.18845975974201_wp, &
      &  0.99603796677771_wp, -1.02756237812650_wp, -1.46434307683602_wp, &
      &  1.86339901814078_wp,  0.35404720016215_wp, -3.11594784327857_wp, &
      & -2.48417049667272_wp,  0.21287124098005_wp,  3.71187663848102_wp, &
      & -2.89312801881664_wp, -2.25082613456229_wp,  1.49943554881364_wp, &
      & -3.31958270419756_wp,  0.96349427151150_wp,  0.56746967425517_wp, &
      &  1.70139279676850_wp, -1.43580161536731_wp,  2.29114897335118_wp, &
      &  1.31834115223509_wp,  1.78689254514651_wp,  1.29308010071911_wp, &
      &  2.09419920759988_wp, -0.47063004011960_wp, -4.99094790732847_wp, &
      &  2.37793440549334_wp,  2.31628439243797_wp, -2.71632537925850_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_2p1

subroutine case_2p2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "O", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.44742700071064_wp,  0.04411518897775_wp,  0.00000000000000_wp, &
      &  1.46031822113623_wp,  0.06074415029915_wp,  0.00000000000000_wp, &
      &  2.19739361741726_wp, -2.40180089441015_wp,  0.00000000000000_wp, &
      & -2.10862610372348_wp,  1.99735353466492_wp,  0.00000000000000_wp, &
      & -2.16017185518976_wp, -0.91256439975375_wp,  1.67878524567186_wp, &
      & -2.16017185518976_wp, -0.91256439975375_wp, -1.67878524567186_wp, &
      &  2.10934248813008_wp,  1.06235840998790_wp,  1.70372776015415_wp, &
      &  2.10934248813008_wp,  1.06235840998790_wp, -1.70372776015415_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine case_2p2

subroutine case_2p3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp, -1.29836582166773_wp, -0.29407256202659_wp, &
      &  0.00000000000000_wp,  1.29836582166773_wp, -0.29407256202659_wp, &
      & -1.76772909778260_wp,  2.34815785851919_wp, -0.30797570916996_wp, &
      &  1.76772909778260_wp,  2.34815785851919_wp, -0.30797570916996_wp, &
      &  1.76772909778260_wp, -2.34815785851919_wp, -0.30797570916996_wp, &
      & -1.76772909778260_wp, -2.34815785851919_wp, -0.30797570916996_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.82004796073295_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_2p3

subroutine case_3e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 15
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -3.87611762821757_wp, -0.21245926350376_wp,  0.17092798379268_wp, &
      & -2.68572093354802_wp,  2.17607255434953_wp,  0.11793685049216_wp, &
      & -0.11949344310415_wp,  2.29949812699283_wp,  0.00724992171064_wp, &
      &  1.36566953678699_wp,  0.03042761430126_wp, -0.04968072365914_wp, &
      &  4.12471334347371_wp,  0.21020387536416_wp, -0.17202272733784_wp, &
      &  0.12239981670019_wp, -2.35921441765508_wp,  0.00125665528019_wp, &
      & -2.44544263369682_wp, -2.47851547404073_wp,  0.11178333528563_wp, &
      & -5.92100104805209_wp, -0.31975470079457_wp,  0.25670265296437_wp, &
      & -3.82227479534894_wp,  3.87685668750998_wp,  0.16535121300194_wp, &
      &  0.84745126217406_wp,  4.10507853463334_wp, -0.03702957411729_wp, &
      &  5.06633477370817_wp, -1.61346612465601_wp, -0.07870559425195_wp, &
      &  4.83045386656822_wp,  1.45638056116116_wp,  1.32981821951170_wp, &
      &  4.67055771339591_wp,  1.17302500924779_wp, -1.93417647255418_wp, &
      &  1.25646226356753_wp, -4.06318198345897_wp, -0.04532244827886_wp, &
      & -3.41399209440709_wp, -4.28095099945091_wp,  0.15591070815986_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_3e

subroutine case_3int1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 15
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  1.59965856292753_wp,  0.35451483661914_wp,  0.00000000000000_wp, &
      &  0.17701094082188_wp,  0.04604641779812_wp, -2.34524875490694_wp, &
      & -2.30428020483591_wp, -0.66676746171116_wp, -2.31177105449847_wp, &
      & -3.54181422152760_wp, -0.99760550742384_wp,  0.00000000000000_wp, &
      & -2.30428020483591_wp, -0.66676746171116_wp,  2.31177105449847_wp, &
      &  0.17701094082188_wp,  0.04604641779812_wp,  2.34524875490694_wp, &
      &  3.93861557878944_wp,  1.84967167742612_wp,  0.00000000000000_wp, &
      &  4.86870369416205_wp,  2.28863982331388_wp,  1.76481341815763_wp, &
      &  4.86870369416205_wp,  2.28863982331388_wp, -1.76481341815763_wp, &
      &  1.18034196426420_wp,  0.30274299797452_wp, -4.11448628341555_wp, &
      & -3.31999041692834_wp, -0.95003212117605_wp, -4.06542320968971_wp, &
      & -5.51604432713814_wp, -1.55006222449344_wp,  0.00000000000000_wp, &
      &  1.18034196426420_wp,  0.30274299797452_wp,  4.11448628341555_wp, &
      &  2.31601245198107_wp, -1.69777809452658_wp,  0.00000000000000_wp, &
      & -3.31999041692834_wp, -0.95003212117605_wp,  4.06542320968971_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_3int1

subroutine case_3int2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 15
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.59961991969784_wp,  0.89222864044116_wp, -1.43021136893067_wp, &
      &  0.77413575010823_wp,  0.42893797647107_wp, -2.65830432968936_wp, &
      &  2.90164889973484_wp, -0.30220011809591_wp, -1.32184742812694_wp, &
      &  2.90164889973484_wp, -0.30220011809591_wp,  1.32184742812694_wp, &
      &  0.77413575010823_wp,  0.42893797647107_wp,  2.65830432968936_wp, &
      & -1.59961991969784_wp,  0.89222864044116_wp,  1.43021136893067_wp, &
      & -2.73322382015202_wp, -1.34363220000826_wp,  0.00000000000000_wp, &
      & -2.97903688435257_wp,  2.05864071642442_wp, -2.39390828001313_wp, &
      &  0.88061387167983_wp,  0.64505716174597_wp, -4.69489224360744_wp, &
      &  4.62030778080954_wp, -0.76229807342963_wp, -2.33480921882698_wp, &
      &  4.62030778080954_wp, -0.76229807342963_wp,  2.33480921882698_wp, &
      &  0.88061387167983_wp,  0.64505716174597_wp,  4.69489224360744_wp, &
      & -2.97903688435257_wp,  2.05864071642442_wp,  2.39390828001313_wp, &
      & -4.77247269406420_wp, -1.47573403342221_wp,  0.00000000000000_wp, &
      & -1.69040248234791_wp, -3.10136637368376_wp,  0.00000000000000_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_3int2

subroutine case_3int3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 15
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.92511875691107_wp,  0.01665250221078_wp, -2.99699006386387_wp, &
      &  1.58932018203912_wp, -0.01609186359204_wp, -2.44769926664911_wp, &
      &  2.91352989391825_wp, -0.04326539457430_wp,  0.00000000000000_wp, &
      &  1.58932018203912_wp, -0.01609186359204_wp,  2.44769926664911_wp, &
      & -0.92511875691107_wp,  0.01665250221078_wp,  2.99699006386387_wp, &
      & -2.98723620347813_wp,  0.03581080498376_wp,  1.30755159153473_wp, &
      & -2.98723620347813_wp,  0.03581080498376_wp, -1.30755159153473_wp, &
      & -1.41541770461346_wp,  0.02866210147571_wp, -4.98754551198723_wp, &
      &  2.86167722823654_wp, -0.02718075775269_wp, -4.06039253311008_wp, &
      &  4.29416238362000_wp,  1.52730509100755_wp,  0.00000000000000_wp, &
      &  4.22642941544277_wp, -1.67192470024990_wp,  0.00000000000000_wp, &
      &  2.86167722823654_wp, -0.02718075775269_wp,  4.06039253311008_wp, &
      & -1.41541770461346_wp,  0.02866210147571_wp,  4.98754551198723_wp, &
      & -4.84028559176354_wp,  0.05608971458281_wp,  2.18753370513228_wp, &
      & -4.84028559176354_wp,  0.05608971458281_wp, -2.18753370513228_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_3int3

subroutine case_3p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 14
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  2.34009143495546_wp,  0.00000000000000_wp, -0.34444863350013_wp, &
      &  2.32158080768342_wp,  0.00000000000000_wp,  2.23740802585435_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  3.51195643282548_wp, &
      & -2.32158080768342_wp,  0.00000000000000_wp,  2.23740802585435_wp, &
      & -2.34009143495546_wp,  0.00000000000000_wp, -0.34444863350013_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -1.70519925812894_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -4.27848508331225_wp, &
      & -1.75233837599908_wp,  0.00000000000000_wp, -5.34541430549799_wp, &
      &  1.75233837599908_wp,  0.00000000000000_wp, -5.34541430549799_wp, &
      &  4.09993314572395_wp,  0.00000000000000_wp, -1.39352010415267_wp, &
      &  4.06903794907112_wp,  0.00000000000000_wp,  3.30052974334034_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  5.56261845652796_wp, &
      & -4.06903794907112_wp,  0.00000000000000_wp,  3.30052974334034_wp, &
      & -4.09993314572395_wp,  0.00000000000000_wp, -1.39352010415267_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_3p1

subroutine case_3p2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 1
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.00000000000000_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine case_3p2

subroutine case_3p3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 14
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.72517343614488_wp, -1.31237435869588_wp,  0.00000000000000_wp, &
      & -2.72517343614488_wp,  1.31237435869588_wp,  0.00000000000000_wp, &
      & -0.67306225532043_wp,  2.94887841643143_wp,  0.00000000000000_wp, &
      &  1.88587853172822_wp,  2.36481688043678_wp,  0.00000000000000_wp, &
      &  3.02471431947419_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  1.88587853172822_wp, -2.36481688043678_wp,  0.00000000000000_wp, &
      & -0.67306225532043_wp, -2.94887841643143_wp,  0.00000000000000_wp, &
      & -4.57338280688276_wp,  2.20242508188547_wp,  0.00000000000000_wp, &
      & -4.57338280688276_wp, -2.20242508188547_wp,  0.00000000000000_wp, &
      & -1.12953227329219_wp, -4.94880423771244_wp,  0.00000000000000_wp, &
      &  3.16487910034678_wp, -3.96863286533251_wp,  0.00000000000000_wp, &
      &  5.07607195965632_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  3.16487910034678_wp,  3.96863286533251_wp,  0.00000000000000_wp, &
      & -1.12953227329219_wp,  4.94880423771244_wp,  0.00000000000000_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_3p3

subroutine case_4e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 12
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "H", "H", "H", "H", &
      & "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.22032918449128_wp, -2.65861259485325_wp,  0.60011240691846_wp, &
      & -1.44734785921859_wp, -0.60991627460042_wp, -0.87432543230548_wp, &
      & -0.37187769390966_wp,  1.66470511963387_wp,  0.15813267649281_wp, &
      &  2.56323416424935_wp,  1.05338797530341_wp, -0.10386046635136_wp, &
      & -3.31135047306553_wp, -4.18999808698216_wp, -0.22003931722027_wp, &
      & -1.78903472151142_wp, -2.73846864177674_wp,  2.60585638600648_wp, &
      & -1.55437060902843_wp, -0.79197419099614_wp, -2.92202851250289_wp, &
      & -0.81405149920915_wp,  1.98282804191268_wp,  2.14289767194401_wp, &
      & -0.75699140759927_wp,  3.32985827758614_wp, -0.99048236235125_wp, &
      &  3.11719452996708_wp, -0.57486090997234_wp,  1.02401010386897_wp, &
      &  3.47603538123120_wp,  2.74833909080946_wp,  0.64840454721696_wp, &
      &  3.10888937258567_wp,  0.78471219393551_wp, -2.06867770171642_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_4e

subroutine case_4p(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "H", "C", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.22647143073577_wp,  0.00000000000000_wp,  0.31290886057768_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -1.02615842506212_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -3.07527500585612_wp, &
      &  2.22647143073577_wp,  0.00000000000000_wp,  0.31290886057768_wp, &
      &  2.22447340820003_wp,  0.00000000000000_wp,  2.36702693547274_wp, &
      &  4.05221435815292_wp,  0.00000000000000_wp, -0.62921908059131_wp, &
      & -2.22447340820003_wp,  0.00000000000000_wp,  2.36702693547274_wp, &
      & -4.05221435815292_wp,  0.00000000000000_wp, -0.62921908059131_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_4p

subroutine case_5e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 14
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "H", "H", "H", "H", &
      & "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -3.38918206947903_wp, -1.44724857510348_wp,  0.00000000000000_wp, &
      & -0.63178169143009_wp, -1.64687612646749_wp,  0.00000000000000_wp, &
      &  0.63178169143009_wp,  1.64687612646749_wp,  0.00000000000000_wp, &
      &  3.38918206947903_wp,  1.44724857510348_wp,  0.00000000000000_wp, &
      & -4.01652434699617_wp, -3.45543278158571_wp,  0.00000000000000_wp, &
      & -4.18250654119081_wp, -0.61121770835048_wp, -1.70802999617077_wp, &
      & -4.18250654119081_wp, -0.61121770835048_wp,  1.70802999617077_wp, &
      &  0.27660198782180_wp, -2.29656950297935_wp, -1.72667500025546_wp, &
      &  0.27660198782180_wp, -2.29656950297935_wp,  1.72667500025546_wp, &
      & -0.27660198782180_wp,  2.29656950297935_wp,  1.72667500025546_wp, &
      & -0.27660198782180_wp,  2.29656950297935_wp, -1.72667500025546_wp, &
      &  4.18250654119081_wp,  0.61121770835048_wp,  1.70802999617077_wp, &
      &  4.01652434699617_wp,  3.45543278158571_wp,  0.00000000000000_wp, &
      &  4.18250654119081_wp,  0.61121770835048_wp, -1.70802999617077_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_5e

subroutine case_5p(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 10
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "H", "H", "H", "H", "H", &
      & "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.27970175286720_wp,  0.47069485600803_wp, -0.66299697777012_wp, &
      &  0.38443018085009_wp,  0.75538039717373_wp, -0.34722392993755_wp, &
      &  2.01890760345984_wp, -0.98633029073705_wp,  0.90688525924748_wp, &
      &  1.08232248234212_wp, -2.47162026022434_wp,  1.97734859516732_wp, &
      &  3.59360448471880_wp, -0.07718418204145_wp,  1.89083901150256_wp, &
      & -2.97935301144372_wp,  1.23793443739856_wp, -2.45175315364955_wp, &
      & -2.99370869395740_wp,  1.87466783129847_wp,  0.74737631095981_wp, &
      & -3.04349711940794_wp, -1.37290910909751_wp, -0.16715306057054_wp, &
      &  1.24891960438164_wp,  2.45554602297086_wp, -1.13825686409463_wp, &
      &  2.96807622192382_wp, -1.88617970274932_wp, -0.75506519085478_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_5p

subroutine case_6e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 16
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "O", "C", "C", "C", "H", "H", &
      & "H", "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -4.80607376643556_wp, -0.99511127407227_wp,  0.93268492716284_wp, &
      & -2.05191414591191_wp, -1.74161999286721_wp,  0.71657546866082_wp, &
      & -1.33297955495759_wp, -3.75186794522034_wp,  1.42486825587041_wp, &
      & -0.20617497924883_wp,  0.17684074867725_wp, -0.47353096407473_wp, &
      &  2.53907420461304_wp, -0.16740329101861_wp,  0.23078697318693_wp, &
      &  4.09815268835631_wp,  1.86055139519968_wp, -1.08624839546965_wp, &
      & -5.33282056405882_wp, -0.01846509277306_wp, -0.80518678210899_wp, &
      & -4.89429903245637_wp,  0.33612878798001_wp,  2.51588104067708_wp, &
      & -5.97704199776032_wp, -2.64249647103268_wp,  1.29877061047522_wp, &
      & -0.97438153350156_wp,  2.03518174303010_wp,  0.00975591194442_wp, &
      & -0.56510672633994_wp, -0.13804204917412_wp, -2.50041304174000_wp, &
      &  3.17388877059763_wp, -2.05488178364179_wp, -0.31620956275848_wp, &
      &  2.76445673822176_wp, -0.01220783160183_wp,  2.27791387588642_wp, &
      &  3.95860529515553_wp,  1.71665252963271_wp, -3.13912403403799_wp, &
      &  6.08244374049337_wp,  1.63540280968621_wp, -0.56510236815903_wp, &
      &  3.52417086323327_wp,  3.76133771719589_wp, -0.52142191551524_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_6e

subroutine case_6int(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 16
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "O", "H", "H", &
      & "H", "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -4.37404326693774_wp,  0.20034742998945_wp,  0.99995709879484_wp, &
      & -1.75175931843321_wp, -0.42008102889072_wp,  0.38889506749230_wp, &
      &  0.05396962168135_wp,  1.49595670580617_wp, -0.54696553262606_wp, &
      &  2.82822137736770_wp,  0.89629973863346_wp, -0.00265429751546_wp, &
      &  3.68452450729157_wp, -1.57925718017364_wp, -1.01495639840780_wp, &
      & -1.06508499818382_wp, -2.68494784579966_wp,  0.68393385122225_wp, &
      & -5.13899571278965_wp,  1.52731954557061_wp, -0.38503017598376_wp, &
      & -4.35183237083489_wp,  1.21675344912459_wp,  2.81190210405090_wp, &
      & -5.54391129970793_wp, -1.48012006854372_wp,  1.18107922280954_wp, &
      & -0.50803039489611_wp,  3.34828610338507_wp,  0.17056196824972_wp, &
      & -0.29649218390954_wp,  1.58307213566913_wp, -2.60024028135366_wp, &
      &  3.96433927934976_wp,  2.41045905425282_wp, -0.86349333707611_wp, &
      &  3.18457364828740_wp,  1.02232708621651_wp,  2.02874932786796_wp, &
      &  0.78915755813202_wp, -2.89738292080891_wp,  0.22444278461361_wp, &
      &  5.22801728578128_wp, -2.56975203046508_wp, -0.10379584165453_wp, &
      &  3.29734626780177_wp, -2.06928017396607_wp, -2.97238556048372_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_6int

subroutine case_6p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 10
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "O", "H", "H", "H", "H", &
      & "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.39197283906591_wp,  0.33198973264194_wp,  0.28915774813452_wp, &
      &  0.37475364921405_wp,  0.29706135760998_wp,  0.32947940154975_wp, &
      &  1.82462845021751_wp, -1.27689342819661_wp, -1.28430344120615_wp, &
      &  1.44922358248083_wp,  1.78820796632827_wp,  1.91831181222793_wp, &
      &  3.28733959392383_wp,  1.73080526737053_wp,  1.89954588686532_wp, &
      &  0.87869466494779_wp, -2.51731533100115_wp, -2.60865072718871_wp, &
      &  3.87491308528504_wp, -1.27049727215464_wp, -1.22700543307616_wp, &
      & -3.11053572088990_wp, -1.58976839203180_wp,  0.57466966653814_wp, &
      & -3.03284246552550_wp,  0.89928381698069_wp, -1.59977092032304_wp, &
      & -3.15420200058770_wp,  1.60712628245280_wp,  1.70856600647839_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_6p1

subroutine case_6p3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 12
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "C", "C", "C", "C", "H", "H", "H", &
      & "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -5.84744966360287_wp,  1.62166726559720_wp,  0.00000000000000_wp, &
      & -3.74326688747708_wp,  1.67164644479202_wp,  0.00000000000000_wp, &
      & -1.06555711362529_wp,  1.59628092047607_wp,  0.00000000000000_wp, &
      & -0.09032072781491_wp, -1.18286864641345_wp,  0.00000000000000_wp, &
      &  2.77636771973186_wp, -1.16046098310474_wp,  0.00000000000000_wp, &
      & -0.44661869052682_wp,  2.64820443706351_wp,  1.68211736431502_wp, &
      & -0.44661869052682_wp,  2.64820443706351_wp, -1.68211736431502_wp, &
      & -0.82782752879365_wp, -2.14347745813007_wp, -1.66957674208315_wp, &
      & -0.82782752879365_wp, -2.14347745813007_wp,  1.66957674208315_wp, &
      &  3.53080178259694_wp, -0.22510834268515_wp, -1.67701733118894_wp, &
      &  3.45751554623532_wp, -3.10550227384378_wp,  0.00000000000000_wp, &
      &  3.53080178259694_wp, -0.22510834268515_wp,  1.67701733118894_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_6p3

subroutine case_7e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 12
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "C", "O", "C", "C", "C", "H", "H", &
      & "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.80261805672186_wp,  2.54055063189294_wp, -0.87308808687685_wp, &
      & -2.64621336155266_wp,  0.27028411153186_wp, -1.16385249906681_wp, &
      & -3.49613753194237_wp, -0.94582136231253_wp, -3.08936683237489_wp, &
      & -1.42941643691623_wp, -0.76470093905133_wp,  1.12121284458611_wp, &
      &  1.09639415791530_wp, -0.44212090871548_wp,  1.60736019905263_wp, &
      &  2.92711094668501_wp,  0.98525594678553_wp,  0.12479748232032_wp, &
      & -3.21372022905398_wp, -2.75378093515434_wp, -2.97976082694137_wp, &
      & -2.70479676139825_wp, -1.54041008399683_wp,  2.53466355232344_wp, &
      &  1.80361490348969_wp, -1.41121272082586_wp,  3.28029411538443_wp, &
      &  2.16667740293467_wp,  1.89699003542011_wp, -1.55261867542608_wp, &
      &  4.51427926453057_wp, -0.26216758031396_wp, -0.36155245078763_wp, &
      &  3.78482570203015_wp,  2.42713380473986_wp,  1.35191117780663_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_7e

subroutine case_7p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 9
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "C", "C", "H", "C", "H", "C", "H", &
      & "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.57958864256026_wp,  4.99854560659767_wp,  0.00000000000000_wp, &
      & -1.19401060692638_wp,  2.96304675571430_wp,  0.00000000000000_wp, &
      & -1.96713706316538_wp,  0.49424957625977_wp,  0.00000000000000_wp, &
      & -4.00521557919471_wp,  0.23000066733325_wp,  0.00000000000000_wp, &
      & -0.26620628725753_wp, -1.52066524375305_wp,  0.00000000000000_wp, &
      & -1.11062133905306_wp, -3.38958416911115_wp,  0.00000000000000_wp, &
      &  2.33060982215444_wp, -1.30437739510288_wp,  0.00000000000000_wp, &
      &  3.28872289235148_wp,  0.50999250673024_wp,  0.00000000000000_wp, &
      &  3.50344680365141_wp, -2.98120830466813_wp,  0.00000000000000_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_7p1

subroutine case_7p2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 3
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -0.73538468287854_wp, &
      & -1.43911857643907_wp,  0.00000000000000_wp,  0.36769234143926_wp, &
      &  1.43911857643907_wp,  0.00000000000000_wp,  0.36769234143926_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine case_7p2

subroutine case_7p3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "H", "C", "H", "C", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.13524290518611_wp, -1.81851976034662_wp,  0.00000000000000_wp, &
      & -4.07206213359549_wp, -1.09873229347002_wp,  0.00000000000000_wp, &
      & -0.25166707560361_wp, -0.34993892242533_wp,  0.00000000000000_wp, &
      & -1.85248353174624_wp, -3.86805466909789_wp,  0.00000000000000_wp, &
      &  1.77015015107168_wp,  1.34733785158806_wp,  0.00000000000000_wp, &
      &  0.76665754865139_wp,  3.20252734345154_wp,  0.00000000000000_wp, &
      &  2.88732397320419_wp,  1.29269022515014_wp, -1.74041825029616_wp, &
      &  2.88732397320419_wp,  1.29269022515014_wp,  1.74041825029616_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_7p3

subroutine case_7p4(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "C", "O", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.61784606843184_wp, -0.18050981285114_wp,  0.00000000000000_wp, &
      & -0.78516963360569_wp,  1.08550314893172_wp,  0.00000000000000_wp, &
      &  1.62314780576107_wp,  0.46587474942334_wp,  0.00000000000000_wp, &
      &  1.77986789627645_wp, -1.37086808550391_wp,  0.00000000000000_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine case_7p4

subroutine case_7p5(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 10
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "C", "C", "H", "C", "H", "C", "H", &
      & "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -4.65443368183900_wp,  2.23966691124035_wp,  0.00000000000000_wp, &
      & -2.53564560309263_wp,  2.27456367059160_wp,  0.00000000000000_wp, &
      &  0.06404296194935_wp,  2.36690059370489_wp,  0.00000000000000_wp, &
      &  0.88298655780703_wp,  4.24985754958228_wp,  0.00000000000000_wp, &
      &  1.42728484521148_wp,  0.18977276156814_wp,  0.00000000000000_wp, &
      &  3.46986466730599_wp,  0.42762486469806_wp,  0.00000000000000_wp, &
      &  0.46140086522934_wp, -2.39640412521254_wp,  0.00000000000000_wp, &
      & -1.59195839271889_wp, -2.55739349422324_wp,  0.00000000000000_wp, &
      &  1.23822889007365_wp, -3.39729436597476_wp,  1.64305619523887_wp, &
      &  1.23822889007365_wp, -3.39729436597476_wp, -1.64305619523887_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   call new(self, sym, xyz, charge=charge)
end subroutine case_7p5

subroutine case_7p6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -0.91953374427545_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.91953374427545_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine case_7p6

subroutine case_8e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 16
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "C", "H", "H", &
      & "H", "H", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.30027265681554_wp, -0.61309276057397_wp,  2.07720673493904_wp, &
      &  1.30027265681554_wp,  0.61309276057397_wp,  2.07720673493904_wp, &
      &  2.77341404782649_wp, -0.11498875721395_wp, -0.27778497331398_wp, &
      &  1.31887980258281_wp, -0.13537049365674_wp, -2.60537020196915_wp, &
      & -1.31887980258281_wp,  0.13537049365674_wp, -2.60537020196915_wp, &
      & -2.77341404782649_wp,  0.11498875721395_wp, -0.27778497331398_wp, &
      & -2.36292152154135_wp, -0.06392617506307_wp,  3.75398892018767_wp, &
      & -1.11588263436045_wp, -2.67266923056031_wp,  2.14820304081795_wp, &
      &  2.36292152154135_wp,  0.06392617506307_wp,  3.75398892018767_wp, &
      &  1.11588263436045_wp,  2.67266923056031_wp,  2.14820304081795_wp, &
      &  3.53259618066782_wp, -2.07421277763128_wp, -0.14364594657900_wp, &
      &  4.51933942597611_wp,  0.97670288743803_wp, -0.55688918578746_wp, &
      &  2.28796294775883_wp, -0.41602074518998_wp, -4.39570838829511_wp, &
      & -2.28796294775883_wp,  0.41602074518998_wp, -4.39570838829511_wp, &
      & -3.53259618066782_wp,  2.07421277763128_wp, -0.14364594657900_wp, &
      & -4.51933942597611_wp, -0.97670288743803_wp, -0.55688918578746_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_8e

subroutine case_8p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 10
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "H", "H", "H", "H", &
      & "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  2.82498688626850_wp,  0.00000000000000_wp, -0.87421279672944_wp, &
      &  1.33122827072642_wp,  0.00000000000000_wp,  1.26099196084265_wp, &
      & -1.33122827072642_wp,  0.00000000000000_wp,  1.26099196084265_wp, &
      & -2.82498688626850_wp,  0.00000000000000_wp, -0.87421279672944_wp, &
      &  4.86699805521208_wp,  0.00000000000000_wp, -0.70465159986944_wp, &
      &  2.05025006996462_wp,  0.00000000000000_wp, -2.77065032544706_wp, &
      &  2.26428616706260_wp,  0.00000000000000_wp,  3.08852276120327_wp, &
      & -2.26428616706260_wp,  0.00000000000000_wp,  3.08852276120327_wp, &
      & -2.05025006996462_wp,  0.00000000000000_wp, -2.77065032544706_wp, &
      & -4.86699805521208_wp,  0.00000000000000_wp, -0.70465159986944_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_8p1

subroutine case_9e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 13
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "O", "C", "C", "C", "C", "C", "C", "H", &
      & "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -4.56504642283542_wp, -0.29409690660327_wp,  0.00000000000000_wp, &
      & -2.11476559645821_wp, -0.12458504892623_wp,  0.00000000000000_wp, &
      & -0.82489485088160_wp,  2.23779329966943_wp,  0.00000000000000_wp, &
      &  1.74673170464577_wp,  2.23625322267933_wp,  0.00000000000000_wp, &
      &  3.08481297613115_wp, -0.08052446002564_wp,  0.00000000000000_wp, &
      &  1.80539951980268_wp, -2.41625352604177_wp,  0.00000000000000_wp, &
      & -0.76837413367726_wp, -2.46363208567021_wp,  0.00000000000000_wp, &
      & -5.40792783537513_wp,  1.33495723579219_wp,  0.00000000000000_wp, &
      & -1.89878146764729_wp,  3.98426971400265_wp,  0.00000000000000_wp, &
      &  2.78345440955883_wp,  4.00061726907986_wp,  0.00000000000000_wp, &
      &  5.13283578240890_wp, -0.05134530120670_wp,  0.00000000000000_wp, &
      &  2.87461805471931_wp, -4.16061165165831_wp,  0.00000000000000_wp, &
      & -1.84806214039174_wp, -4.20284176109130_wp,  0.00000000000000_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_9e

subroutine case_9p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 11
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "C", "H", "H", "H", &
      & "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.00000000000000_wp,  2.18010277563003_wp,  0.39254791413372_wp, &
      & -0.00000000000000_wp,  1.32292520371330_wp, -2.09777966554598_wp, &
      & -0.00000000000000_wp, -1.32292520371330_wp, -2.09777966554598_wp, &
      & -0.00000000000000_wp, -2.18010277563003_wp,  0.39254791413372_wp, &
      & -0.00000000000000_wp,  0.00000000000000_wp,  2.14635115102111_wp, &
      & -0.00000000000000_wp,  4.14241980282891_wp,  0.97684135468331_wp, &
      & -0.00000000000000_wp,  2.51504373107884_wp, -3.75951719887176_wp, &
      & -0.00000000000000_wp, -2.51504373107884_wp, -3.75951719887176_wp, &
      & -0.00000000000000_wp, -4.14241980282891_wp,  0.97684135468331_wp, &
      & -1.64744532842283_wp,  0.00000000000000_wp,  3.41473202009015_wp, &
      &  1.64744532842283_wp,  0.00000000000000_wp,  3.41473202009015_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_9p1

subroutine case_9p2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "O"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -1.06128680089597_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.06128680089597_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine case_9p2

subroutine case_10e(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 11
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "N", "C", "C", "C", "C", "C", "H", "H", &
      & "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.70811744557548_wp, &
      &  2.25449428839939_wp,  0.00000000000000_wp, -1.72340626319836_wp, &
      &  2.28335461144637_wp,  0.00000000000000_wp,  0.91297120053672_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.20079871077756_wp, &
      & -2.28335461144637_wp,  0.00000000000000_wp,  0.91297120053672_wp, &
      & -2.25449428839939_wp,  0.00000000000000_wp, -1.72340626319836_wp, &
      &  3.91404073319915_wp,  0.00000000000000_wp, -2.92897882813245_wp, &
      &  4.09428917919981_wp,  0.00000000000000_wp,  1.86811578200683_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  4.24991495237247_wp, &
      & -4.09428917919981_wp,  0.00000000000000_wp,  1.86811578200683_wp, &
      & -3.91404073319915_wp,  0.00000000000000_wp, -2.92897882813245_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_10e

subroutine case_10p1(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "C", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  2.39948530763654_wp,  1.73735207627717_wp, -0.75104260596679_wp, &
      &  0.58776182734497_wp,  1.39626652667663_wp,  0.74895032318476_wp, &
      & -0.58776182734497_wp, -1.39626652667663_wp,  0.74895032318476_wp, &
      & -2.39948530763654_wp, -1.73735207627717_wp, -0.75104260596679_wp, &
      &  3.88290270363375_wp,  1.73159626930053_wp, -2.13973324737913_wp, &
      & -0.27575642822627_wp,  2.63387072575764_wp,  2.14182553016116_wp, &
      &  0.27575642822627_wp, -2.63387072575764_wp,  2.14182553016116_wp, &
      & -3.88290270363375_wp, -1.73159626930053_wp, -2.13973324737913_wp],&
      & shape(xyz))
   real(wp), parameter :: charge = 1.0_wp
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, charge=charge, uhf=uhf)
end subroutine case_10p1

subroutine case_10p2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 3
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "N", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -0.04850001017312_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.11638158601747_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.06788157584433_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine case_10p2

subroutine ethylene(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.25001440820830_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  1.25001440820830_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      & -2.32598933007504_wp, -1.74444622201805_wp,  0.00000000000000_wp, &
      & -2.32598933007504_wp,  1.74444622201805_wp,  0.00000000000000_wp, &
      &  2.32598933007504_wp, -1.74444622201805_wp,  0.00000000000000_wp, &
      &  2.32598933007504_wp,  1.74444622201805_wp,  0.00000000000000_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine ethylene

subroutine me(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      & -1.02046491274633_wp, -1.76749707621799_wp,  0.00000000000000_wp, &
      & -1.02046491274633_wp,  1.76749707621799_wp,  0.00000000000000_wp, &
      &  2.04092982549266_wp,  0.00000000000000_wp,  0.00000000000000_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine me

subroutine pr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 10
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "C", "C", "C", "H", "H", "H", "H", "H", &
      & "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -2.07190727832248_wp,  0.68201860197577_wp, -0.27522954056936_wp, &
      &  0.80294990619902_wp,  0.68151517893398_wp, -0.32123756889104_wp, &
      &  1.93721115225553_wp, -1.61377415001569_wp,  0.80670065352483_wp, &
      &  0.90246212846632_wp, -3.38070720159471_wp,  0.83666509595092_wp, &
      &  3.89731805626177_wp, -1.64510788799768_wp,  1.39241578820694_wp, &
      & -2.83891272264515_wp,  2.37797410692146_wp, -1.16629796215994_wp, &
      & -2.77864047455460_wp,  0.60266389939536_wp,  1.66289474625085_wp, &
      & -2.82364090090225_wp, -0.95656179401369_wp, -1.28402109722464_wp, &
      &  1.52837079313185_wp,  2.36711914206895_wp,  0.64219829290361_wp, &
      &  1.44478934011002_wp,  0.88486010432621_wp, -2.29408840799222_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine pr

end module mstore_rc21
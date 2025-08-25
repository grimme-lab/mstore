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

module mstore_f_block
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type, new
   use mstore_data_record, only : record_type, new_record, select_record
   implicit none
   private

   public :: get_f_block_records

contains

subroutine get_f_block_records(records)
   type(record_type), allocatable, intent(out) :: records(:)

   records = [ &
      new_record('LaCl3', LaCl3), &
      new_record('CeCl3', CeCl3), &
      new_record('CeF3', CeF3), &
      new_record('CeH4', CeH4), &
      new_record('PrCl3', PrCl3), &
      new_record('PmCl3', PmCl6), &
      new_record('SmCl3', SmCl3), &
      new_record('EuCl5', EuCl5), &
      new_record('GdCl3', GdCl3), &
      new_record('TbCl3', TbCl3), &
      new_record('DyCl3', DyCl3), &
      new_record('HoCl3', HoCl3), &
      new_record('ErCl3', ErCl3), &
      new_record('TmCl3', TmCl3), &
      new_record('YbCl3', YbCl3), &
      new_record('LuCl3', LuCl3), &
      new_record('AcCl3', AcCl3), &
      new_record('AcCl6', AcCl6), &
      new_record('AcF3', AcF3), &
      new_record('AcH6', AcH6), &
      new_record('ThCl3', ThCl3), &
      new_record('PaCl3', PaCl3), &
      new_record('UCl3',  UCl3), &
      new_record('UF3',  UF3), &
      new_record('UH6',  UH6), &
      new_record('NpCl3', NpCl3), &
      new_record('PuCl3', PuCl3), &
      new_record('AmCl3', AmCl3), &
      new_record('CmCl3', CmCl3), &
      new_record('BkCl3', BkCl3), &
      new_record('CfCl3', CfCl3), &
      new_record('EsCl3', EsCl3), &
      new_record('FmCl3', FmCl3), &
      new_record('MdCl3', MdCl3), &
      new_record('NoCl3', NoCl3), &
      new_record('LrCl3', LrCl3), &
      new_record('LrF6', LrF6), &      
      new_record('LrH6', LrH6), &
      new_record('La2', La2), &
      new_record('Ce2', Ce2), &
      new_record('Pr2', Pr2), &
      new_record('Gd2', Gd2), &
      new_record('U2', U2), &
      new_record('PaNp', PaNp), &
      new_record('Fr_to_Lr', Fr_to_Lr) &
      ]


end subroutine get_f_block_records


subroutine LaCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "La", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.12718349490125_wp,  0.17995754316390_wp, -0.29584060720299_wp, &
      & -1.96159258725379_wp, -2.83681248004335_wp, -3.60101554290256_wp, &
      & -1.99563076140172_wp,  4.55245524789112_wp,  0.58839880286614_wp, &
      &  4.95724258983230_wp, -0.34245330644302_wp,  0.59367679660207_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine LaCl3

subroutine CeCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ce", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.43890984908242_wp,  0.64498498723499_wp, -1.04611347480244_wp, &
      & -1.97273655437244_wp, -2.89691658711067_wp, -3.26154379372671_wp, &
      & -2.07978308053509_wp,  4.32298150611661_wp,  0.80327614241411_wp, &
      &  4.74081252190318_wp, -0.51790290167228_wp,  0.78960057547768_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine CeCl3

subroutine CeF3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ce", "F", "F", "F"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.01810732317964_wp, -0.00605396528891_wp, -0.02271057817759_wp, &
      & -1.42643853274431_wp, -2.02861737958072_wp, -3.06861402709762_wp, &
      & -1.40983837080557_wp,  3.66100529370506_wp,  0.18135820710048_wp, &
      &  3.94537231644828_wp, -0.07318694426675_wp,  0.19518584753734_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine CeF3

subroutine CeH4(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 5
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ce", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.26675351222287_wp, -1.75518185867179_wp,  0.40638050000581_wp, &
      &  3.08477310710560_wp,  0.66161508713108_wp,  0.23653496622209_wp, &
      &  1.55707638053621_wp, -5.11588045343893_wp,  1.34724698126015_wp, &
      & -1.39935123552890_wp, -1.95443846312390_wp, -2.91270317129335_wp, &
      & -2.19685586804443_wp, -0.61774720207740_wp,  2.94844051297163_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine CeH4

subroutine PrCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pr", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.38725290588091_wp,  0.65024152291200_wp, -1.13885863176151_wp, &
      & -1.93741495090061_wp, -2.96598938424885_wp, -3.25901607679934_wp, &
      & -1.95216128677928_wp,  4.29959277331523_wp,  0.88839340438119_wp, &
      &  4.62952606787703_wp, -0.43069790740967_wp,  0.79470075354227_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 2
   call new(self, sym, xyz, uhf=uhf)
end subroutine PrCl3

subroutine PmCl6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pm", "Cl", "Cl", "Cl", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.02692982872878_wp, -0.02082587368829_wp,  0.00982087793192_wp, &
      & -2.12579388636294_wp,  1.73965457113297_wp,  4.30428781076346_wp, &
      & -1.71902209822812_wp,  1.74829266553290_wp, -4.45060581764264_wp, &
      &  2.25398751234073_wp, -4.23121582995462_wp, -1.69999235907846_wp, &
      &  1.64553328626899_wp,  4.35327470566445_wp, -1.97788098111244_wp, &
      & -4.39371985257519_wp, -1.87726544773064_wp,  1.85648502067492_wp, &
      &  4.36596376454656_wp, -1.71193368821799_wp,  1.95786655120202_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine PmCl6

subroutine SmCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Sm", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.30257334634244_wp,  0.41382844819383_wp, -0.72341943328314_wp, &
      & -1.95935084231113_wp, -2.80704683657593_wp, -3.37196652145330_wp, &
      & -1.97483080241390_wp,  4.33339811608465_wp,  0.68135972981775_wp, &
      &  4.75881103446062_wp, -0.38703272313388_wp,  0.69924567428131_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 5
   call new(self, sym, xyz, uhf=uhf)
end subroutine SmCl3

subroutine EuCl5(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Eu", "Cl", "Cl", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.08442123984016_wp,  0.23080618191457_wp,  0.55740673589250_wp, &
      &  0.93473956091233_wp,  2.11677725215957_wp,  4.77479906714616_wp, &
      &  0.63461979093377_wp,  3.21638629200238_wp, -3.50669678095132_wp, &
      & -1.09515309855698_wp, -4.67286561145128_wp,  0.81251894804286_wp, &
      &  3.32299185777184_wp, -3.32783781989784_wp, -0.99444777406908_wp, &
      & -3.88160045363992_wp,  2.43673370527259_wp, -1.64356129879986_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 4
   call new(self, sym, xyz, uhf=uhf)
end subroutine EuCl5

subroutine GdCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Gd", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.05218248561689_wp,  0.06343949445811_wp, -0.10608063318711_wp, &
      & -1.78485441148807_wp, -2.56584194430498_wp, -3.47156736769606_wp, &
      & -1.79954341401307_wp,  4.29466424937922_wp,  0.42444830736804_wp, &
      &  4.65941807596226_wp, -0.23911479496367_wp,  0.43841914287775_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 7
   call new(self, sym, xyz, uhf=uhf)
end subroutine GdCl3

subroutine TbCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Tb", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.22563466860733_wp,  0.32070705223075_wp, -0.54216087150784_wp, &
      & -1.86337883115102_wp, -2.70251945823976_wp, -3.36256602397492_wp, &
      & -1.89926618457793_wp,  4.28229044942488_wp,  0.59320012915887_wp, &
      &  4.66421308319966_wp, -0.34733103884719_wp,  0.59674621568651_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 6
   call new(self, sym, xyz, uhf=uhf)
end subroutine TbCl3

subroutine DyCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Dy", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.13931139813720_wp,  0.22517813476481_wp, -0.37815841397010_wp, &
      & -1.80292982287505_wp, -2.67412081129068_wp, -3.40980848785579_wp, &
      & -1.83728309607977_wp,  4.30477467289627_wp,  0.54829052340063_wp, &
      &  4.62810425689565_wp, -0.30268499180174_wp,  0.52489582778788_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 5
   call new(self, sym, xyz, uhf=uhf)
end subroutine DyCl3

subroutine HoCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ho", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.30321209063714_wp,  0.42318167051916_wp, -0.73461771727010_wp, &
      & -1.88603340930695_wp, -2.71574210269661_wp, -3.29191909481678_wp, &
      & -1.90833469075290_wp,  4.21352735115935_wp,  0.65309677175909_wp, &
      &  4.61835874550072_wp, -0.36781991441322_wp,  0.65865948969038_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 4
   call new(self, sym, xyz, uhf=uhf)
end subroutine HoCl3

subroutine ErCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Er", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.28087338364432_wp,  0.39516976916837_wp, -0.71959420600424_wp, &
      & -1.88163053399729_wp, -2.71693501766444_wp, -3.29901923353894_wp, &
      & -1.86132483429542_wp,  4.20552471684346_wp,  0.64953622505924_wp, &
      &  4.58928472072642_wp, -0.33061246377868_wp,  0.65429666384656_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine ErCl3

subroutine TmCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Tm", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.30238207087297_wp,  0.40915658962222_wp, -0.74135961337020_wp, & 
      & -1.88316193081721_wp, -2.68542935396554_wp, -3.27476690321514_wp, & 
      & -1.87199842360969_wp,  4.16773917183794_wp,  0.64664211658554_wp, & 
      &  4.57998101963198_wp, -0.33831940292595_wp,  0.65470384936243_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 2
   call new(self, sym, xyz, uhf=uhf)
end subroutine TmCl3

subroutine YbCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Yb", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.25466340356520_wp,  0.41281044012189_wp, -0.69215127106106_wp, &
      & -1.81122612363893_wp, -2.71607662474722_wp, -3.24959699839278_wp, &
      & -1.85569036738285_wp,  4.20641141423360_wp,  0.62587268278042_wp, &
      &  4.53945582353460_wp, -0.34999822503959_wp,  0.60109503603606_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine YbCl3

subroutine LuCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Lu", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.28367564663329_wp,  0.39317127286647_wp, -0.67788785298920_wp, &
      & -1.83944542958897_wp, -2.64595471964360_wp, -3.25518679752306_wp, &
      & -1.86294644500533_wp,  4.15345927589976_wp,  0.60293135001353_wp, &
      &  4.54591896403901_wp, -0.34752882455396_wp,  0.61536274986135_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine LuCl3

subroutine AcCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ac", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00061348140566_wp, -0.00398020381100_wp,  0.01632487344445_wp, &
      & -1.93112403115039_wp, -2.78595407044106_wp, -3.71500365971373_wp, &
      & -1.95175938355530_wp,  4.61895164490745_wp,  0.48735140290392_wp, &
      &  5.00947266937806_wp, -0.27587036608674_wp,  0.49654683272800_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine AcCl3

subroutine AcCl6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ac", "Cl", "Cl", "Cl", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00008500680087_wp, -0.00013948134627_wp,  0.00028153808429_wp, &
      &  0.00019164221735_wp, -0.00014687089066_wp,  5.15320096510534_wp, &
      &  0.00004226821807_wp,  0.00005671238880_wp, -5.15267233955303_wp, &
      & -0.00032197779468_wp, -5.15352035687762_wp, -0.00012585350687_wp, &
      & -0.00003451216376_wp,  5.15327261073759_wp, -0.00038499090056_wp, &
      & -5.15287934187094_wp,  0.00007143910176_wp, -0.00022458050183_wp, &
      &  5.15293581185431_wp,  0.00038704962512_wp, -0.00009363598857_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine AcCl6

subroutine AcF3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ac", "F", "F", "F"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.02973193615572_wp, -0.04565511825587_wp,  0.08782187952105_wp, &
      & -1.45627277668335_wp, -2.10847606530186_wp, -3.17523753381785_wp, &
      & -1.47172009035903_wp,  3.80831331634985_wp,  0.18329027736997_wp, &
      &  4.08492753927610_wp, -0.10103512822345_wp,  0.18934482628942_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine AcF3

subroutine AcH6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ac", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000971821551_wp, -0.00003790794329_wp, -0.00008945800563_wp, &
      &  0.00000841790411_wp, -0.00000413040589_wp,  4.32423211611110_wp, &
      & -0.00001973589999_wp,  0.00002638536649_wp, -4.32441273772383_wp, &
      &  0.00001377936028_wp, -4.32434921042449_wp,  0.00006660038976_wp, &
      &  0.00000759202966_wp,  4.32434161108762_wp,  0.00005738085616_wp, &
      & -4.32441783375872_wp,  0.00000527193304_wp,  0.00006017164264_wp, &
      &  4.32441695941041_wp, -0.00000091687473_wp,  0.00006702946854_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine AcH6

subroutine ThCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Th", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.28345617702357_wp,  0.39213370065303_wp, -0.68293555570348_wp, &
      & -1.97684884565068_wp, -2.84120511011771_wp, -3.42057915141336_wp, &
      & -1.99399019062933_wp,  4.39155689915303_wp,  0.68735819367371_wp, &
      &  4.81458559533448_wp, -0.38933848511970_wp,  0.70137596280576_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine ThCl3

subroutine PaCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pa", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.12566820818671_wp,  0.14027013823455_wp, -0.16610455260099_wp, &
      & -1.85623793334527_wp, -2.61751646604149_wp, -3.49562743542151_wp, &
      & -1.95701798560547_wp,  4.37317785370415_wp,  0.45212648268381_wp, &
      &  4.81479044684208_wp, -0.34278452132853_wp,  0.49482495470129_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 2
   call new(self, sym, xyz, uhf=uhf)
end subroutine PaCl3

subroutine UCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "U", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.13237686034048_wp, -0.20749921454142_wp,  0.35780039698693_wp, &
      & -1.67835525250665_wp, -2.39784434481898_wp, -3.56390928186505_wp, &
      & -1.67827058528793_wp,  4.28242467437262_wp,  0.23413100847485_wp, &
      &  4.61620543421308_wp, -0.12393411044355_wp,  0.25719732576588_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine UCl3

subroutine UF3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "U", "F", "F", "F"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.11579153504213_wp, -0.16741406911463_wp,  0.30202429610079_wp, &
      & -1.24935298655612_wp, -1.81399903444222_wp, -3.03076209385839_wp, &
      & -1.26323920167362_wp,  3.53318820126220_wp,  0.00511349438828_wp, &
      &  3.75558645934986_wp,  0.00137190686334_wp,  0.00884375273193_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine UF3

subroutine UH6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "U", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00001818234754_wp, -0.00002310060783_wp, -0.00007878744014_wp, &
      & -0.00000384842500_wp,  0.00000780259549_wp,  3.67825548310232_wp, &
      & -0.00001765596471_wp,  0.00002167202826_wp, -3.67833736398782_wp, &
      & -0.00000588144105_wp, -3.67830947243931_wp,  0.00003340200127_wp, &
      & -0.00000587681602_wp,  3.67828361904990_wp,  0.00003764636745_wp, &
      & -3.67827861602847_wp,  0.00000028862925_wp,  0.00003751425426_wp, &
      &  3.67831259358891_wp,  0.00000029348301_wp,  0.00003320844144_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine UH6

subroutine NpCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Np", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.10890028569972_wp, -0.17216001923380_wp,  0.30112327758625_wp, &
      & -1.69300387746915_wp, -2.42252610791402_wp, -3.55535243050506_wp, &
      & -1.69812472473842_wp,  4.28939676350022_wp,  0.25919722212250_wp, &
      &  4.62723162398534_wp, -0.14156363178372_wp,  0.28025138015893_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 4
   call new(self, sym, xyz, uhf=uhf)
end subroutine NpCl3

subroutine PuCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pu", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.06479933785678_wp, -0.14384452788683_wp,  0.20400692624104_wp, &
      & -1.75875487889018_wp, -2.45650984301728_wp, -3.55305117766382_wp, &
      & -1.69776695857429_wp,  4.27761224109571_wp,  0.29002435681173_wp, &
      &  4.64852391139927_wp, -0.12411086562293_wp,  0.34423934397364_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 5
   call new(self, sym, xyz, uhf=uhf)
end subroutine PuCl3

subroutine AmCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Am", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.01193364698947_wp, -0.03541789303411_wp,  0.07038809832668_wp, & 
      & -1.76279500408354_wp, -2.52042220577224_wp, -3.52343055390955_wp, & 
      & -1.78089448153841_wp,  4.31798053098351_wp,  0.35803967157097_wp, & 
      &  4.68282586868943_wp, -0.20899342760849_wp,  0.38022223337450_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 6
   call new(self, sym, xyz, uhf=uhf)
end subroutine AmCl3

subroutine CmCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Cm", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.08006061837137_wp, -0.12115504566405_wp,  0.21801582420402_wp, &
      & -1.69337122626512_wp, -2.44039749501877_wp, -3.52400748454540_wp, &
      & -1.70730086984632_wp,  4.27564025699863_wp,  0.28992476481500_wp, &
      &  4.60793545056081_wp, -0.16094071174713_wp,  0.30128634488900_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 7
   call new(self, sym, xyz, uhf=uhf)
end subroutine CmCl3

subroutine BkCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Bk", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.04092808951418_wp, -0.07900596495710_wp,  0.11742203920174_wp, &
      & -1.73184476143690_wp, -2.47193705520268_wp, -3.50762750393996_wp, &
      & -1.71334847274072_wp,  4.26584557495551_wp,  0.32556362383818_wp, &
      &  4.61332405976984_wp, -0.16175555022705_wp,  0.34986129026266_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 6
   call new(self, sym, xyz, uhf=uhf)
end subroutine BkCl3

subroutine CfCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Cf", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.00991100574507_wp, -0.04843904166646_wp,  0.10691107916509_wp, &
      & -1.72620324487154_wp, -2.42489821481915_wp, -3.47208366510848_wp, &
      & -1.75055970043792_wp,  4.22354902783607_wp,  0.30204924539382_wp, &
      &  4.61387668713258_wp, -0.19706476678181_wp,  0.34834278991219_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 5
   call new(self, sym, xyz, uhf=uhf)
end subroutine CfCl3

subroutine EsCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Es", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.03816696934744_wp,  0.05302249145835_wp, -0.06792237560810_wp, &
      & -1.73153522520585_wp, -2.50853611081141_wp, -3.43537242033123_wp, &
      & -1.77045146910048_wp,  4.24433275821738_wp,  0.39238116834960_wp, &
      &  4.59102246103691_wp, -0.23567213429563_wp,  0.39613307695235_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 4
   call new(self, sym, xyz, uhf=uhf)
end subroutine EsCl3

subroutine FmCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Fm", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.03816696934744_wp,  0.05302249145835_wp, -0.06792237560810_wp, &
      & -1.73153522520585_wp, -2.50853611081141_wp, -3.43537242033123_wp, &
      & -1.77045146910048_wp,  4.24433275821738_wp,  0.39238116834960_wp, &
      &  4.59102246103691_wp, -0.23567213429563_wp,  0.39613307695235_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine FmCl3

subroutine MdCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Md", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.23767727392914_wp,  0.37299704300802_wp, -0.51184445084786_wp, &
      & -1.78906614693551_wp, -2.65503517199116_wp, -3.30818638564725_wp, &
      & -1.96100030902549_wp,  4.26563023702853_wp,  0.57177122118024_wp, &
      &  4.63959191810991_wp, -0.43044510347671_wp,  0.53347906467750_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 2
   call new(self, sym, xyz, uhf=uhf)
end subroutine MdCl3

subroutine NoCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "No", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.47184533572796_wp,  0.67607946459626_wp, -0.43128412740764_wp, &
      & -1.68320399850926_wp, -2.45488702264303_wp, -3.05802777633548_wp, &
      & -2.43859811539342_wp,  4.20678650445327_wp,  0.38453491328473_wp, &
      &  4.77715951425275_wp, -0.87483194183781_wp,  0.38999643982103_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 1
   call new(self, sym, xyz, uhf=uhf)
end subroutine NoCl3

subroutine LrCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Lr", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.02267424547810_wp,  0.02187778505625_wp, -0.03276177609475_wp, &
      & -1.72268618929344_wp, -2.47791389736263_wp, -3.43222347143518_wp, &
      & -1.73736813815814_wp,  4.21683875252265_wp,  0.36833125031890_wp, &
      &  4.56458281805150_wp, -0.20765563564759_wp,  0.38187344657365_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine LrCl3

subroutine LrF6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Lr", "F", "F", "F", "F", "F", "F"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00287418847613_wp, -0.00263740835179_wp,  0.00157024628021_wp, &
      &  1.10142208088305_wp, -1.46820687489147_wp,  3.65650687862645_wp, &
      &  1.43369977027932_wp, -1.09497077915493_wp, -3.66955981621906_wp, &
      & -1.06420846200011_wp, -3.63799993614120_wp,  1.53575148222084_wp, &
      & -1.45133178265859_wp,  3.67223463197002_wp,  1.04523540450183_wp, &
      & -3.66069085756512_wp,  1.49266372337150_wp, -1.01873880544872_wp, &
      &  3.63825395984658_wp,  1.03889774593659_wp, -1.55078428722278_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine LrF6

subroutine LrH6(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 7
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Lr", "H", "H", "H", "H", "H", "H"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00003645290924_wp,  0.00000541485418_wp, -0.00011651977956_wp, &
      & -0.00002317364612_wp, -0.00001306067243_wp,  3.82840132251820_wp, &
      & -0.00003999025798_wp,  0.00000426918269_wp, -3.82853657444602_wp, &
      &  0.00000731679485_wp, -3.82850255971882_wp,  0.00006011354863_wp, &
      &  0.00000918421969_wp,  3.82849718863115_wp,  0.00006163673514_wp, &
      & -3.82846469714774_wp, -0.00000600727118_wp,  0.00005657931663_wp, &
      &  3.82849380438929_wp, -0.00000414226685_wp,  0.00005454484571_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine LrH6

subroutine La2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "La", "La"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.55113026824479_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.55113026824479_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine La2

subroutine Ce2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ce", "Ce"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.46042341426275_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.460423414262753_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine Ce2

subroutine Pr2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pr", "Pr"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.384834369277722_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.384834369277722_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 4
   call new(self, sym, xyz, uhf=uhf)
end subroutine Pr2

subroutine Gd2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pr", "Pr"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.709867262713354_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.709867262713354_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 9
   call new(self, sym, xyz, uhf=uhf)
end subroutine Gd2

subroutine U2(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "U", "U"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.42829807014411_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.42829807014411_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine U2

subroutine PaNp(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 2
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Pa", "Np"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp, -2.42829807014411_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.42829807014411_wp],&
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine PaNp

subroutine Fr_to_Lr(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 17
   integer, parameter :: num(nat) = [&
      & 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.98692316414074_wp, 6.12727238368797_wp,-6.67861597188102_wp, &
      & 3.63898862390869_wp, 5.12109301182962_wp, 3.01908613326278_wp, &
      & 5.14503571563551_wp,-3.97172984617710_wp, 3.82011791828867_wp, &
      & 6.71986847575494_wp, 1.71382138402812_wp, 3.92749159076307_wp, &
      & 4.13783589704826_wp,-2.10695793491818_wp, 0.19753203068899_wp, &
      & 8.97685097698326_wp,-3.08813636191844_wp,-4.45568615593938_wp, &
      & 12.5486412940776_wp,-1.77128765259458_wp, 0.59261498922861_wp, &
      & 7.82051475868325_wp,-3.97159756604558_wp,-0.53637703616916_wp, &
      &-0.43444574624893_wp,-1.69696511583960_wp,-1.65898182093050_wp, &
      &-4.71270645149099_wp,-0.11534827468942_wp, 2.84863373521297_wp, &
      &-2.52061680335614_wp, 1.82937752749537_wp,-2.10366982879172_wp, &
      & 0.13551154616576_wp, 7.99805359235043_wp,-1.55508522619903_wp, &
      & 3.91594542499717_wp,-1.72975169129597_wp,-5.07944366756113_wp, &
      &-1.03393930231679_wp, 4.69307230054046_wp, 0.02656940927472_wp, &
      & 6.20675384557240_wp, 4.24490721493632_wp,-0.71004195169885_wp, &
      & 7.04586341131562_wp, 5.20053667939076_wp,-7.51972863675876_wp, &
      & 2.01082807362334_wp, 1.34838807211157_wp,-4.70482633508447_wp],&
      & [3, nat])
   call new(self, num, xyz)
end subroutine Fr_to_Lr

end module mstore_f_block

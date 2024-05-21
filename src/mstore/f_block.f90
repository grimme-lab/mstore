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
      new_record('AcCl3', AcCl3), &
      new_record('AcCl6', AcCl6), &
      new_record('ThCl3', ThCl3), &
      new_record('PaCl3', PaCl3), &
      new_record('UCl3',  UCl3), &
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
      new_record('CeCl3', CeCl3), &
      new_record('La2', La2), &
      new_record('Ce2', Ce2), &
      new_record('Pr2', Pr2), &
      new_record('Gd2', Gd2), &
      new_record('U2', U2), &
      new_record('PaNp', PaNp) &
      ]


end subroutine get_f_block_records

subroutine AcCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ac", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00032464037919_wp, -0.00210623315153_wp,  0.00863875099768_wp, &
      & -1.02190682871192_wp, -1.47426340469986_wp, -1.96589527514175_wp, &
      & -1.03282658694355_wp,  2.44424394874795_wp,  0.25789525611836_wp, &
      &  2.65089877527629_wp, -0.14598431089657_wp,  0.26276126802572_wp], &
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

subroutine ThCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Th", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.14999854917056_wp,  0.20750821801264_wp, -0.36139393259366_wp, &
      & -1.04610335851824_wp, -1.50350099577544_wp, -1.81009253501787_wp, &
      & -1.05517416764517_wp,  2.32391183141563_wp,  0.36373429181958_wp, &
      &  2.54776897699286_wp, -0.20602905365284_wp,  0.37115217579196_wp], &
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
      &  0.06650075190742_wp,  0.07422776052394_wp, -0.08789874386368_wp, &
      & -0.98227881234000_wp, -1.38513006299251_wp, -1.84980637663236_wp, &
      & -1.03560931930971_wp,  2.31418605940593_wp,  0.23925503108200_wp, &
      &  2.54787737974230_wp, -0.18139375693735_wp,  0.26185008941403_wp], &
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
      & -0.07005081774307_wp, -0.10980385561559_wp,  0.18933981613753_wp, &
      & -0.88814735142587_wp, -1.26888458257084_wp, -1.88593957368866_wp, &
      & -0.88810254746321_wp,  2.26616154508669_wp,  0.12389679405063_wp, &
      &  2.44279071663215_wp, -0.06558310690026_wp,  0.13610296350050_wp], &
      & shape(xyz))
   integer, parameter :: uhf = 3
   call new(self, sym, xyz, uhf=uhf)
end subroutine UCl3

subroutine NpCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Np", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -0.05762754945312_wp, -0.09110315880715_wp,  0.15934757617106_wp, &
      & -0.89589906992709_wp, -1.28194560912564_wp, -1.88141148295187_wp, &
      & -0.89860890560250_wp,  2.26985101576540_wp,  0.13716126307659_wp, &
      &  2.44862552498272_wp, -0.07491224783261_wp,  0.14830264370422_wp], &
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
      & -0.03429033287541_wp, -0.07611924607081_wp,  0.10795581623313_wp, &
      & -0.93069300147315_wp, -1.29992902728365_wp, -1.88019371239176_wp, &
      & -0.89841958390161_wp,  2.26361491506756_wp,  0.15347428023157_wp, &
      &  2.45989291825017_wp, -0.06567664171310_wp,  0.18216361592705_wp], &
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
      & -0.00631501402979_wp, -0.01874234185185_wp,  0.03724777755328_wp, &
      & -0.93283094365467_wp, -1.33374999314854_wp, -1.86451915332827_wp, &
      & -0.94240877465304_wp,  2.28497689411931_wp,  0.18946643479455_wp, &
      &  2.47804473233749_wp, -0.11059455911892_wp,  0.20120494098043_wp], &
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
      & -0.04236625473293_wp, -0.06411248915133_wp,  0.11536900578500_wp, &
      & -0.89609346253837_wp, -1.29140273990870_wp, -1.86482445187303_wp, &
      & -0.90346471247754_wp,  2.26257138602312_wp,  0.15342157841651_wp, &
      &  2.43841442974883_wp, -0.08516615696309_wp,  0.15943386767152_wp], &
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
      & -0.02165821225670_wp, -0.04180815618070_wp,  0.06213706720332_wp, &
      & -0.91645278057415_wp, -1.30809275639993_wp, -1.85615653942160_wp, &
      & -0.90666496610985_wp,  2.25738826349786_wp,  0.17228085043416_wp, &
      &  2.44126595894071_wp, -0.08559735091723_wp,  0.18513862178412_wp], &
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
      & -0.00524467837742_wp, -0.02563283696787_wp,  0.05657490668721_wp, &
      & -0.91346741857283_wp, -1.28320087404166_wp, -1.83734754992397_wp, &
      & -0.92635629979693_wp,  2.23500589466237_wp,  0.15983757723286_wp, &
      &  2.44155839674719_wp, -0.10428218365285_wp,  0.18433506600390_wp], &
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
      &  0.00971255848957_wp,  0.02179254696387_wp, -0.00854090662832_wp, &
      & -0.90445113356681_wp, -1.32513637629971_wp, -1.82514774911527_wp, &
      & -0.94404416737348_wp,  2.25775962236507_wp,  0.20152135988080_wp, &
      &  2.43527274245072_wp, -0.13252579302923_wp,  0.19556729586279_wp], &
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
      &  0.02019709038790_wp,  0.02805829414506_wp, -0.03594297328220_wp, &
      & -0.91628898105473_wp, -1.32746014256864_wp, -1.81792079580397_wp, &
      & -0.93688257045771_wp,  2.24600417113771_wp,  0.20763917227810_wp, &
      &  2.42946446112454_wp, -0.12471232271412_wp,  0.20962459680807_wp], &
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
      &  0.12577339691285_wp,  0.19738153489405_wp, -0.27085641891585_wp, &
      & -0.94673303375631_wp, -1.40498410716365_wp, -1.75061684470409_wp, &
      & -1.03771667411003_wp,  2.25727431157426_wp,  0.30256830009876_wp, &
      &  2.45516631095350_wp, -0.22778173930466_wp,  0.28230496352118_wp], &
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
      &  0.24968979873811_wp,  0.35776584542384_wp, -0.22822573164831_wp, &
      & -0.89071319731191_wp, -1.29907026772421_wp, -1.61823860954511_wp, &
      & -1.29045054921720_wp,  2.22613554929096_wp,  0.20348711290684_wp, &
      &  2.52796394779100_wp, -0.46294112699059_wp,  0.20637722828659_wp], &
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
      &  0.01199869398143_wp,  0.01157722527680_wp, -0.01733678529805_wp, &
      & -0.91160627291142_wp, -1.31125556506414_wp, -1.81625444380988_wp, &
      & -0.91937562566226_wp,  2.23145496988762_wp,  0.19491250373217_wp, &
      &  2.41547320459225_wp, -0.10988663010028_wp,  0.20207872537576_wp], &
      & shape(xyz))
   call new(self, sym, xyz)
end subroutine LrCl3

subroutine CeCl3(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ce", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      & -1.27856652880800_wp,  1.27787818507814_wp, -1.27775264265885_wp, &
      &  3.22922632838831_wp,  2.49019149650794_wp, -2.49029228642597_wp, &
      & -2.48733682732001_wp, -3.23161500918965_wp, -2.48733351877882_wp, &
      & -2.48688477166153_wp,  2.48710712700479_wp,  3.23181664846241_wp],&
      & shape(xyz))
   integer, parameter :: uhf = 4
   call new(self, sym, xyz, uhf=uhf)
end subroutine CeCl3

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

end module mstore_f_block

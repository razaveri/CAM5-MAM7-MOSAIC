




      module mo_lin_matrix

      private
      public :: linmat

      contains

      subroutine linmat01( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

         mat(1348) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(1882) = rxt(135)

         mat(1892) = -( rxt(135) + het_rates(2) )
         mat(1358) = rxt(3)
         mat(1794) = rxt(5)
         mat(1571) = rxt(6)
         mat(232) = rxt(8)
         mat(1867) = rxt(10)
         mat(1205) = rxt(19)
         mat(1381) = rxt(22)
         mat(114) = rxt(23)
         mat(1284) = rxt(30)
         mat(1911) = rxt(138) + rxt(139)
         mat(140) = rxt(186)

         mat(1912) = -( rxt(138) + rxt(139) + rxt(141)*y(4) + rxt(142)*y(4) &
                      + rxt(144)*y(154) + rxt(145)*y(155) + rxt(146)*y(156) &
                      + rxt(147)*y(164) + rxt(148)*y(165) + rxt(149)*y(157) &
                      + rxt(150)*y(162) + rxt(151)*y(163) + rxt(152)*y(158) &
                      + rxt(153)*y(153) + rxt(154)*y(161) + rxt(155)*y(160) &
                      + rxt(156)*y(166) + rxt(157)*y(167) + rxt(158)*y(168) &
                      + rxt(159)*y(169) + rxt(160)*y(12) + rxt(161)*y(12) + rxt(162)*y(12) &
                 + het_rates(3) )
         mat(1359) = rxt(2)
         mat(1206) = rxt(18)

         mat(871) = -( het_rates(20) )
         mat(1290) = rxt(16)
         mat(1199) = rxt(18)
         mat(1896) = rxt(162)*y(12)

         mat(936) = -( het_rates(17) )
         mat(1291) = rxt(15) + rxt(16)
         mat(976) = rxt(56)
         mat(1011) = 1.340_r8*rxt(62)
         mat(1080) = .700_r8*rxt(63)
         mat(1024) = rxt(69)
         mat(943) = .330_r8*rxt(78)
         mat(881) = rxt(81)
         mat(302) = .450_r8*rxt(83)
         mat(762) = 2.000_r8*rxt(84)
         mat(506) = .250_r8*rxt(89)
         mat(848) = rxt(90)
         mat(902) = 1.700_r8*rxt(91)
         mat(128) = 1.500_r8*rxt(93)
         mat(163) = 1.500_r8*rxt(94)
         mat(512) = .600_r8*rxt(96)
         mat(243) = rxt(97)
         mat(1607) = rxt(251)*y(152)

         mat(137) = -( rxt(186) + het_rates(5) )
         mat(1720) = rxt(5)

         mat(1791) = -( rxt(5) + het_rates(6) )
         mat(1568) = rxt(6) + .500_r8*rxt(484)
         mat(230) = rxt(8)
         mat(1864) = rxt(11)
         mat(139) = rxt(186)
         mat(1908) = 2.000_r8*rxt(141)*y(4)

         mat(1564) = -( rxt(6) + rxt(484) + het_rates(7) )
         mat(229) = rxt(7) + rxt(198)
         mat(678) = rxt(9)
         mat(1860) = rxt(10)
         mat(327) = rxt(13) + rxt(207)
         mat(755) = rxt(28)
         mat(435) = rxt(34)
         mat(391) = .600_r8*rxt(59) + rxt(308)
         mat(464) = rxt(60) + rxt(356)
         mat(111) = rxt(71)
         mat(531) = rxt(72)
         mat(355) = rxt(73)
         mat(706) = rxt(74)
         mat(424) = rxt(75)
         mat(280) = rxt(76)
         mat(735) = rxt(77)
         mat(954) = rxt(78)
         mat(103) = rxt(423)

         mat(1524) = -( rxt(252)*y(152) + rxt(253)*y(159) + rxt(254)*y(157) &
                      + rxt(255)*y(153) + rxt(257)*y(162) + rxt(258)*y(163) &
                      + rxt(259)*y(169) + rxt(260)*y(168) + rxt(263)*y(12) + het_rates(22) &
       )
         mat(677) = rxt(9)
         mat(326) = rxt(12)
         mat(362) = rxt(14)
         mat(1202) = rxt(17)
         mat(485) = 2.000_r8*rxt(20)
         mat(684) = rxt(25)
         mat(644) = rxt(31)
         mat(449) = rxt(57)
         mat(420) = rxt(58)
         mat(255) = rxt(64)
         mat(109) = rxt(65)
         mat(310) = rxt(66)
         mat(317) = rxt(67)
         mat(220) = rxt(70)
         mat(279) = rxt(76)
         mat(542) = rxt(85)
         mat(285) = rxt(86)
         mat(499) = rxt(87)
         mat(384) = rxt(88)
         mat(508) = rxt(89)
         mat(191) = rxt(98)
         mat(263) = rxt(99)
         mat(208) = rxt(100)
         mat(341) = rxt(101)
         mat(269) = rxt(102)
         mat(298) = rxt(103)
         mat(565) = rxt(104)
         mat(1563) = .500_r8*rxt(484)
         mat(1903) = rxt(160)*y(12)

         mat(1866) = -( rxt(10) + rxt(11) + rxt(483) + het_rates(8) )
         mat(231) = rxt(7) + rxt(8) + rxt(198)
         mat(329) = rxt(12)
         mat(758) = rxt(27)
         mat(437) = rxt(33)
         mat(392) = .400_r8*rxt(59)

         mat(675) = -( rxt(9) + het_rates(9) )
         mat(228) = 2.000_r8*rxt(482) + 2.000_r8*rxt(491) + 2.000_r8*rxt(497) &
                      + 2.000_r8*rxt(502)
         mat(1822) = rxt(483)
         mat(1547) = .500_r8*rxt(484)
         mat(749) = rxt(492) + rxt(498) + rxt(503)
         mat(433) = rxt(493) + rxt(501) + rxt(504)

         mat(324) = -( rxt(12) + rxt(13) + rxt(207) + het_rates(10) )

         mat(227) = -( rxt(7) + rxt(8) + rxt(198) + rxt(482) + rxt(491) + rxt(497) &
                      + rxt(502) + het_rates(11) )

         mat(1253) = -( het_rates(13) )
         mat(981) = rxt(56)
         mat(418) = rxt(58)
         mat(388) = .400_r8*rxt(59)
         mat(1089) = .300_r8*rxt(63)
         mat(842) = rxt(68)
         mat(1899) = rxt(160)*y(12)
         mat(1612) = rxt(214)*y(12)
         mat(1519) = rxt(263)*y(12)

         mat(357) = -( rxt(14) + het_rates(14) )

         mat(141) = -( het_rates(18) )

         mat(88) = -( het_rates(19) )

         mat(1294) = -( rxt(15) + rxt(16) + het_rates(16) )
         mat(361) = rxt(14)
         mat(448) = rxt(57)
         mat(1019) = 1.340_r8*rxt(61)
         mat(316) = rxt(67)
         mat(529) = .100_r8*rxt(72)
         mat(733) = rxt(77)
         mat(952) = .330_r8*rxt(78)
         mat(602) = .690_r8*rxt(79)
         mat(959) = rxt(80)
         mat(882) = rxt(81)
         mat(541) = .100_r8*rxt(85)
         mat(383) = .400_r8*rxt(88)
         mat(507) = .375_r8*rxt(89)
         mat(907) = .680_r8*rxt(91)
         mat(320) = rxt(277)
         mat(215) = 2.000_r8*rxt(289)
         mat(1900) = rxt(161)*y(12) + rxt(162)*y(12)

         mat(1209) = -( rxt(167) + het_rates(21) )
         mat(359) = rxt(14)
         mat(1293) = 2.000_r8*rxt(15)
         mat(1201) = rxt(17) + 2.000_r8*rxt(19)
         mat(1580) = rxt(26)
         mat(651) = rxt(32)
         mat(1898) = rxt(161)*y(12)

         mat(1713) = -( rxt(485) + het_rates(23) )
         mat(328) = rxt(13) + rxt(207)
         mat(984) = rxt(56)
         mat(450) = rxt(57)
         mat(1022) = 1.340_r8*rxt(61) + .660_r8*rxt(62)
         mat(256) = rxt(64)
         mat(311) = rxt(66)
         mat(1031) = rxt(69)
         mat(532) = rxt(72)
         mat(356) = rxt(73)
         mat(707) = rxt(74)
         mat(425) = rxt(75)
         mat(955) = .670_r8*rxt(78)
         mat(604) = rxt(79)
         mat(961) = rxt(80)
         mat(884) = 2.000_r8*rxt(81)
         mat(305) = .560_r8*rxt(83)
         mat(764) = 2.000_r8*rxt(84)
         mat(543) = .900_r8*rxt(85)
         mat(500) = rxt(87)
         mat(385) = rxt(88)
         mat(509) = rxt(89)
         mat(850) = rxt(90)
         mat(909) = 1.200_r8*rxt(91)
         mat(365) = rxt(92)
         mat(129) = 1.500_r8*rxt(93)
         mat(165) = rxt(94)
         mat(235) = .600_r8*rxt(95)
         mat(513) = .600_r8*rxt(96)
         mat(246) = rxt(97)
         mat(192) = rxt(98)
         mat(264) = rxt(99)
         mat(342) = rxt(101)
         mat(270) = rxt(102)
         mat(299) = rxt(103)
         mat(566) = rxt(104)
         mat(1214) = rxt(167)
         mat(322) = rxt(277)
         mat(216) = rxt(288) + rxt(289)
         mat(1148) = rxt(376)
         mat(1907) = rxt(161)*y(12)
         mat(1620) = rxt(251)*y(152) + rxt(256)*y(153)
         mat(1528) = rxt(252)*y(152) + rxt(255)*y(153)


      end subroutine linmat01

      subroutine linmat02( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

         mat(483) = -( rxt(20) + het_rates(24) )
         mat(1655) = .500_r8*rxt(485)

         mat(1200) = -( rxt(17) + rxt(18) + rxt(19) + het_rates(237) )
         mat(1517) = rxt(252)*y(152) + rxt(253)*y(159) + rxt(254)*y(157) + rxt(255)*y(153) &
                      + rxt(259)*y(169) + rxt(263)*y(12)

         mat(1619) = -( rxt(214)*y(12) + rxt(251)*y(152) + rxt(256)*y(153) &
                      + rxt(261)*y(169) + rxt(262)*y(168) + het_rates(27) )
         mat(136) = 2.000_r8*rxt(21)
         mat(1376) = rxt(22)
         mat(93) = 2.000_r8*rxt(24)
         mat(686) = rxt(25)
         mat(1587) = rxt(26)
         mat(757) = rxt(27)
         mat(200) = rxt(29)
         mat(1906) = 3.000_r8*rxt(144)*y(154) + 2.000_r8*rxt(145)*y(155) &
                      + 3.000_r8*rxt(146)*y(156) + 2.000_r8*rxt(147)*y(164) &
                      + rxt(148)*y(165) + rxt(149)*y(157) + 2.000_r8*rxt(150)*y(162) &
                      + rxt(151)*y(163) + 4.000_r8*rxt(152)*y(158) + rxt(154)*y(161)
         mat(1527) = rxt(252)*y(152) + 3.000_r8*rxt(253)*y(159) + rxt(254)*y(157) &
                      + 2.000_r8*rxt(257)*y(162) + rxt(258)*y(163)

         mat(135) = -( rxt(21) + het_rates(28) )

         mat(1372) = -( rxt(22) + het_rates(29) )
         mat(113) = rxt(23)
         mat(753) = rxt(28)
         mat(92) = 2.000_r8*rxt(226)

         mat(112) = -( rxt(23) + het_rates(30) )

         mat(91) = -( rxt(24) + rxt(226) + het_rates(31) )

         mat(1586) = -( rxt(26) + het_rates(32) )
         mat(1618) = rxt(214)*y(12) + 2.000_r8*rxt(251)*y(152) + rxt(256)*y(153) &
                      + rxt(261)*y(169) + rxt(262)*y(168)

         mat(681) = -( rxt(25) + het_rates(33) )
         mat(750) = rxt(492) + rxt(498) + rxt(503)

         mat(751) = -( rxt(27) + rxt(28) + rxt(492) + rxt(498) + rxt(503) + het_rates(34) &
       )

         mat(199) = -( rxt(29) + het_rates(35) )

         mat(1810) = -( het_rates(36) )
         mat(201) = rxt(29)
         mat(1282) = rxt(30)
         mat(647) = rxt(31)
         mat(653) = rxt(32)
         mat(436) = rxt(33)
         mat(1909) = rxt(153)*y(153) + rxt(154)*y(161) + rxt(155)*y(160) &
                      + 2.000_r8*rxt(156)*y(166) + 2.000_r8*rxt(157)*y(167) &
                      + 3.000_r8*rxt(158)*y(168) + 2.000_r8*rxt(159)*y(169)
         mat(1530) = rxt(255)*y(153) + 2.000_r8*rxt(259)*y(169) + 3.000_r8*rxt(260)*y(168)
         mat(1622) = rxt(256)*y(153) + 2.000_r8*rxt(261)*y(169) + 3.000_r8*rxt(262)*y(168)

         mat(1273) = -( rxt(30) + het_rates(37) )
         mat(434) = rxt(34)

         mat(649) = -( rxt(32) + het_rates(38) )

         mat(641) = -( rxt(31) + het_rates(39) )
         mat(432) = rxt(493) + rxt(501) + rxt(504)

         mat(431) = -( rxt(33) + rxt(34) + rxt(493) + rxt(501) + rxt(504) + het_rates(40) &
       )

         mat(856) = -( het_rates(59) )
         mat(1079) = .700_r8*rxt(63)

         mat(714) = -( het_rates(92) )

         mat(631) = -( het_rates(64) )

         mat(977) = -( rxt(56) + het_rates(50) )
         mat(446) = rxt(57)
         mat(254) = rxt(64)
         mat(527) = .400_r8*rxt(72)
         mat(947) = .330_r8*rxt(78)
         mat(539) = .400_r8*rxt(85)
         mat(283) = rxt(86)

         mat(439) = -( het_rates(49) )

         mat(443) = -( rxt(57) + het_rates(65) )

         mat(1188) = -( het_rates(48) )
         mat(387) = .600_r8*rxt(59) + rxt(308)
         mat(1016) = 1.340_r8*rxt(61)
         mat(1086) = .300_r8*rxt(63)
         mat(314) = rxt(67)
         mat(840) = rxt(68)
         mat(1026) = rxt(69)
         mat(732) = rxt(77)
         mat(948) = .330_r8*rxt(78)
         mat(958) = rxt(80)
         mat(429) = rxt(82)
         mat(304) = .130_r8*rxt(83)
         mat(284) = rxt(86)
         mat(904) = .650_r8*rxt(91)
         mat(164) = .500_r8*rxt(94)
         mat(245) = rxt(97)

         mat(415) = -( rxt(58) + het_rates(54) )

         mat(386) = -( rxt(59) + rxt(308) + het_rates(58) )

         mat(236) = -( het_rates(45) )

         mat(451) = -( het_rates(44) )

         mat(202) = -( het_rates(71) )

         mat(459) = -( rxt(60) + rxt(356) + het_rates(81) )

         mat(523) = -( rxt(72) + het_rates(82) )

         mat(730) = -( rxt(77) + het_rates(83) )

         mat(944) = -( rxt(78) + het_rates(84) )

         mat(467) = -( het_rates(85) )

         mat(401) = -( het_rates(86) )

         mat(351) = -( rxt(73) + het_rates(87) )

         mat(700) = -( rxt(74) + het_rates(88) )

         mat(271) = -( het_rates(89) )

         mat(421) = -( rxt(75) + het_rates(90) )

         mat(276) = -( rxt(76) + het_rates(91) )

         mat(286) = -( het_rates(70) )

         mat(475) = -( het_rates(73) )

         mat(828) = -( het_rates(93) )

         mat(534) = -( rxt(85) + het_rates(94) )

         mat(426) = -( rxt(82) + het_rates(72) )
         mat(522) = .800_r8*rxt(72)
         mat(533) = .800_r8*rxt(85)

         mat(545) = -( het_rates(74) )

         mat(281) = -( rxt(86) + het_rates(75) )

         mat(172) = -( het_rates(106) )

         mat(180) = -( het_rates(107) )

         mat(578) = -( het_rates(108) )

         mat(492) = -( rxt(87) + het_rates(109) )

         mat(739) = -( het_rates(143) )

         mat(379) = -( rxt(88) + het_rates(144) )

         mat(300) = -( rxt(83) + het_rates(95) )

         mat(761) = -( rxt(84) + rxt(486) + het_rates(53) )
         mat(301) = .130_r8*rxt(83)
         mat(497) = .600_r8*rxt(87)
         mat(262) = .700_r8*rxt(99)
         mat(340) = rxt(101)
         mat(296) = .170_r8*rxt(103)
         mat(560) = .340_r8*rxt(104)

         mat(120) = -( het_rates(110) )

         mat(150) = -( het_rates(133) )

         mat(130) = -( het_rates(111) )

         mat(126) = -( rxt(93) + het_rates(112) )

         mat(368) = -( het_rates(113) )

         mat(331) = -( het_rates(114) )

         mat(375) = -( het_rates(115) )
         mat(206) = rxt(100)

         mat(260) = -( rxt(99) + het_rates(116) )

         mat(516) = -( het_rates(117) )

         mat(205) = -( rxt(100) + het_rates(118) )

         mat(337) = -( rxt(101) + het_rates(119) )

         mat(363) = -( rxt(92) + het_rates(120) )
         mat(491) = .200_r8*rxt(87)
         mat(127) = rxt(93)
         mat(338) = .500_r8*rxt(101)
         mat(554) = .060_r8*rxt(104)

         mat(233) = -( rxt(95) + het_rates(121) )
         mat(490) = .200_r8*rxt(87)
         mat(552) = .200_r8*rxt(104)

         mat(510) = -( rxt(96) + het_rates(122) )
         mat(699) = rxt(74)
         mat(493) = .200_r8*rxt(87)
         mat(189) = rxt(98)
         mat(555) = .150_r8*rxt(104)

         mat(242) = -( rxt(97) + het_rates(123) )
         mat(553) = .210_r8*rxt(104)

         mat(567) = -( het_rates(124) )
         mat(364) = .600_r8*rxt(92)

         mat(162) = -( rxt(94) + het_rates(125) )

         mat(345) = -( het_rates(126) )

         mat(266) = -( rxt(102) + het_rates(127) )

         mat(185) = -( het_rates(128) )
         mat(265) = rxt(102)

         mat(409) = -( het_rates(129) )
         mat(102) = rxt(423)

         mat(622) = -( het_rates(130) )
         mat(234) = .600_r8*rxt(95)

         mat(928) = -( het_rates(131) )
         mat(511) = .600_r8*rxt(96)

         mat(101) = -( rxt(423) + het_rates(132) )

         mat(157) = -( het_rates(134) )

         mat(394) = -( het_rates(135) )

         mat(294) = -( rxt(103) + het_rates(136) )

         mat(611) = -( het_rates(137) )

         mat(556) = -( rxt(104) + het_rates(138) )

         mat(804) = -( het_rates(141) )

         mat(775) = -( het_rates(142) )

         mat(12) = -( het_rates(140) )

         mat(29) = -( het_rates(139) )

         mat(18) = -( het_rates(148) )

         mat(35) = -( het_rates(147) )

         mat(844) = -( rxt(90) + het_rates(145) )
         mat(422) = rxt(75)
         mat(277) = rxt(76)
         mat(382) = rxt(88)

         mat(916) = -( het_rates(146) )

         mat(901) = -( rxt(91) + het_rates(149) )
         mat(504) = rxt(89)
         mat(846) = rxt(90)

         mat(501) = -( rxt(89) + het_rates(150) )

         mat(889) = -( het_rates(151) )

         mat(1063) = -( het_rates(97) )

         mat(1138) = -( rxt(376) + het_rates(98) )

         mat(188) = -( rxt(98) + het_rates(99) )
         mat(1118) = rxt(376)

         mat(104) = -( het_rates(100) )

         mat(1084) = -( rxt(63) + het_rates(77) )
         mat(601) = .402_r8*rxt(79)

         mat(1012) = -( rxt(61) + rxt(62) + het_rates(78) )
         mat(599) = .288_r8*rxt(79)

         mat(1106) = -( het_rates(79) )

         mat(247) = -( het_rates(80) )

         mat(1156) = -( het_rates(76) )
         mat(461) = rxt(60) + rxt(356)
         mat(1015) = .660_r8*rxt(61)

         mat(691) = -( het_rates(46) )
         mat(428) = rxt(82)

         mat(252) = -( rxt(64) + het_rates(47) )

         mat(94) = -( het_rates(60) )


      end subroutine linmat02

      subroutine linmat03( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

         mat(657) = -( het_rates(61) )

         mat(306) = -( rxt(66) + het_rates(62) )

         mat(838) = -( rxt(68) + het_rates(63) )
         mat(308) = .820_r8*rxt(66)
         mat(526) = .250_r8*rxt(72)
         mat(941) = .170_r8*rxt(78)
         mat(538) = .250_r8*rxt(85)
         mat(381) = .050_r8*rxt(88)
         mat(502) = .300_r8*rxt(89)
         mat(900) = .500_r8*rxt(91)

         mat(312) = -( rxt(67) + het_rates(69) )

         mat(667) = -( het_rates(15) )

         mat(209) = -( het_rates(51) )

         mat(880) = -( rxt(81) + het_rates(52) )
         mat(942) = .330_r8*rxt(78)
         mat(503) = .250_r8*rxt(89)
         mat(214) = rxt(288)

         mat(956) = -( rxt(80) + het_rates(66) )
         mat(945) = .170_r8*rxt(78)

         mat(590) = -( het_rates(55) )

         mat(213) = -( rxt(288) + rxt(289) + het_rates(56) )
         mat(108) = rxt(65)

         mat(107) = -( rxt(65) + het_rates(57) )

         mat(257) = -( het_rates(96) )

         mat(965) = -( het_rates(67) )
         mat(903) = .150_r8*rxt(91)

         mat(1025) = -( rxt(69) + het_rates(68) )
         mat(303) = .180_r8*rxt(83)
         mat(498) = .400_r8*rxt(87)
         mat(244) = rxt(97)
         mat(297) = .510_r8*rxt(103)
         mat(563) = .540_r8*rxt(104)

         mat(996) = -( het_rates(101) )

         mat(110) = -( rxt(71) + het_rates(102) )

         mat(1040) = -( het_rates(103) )

         mat(217) = -( rxt(70) + het_rates(104) )

         mat(598) = -( rxt(79) + het_rates(105) )
         mat(352) = rxt(73)

         mat(221) = -( het_rates(41) )

         mat(671) = -( het_rates(42) )

         mat(318) = -( rxt(277) + het_rates(43) )

         mat(99) = -( het_rates(170) )

         mat(194) = -( het_rates(171) )

         mat(1) = -( het_rates(172) )

         mat(2) = -( het_rates(173) )

         mat(3) = -( het_rates(174) )

         mat(4) = -( het_rates(175) )

         mat(5) = -( het_rates(176) )

         mat(6) = -( het_rates(177) )

         mat(19) = -( het_rates(178) )

         mat(20) = -( het_rates(179) )

         mat(21) = -( het_rates(180) )

         mat(22) = -( het_rates(181) )

         mat(23) = -( het_rates(182) )

         mat(36) = -( het_rates(183) )
         mat(760) = rxt(486)

         mat(37) = -( het_rates(184) )

         mat(38) = -( het_rates(185) )

         mat(39) = -( het_rates(186) )

         mat(40) = -( het_rates(187) )

         mat(41) = -( het_rates(218) )

         mat(42) = -( het_rates(219) )

         mat(43) = -( het_rates(220) )

         mat(44) = -( het_rates(221) )

         mat(45) = -( het_rates(222) )

         mat(46) = -( het_rates(223) )

         mat(47) = -( het_rates(224) )

         mat(48) = -( het_rates(225) )

         mat(49) = -( rxt(115) + het_rates(188) )

         mat(50) = -( rxt(116) + het_rates(189) )

         mat(51) = -( rxt(117) + het_rates(190) )

         mat(52) = -( rxt(118) + het_rates(191) )

         mat(53) = -( rxt(119) + het_rates(192) )

         mat(54) = -( rxt(105) + het_rates(193) )

         mat(55) = -( rxt(106) + het_rates(194) )

         mat(56) = -( rxt(107) + het_rates(195) )

         mat(57) = -( rxt(108) + het_rates(196) )

         mat(58) = -( rxt(109) + het_rates(197) )

         mat(59) = -( rxt(125) + het_rates(198) )

         mat(60) = -( rxt(126) + het_rates(199) )

         mat(61) = -( rxt(127) + het_rates(200) )

         mat(62) = -( rxt(128) + het_rates(201) )

         mat(63) = -( rxt(129) + het_rates(202) )

         mat(64) = -( rxt(120) + het_rates(203) )

         mat(65) = -( rxt(121) + het_rates(204) )

         mat(66) = -( rxt(122) + het_rates(205) )

         mat(67) = -( rxt(123) + het_rates(206) )

         mat(68) = -( rxt(124) + het_rates(207) )

         mat(69) = -( rxt(110) + het_rates(208) )

         mat(70) = -( rxt(111) + het_rates(209) )

         mat(71) = -( rxt(112) + het_rates(210) )

         mat(72) = -( rxt(113) + het_rates(211) )

         mat(73) = -( rxt(114) + het_rates(212) )

         mat(74) = -( rxt(130) + het_rates(213) )

         mat(75) = -( rxt(131) + het_rates(214) )

         mat(76) = -( rxt(132) + het_rates(215) )

         mat(77) = -( rxt(133) + het_rates(216) )

         mat(78) = -( rxt(134) + het_rates(217) )

         mat(79) = -( het_rates(226) )

         mat(80) = -( het_rates(227) )

         mat(81) = -( het_rates(228) )

         mat(82) = -( het_rates(229) )

         mat(83) = -( het_rates(230) )

         mat(84) = -( het_rates(231) )

         mat(85) = -( het_rates(232) )

         mat(86) = -( het_rates(233) )

         mat(87) = -( het_rates(234) )


      end subroutine linmat03

      subroutine linmat( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

      call linmat01( mat, y, rxt, het_rates )
      call linmat02( mat, y, rxt, het_rates )
      call linmat03( mat, y, rxt, het_rates )

      end subroutine linmat

      end module mo_lin_matrix

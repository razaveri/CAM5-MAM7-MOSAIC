




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

         mat(1107) = -( rxt(3) + rxt(4) + het_rates(1) )

         mat(1061) = -( rxt(92) + rxt(93) + rxt(94) + rxt(105) + rxt(106) + rxt(107) &
                 + het_rates(2) )
         mat(942) = rxt(1) + 2.000_r8*rxt(2) + rxt(98) + rxt(99) + rxt(100) &
                      + 2.000_r8*rxt(103) + rxt(110) + rxt(111) + rxt(112) &
                      + 2.000_r8*rxt(115)
         mat(1106) = rxt(4)
         mat(1020) = rxt(6)
         mat(1169) = rxt(8)
         mat(129) = rxt(10)
         mat(1354) = rxt(12)
         mat(1380) = rxt(21)
         mat(1543) = rxt(24)
         mat(135) = rxt(25)
         mat(1193) = rxt(32)
         mat(589) = rxt(88)
         mat(106) = rxt(89)
         mat(846) = rxt(91)
         mat(1132) = rxt(131)

         mat(1134) = -( rxt(131) + rxt(135)*y(7) + rxt(136)*y(7) + rxt(138)*y(110) &
                      + rxt(139)*y(111) + rxt(140)*y(112) + rxt(141)*y(120) &
                      + rxt(142)*y(121) + rxt(143)*y(113) + rxt(144)*y(118) &
                      + rxt(145)*y(119) + rxt(146)*y(114) + rxt(147)*y(109) &
                      + rxt(148)*y(117) + rxt(149)*y(116) + rxt(150)*y(122) &
                      + rxt(151)*y(123) + rxt(152)*y(124) + rxt(153)*y(125) &
                      + rxt(156)*y(15) + rxt(157)*y(15) + rxt(158)*y(15) + het_rates(3) )
         mat(944) = rxt(1)
         mat(1108) = rxt(3)
         mat(1382) = rxt(20)

         mat(940) = -( rxt(1) + rxt(2) + rxt(96) + rxt(98) + rxt(99) + rxt(100) + rxt(103) &
                      + rxt(108) + rxt(110) + rxt(111) + rxt(112) + rxt(115) &
                 + het_rates(4) )
         mat(1103) = rxt(4)
         mat(1351) = rxt(13)
         mat(71) = rxt(126)
         mat(68) = rxt(129) + rxt(130)
         mat(1129) = rxt(136)*y(7)

         mat(70) = -( rxt(123) + rxt(126) + rxt(125)*y(126) + het_rates(5) )

         mat(67) = -( rxt(129) + rxt(130) + het_rates(6) )
         mat(1074) = rxt(3)
         mat(69) = rxt(123) + rxt(125)*y(126)

         mat(684) = -( het_rates(21) )
         mat(1210) = rxt(18)
         mat(1372) = rxt(20)
         mat(1125) = rxt(158)*y(15)

         mat(636) = -( het_rates(20) )
         mat(1209) = rxt(17) + rxt(18)
         mat(640) = rxt(61)
         mat(670) = 1.340_r8*rxt(67)
         mat(772) = .700_r8*rxt(68)
         mat(695) = rxt(74)
         mat(579) = rxt(76)
         mat(554) = rxt(79)
         mat(279) = .450_r8*rxt(81)
         mat(418) = 2.000_r8*rxt(82)
         mat(165) = rxt(90)
         mat(1405) = rxt(254)*y(108)
         mat(355) = rxt(467)*y(126)

         mat(509) = -( rxt(95) + het_rates(8) )
         mat(997) = rxt(6)
         mat(354) = rxt(464)

         mat(1019) = -( rxt(6) + rxt(7) + het_rates(9) )
         mat(1168) = rxt(8) + .500_r8*rxt(405)
         mat(128) = rxt(10)
         mat(1353) = rxt(13)
         mat(454) = rxt(474)
         mat(1131) = 2.000_r8*rxt(135)*y(7)

         mat(1172) = -( rxt(8) + rxt(405) + het_rates(10) )
         mat(130) = rxt(9) + rxt(197)
         mat(966) = rxt(11)
         mat(1357) = rxt(12)
         mat(238) = rxt(15) + rxt(206)
         mat(612) = rxt(30)
         mat(320) = rxt(36)
         mat(246) = .600_r8*rxt(64) + rxt(311)
         mat(286) = rxt(65) + rxt(357)
         mat(581) = rxt(76)

         mat(1527) = -( rxt(255)*y(108) + rxt(256)*y(115) + rxt(257)*y(113) &
                      + rxt(258)*y(109) + rxt(260)*y(118) + rxt(261)*y(119) &
                      + rxt(262)*y(125) + rxt(263)*y(124) + rxt(266)*y(15) + het_rates(23) &
       )
         mat(974) = rxt(11)
         mat(242) = rxt(14)
         mat(210) = rxt(16)
         mat(1391) = rxt(19)
         mat(350) = 2.000_r8*rxt(22)
         mat(536) = rxt(27)
         mat(439) = rxt(33)
         mat(330) = rxt(62)
         mat(267) = rxt(63)
         mat(158) = rxt(69)
         mat(63) = rxt(70)
         mat(192) = rxt(71)
         mat(198) = rxt(72)
         mat(102) = rxt(75)
         mat(368) = rxt(83)
         mat(148) = rxt(84)
         mat(175) = rxt(85)
         mat(227) = rxt(86)
         mat(1180) = .500_r8*rxt(405)
         mat(1143) = rxt(156)*y(15)

         mat(1362) = -( rxt(12) + rxt(13) + rxt(404) + het_rates(11) )
         mat(131) = rxt(9) + rxt(10) + rxt(197)
         mat(240) = rxt(14)
         mat(613) = rxt(29)
         mat(322) = rxt(35)
         mat(248) = .400_r8*rxt(64)

         mat(961) = -( rxt(11) + het_rates(12) )
         mat(127) = 2.000_r8*rxt(403) + 2.000_r8*rxt(446) + 2.000_r8*rxt(452) &
                      + 2.000_r8*rxt(457)
         mat(1352) = rxt(404)
         mat(1167) = .500_r8*rxt(405)
         mat(610) = rxt(447) + rxt(453) + rxt(458)
         mat(318) = rxt(448) + rxt(456) + rxt(459)

         mat(236) = -( rxt(14) + rxt(15) + rxt(206) + het_rates(13) )

         mat(126) = -( rxt(9) + rxt(10) + rxt(197) + rxt(403) + rxt(446) + rxt(452) &
                      + rxt(457) + het_rates(14) )

         mat(909) = -( het_rates(16) )
         mat(643) = rxt(61)
         mat(264) = rxt(63)
         mat(245) = .400_r8*rxt(64)
         mat(780) = .300_r8*rxt(68)
         mat(407) = rxt(73)
         mat(1128) = rxt(156)*y(15)
         mat(1411) = rxt(213)*y(15)
         mat(443) = rxt(252)*y(15)
         mat(1512) = rxt(266)*y(15)

         mat(205) = -( rxt(16) + het_rates(17) )

         mat(85) = -( het_rates(42) )

         mat(46) = -( het_rates(43) )

         mat(1222) = -( rxt(17) + rxt(18) + het_rates(19) )
         mat(207) = rxt(16)
         mat(327) = rxt(62)
         mat(677) = 1.340_r8*rxt(66)
         mat(196) = rxt(72)
         mat(582) = rxt(76)
         mat(312) = .690_r8*rxt(77)
         mat(667) = rxt(78)
         mat(555) = rxt(79)
         mat(366) = .100_r8*rxt(83)
         mat(203) = rxt(280)
         mat(219) = 2.000_r8*rxt(292)
         mat(1137) = rxt(157)*y(15) + rxt(158)*y(15)

         mat(1243) = -( het_rates(22) )
         mat(208) = rxt(16)
         mat(1223) = 2.000_r8*rxt(17)
         mat(1386) = rxt(19) + 2.000_r8*rxt(21)
         mat(868) = rxt(28)
         mat(501) = rxt(34)
         mat(91) = rxt(57)
         mat(1138) = rxt(157)*y(15)

         mat(1317) = -( rxt(408) + het_rates(24) )
         mat(239) = rxt(15) + rxt(206)
         mat(646) = rxt(61)
         mat(328) = rxt(62)
         mat(679) = 1.340_r8*rxt(66) + .660_r8*rxt(67)
         mat(157) = rxt(69)
         mat(190) = rxt(71)
         mat(700) = rxt(74)
         mat(583) = rxt(76)
         mat(313) = rxt(77)
         mat(668) = rxt(78)
         mat(556) = 2.000_r8*rxt(79)
         mat(282) = .560_r8*rxt(81)
         mat(419) = 2.000_r8*rxt(82)
         mat(367) = .900_r8*rxt(83)
         mat(226) = rxt(86)
         mat(204) = rxt(280)
         mat(220) = rxt(292)
         mat(1139) = rxt(157)*y(15)
         mat(1422) = rxt(254)*y(108) + rxt(259)*y(109)
         mat(1523) = rxt(255)*y(108) + rxt(258)*y(109)

         mat(344) = -( rxt(22) + het_rates(25) )
         mat(1274) = .500_r8*rxt(408)

         mat(1389) = -( rxt(19) + rxt(20) + rxt(21) + het_rates(134) )
         mat(74) = rxt(87)
         mat(1525) = rxt(255)*y(108) + rxt(256)*y(115) + rxt(257)*y(113) + rxt(258)*y(109) &
                      + rxt(262)*y(125) + rxt(266)*y(15)

         mat(1425) = -( rxt(213)*y(15) + rxt(254)*y(108) + rxt(259)*y(109) &
                      + rxt(264)*y(125) + rxt(265)*y(124) + het_rates(28) )
         mat(76) = 2.000_r8*rxt(23)
         mat(1553) = rxt(24)
         mat(32) = 2.000_r8*rxt(26)
         mat(535) = rxt(27)
         mat(871) = rxt(28)
         mat(615) = rxt(29)
         mat(94) = rxt(31)
         mat(84) = rxt(56)
         mat(1142) = 2.000_r8*rxt(138)*y(110) + 2.000_r8*rxt(139)*y(111) &
                      + 2.000_r8*rxt(140)*y(112) + 2.000_r8*rxt(141)*y(120) &
                      + rxt(142)*y(121) + rxt(143)*y(113) + rxt(144)*y(118) &
                      + rxt(145)*y(119) + 4.000_r8*rxt(146)*y(114) + rxt(148)*y(117)
         mat(1526) = rxt(255)*y(108) + 3.000_r8*rxt(256)*y(115) + rxt(257)*y(113) &
                      + rxt(260)*y(118) + rxt(261)*y(119)

         mat(75) = -( rxt(23) + het_rates(29) )

         mat(1555) = -( rxt(24) + het_rates(30) )
         mat(136) = rxt(25)
         mat(617) = rxt(30)
         mat(33) = 2.000_r8*rxt(225)

         mat(132) = -( rxt(25) + het_rates(31) )

         mat(31) = -( rxt(26) + rxt(225) + het_rates(32) )


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

         mat(861) = -( rxt(28) + het_rates(33) )
         mat(1409) = rxt(213)*y(15) + 2.000_r8*rxt(254)*y(108) + rxt(259)*y(109) &
                      + rxt(264)*y(125) + rxt(265)*y(124)

         mat(531) = -( rxt(27) + het_rates(34) )
         mat(607) = rxt(447) + rxt(453) + rxt(458)

         mat(608) = -( rxt(29) + rxt(30) + rxt(447) + rxt(453) + rxt(458) + het_rates(35) &
       )

         mat(92) = -( rxt(31) + het_rates(36) )

         mat(876) = -( het_rates(37) )
         mat(93) = rxt(31)
         mat(1189) = rxt(32)
         mat(434) = rxt(33)
         mat(498) = rxt(34)
         mat(317) = rxt(35)
         mat(1127) = rxt(147)*y(109) + rxt(148)*y(117) + rxt(149)*y(116) &
                      + 2.000_r8*rxt(150)*y(122) + 2.000_r8*rxt(151)*y(123) &
                      + 3.000_r8*rxt(152)*y(124) + 2.000_r8*rxt(153)*y(125)
         mat(1511) = rxt(258)*y(109) + 2.000_r8*rxt(262)*y(125) + 3.000_r8*rxt(263)*y(124)
         mat(1410) = rxt(259)*y(109) + 2.000_r8*rxt(264)*y(125) + 3.000_r8*rxt(265)*y(124)

         mat(1197) = -( rxt(32) + het_rates(38) )
         mat(321) = rxt(36)

         mat(497) = -( rxt(34) + het_rates(39) )

         mat(432) = -( rxt(33) + het_rates(40) )
         mat(316) = rxt(448) + rxt(456) + rxt(459)

         mat(315) = -( rxt(35) + rxt(36) + rxt(448) + rxt(456) + rxt(459) + het_rates(41) &
       )

         mat(379) = -( het_rates(127) )

         mat(449) = -( rxt(474) + het_rates(128) )
         mat(931) = rxt(96) + rxt(108)
         mat(352) = rxt(467)*y(126)

         mat(250) = -( het_rates(129) )
         mat(504) = rxt(95)

         mat(351) = -( rxt(464) + rxt(467)*y(126) + het_rates(130) )
         mat(1040) = rxt(92) + rxt(93) + rxt(94) + rxt(105) + rxt(106) + rxt(107)
         mat(928) = rxt(98) + rxt(99) + rxt(100) + rxt(110) + rxt(111) + rxt(112)

         mat(458) = -( het_rates(131) )
         mat(993) = rxt(7)
         mat(353) = rxt(464)
         mat(450) = rxt(474)

         mat(257) = -( het_rates(133) )

         mat(469) = -( het_rates(132) )
         mat(994) = rxt(7)
         mat(1046) = rxt(92) + rxt(93) + rxt(94) + rxt(105) + rxt(106) + rxt(107)
         mat(508) = rxt(95)
         mat(933) = rxt(96) + rxt(98) + rxt(99) + rxt(100) + rxt(108) + rxt(110) &
                      + rxt(111) + rxt(112)

         mat(621) = -( het_rates(59) )
         mat(771) = .700_r8*rxt(68)

         mat(560) = -( het_rates(83) )

         mat(487) = -( het_rates(64) )

         mat(641) = -( rxt(61) + het_rates(50) )
         mat(325) = rxt(62)
         mat(156) = rxt(69)
         mat(364) = .400_r8*rxt(83)
         mat(146) = rxt(84)

         mat(340) = -( het_rates(49) )

         mat(323) = -( rxt(62) + het_rates(65) )

         mat(826) = -( het_rates(48) )
         mat(244) = .600_r8*rxt(64) + rxt(311)
         mat(675) = 1.340_r8*rxt(66)
         mat(779) = .300_r8*rxt(68)
         mat(195) = rxt(72)
         mat(406) = rxt(73)
         mat(697) = rxt(74)
         mat(666) = rxt(78)
         mat(214) = rxt(80)
         mat(281) = .130_r8*rxt(81)
         mat(147) = rxt(84)

         mat(262) = -( rxt(63) + het_rates(54) )

         mat(243) = -( rxt(64) + rxt(311) + het_rates(58) )

         mat(183) = -( het_rates(82) )

         mat(108) = -( het_rates(45) )

         mat(291) = -( het_rates(44) )

         mat(34) = -( het_rates(71) )

         mat(283) = -( rxt(65) + rxt(357) + het_rates(81) )

         mat(37) = -( het_rates(70) )

         mat(137) = -( het_rates(73) )

         mat(393) = -( het_rates(84) )

         mat(359) = -( rxt(83) + het_rates(85) )

         mat(211) = -( rxt(80) + het_rates(72) )
         mat(358) = .800_r8*rxt(83)

         mat(370) = -( het_rates(74) )

         mat(144) = -( rxt(84) + het_rates(75) )

         mat(53) = -( het_rates(94) )

         mat(58) = -( het_rates(95) )

         mat(269) = -( het_rates(96) )

         mat(170) = -( rxt(85) + het_rates(97) )

         mat(77) = -( het_rates(98) )

         mat(595) = -( het_rates(106) )

         mat(221) = -( rxt(86) + het_rates(107) )

         mat(277) = -( rxt(81) + het_rates(86) )
         mat(172) = .900_r8*rxt(85)

         mat(417) = -( rxt(82) + het_rates(53) )
         mat(278) = .130_r8*rxt(81)
         mat(173) = .450_r8*rxt(85)

         mat(40) = -( het_rates(99) )

         mat(177) = -( het_rates(100) )

         mat(1) = -( het_rates(101) )

         mat(43) = -( het_rates(102) )

         mat(229) = -( het_rates(103) )

         mat(2) = -( het_rates(104) )

         mat(734) = -( het_rates(88) )

         mat(777) = -( rxt(68) + het_rates(77) )
         mat(311) = .402_r8*rxt(77)
         mat(225) = rxt(86)

         mat(671) = -( rxt(66) + rxt(67) + het_rates(78) )
         mat(308) = .288_r8*rxt(77)
         mat(224) = rxt(86)

         mat(758) = -( het_rates(79) )

         mat(149) = -( het_rates(80) )

         mat(797) = -( het_rates(76) )
         mat(285) = rxt(65) + rxt(357)
         mat(674) = .660_r8*rxt(66)

         mat(521) = -( het_rates(46) )
         mat(213) = rxt(80)

         mat(154) = -( rxt(69) + het_rates(47) )

         mat(331) = -( het_rates(105) )

         mat(49) = -( het_rates(60) )

         mat(540) = -( het_rates(61) )

         mat(187) = -( rxt(71) + het_rates(62) )

         mat(404) = -( rxt(73) + het_rates(63) )
         mat(188) = .820_r8*rxt(71)
         mat(362) = .250_r8*rxt(83)
         mat(222) = .100_r8*rxt(86)

         mat(193) = -( rxt(72) + het_rates(69) )

         mat(303) = -( het_rates(18) )

         mat(95) = -( het_rates(51) )

         mat(553) = -( rxt(79) + het_rates(52) )

         mat(664) = -( rxt(78) + het_rates(66) )

         mat(423) = -( het_rates(55) )

         mat(216) = -( rxt(292) + het_rates(56) )
         mat(62) = rxt(70)

         mat(61) = -( rxt(70) + het_rates(57) )

         mat(159) = -( het_rates(87) )

         mat(652) = -( het_rates(67) )

         mat(696) = -( rxt(74) + het_rates(68) )
         mat(280) = .180_r8*rxt(81)
         mat(174) = .450_r8*rxt(85)

         mat(476) = -( het_rates(89) )

         mat(578) = -( rxt(76) + het_rates(90) )

         mat(711) = -( het_rates(91) )

         mat(99) = -( rxt(75) + het_rates(92) )

         mat(307) = -( rxt(77) + het_rates(93) )

         mat(114) = -( het_rates(135) )

         mat(299) = -( het_rates(136) )

         mat(199) = -( rxt(280) + het_rates(137) )

         mat(64) = -( rxt(55) + het_rates(138) )
         mat(1119) = rxt(139)*y(111) + rxt(140)*y(112) + 2.000_r8*rxt(141)*y(120) &
                      + 2.000_r8*rxt(142)*y(121) + rxt(143)*y(113) + rxt(145)*y(119) &
                      + rxt(148)*y(117) + rxt(149)*y(116) + rxt(150)*y(122) &
                      + 2.000_r8*rxt(151)*y(123)
         mat(1436) = rxt(257)*y(113) + rxt(261)*y(119)

         mat(81) = -( rxt(56) + het_rates(139) )
         mat(1121) = rxt(138)*y(110) + rxt(140)*y(112) + rxt(144)*y(118)
         mat(1438) = rxt(260)*y(118)

         mat(89) = -( rxt(57) + het_rates(140) )
         mat(440) = rxt(252)*y(15)

         mat(441) = -( rxt(252)*y(15) + het_rates(141) )
         mat(65) = 2.000_r8*rxt(55)
         mat(82) = rxt(56)
         mat(90) = rxt(57)
         mat(1123) = rxt(142)*y(121) + rxt(149)*y(116)

         mat(587) = -( rxt(88) + het_rates(156) )
         mat(105) = rxt(89)

         mat(120) = -( het_rates(157) )

         mat(3) = -( rxt(413) + het_rates(158) )

         mat(4) = -( het_rates(159) )

         mat(5) = -( rxt(419) + het_rates(160) )

         mat(6) = -( rxt(420) + het_rates(161) )

         mat(7) = -( rxt(414) + het_rates(146) )

         mat(8) = -( rxt(415) + het_rates(147) )

         mat(9) = -( rxt(417) + het_rates(148) )

         mat(10) = -( rxt(416) + het_rates(149) )

         mat(11) = -( rxt(418) + het_rates(150) )

         mat(12) = -( het_rates(151) )

         mat(13) = -( het_rates(152) )

         mat(14) = -( het_rates(153) )

         mat(15) = -( het_rates(154) )

         mat(16) = -( het_rates(155) )

         mat(162) = -( rxt(90) + het_rates(162) )

         mat(410) = -( het_rates(163) )
         mat(163) = rxt(90)
         mat(840) = rxt(91)

         mat(842) = -( rxt(91) + het_rates(164) )
         mat(588) = rxt(88)

         mat(104) = -( rxt(89) + het_rates(165) )
         mat(73) = rxt(87)

         mat(72) = -( rxt(87) + het_rates(166) )

         mat(17) = -( rxt(406) + rxt(409) + het_rates(142) )

         mat(19) = -( rxt(410) + het_rates(143) )
         mat(18) = rxt(406)

         mat(20) = -( rxt(407) + rxt(411) + het_rates(144) )

         mat(22) = -( rxt(412) + het_rates(145) )
         mat(21) = rxt(407)

         mat(23) = -( rxt(421) + het_rates(167) )

         mat(24) = -( rxt(422) + het_rates(168) )

         mat(25) = -( rxt(423) + het_rates(169) )

         mat(26) = -( rxt(424) + het_rates(170) )

         mat(27) = -( rxt(425) + het_rates(171) )

         mat(28) = -( rxt(426) + het_rates(172) )

         mat(29) = -( rxt(427) + het_rates(173) )


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

         mat(30) = -( rxt(428) + het_rates(174) )


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

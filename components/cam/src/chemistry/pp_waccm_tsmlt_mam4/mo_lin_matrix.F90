




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

         mat(1046) = -( rxt(3) + rxt(4) + het_rates(1) )

         mat(948) = -( rxt(92) + rxt(93) + rxt(94) + rxt(105) + rxt(106) + rxt(107) &
                 + het_rates(2) )
         mat(909) = rxt(1) + 2.000_r8*rxt(2) + rxt(98) + rxt(99) + rxt(100) &
                      + 2.000_r8*rxt(103) + rxt(110) + rxt(111) + rxt(112) &
                      + 2.000_r8*rxt(115)
         mat(1043) = rxt(4)
         mat(1098) = rxt(6)
         mat(1135) = rxt(8)
         mat(107) = rxt(10)
         mat(1277) = rxt(12)
         mat(998) = rxt(21)
         mat(1326) = rxt(24)
         mat(141) = rxt(25)
         mat(1474) = rxt(32)
         mat(559) = rxt(88)
         mat(86) = rxt(89)
         mat(813) = rxt(91)
         mat(1500) = rxt(131)

         mat(1514) = -( rxt(131) + rxt(135)*y(4) + rxt(136)*y(4) + rxt(138)*y(81) &
                      + rxt(139)*y(82) + rxt(140)*y(83) + rxt(141)*y(91) + rxt(142)*y(92) &
                      + rxt(143)*y(84) + rxt(144)*y(89) + rxt(145)*y(90) + rxt(146)*y(85) &
                      + rxt(147)*y(80) + rxt(148)*y(88) + rxt(149)*y(87) + rxt(150)*y(93) &
                      + rxt(151)*y(94) + rxt(152)*y(95) + rxt(153)*y(96) + rxt(156)*y(12) &
                      + rxt(157)*y(12) + rxt(158)*y(12) + het_rates(161) )
         mat(921) = rxt(1)
         mat(1057) = rxt(3)
         mat(1012) = rxt(20)

         mat(908) = -( rxt(1) + rxt(2) + rxt(96) + rxt(98) + rxt(99) + rxt(100) + rxt(103) &
                      + rxt(108) + rxt(110) + rxt(111) + rxt(112) + rxt(115) &
                 + het_rates(3) )
         mat(1042) = rxt(4)
         mat(1276) = rxt(13)
         mat(58) = rxt(126)
         mat(55) = rxt(129) + rxt(130)
         mat(1499) = rxt(136)*y(4)

         mat(57) = -( rxt(123) + rxt(126) + rxt(125)*y(97) + het_rates(159) )

         mat(54) = -( rxt(129) + rxt(130) + het_rates(160) )
         mat(1013) = rxt(3)
         mat(56) = rxt(123) + rxt(125)*y(97)

         mat(655) = -( het_rates(18) )
         mat(967) = rxt(18)
         mat(992) = rxt(20)
         mat(1495) = rxt(158)*y(12)

         mat(607) = -( het_rates(17) )
         mat(966) = rxt(17) + rxt(18)
         mat(611) = rxt(61)
         mat(641) = 1.340_r8*rxt(67)
         mat(740) = .700_r8*rxt(68)
         mat(666) = rxt(74)
         mat(536) = rxt(76)
         mat(516) = rxt(79)
         mat(248) = .450_r8*rxt(81)
         mat(360) = 2.000_r8*rxt(82)
         mat(149) = rxt(90)
         mat(1442) = rxt(254)*y(79)
         mat(318) = rxt(439)*y(97)

         mat(469) = -( rxt(95) + het_rates(5) )
         mat(1076) = rxt(6)
         mat(317) = rxt(436)

         mat(1102) = -( rxt(6) + rxt(7) + het_rates(6) )
         mat(1139) = rxt(8) + .500_r8*rxt(399)
         mat(108) = rxt(10)
         mat(1281) = rxt(13)
         mat(407) = rxt(446)
         mat(1504) = 2.000_r8*rxt(135)*y(4)

         mat(1140) = -( rxt(8) + rxt(399) + het_rates(7) )
         mat(109) = rxt(9) + rxt(197)
         mat(1304) = rxt(11)
         mat(1282) = rxt(12)
         mat(222) = rxt(15) + rxt(206)
         mat(569) = rxt(30)
         mat(260) = rxt(36)
         mat(209) = .600_r8*rxt(64) + rxt(311)
         mat(268) = rxt(65) + rxt(357)
         mat(539) = rxt(76)

         mat(1239) = -( rxt(255)*y(79) + rxt(256)*y(86) + rxt(257)*y(84) + rxt(258)*y(80) &
                      + rxt(260)*y(89) + rxt(261)*y(90) + rxt(262)*y(96) + rxt(263)*y(95) &
                      + rxt(266)*y(12) + het_rates(133) )
         mat(1305) = rxt(11)
         mat(223) = rxt(14)
         mat(186) = rxt(16)
         mat(1004) = rxt(19)
         mat(325) = 2.000_r8*rxt(22)
         mat(511) = rxt(27)
         mat(434) = rxt(33)
         mat(290) = rxt(62)
         mat(231) = rxt(63)
         mat(132) = rxt(69)
         mat(47) = rxt(70)
         mat(168) = rxt(71)
         mat(175) = rxt(72)
         mat(137) = rxt(75)
         mat(349) = rxt(83)
         mat(123) = rxt(84)
         mat(163) = rxt(85)
         mat(203) = rxt(86)
         mat(1141) = .500_r8*rxt(399)
         mat(1506) = rxt(156)*y(12)

         mat(1284) = -( rxt(12) + rxt(13) + rxt(398) + het_rates(8) )
         mat(110) = rxt(9) + rxt(10) + rxt(197)
         mat(224) = rxt(14)
         mat(571) = rxt(29)
         mat(261) = rxt(35)
         mat(211) = .400_r8*rxt(64)

         mat(1307) = -( rxt(11) + het_rates(9) )
         mat(111) = 2.000_r8*rxt(397) + 2.000_r8*rxt(418) + 2.000_r8*rxt(424) &
                      + 2.000_r8*rxt(429)
         mat(1285) = rxt(398)
         mat(1143) = .500_r8*rxt(399)
         mat(572) = rxt(419) + rxt(425) + rxt(430)
         mat(262) = rxt(420) + rxt(428) + rxt(431)

         mat(219) = -( rxt(14) + rxt(15) + rxt(206) + het_rates(10) )

         mat(106) = -( rxt(9) + rxt(10) + rxt(197) + rxt(397) + rxt(418) + rxt(424) &
                      + rxt(429) + het_rates(11) )

         mat(877) = -( het_rates(13) )
         mat(614) = rxt(61)
         mat(228) = rxt(63)
         mat(207) = .400_r8*rxt(64)
         mat(748) = .300_r8*rxt(68)
         mat(381) = rxt(73)
         mat(1498) = rxt(156)*y(12)
         mat(1448) = rxt(213)*y(12)
         mat(440) = rxt(252)*y(12)
         mat(1231) = rxt(266)*y(12)

         mat(182) = -( rxt(16) + het_rates(14) )

         mat(69) = -( het_rates(35) )

         mat(30) = -( het_rates(36) )

         mat(973) = -( rxt(17) + rxt(18) + het_rates(16) )
         mat(184) = rxt(16)
         mat(288) = rxt(62)
         mat(647) = 1.340_r8*rxt(66)
         mat(173) = rxt(72)
         mat(538) = rxt(76)
         mat(277) = .690_r8*rxt(77)
         mat(624) = rxt(78)
         mat(517) = rxt(79)
         mat(348) = .100_r8*rxt(83)
         mat(178) = rxt(280)
         mat(196) = 2.000_r8*rxt(292)
         mat(1501) = rxt(157)*y(12) + rxt(158)*y(12)

         mat(1356) = -( het_rates(19) )
         mat(187) = rxt(16)
         mat(982) = 2.000_r8*rxt(17)
         mat(1008) = rxt(19) + 2.000_r8*rxt(21)
         mat(838) = rxt(28)
         mat(462) = rxt(34)
         mat(78) = rxt(57)
         mat(1510) = rxt(157)*y(12)

         mat(1426) = -( rxt(400) + het_rates(134) )
         mat(225) = rxt(15) + rxt(206)
         mat(620) = rxt(61)
         mat(291) = rxt(62)
         mat(652) = 1.340_r8*rxt(66) + .660_r8*rxt(67)
         mat(133) = rxt(69)
         mat(169) = rxt(71)
         mat(674) = rxt(74)
         mat(542) = rxt(76)
         mat(279) = rxt(77)
         mat(626) = rxt(78)
         mat(519) = 2.000_r8*rxt(79)
         mat(251) = .560_r8*rxt(81)
         mat(362) = 2.000_r8*rxt(82)
         mat(350) = .900_r8*rxt(83)
         mat(204) = rxt(86)
         mat(181) = rxt(280)
         mat(197) = rxt(292)
         mat(1511) = rxt(157)*y(12)
         mat(1461) = rxt(254)*y(79) + rxt(259)*y(80)
         mat(1244) = rxt(255)*y(79) + rxt(258)*y(80)

         mat(321) = -( rxt(22) + het_rates(20) )
         mat(1381) = .500_r8*rxt(400)

         mat(1000) = -( rxt(19) + rxt(20) + rxt(21) + het_rates(162) )
         mat(53) = rxt(87)
         mat(1235) = rxt(255)*y(79) + rxt(256)*y(86) + rxt(257)*y(84) + rxt(258)*y(80) &
                      + rxt(262)*y(96) + rxt(266)*y(12)

         mat(1462) = -( rxt(213)*y(12) + rxt(254)*y(79) + rxt(259)*y(80) + rxt(264)*y(96) &
                      + rxt(265)*y(95) + het_rates(131) )
         mat(60) = 2.000_r8*rxt(23)
         mat(1338) = rxt(24)
         mat(23) = 2.000_r8*rxt(26)
         mat(513) = rxt(27)
         mat(839) = rxt(28)
         mat(574) = rxt(29)
         mat(75) = rxt(31)
         mat(67) = rxt(56)
         mat(1512) = 2.000_r8*rxt(138)*y(81) + 2.000_r8*rxt(139)*y(82) &
                      + 2.000_r8*rxt(140)*y(83) + 2.000_r8*rxt(141)*y(91) + rxt(142)*y(92) &
                      + rxt(143)*y(84) + rxt(144)*y(89) + rxt(145)*y(90) &
                      + 4.000_r8*rxt(146)*y(85) + rxt(148)*y(88)
         mat(1245) = rxt(255)*y(79) + 3.000_r8*rxt(256)*y(86) + rxt(257)*y(84) &
                      + rxt(260)*y(89) + rxt(261)*y(90)

         mat(59) = -( rxt(23) + het_rates(23) )

         mat(1335) = -( rxt(24) + het_rates(24) )
         mat(142) = rxt(25)
         mat(573) = rxt(30)
         mat(22) = 2.000_r8*rxt(225)

         mat(138) = -( rxt(25) + het_rates(25) )

         mat(21) = -( rxt(26) + rxt(225) + het_rates(26) )

         mat(829) = -( rxt(28) + het_rates(27) )
         mat(1446) = rxt(213)*y(12) + 2.000_r8*rxt(254)*y(79) + rxt(259)*y(80) &
                      + rxt(264)*y(96) + rxt(265)*y(95)


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

         mat(507) = -( rxt(27) + het_rates(28) )
         mat(564) = rxt(419) + rxt(425) + rxt(430)

         mat(565) = -( rxt(29) + rxt(30) + rxt(419) + rxt(425) + rxt(430) + het_rates(29) &
       )

         mat(73) = -( rxt(31) + het_rates(30) )

         mat(844) = -( het_rates(132) )
         mat(74) = rxt(31)
         mat(1472) = rxt(32)
         mat(431) = rxt(33)
         mat(458) = rxt(34)
         mat(258) = rxt(35)
         mat(1497) = rxt(147)*y(80) + rxt(148)*y(88) + rxt(149)*y(87) &
                      + 2.000_r8*rxt(150)*y(93) + 2.000_r8*rxt(151)*y(94) &
                      + 3.000_r8*rxt(152)*y(95) + 2.000_r8*rxt(153)*y(96)
         mat(1230) = rxt(258)*y(80) + 2.000_r8*rxt(262)*y(96) + 3.000_r8*rxt(263)*y(95)
         mat(1447) = rxt(259)*y(80) + 2.000_r8*rxt(264)*y(96) + 3.000_r8*rxt(265)*y(95)

         mat(1487) = -( rxt(32) + het_rates(31) )
         mat(263) = rxt(36)

         mat(457) = -( rxt(34) + het_rates(32) )

         mat(429) = -( rxt(33) + het_rates(33) )
         mat(257) = rxt(420) + rxt(428) + rxt(431)

         mat(256) = -( rxt(35) + rxt(36) + rxt(420) + rxt(428) + rxt(431) + het_rates(34) &
       )

         mat(330) = -( het_rates(152) )

         mat(401) = -( rxt(446) + het_rates(153) )
         mat(899) = rxt(96) + rxt(108)
         mat(315) = rxt(439)*y(97)

         mat(212) = -( het_rates(154) )
         mat(464) = rxt(95)

         mat(314) = -( rxt(436) + rxt(439)*y(97) + het_rates(155) )
         mat(928) = rxt(92) + rxt(93) + rxt(94) + rxt(105) + rxt(106) + rxt(107)
         mat(896) = rxt(98) + rxt(99) + rxt(100) + rxt(110) + rxt(111) + rxt(112)

         mat(410) = -( het_rates(156) )
         mat(1073) = rxt(7)
         mat(316) = rxt(436)
         mat(402) = rxt(446)

         mat(232) = -( het_rates(158) )

         mat(421) = -( het_rates(157) )
         mat(1074) = rxt(7)
         mat(934) = rxt(92) + rxt(93) + rxt(94) + rxt(105) + rxt(106) + rxt(107)
         mat(468) = rxt(95)
         mat(901) = rxt(96) + rxt(98) + rxt(99) + rxt(100) + rxt(108) + rxt(110) &
                      + rxt(111) + rxt(112)

         mat(592) = -( het_rates(48) )
         mat(739) = .700_r8*rxt(68)

         mat(491) = -( het_rates(65) )

         mat(447) = -( het_rates(141) )

         mat(612) = -( rxt(61) + het_rates(41) )
         mat(286) = rxt(62)
         mat(131) = rxt(69)
         mat(346) = .400_r8*rxt(83)
         mat(121) = rxt(84)

         mat(310) = -( het_rates(40) )

         mat(284) = -( rxt(62) + het_rates(52) )

         mat(794) = -( het_rates(137) )
         mat(206) = .600_r8*rxt(64) + rxt(311)
         mat(646) = 1.340_r8*rxt(66)
         mat(747) = .300_r8*rxt(68)
         mat(172) = rxt(72)
         mat(380) = rxt(73)
         mat(668) = rxt(74)
         mat(623) = rxt(78)
         mat(191) = rxt(80)
         mat(250) = .130_r8*rxt(81)
         mat(122) = rxt(84)

         mat(226) = -( rxt(63) + het_rates(45) )

         mat(205) = -( rxt(64) + rxt(311) + het_rates(47) )

         mat(154) = -( het_rates(64) )

         mat(88) = -( het_rates(38) )

         mat(293) = -( het_rates(37) )

         mat(24) = -( het_rates(57) )

         mat(264) = -( rxt(65) + rxt(357) + het_rates(63) )

         mat(27) = -( het_rates(56) )

         mat(112) = -( het_rates(143) )

         mat(367) = -( het_rates(147) )

         mat(341) = -( rxt(83) + het_rates(66) )

         mat(188) = -( rxt(80) + het_rates(58) )
         mat(340) = .800_r8*rxt(83)

         mat(352) = -( het_rates(144) )

         mat(119) = -( rxt(84) + het_rates(59) )

         mat(37) = -( het_rates(73) )

         mat(42) = -( het_rates(74) )

         mat(238) = -( het_rates(150) )

         mat(158) = -( rxt(85) + het_rates(75) )

         mat(61) = -( het_rates(76) )

         mat(545) = -( het_rates(151) )

         mat(198) = -( rxt(86) + het_rates(78) )

         mat(246) = -( rxt(81) + het_rates(67) )
         mat(160) = .900_r8*rxt(85)

         mat(359) = -( rxt(82) + het_rates(44) )
         mat(247) = .130_r8*rxt(81)
         mat(161) = .450_r8*rxt(85)

         mat(702) = -( het_rates(148) )

         mat(745) = -( rxt(68) + het_rates(60) )
         mat(276) = .402_r8*rxt(77)
         mat(202) = rxt(86)

         mat(642) = -( rxt(66) + rxt(67) + het_rates(61) )
         mat(273) = .288_r8*rxt(77)
         mat(201) = rxt(86)

         mat(726) = -( het_rates(146) )

         mat(124) = -( het_rates(62) )

         mat(765) = -( het_rates(145) )
         mat(266) = rxt(65) + rxt(357)
         mat(645) = .660_r8*rxt(66)

         mat(481) = -( het_rates(136) )
         mat(190) = rxt(80)

         mat(129) = -( rxt(69) + het_rates(39) )

         mat(301) = -( het_rates(77) )

         mat(33) = -( het_rates(49) )

         mat(522) = -( het_rates(140) )

         mat(164) = -( rxt(71) + het_rates(50) )

         mat(378) = -( rxt(73) + het_rates(51) )
         mat(165) = .820_r8*rxt(71)
         mat(344) = .250_r8*rxt(83)
         mat(199) = .100_r8*rxt(86)

         mat(170) = -( rxt(72) + het_rates(55) )

         mat(252) = -( het_rates(15) )

         mat(79) = -( het_rates(42) )

         mat(515) = -( rxt(79) + het_rates(43) )

         mat(621) = -( rxt(78) + het_rates(53) )

         mat(393) = -( het_rates(138) )

         mat(193) = -( rxt(292) + het_rates(139) )
         mat(46) = rxt(70)

         mat(45) = -( rxt(70) + het_rates(46) )

         mat(143) = -( het_rates(68) )

         mat(630) = -( het_rates(142) )

         mat(667) = -( rxt(74) + het_rates(54) )
         mat(249) = .180_r8*rxt(81)
         mat(162) = .450_r8*rxt(85)

         mat(577) = -( het_rates(69) )

         mat(535) = -( rxt(76) + het_rates(70) )

         mat(682) = -( het_rates(149) )

         mat(134) = -( rxt(75) + het_rates(71) )

         mat(272) = -( rxt(77) + het_rates(72) )

         mat(94) = -( het_rates(98) )

         mat(280) = -( het_rates(99) )

         mat(176) = -( rxt(280) + het_rates(135) )

         mat(48) = -( rxt(55) + het_rates(100) )
         mat(1489) = rxt(139)*y(82) + rxt(140)*y(83) + 2.000_r8*rxt(141)*y(91) &
                      + 2.000_r8*rxt(142)*y(92) + rxt(143)*y(84) + rxt(145)*y(90) &
                      + rxt(148)*y(88) + rxt(149)*y(87) + rxt(150)*y(93) &
                      + 2.000_r8*rxt(151)*y(94)
         mat(1156) = rxt(257)*y(84) + rxt(261)*y(90)

         mat(65) = -( rxt(56) + het_rates(101) )
         mat(1491) = rxt(138)*y(81) + rxt(140)*y(83) + rxt(144)*y(89)
         mat(1158) = rxt(260)*y(89)

         mat(76) = -( rxt(57) + het_rates(102) )
         mat(437) = rxt(252)*y(12)

         mat(438) = -( rxt(252)*y(12) + het_rates(103) )
         mat(49) = 2.000_r8*rxt(55)
         mat(66) = rxt(56)
         mat(77) = rxt(57)
         mat(1493) = rxt(142)*y(92) + rxt(149)*y(87)

         mat(557) = -( rxt(88) + het_rates(104) )
         mat(85) = rxt(89)

         mat(100) = -( het_rates(105) )

         mat(146) = -( rxt(90) + het_rates(106) )

         mat(384) = -( het_rates(107) )
         mat(147) = rxt(90)
         mat(808) = rxt(91)

         mat(810) = -( rxt(91) + het_rates(108) )
         mat(558) = rxt(88)

         mat(84) = -( rxt(89) + het_rates(109) )
         mat(52) = rxt(87)

         mat(51) = -( rxt(87) + het_rates(110) )

         mat(1) = -( het_rates(111) )

         mat(2) = -( het_rates(112) )

         mat(3) = -( het_rates(113) )

         mat(4) = -( het_rates(114) )

         mat(5) = -( het_rates(115) )

         mat(6) = -( het_rates(116) )

         mat(7) = -( het_rates(117) )

         mat(8) = -( het_rates(118) )

         mat(9) = -( het_rates(119) )

         mat(10) = -( het_rates(120) )

         mat(11) = -( het_rates(121) )

         mat(12) = -( het_rates(123) )

         mat(13) = -( het_rates(122) )

         mat(14) = -( het_rates(124) )

         mat(15) = -( het_rates(125) )

         mat(16) = -( het_rates(126) )

         mat(17) = -( het_rates(127) )

         mat(18) = -( het_rates(128) )

         mat(19) = -( het_rates(129) )

         mat(20) = -( het_rates(130) )


      end subroutine linmat02

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

      end subroutine linmat

      end module mo_lin_matrix

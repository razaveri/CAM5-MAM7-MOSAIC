




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

         mat(1005) = -( rxt(3) + rxt(4) + het_rates(1) )
         mat(423) = rxt(96)

         mat(1063) = -( rxt(67) + rxt(68) + rxt(69) + rxt(80) + rxt(81) + rxt(82) &
                 + het_rates(2) )
         mat(1150) = rxt(1) + 2.000_r8*rxt(2) + rxt(73) + rxt(74) + rxt(75) &
                      + 2.000_r8*rxt(78) + rxt(85) + rxt(86) + rxt(87) + 2.000_r8*rxt(90)
         mat(1006) = rxt(4)
         mat(1421) = rxt(6)
         mat(1466) = rxt(8)
         mat(283) = rxt(10)
         mat(1548) = rxt(12)
         mat(1208) = rxt(21)
         mat(1808) = rxt(24)
         mat(67) = rxt(25)
         mat(474) = rxt(32)
         mat(254) = rxt(62)
         mat(56) = rxt(63)
         mat(365) = rxt(65)
         mat(966) = rxt(94) + rxt(367)
         mat(623) = rxt(118)

         mat(617) = -( rxt(118) + rxt(122)*y(4) + rxt(123)*y(4) + rxt(125)*y(37) &
                      + rxt(126)*y(38) + rxt(127)*y(39) + rxt(128)*y(47) + rxt(129)*y(48) &
                      + rxt(130)*y(40) + rxt(131)*y(45) + rxt(132)*y(46) + rxt(133)*y(41) &
                      + rxt(134)*y(36) + rxt(135)*y(44) + rxt(136)*y(43) + rxt(137)*y(49) &
                      + rxt(138)*y(50) + rxt(139)*y(51) + rxt(140)*y(52) + rxt(143)*y(12) &
                      + rxt(144)*y(12) + rxt(145)*y(12) + het_rates(134) )
         mat(1138) = rxt(1)
         mat(994) = rxt(3)
         mat(1197) = rxt(20)

         mat(1152) = -( rxt(1) + rxt(2) + rxt(71) + rxt(73) + rxt(74) + rxt(75) + rxt(78) &
                      + rxt(83) + rxt(85) + rxt(86) + rxt(87) + rxt(90) + het_rates(3) )
         mat(1008) = rxt(4)
         mat(1550) = rxt(13)
         mat(1585) = rxt(95) + rxt(371)
         mat(425) = rxt(100) + rxt(398)*y(57)
         mat(93) = rxt(101) + rxt(404)*y(57)
         mat(44) = rxt(113)
         mat(246) = rxt(116) + rxt(117)
         mat(624) = rxt(123)*y(4)

         mat(42) = -( rxt(110) + rxt(113) + rxt(112)*y(57) + het_rates(132) )

         mat(241) = -( rxt(116) + rxt(117) + het_rates(133) )
         mat(987) = rxt(3)
         mat(43) = rxt(110) + rxt(112)*y(57)

         mat(563) = -( het_rates(17) )
         mat(435) = rxt(18)
         mat(1195) = rxt(20)
         mat(615) = rxt(145)*y(12)

         mat(133) = -( het_rates(16) )
         mat(432) = rxt(17) + rxt(18)
         mat(70) = rxt(64)
         mat(1270) = rxt(240)*y(35)
         mat(198) = rxt(303)*y(57)

         mat(292) = -( rxt(70) + het_rates(5) )
         mat(1400) = rxt(6)
         mat(200) = rxt(300)

         mat(1429) = -( rxt(6) + rxt(7) + het_rates(6) )
         mat(1474) = rxt(8) + .500_r8*rxt(263)
         mat(286) = rxt(10)
         mat(1556) = rxt(13)
         mat(196) = rxt(66)
         mat(1710) = rxt(310)
         mat(628) = 2.000_r8*rxt(122)*y(4)

         mat(1475) = -( rxt(8) + rxt(263) + het_rates(7) )
         mat(287) = rxt(9) + rxt(183)
         mat(1631) = rxt(11)
         mat(1557) = rxt(12)
         mat(126) = rxt(15) + rxt(192)
         mat(264) = rxt(30)
         mat(142) = rxt(36)
         mat(1772) = rxt(98)

         mat(774) = -( rxt(241)*y(35) + rxt(242)*y(42) + rxt(243)*y(40) + rxt(244)*y(36) &
                      + rxt(246)*y(45) + rxt(247)*y(46) + rxt(248)*y(52) + rxt(249)*y(51) &
                      + rxt(252)*y(12) + het_rates(87) )
         mat(1615) = rxt(11)
         mat(123) = rxt(14)
         mat(98) = rxt(16)
         mat(1201) = rxt(19)
         mat(147) = 2.000_r8*rxt(22)
         mat(235) = rxt(27)
         mat(212) = rxt(33)
         mat(194) = rxt(66)
         mat(1727) = rxt(97)
         mat(1459) = .500_r8*rxt(263)
         mat(619) = rxt(143)*y(12)

         mat(1559) = -( rxt(12) + rxt(13) + rxt(262) + het_rates(8) )
         mat(288) = rxt(9) + rxt(10) + rxt(183)
         mat(127) = rxt(14)
         mat(265) = rxt(29)
         mat(143) = rxt(35)
         mat(743) = rxt(99)

         mat(193) = -( rxt(66) + het_rates(20) )

         mat(1635) = -( rxt(11) + het_rates(9) )
         mat(289) = 2.000_r8*rxt(261) + 2.000_r8*rxt(282) + 2.000_r8*rxt(288) &
                      + 2.000_r8*rxt(293)
         mat(1561) = rxt(262)
         mat(1479) = .500_r8*rxt(263)
         mat(266) = rxt(283) + rxt(289) + rxt(294)
         mat(144) = rxt(284) + rxt(292) + rxt(295)
         mat(497) = rxt(462)

         mat(121) = -( rxt(14) + rxt(15) + rxt(192) + het_rates(10) )

         mat(278) = -( rxt(9) + rxt(10) + rxt(183) + rxt(261) + rxt(282) + rxt(288) &
                      + rxt(293) + het_rates(11) )

         mat(304) = -( het_rates(13) )
         mat(612) = rxt(143)*y(12)
         mat(1275) = rxt(199)*y(12)
         mat(219) = rxt(238)*y(12)
         mat(766) = rxt(252)*y(12)

         mat(95) = -( rxt(16) + het_rates(14) )

         mat(434) = -( rxt(17) + rxt(18) + het_rates(15) )
         mat(97) = rxt(16)
         mat(614) = rxt(144)*y(12) + rxt(145)*y(12)

         mat(1672) = -( het_rates(18) )
         mat(100) = rxt(16)
         mat(446) = 2.000_r8*rxt(17)
         mat(1222) = rxt(19) + 2.000_r8*rxt(21)
         mat(1393) = rxt(28)
         mat(232) = rxt(34)
         mat(41) = rxt(57)
         mat(634) = rxt(144)*y(12)

         mat(702) = -( rxt(264) + het_rates(88) )
         mat(122) = rxt(15) + rxt(192)
         mat(618) = rxt(144)*y(12)
         mat(1282) = rxt(240)*y(35) + rxt(245)*y(36)
         mat(773) = rxt(241)*y(35) + rxt(244)*y(36)

         mat(145) = -( rxt(22) + het_rates(19) )
         mat(691) = .500_r8*rxt(264)

         mat(1211) = -( rxt(19) + rxt(20) + rxt(21) + het_rates(135) )
         mat(29) = rxt(61)
         mat(189) = rxt(93)
         mat(510) = rxt(104) + rxt(452)
         mat(155) = rxt(341)
         mat(1511) = rxt(343)
         mat(1253) = rxt(345)
         mat(934) = rxt(347)
         mat(395) = rxt(445)
         mat(549) = rxt(449)
         mat(349) = rxt(455)
         mat(409) = rxt(457)
         mat(529) = rxt(460)
         mat(781) = rxt(241)*y(35) + rxt(242)*y(42) + rxt(243)*y(40) + rxt(244)*y(36) &
                      + rxt(248)*y(52) + rxt(252)*y(12)

         mat(1296) = -( rxt(199)*y(12) + rxt(240)*y(35) + rxt(245)*y(36) + rxt(250)*y(52) &
                      + rxt(251)*y(51) + het_rates(85) )
         mat(31) = 2.000_r8*rxt(23)
         mat(1813) = rxt(24)
         mat(22) = 2.000_r8*rxt(26)
         mat(238) = rxt(27)
         mat(1384) = rxt(28)
         mat(262) = rxt(29)
         mat(38) = rxt(31)
         mat(35) = rxt(56)
         mat(626) = 2.000_r8*rxt(125)*y(37) + 2.000_r8*rxt(126)*y(38) &
                      + 2.000_r8*rxt(127)*y(39) + 2.000_r8*rxt(128)*y(47) + rxt(129)*y(48) &
                      + rxt(130)*y(40) + rxt(131)*y(45) + rxt(132)*y(46) &
                      + 4.000_r8*rxt(133)*y(41) + rxt(135)*y(44)
         mat(782) = rxt(241)*y(35) + 3.000_r8*rxt(242)*y(42) + rxt(243)*y(40) &
                      + rxt(246)*y(45) + rxt(247)*y(46)

         mat(30) = -( rxt(23) + het_rates(23) )

         mat(1826) = -( rxt(24) + het_rates(24) )
         mat(68) = rxt(25)
         mat(267) = rxt(30)
         mat(23) = 2.000_r8*rxt(211)

         mat(64) = -( rxt(25) + het_rates(25) )

         mat(21) = -( rxt(26) + rxt(211) + het_rates(26) )

         mat(1386) = -( rxt(28) + het_rates(27) )
         mat(383) = rxt(446)
         mat(1298) = rxt(199)*y(12) + 2.000_r8*rxt(240)*y(35) + rxt(245)*y(36) &
                      + rxt(250)*y(52) + rxt(251)*y(51)

         mat(234) = -( rxt(27) + het_rates(28) )
         mat(257) = rxt(283) + rxt(289) + rxt(294)

         mat(258) = -( rxt(29) + rxt(30) + rxt(283) + rxt(289) + rxt(294) + het_rates(29) &
       )

         mat(36) = -( rxt(31) + het_rates(30) )

         mat(587) = -( het_rates(86) )
         mat(37) = rxt(31)
         mat(470) = rxt(32)
         mat(211) = rxt(33)
         mat(227) = rxt(34)
         mat(140) = rxt(35)
         mat(616) = rxt(134)*y(36) + rxt(135)*y(44) + rxt(136)*y(43) &
                      + 2.000_r8*rxt(137)*y(49) + 2.000_r8*rxt(138)*y(50) &
                      + 3.000_r8*rxt(139)*y(51) + 2.000_r8*rxt(140)*y(52)
         mat(771) = rxt(244)*y(36) + 2.000_r8*rxt(248)*y(52) + 3.000_r8*rxt(249)*y(51)
         mat(1278) = rxt(245)*y(36) + 2.000_r8*rxt(250)*y(52) + 3.000_r8*rxt(251)*y(51)

         mat(469) = -( rxt(32) + het_rates(31) )
         mat(139) = rxt(36)

         mat(226) = -( rxt(34) + het_rates(32) )

         mat(209) = -( rxt(33) + het_rates(33) )
         mat(138) = rxt(284) + rxt(292) + rxt(295)

         mat(137) = -( rxt(35) + rxt(36) + rxt(284) + rxt(292) + rxt(295) + het_rates(34) &
       )

         mat(159) = -( het_rates(89) )

         mat(1717) = -( rxt(310) + het_rates(90) )
         mat(1165) = rxt(71) + rxt(83)
         mat(192) = rxt(93)
         mat(207) = rxt(303)*y(57)

         mat(101) = -( het_rates(127) )
         mat(290) = rxt(70)

         mat(199) = -( rxt(300) + rxt(303)*y(57) + het_rates(128) )
         mat(1035) = rxt(67) + rxt(68) + rxt(69) + rxt(80) + rxt(81) + rxt(82)
         mat(1129) = rxt(73) + rxt(74) + rxt(75) + rxt(85) + rxt(86) + rxt(87)


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

         mat(1342) = -( rxt(362) + rxt(360)*y(57) + het_rates(129) )
         mat(1427) = rxt(7)
         mat(205) = rxt(300)
         mat(1708) = rxt(310)
         mat(119) = rxt(361)
         mat(113) = rxt(363)

         mat(128) = -( het_rates(131) )

         mat(330) = -( het_rates(91) )

         mat(185) = -( rxt(93) + het_rates(92) )

         mat(177) = -( het_rates(93) )
         mat(153) = rxt(341)

         mat(152) = -( rxt(341) + het_rates(94) )
         mat(1485) = rxt(343)

         mat(1518) = -( rxt(343) + het_rates(95) )
         mat(1260) = rxt(345)

         mat(1254) = -( rxt(345) + het_rates(96) )
         mat(935) = rxt(347)

         mat(928) = -( rxt(347) + het_rates(97) )

         mat(77) = -( het_rates(98) )

         mat(45) = -( het_rates(99) )

         mat(49) = -( het_rates(100) )

         mat(1105) = -( het_rates(101) )

         mat(885) = -( het_rates(102) )

         mat(83) = -( het_rates(103) )

         mat(115) = -( rxt(361) + het_rates(104) )
         mat(109) = rxt(330)*y(57)
         mat(1311) = rxt(360)*y(57)

         mat(108) = -( rxt(363) + rxt(330)*y(57) + het_rates(105) )
         mat(1310) = rxt(362)

         mat(964) = -( rxt(94) + rxt(367) + rxt(381)*y(57) + het_rates(106) )
         mat(422) = rxt(100)
         mat(806) = rxt(102)

         mat(1595) = -( rxt(95) + rxt(371) + rxt(389)*y(57) + het_rates(107) )
         mat(94) = rxt(101)
         mat(662) = rxt(103)

         mat(418) = -( rxt(96) + rxt(100) + rxt(398)*y(57) + het_rates(108) )

         mat(89) = -( rxt(101) + rxt(404)*y(57) + het_rates(109) )

         mat(802) = -( rxt(102) + het_rates(111) )
         mat(504) = rxt(104) + rxt(452)
         mat(960) = rxt(381)*y(57)
         mat(420) = rxt(398)*y(57)

         mat(640) = -( rxt(103) + het_rates(112) )
         mat(1572) = rxt(389)*y(57)
         mat(91) = rxt(404)*y(57)

         mat(1780) = -( rxt(98) + het_rates(113) )
         mat(417) = rxt(457)

         mat(727) = -( rxt(99) + het_rates(114) )
         mat(543) = rxt(449)
         mat(486) = rxt(462)

         mat(1750) = -( rxt(97) + rxt(407)*y(57) + het_rates(110) )

         mat(315) = -( het_rates(122) )
         mat(1722) = rxt(407)*y(57)

         mat(501) = -( rxt(104) + rxt(452) + het_rates(115) )
         mat(343) = rxt(455)

         mat(342) = -( rxt(455) + het_rates(116) )

         mat(404) = -( rxt(457) + het_rates(117) )

         mat(542) = -( rxt(449) + het_rates(118) )
         mat(522) = rxt(460)

         mat(521) = -( rxt(460) + het_rates(119) )

         mat(485) = -( rxt(462) + het_rates(120) )

         mat(447) = -( het_rates(121) )

         mat(670) = -( het_rates(123) )
         mat(390) = rxt(445)
         mat(374) = rxt(446)

         mat(268) = -( het_rates(124) )

         mat(389) = -( rxt(445) + het_rates(125) )

         mat(373) = -( rxt(446) + het_rates(126) )

         mat(843) = -( het_rates(130) )
         mat(1416) = rxt(7)
         mat(1058) = rxt(67) + rxt(68) + rxt(69) + rxt(80) + rxt(81) + rxt(82)
         mat(295) = rxt(70)
         mat(1145) = rxt(71) + rxt(73) + rxt(74) + rxt(75) + rxt(83) + rxt(85) + rxt(86) &
                      + rxt(87)
         mat(961) = rxt(94) + rxt(367)
         mat(1578) = rxt(95) + rxt(371)
         mat(421) = rxt(96)
         mat(1729) = rxt(97)
         mat(1758) = rxt(98)
         mat(728) = rxt(99)

         mat(24) = -( rxt(55) + het_rates(53) )
         mat(606) = rxt(126)*y(38) + rxt(127)*y(39) + 2.000_r8*rxt(128)*y(47) &
                      + 2.000_r8*rxt(129)*y(48) + rxt(130)*y(40) + rxt(132)*y(46) &
                      + rxt(135)*y(44) + rxt(136)*y(43) + rxt(137)*y(49) &
                      + 2.000_r8*rxt(138)*y(50)
         mat(748) = rxt(243)*y(40) + rxt(247)*y(46)

         mat(32) = -( rxt(56) + het_rates(54) )
         mat(607) = rxt(125)*y(37) + rxt(127)*y(39) + rxt(131)*y(45)
         mat(749) = rxt(246)*y(45)

         mat(39) = -( rxt(57) + het_rates(55) )
         mat(217) = rxt(238)*y(12)

         mat(218) = -( rxt(238)*y(12) + het_rates(56) )
         mat(25) = 2.000_r8*rxt(55)
         mat(33) = rxt(56)
         mat(40) = rxt(57)
         mat(609) = rxt(129)*y(48) + rxt(136)*y(43)

         mat(69) = -( rxt(64) + het_rates(58) )

         mat(169) = -( het_rates(59) )
         mat(71) = rxt(64)
         mat(357) = rxt(65)

         mat(359) = -( rxt(65) + het_rates(60) )
         mat(251) = rxt(62)

         mat(250) = -( rxt(62) + het_rates(61) )
         mat(55) = rxt(63)

         mat(54) = -( rxt(63) + het_rates(62) )
         mat(28) = rxt(61)

         mat(27) = -( rxt(61) + het_rates(63) )

         mat(58) = -( het_rates(64) )

         mat(1) = -( het_rates(65) )

         mat(2) = -( het_rates(66) )

         mat(3) = -( het_rates(67) )

         mat(4) = -( het_rates(68) )

         mat(5) = -( het_rates(69) )

         mat(6) = -( het_rates(70) )

         mat(7) = -( het_rates(71) )

         mat(8) = -( het_rates(72) )

         mat(9) = -( het_rates(73) )

         mat(10) = -( het_rates(74) )

         mat(11) = -( het_rates(75) )

         mat(12) = -( het_rates(76) )

         mat(13) = -( het_rates(77) )

         mat(14) = -( het_rates(78) )

         mat(15) = -( het_rates(79) )

         mat(16) = -( het_rates(80) )

         mat(17) = -( het_rates(81) )

         mat(18) = -( het_rates(82) )

         mat(19) = -( het_rates(83) )

         mat(20) = -( het_rates(84) )


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

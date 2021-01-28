




      module mo_nln_matrix

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: nlnmat

      contains

      subroutine nlnmat01( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------


         mat(497) = -(rxt(93)*y(2) + rxt(111)*y(93) + rxt(137)*y(18) + rxt(142)*y(82) &
                      + rxt(150)*y(83) + rxt(163)*y(6) + rxt(166)*y(7) + rxt(178) &
                      *y(80) + rxt(205)*y(81) + rxt(254)*y(58) + rxt(257)*y(59))
         mat(390) = -rxt(93)*y(1)
         mat(349) = -rxt(111)*y(1)
         mat(273) = -rxt(137)*y(1)
         mat(633) = -rxt(142)*y(1)
         mat(446) = -rxt(150)*y(1)
         mat(569) = -rxt(163)*y(1)
         mat(596) = -rxt(166)*y(1)
         mat(682) = -rxt(178)*y(1)
         mat(703) = -rxt(205)*y(1)
         mat(133) = -rxt(254)*y(1)
         mat(261) = -rxt(257)*y(1)

         mat(390) = mat(390) + rxt(92)*y(3)
         mat(421) = rxt(92)*y(2)

         mat(386) = -(rxt(92)*y(3) + rxt(93)*y(1) + 4._r8*rxt(94)*y(2) + rxt(141) &
                      *y(82) + rxt(148)*y(17) + rxt(149)*y(83) + rxt(152)*y(19) &
                      + rxt(161)*y(6) + (rxt(164) + rxt(165)) * y(7) + rxt(172)*y(8) &
                      + rxt(185)*y(23) + rxt(198)*y(26) + rxt(199)*y(27) + rxt(202) &
                      *y(28) + rxt(208)*y(30) + rxt(218)*y(31) + rxt(219)*y(32) &
                      + rxt(220)*y(33) + rxt(242)*y(15) + rxt(250)*y(57) + (rxt(286) &
                      + rxt(287)) * y(84) + rxt(293)*y(86))
         mat(417) = -rxt(92)*y(2)
         mat(493) = -rxt(93)*y(2)
         mat(629) = -rxt(141)*y(2)
         mat(325) = -rxt(148)*y(2)
         mat(442) = -rxt(149)*y(2)
         mat(111) = -rxt(152)*y(2)
         mat(565) = -rxt(161)*y(2)
         mat(592) = -(rxt(164) + rxt(165)) * y(2)
         mat(654) = -rxt(172)*y(2)
         mat(470) = -rxt(185)*y(2)
         mat(517) = -rxt(198)*y(2)
         mat(216) = -rxt(199)*y(2)
         mat(233) = -rxt(202)*y(2)
         mat(305) = -rxt(208)*y(2)
         mat(196) = -rxt(218)*y(2)
         mat(187) = -rxt(219)*y(2)
         mat(102) = -rxt(220)*y(2)
         mat(283) = -rxt(242)*y(2)
         mat(71) = -rxt(250)*y(2)
         mat(125) = -(rxt(286) + rxt(287)) * y(2)
         mat(84) = -rxt(293)*y(2)

         mat(346) = (rxt(106)+rxt(107))*y(3)
         mat(417) = mat(417) + (rxt(106)+rxt(107))*y(93) + rxt(156)*y(5) + rxt(292) &
                      *y(86) + rxt(284)*y(87) + rxt(253)*y(58) + rxt(256)*y(59)
         mat(208) = rxt(156)*y(3) + rxt(157)*y(6) + rxt(158)*y(7) + rxt(289)*y(85)
         mat(565) = mat(565) + rxt(157)*y(5)
         mat(592) = mat(592) + rxt(158)*y(5)
         mat(629) = mat(629) + 2.000_r8*rxt(144)*y(82)
         mat(270) = rxt(140)*y(83)
         mat(442) = mat(442) + rxt(140)*y(18)
         mat(159) = rxt(289)*y(5) + 1.150_r8*rxt(297)*y(89)
         mat(84) = mat(84) + rxt(292)*y(3)
         mat(141) = rxt(284)*y(3)
         mat(167) = rxt(296)*y(89)
         mat(179) = 1.150_r8*rxt(297)*y(85) + rxt(296)*y(88)
         mat(131) = rxt(253)*y(3)
         mat(257) = rxt(256)*y(3)

         mat(345) = -((rxt(106) + rxt(107)) * y(3) + rxt(108)*y(94) + rxt(111)*y(1) &
                      + rxt(128)*y(52) + rxt(129)*y(53) + rxt(133)*y(17) + rxt(134) &
                      *y(26) + rxt(135)*y(31))
         mat(416) = -(rxt(106) + rxt(107)) * y(93)
         mat(541) = -rxt(108)*y(93)
         mat(492) = -rxt(111)*y(93)
         mat(22) = -rxt(128)*y(93)
         mat(35) = -rxt(129)*y(93)
         mat(324) = -rxt(133)*y(93)
         mat(516) = -rxt(134)*y(93)
         mat(195) = -rxt(135)*y(93)

         mat(416) = mat(416) + rxt(153)*y(90)
         mat(158) = .850_r8*rxt(297)*y(89)
         mat(95) = rxt(153)*y(3)
         mat(178) = .850_r8*rxt(297)*y(85)

         mat(418) = -(rxt(92)*y(2) + rxt(102)*y(92) + rxt(106)*y(93) + rxt(136)*y(18) &
                      + rxt(153)*y(90) + rxt(156)*y(5) + rxt(253)*y(58) + rxt(256) &
                      *y(59) + rxt(284)*y(87) + (rxt(291) + rxt(292)) * y(86) + rxt(294) &
                      *y(84))
         mat(387) = -rxt(92)*y(3)
         mat(27) = -rxt(102)*y(3)
         mat(347) = -rxt(106)*y(3)
         mat(271) = -rxt(136)*y(3)
         mat(96) = -rxt(153)*y(3)
         mat(209) = -rxt(156)*y(3)
         mat(132) = -rxt(253)*y(3)
         mat(258) = -rxt(256)*y(3)
         mat(142) = -rxt(284)*y(3)
         mat(85) = -(rxt(291) + rxt(292)) * y(3)
         mat(126) = -rxt(294)*y(3)

         mat(494) = 2.000_r8*rxt(93)*y(2) + 2.000_r8*rxt(111)*y(93) + rxt(163)*y(6) &
                      + rxt(166)*y(7) + rxt(142)*y(82) + rxt(137)*y(18) &
                      + 2.000_r8*rxt(150)*y(83) + rxt(178)*y(80) + rxt(205)*y(81) &
                      + rxt(254)*y(58) + rxt(257)*y(59)
         mat(387) = mat(387) + 2.000_r8*rxt(93)*y(1) + 2.000_r8*rxt(94)*y(2) &
                      + rxt(101)*y(92) + rxt(164)*y(7) + rxt(141)*y(82) + rxt(172) &
                      *y(8) + rxt(149)*y(83) + rxt(185)*y(23) + rxt(208)*y(30)
         mat(347) = mat(347) + 2.000_r8*rxt(111)*y(1)
         mat(418) = mat(418) + 2.000_r8*rxt(102)*y(92)
         mat(27) = mat(27) + rxt(101)*y(2) + 2.000_r8*rxt(102)*y(3)
         mat(209) = mat(209) + rxt(160)*y(7)
         mat(566) = rxt(163)*y(1) + rxt(290)*y(85)
         mat(593) = rxt(166)*y(1) + rxt(164)*y(2) + rxt(160)*y(5)
         mat(630) = rxt(142)*y(1) + rxt(141)*y(2) + rxt(176)*y(10) + rxt(143)*y(83) &
                      + rxt(187)*y(23)
         mat(655) = rxt(172)*y(2) + rxt(174)*y(83)
         mat(87) = rxt(176)*y(82)
         mat(719) = rxt(244)*y(83)
         mat(271) = mat(271) + rxt(137)*y(1) + rxt(139)*y(83)
         mat(443) = 2.000_r8*rxt(150)*y(1) + rxt(149)*y(2) + rxt(143)*y(82) + rxt(174) &
                      *y(8) + rxt(244)*y(13) + rxt(139)*y(18) + 2.000_r8*rxt(151) &
                      *y(83) + rxt(181)*y(80) + rxt(188)*y(23) + rxt(206)*y(81) &
                      + rxt(210)*y(30)
         mat(679) = rxt(178)*y(1) + rxt(181)*y(83)
         mat(471) = rxt(185)*y(2) + rxt(187)*y(82) + rxt(188)*y(83) + ( &
                      + 2.000_r8*rxt(192)+2.000_r8*rxt(193))*y(23) + (rxt(214) &
                       +rxt(215))*y(30)
         mat(700) = rxt(205)*y(1) + rxt(206)*y(83)
         mat(306) = rxt(208)*y(2) + rxt(210)*y(83) + (rxt(214)+rxt(215))*y(23) &
                      + 2.000_r8*rxt(216)*y(30)
         mat(160) = rxt(290)*y(6)
         mat(132) = mat(132) + rxt(254)*y(1)
         mat(258) = mat(258) + rxt(257)*y(1)

         mat(29) = -(rxt(95)*y(2) + rxt(96)*y(3) + rxt(98)*y(1))
         mat(360) = -rxt(95)*y(91)
         mat(401) = -rxt(96)*y(91)
         mat(485) = -rxt(98)*y(91)

         mat(337) = rxt(106)*y(3)
         mat(401) = mat(401) + rxt(106)*y(93)

         mat(26) = -(rxt(101)*y(2) + rxt(102)*y(3))
         mat(359) = -rxt(101)*y(92)
         mat(400) = -rxt(102)*y(92)

         mat(484) = rxt(98)*y(91)
         mat(359) = mat(359) + rxt(95)*y(91)
         mat(400) = mat(400) + rxt(96)*y(91)
         mat(28) = rxt(98)*y(1) + rxt(95)*y(2) + rxt(96)*y(3)

         mat(323) = -(rxt(133)*y(93) + rxt(146)*y(82) + rxt(148)*y(2) + rxt(179)*y(80) &
                      + rxt(222)*y(55))
         mat(344) = -rxt(133)*y(17)
         mat(627) = -rxt(146)*y(17)
         mat(384) = -rxt(148)*y(17)
         mat(676) = -rxt(179)*y(17)
         mat(149) = -rxt(222)*y(17)

         mat(269) = rxt(139)*y(83)
         mat(440) = rxt(139)*y(18)

         mat(106) = -((rxt(238) + rxt(239)) * y(82))
         mat(613) = -(rxt(238) + rxt(239)) * y(16)

         mat(365) = rxt(242)*y(15) + rxt(250)*y(57)
         mat(613) = mat(613) + rxt(241)*y(15) + rxt(251)*y(57)
         mat(645) = rxt(240)*y(15)
         mat(276) = rxt(242)*y(2) + rxt(241)*y(82) + rxt(240)*y(8) + rxt(183)*y(80) &
                      + rxt(207)*y(81)
         mat(669) = rxt(183)*y(15)
         mat(692) = rxt(207)*y(15)
         mat(66) = rxt(250)*y(2) + rxt(251)*y(82)

         mat(205) = -(rxt(155)*y(82) + rxt(156)*y(3) + rxt(157)*y(6) + (rxt(158) &
                      + rxt(159) + rxt(160)) * y(7) + rxt(289)*y(85))
         mat(618) = -rxt(155)*y(5)
         mat(410) = -rxt(156)*y(5)
         mat(559) = -rxt(157)*y(5)
         mat(583) = -(rxt(158) + rxt(159) + rxt(160)) * y(5)
         mat(157) = -rxt(289)*y(5)

         mat(375) = rxt(293)*y(86) + rxt(154)*y(90)
         mat(410) = mat(410) + rxt(291)*y(86)
         mat(123) = 1.100_r8*rxt(298)*y(89)
         mat(83) = rxt(293)*y(2) + rxt(291)*y(3)
         mat(165) = .200_r8*rxt(296)*y(89)
         mat(94) = rxt(154)*y(2)
         mat(176) = 1.100_r8*rxt(298)*y(84) + .200_r8*rxt(296)*y(88)

         mat(572) = -(rxt(157)*y(5) + rxt(161)*y(2) + rxt(162)*y(83) + rxt(163)*y(1) &
                      + rxt(171)*y(8) + rxt(190)*y(23) + rxt(211)*y(30) + rxt(243) &
                      *y(13) + rxt(290)*y(85))
         mat(211) = -rxt(157)*y(6)
         mat(393) = -rxt(161)*y(6)
         mat(449) = -rxt(162)*y(6)
         mat(500) = -rxt(163)*y(6)
         mat(661) = -rxt(171)*y(6)
         mat(477) = -rxt(190)*y(6)
         mat(312) = -rxt(211)*y(6)
         mat(725) = -rxt(243)*y(6)
         mat(161) = -rxt(290)*y(6)

         mat(393) = mat(393) + rxt(164)*y(7)
         mat(424) = rxt(156)*y(5) + rxt(153)*y(90)
         mat(211) = mat(211) + rxt(156)*y(3) + 2.000_r8*rxt(159)*y(7) + rxt(155)*y(82)
         mat(599) = rxt(164)*y(2) + 2.000_r8*rxt(159)*y(5) + rxt(258)*y(59)
         mat(636) = rxt(155)*y(5)
         mat(97) = rxt(153)*y(3)
         mat(263) = rxt(258)*y(7)

         mat(600) = -((rxt(158) + rxt(159) + rxt(160)) * y(5) + (rxt(164) + rxt(165) &
                      ) * y(2) + rxt(166)*y(1) + rxt(167)*y(8) + rxt(169)*y(82) &
                      + rxt(175)*y(83) + rxt(191)*y(23) + rxt(212)*y(30) + rxt(258) &
                      *y(59))
         mat(212) = -(rxt(158) + rxt(159) + rxt(160)) * y(7)
         mat(394) = -(rxt(164) + rxt(165)) * y(7)
         mat(501) = -rxt(166)*y(7)
         mat(662) = -rxt(167)*y(7)
         mat(637) = -rxt(169)*y(7)
         mat(450) = -rxt(175)*y(7)
         mat(478) = -rxt(191)*y(7)
         mat(313) = -rxt(212)*y(7)
         mat(264) = -rxt(258)*y(7)

         mat(501) = mat(501) + rxt(163)*y(6)
         mat(394) = mat(394) + rxt(161)*y(6) + rxt(172)*y(8)
         mat(573) = rxt(163)*y(1) + rxt(161)*y(2) + 2.000_r8*rxt(171)*y(8) + rxt(243) &
                      *y(13) + rxt(162)*y(83) + rxt(190)*y(23) + rxt(211)*y(30)
         mat(637) = mat(637) + rxt(173)*y(8) + rxt(176)*y(10)
         mat(662) = mat(662) + rxt(172)*y(2) + 2.000_r8*rxt(171)*y(6) + rxt(173)*y(82) &
                      + rxt(174)*y(83)
         mat(90) = rxt(176)*y(82)
         mat(726) = rxt(243)*y(6)
         mat(450) = mat(450) + rxt(162)*y(6) + rxt(174)*y(8)
         mat(478) = mat(478) + rxt(190)*y(6)
         mat(313) = mat(313) + rxt(211)*y(6)


      end subroutine nlnmat01

      subroutine nlnmat02( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------


         mat(638) = -(rxt(141)*y(2) + rxt(142)*y(1) + rxt(143)*y(83) + (4._r8*rxt(144) &
                      + 4._r8*rxt(145)) * y(82) + rxt(146)*y(17) + rxt(147)*y(19) &
                      + rxt(155)*y(5) + rxt(169)*y(7) + rxt(170)*y(9) + rxt(173)*y(8) &
                      + rxt(176)*y(10) + (rxt(186) + rxt(187)) * y(23) + rxt(197) &
                      *y(26) + rxt(201)*y(27) + rxt(203)*y(28) + rxt(209)*y(30) &
                      + rxt(217)*y(31) + (rxt(238) + rxt(239)) * y(16) + rxt(241) &
                      *y(15) + rxt(245)*y(14) + rxt(251)*y(57) + rxt(252)*y(58) &
                      + rxt(255)*y(59) + rxt(262)*y(60) + (rxt(264) + rxt(265) &
                      ) * y(63))
         mat(395) = -rxt(141)*y(82)
         mat(502) = -rxt(142)*y(82)
         mat(451) = -rxt(143)*y(82)
         mat(332) = -rxt(146)*y(82)
         mat(115) = -rxt(147)*y(82)
         mat(213) = -rxt(155)*y(82)
         mat(601) = -rxt(169)*y(82)
         mat(248) = -rxt(170)*y(82)
         mat(663) = -rxt(173)*y(82)
         mat(91) = -rxt(176)*y(82)
         mat(479) = -(rxt(186) + rxt(187)) * y(82)
         mat(526) = -rxt(197)*y(82)
         mat(220) = -rxt(201)*y(82)
         mat(238) = -rxt(203)*y(82)
         mat(314) = -rxt(209)*y(82)
         mat(198) = -rxt(217)*y(82)
         mat(109) = -(rxt(238) + rxt(239)) * y(82)
         mat(290) = -rxt(241)*y(82)
         mat(77) = -rxt(245)*y(82)
         mat(72) = -rxt(251)*y(82)
         mat(134) = -rxt(252)*y(82)
         mat(265) = -rxt(255)*y(82)
         mat(228) = -rxt(262)*y(82)
         mat(52) = -(rxt(264) + rxt(265)) * y(82)

         mat(502) = mat(502) + rxt(137)*y(18) + rxt(150)*y(83)
         mat(395) = mat(395) + rxt(148)*y(17) + rxt(242)*y(15) + rxt(149)*y(83) &
                      + rxt(152)*y(19) + rxt(198)*y(26) + rxt(199)*y(27) + rxt(218) &
                      *y(31) + rxt(219)*y(32)
         mat(354) = rxt(133)*y(17) + 2.000_r8*rxt(108)*y(94) + rxt(134)*y(26) &
                      + rxt(135)*y(31)
         mat(332) = mat(332) + rxt(148)*y(2) + rxt(133)*y(93)
         mat(574) = rxt(162)*y(83)
         mat(663) = mat(663) + rxt(174)*y(83)
         mat(290) = mat(290) + rxt(242)*y(2)
         mat(275) = rxt(137)*y(1) + 2.000_r8*rxt(138)*y(83)
         mat(451) = mat(451) + rxt(150)*y(1) + rxt(149)*y(2) + rxt(162)*y(6) &
                      + rxt(174)*y(8) + 2.000_r8*rxt(138)*y(18) + rxt(182)*y(80)
         mat(115) = mat(115) + rxt(152)*y(2)
         mat(551) = 2.000_r8*rxt(108)*y(93) + rxt(221)*y(55)
         mat(687) = rxt(182)*y(83)
         mat(526) = mat(526) + rxt(198)*y(2) + rxt(134)*y(93)
         mat(220) = mat(220) + rxt(199)*y(2)
         mat(198) = mat(198) + rxt(218)*y(2) + rxt(135)*y(93)
         mat(190) = rxt(219)*y(2)
         mat(151) = rxt(221)*y(94)

         mat(664) = -(rxt(167)*y(7) + rxt(171)*y(6) + rxt(172)*y(2) + rxt(173)*y(82) &
                      + rxt(174)*y(83) + rxt(240)*y(15) + rxt(266)*y(63))
         mat(602) = -rxt(167)*y(8)
         mat(575) = -rxt(171)*y(8)
         mat(396) = -rxt(172)*y(8)
         mat(639) = -rxt(173)*y(8)
         mat(452) = -rxt(174)*y(8)
         mat(291) = -rxt(240)*y(8)
         mat(53) = -rxt(266)*y(8)

         mat(503) = rxt(166)*y(7)
         mat(396) = mat(396) + rxt(165)*y(7) + rxt(202)*y(28) + rxt(220)*y(33)
         mat(602) = mat(602) + rxt(166)*y(1) + rxt(165)*y(2)
         mat(639) = mat(639) + rxt(170)*y(9) + rxt(203)*y(28)
         mat(249) = rxt(170)*y(82) + rxt(224)*y(55)
         mat(688) = rxt(204)*y(28)
         mat(239) = rxt(202)*y(2) + rxt(203)*y(82) + rxt(204)*y(80)
         mat(104) = rxt(220)*y(2)
         mat(152) = rxt(224)*y(9)

         mat(243) = -(rxt(170)*y(82) + rxt(224)*y(55))
         mat(622) = -rxt(170)*y(9)
         mat(147) = -rxt(224)*y(9)

         mat(586) = rxt(169)*y(82)
         mat(622) = mat(622) + rxt(169)*y(7)
         mat(647) = rxt(240)*y(15) + rxt(266)*y(63)
         mat(278) = rxt(240)*y(8)
         mat(512) = (rxt(270)+rxt(275)+rxt(281))*y(28)
         mat(232) = (rxt(270)+rxt(275)+rxt(281))*y(26)
         mat(50) = rxt(266)*y(8)

         mat(86) = -(rxt(176)*y(82))
         mat(612) = -rxt(176)*y(10)

         mat(580) = rxt(175)*y(83)
         mat(432) = rxt(175)*y(7)


         mat(579) = rxt(167)*y(8)
         mat(644) = rxt(167)*y(7)

         mat(731) = -(rxt(189)*y(23) + rxt(243)*y(6) + rxt(244)*y(83))
         mat(483) = -rxt(189)*y(13)
         mat(578) = -rxt(243)*y(13)
         mat(455) = -rxt(244)*y(13)

         mat(642) = rxt(245)*y(14)
         mat(78) = rxt(245)*y(82)

         mat(73) = -(rxt(245)*y(82))
         mat(611) = -rxt(245)*y(14)

         mat(713) = rxt(244)*y(83)
         mat(431) = rxt(244)*y(13)

         mat(280) = -(rxt(183)*y(80) + rxt(207)*y(81) + rxt(240)*y(8) + rxt(241)*y(82) &
                      + rxt(242)*y(2))
         mat(675) = -rxt(183)*y(15)
         mat(695) = -rxt(207)*y(15)
         mat(650) = -rxt(240)*y(15)
         mat(625) = -rxt(241)*y(15)
         mat(382) = -rxt(242)*y(15)

         mat(561) = rxt(243)*y(13)
         mat(715) = rxt(243)*y(6) + rxt(189)*y(23)
         mat(466) = rxt(189)*y(13)

         mat(268) = -(rxt(136)*y(3) + rxt(137)*y(1) + (rxt(138) + rxt(139) + rxt(140) &
                      ) * y(83))
         mat(413) = -rxt(136)*y(18)
         mat(489) = -rxt(137)*y(18)
         mat(437) = -(rxt(138) + rxt(139) + rxt(140)) * y(18)

         mat(381) = rxt(148)*y(17) + rxt(141)*y(82)
         mat(342) = rxt(133)*y(17)
         mat(322) = rxt(148)*y(2) + rxt(133)*y(93) + rxt(146)*y(82) + rxt(179)*y(80) &
                      + rxt(222)*y(55)
         mat(107) = rxt(238)*y(82)
         mat(206) = rxt(155)*y(82)
         mat(624) = rxt(141)*y(2) + rxt(146)*y(17) + rxt(238)*y(16) + rxt(155)*y(5) &
                      + rxt(241)*y(15) + rxt(251)*y(57) + rxt(252)*y(58) + rxt(255) &
                      *y(59)
         mat(279) = rxt(241)*y(82)
         mat(674) = rxt(179)*y(17)
         mat(148) = rxt(222)*y(17)
         mat(70) = rxt(251)*y(82)
         mat(130) = rxt(252)*y(82)
         mat(255) = rxt(255)*y(82)

         mat(444) = -((rxt(138) + rxt(139) + rxt(140)) * y(18) + rxt(143)*y(82) &
                      + rxt(149)*y(2) + rxt(150)*y(1) + 4._r8*rxt(151)*y(83) + rxt(162) &
                      *y(6) + rxt(174)*y(8) + rxt(175)*y(7) + (rxt(181) + rxt(182) &
                      ) * y(80) + rxt(188)*y(23) + rxt(206)*y(81) + rxt(210)*y(30) &
                      + rxt(244)*y(13))
         mat(272) = -(rxt(138) + rxt(139) + rxt(140)) * y(83)
         mat(631) = -rxt(143)*y(83)
         mat(388) = -rxt(149)*y(83)
         mat(495) = -rxt(150)*y(83)
         mat(567) = -rxt(162)*y(83)
         mat(656) = -rxt(174)*y(83)
         mat(594) = -rxt(175)*y(83)
         mat(680) = -(rxt(181) + rxt(182)) * y(83)
         mat(472) = -rxt(188)*y(83)
         mat(701) = -rxt(206)*y(83)
         mat(307) = -rxt(210)*y(83)
         mat(720) = -rxt(244)*y(83)

         mat(495) = mat(495) + rxt(142)*y(82)
         mat(388) = mat(388) + rxt(242)*y(15) + rxt(152)*y(19)
         mat(419) = rxt(136)*y(18)
         mat(108) = rxt(239)*y(82)
         mat(567) = mat(567) + rxt(243)*y(13)
         mat(631) = mat(631) + rxt(142)*y(1) + rxt(239)*y(16) + rxt(173)*y(8) &
                      + rxt(147)*y(19) + rxt(186)*y(23) + rxt(209)*y(30) + rxt(262) &
                      *y(60) + .500_r8*rxt(264)*y(63)
         mat(656) = mat(656) + rxt(173)*y(82) + rxt(240)*y(15)
         mat(720) = mat(720) + rxt(243)*y(6) + rxt(189)*y(23)
         mat(285) = rxt(242)*y(2) + rxt(240)*y(8) + rxt(183)*y(80) + rxt(207)*y(81)
         mat(272) = mat(272) + rxt(136)*y(3)
         mat(112) = rxt(152)*y(2) + rxt(147)*y(82) + rxt(180)*y(80)
         mat(680) = mat(680) + rxt(183)*y(15) + rxt(180)*y(19)
         mat(472) = mat(472) + rxt(186)*y(82) + rxt(189)*y(13)
         mat(701) = mat(701) + rxt(207)*y(15)
         mat(307) = mat(307) + rxt(209)*y(82)
         mat(226) = rxt(262)*y(82)
         mat(51) = .500_r8*rxt(264)*y(82)

         mat(110) = -(rxt(147)*y(82) + rxt(152)*y(2) + rxt(180)*y(80))
         mat(614) = -rxt(147)*y(19)
         mat(366) = -rxt(152)*y(19)
         mat(670) = -rxt(180)*y(19)

         mat(614) = mat(614) + 2.000_r8*rxt(145)*y(82)
         mat(433) = 2.000_r8*rxt(151)*y(83)

         mat(548) = -(rxt(108)*y(93) + rxt(221)*y(55) + rxt(263)*y(61))
         mat(351) = -rxt(108)*y(94)
         mat(150) = -rxt(221)*y(94)
         mat(47) = -rxt(263)*y(94)

         mat(330) = rxt(146)*y(82)
         mat(635) = rxt(146)*y(17) + 2.000_r8*rxt(144)*y(82) + rxt(170)*y(9) &
                      + rxt(176)*y(10) + rxt(245)*y(14) + rxt(241)*y(15) + rxt(143) &
                      *y(83) + rxt(147)*y(19) + rxt(197)*y(26) + rxt(201)*y(27) &
                      + rxt(217)*y(31)
         mat(246) = rxt(170)*y(82)
         mat(89) = rxt(176)*y(82)
         mat(76) = rxt(245)*y(82)
         mat(288) = rxt(241)*y(82)
         mat(274) = rxt(140)*y(83)
         mat(448) = rxt(143)*y(82) + rxt(140)*y(18)
         mat(114) = rxt(147)*y(82)
         mat(523) = rxt(197)*y(82) + (rxt(271)+rxt(276)+rxt(282))*y(27) + (rxt(272) &
                       +rxt(283))*y(32)
         mat(219) = rxt(201)*y(82) + (rxt(271)+rxt(276)+rxt(282))*y(26)
         mat(197) = rxt(217)*y(82)
         mat(189) = (rxt(272)+rxt(283))*y(26)

         mat(689) = -(rxt(178)*y(1) + rxt(179)*y(17) + rxt(180)*y(19) + (rxt(181) &
                      + rxt(182)) * y(83) + rxt(183)*y(15) + rxt(200)*y(27) + rxt(204) &
                      *y(28))
         mat(504) = -rxt(178)*y(80)
         mat(334) = -rxt(179)*y(80)
         mat(116) = -rxt(180)*y(80)
         mat(453) = -(rxt(181) + rxt(182)) * y(80)
         mat(292) = -rxt(183)*y(80)
         mat(221) = -rxt(200)*y(80)
         mat(240) = -rxt(204)*y(80)

         mat(397) = rxt(185)*y(23) + rxt(198)*y(26)
         mat(356) = rxt(134)*y(26) + rxt(129)*y(53)
         mat(576) = rxt(190)*y(23)
         mat(640) = rxt(186)*y(23) + rxt(197)*y(26)
         mat(729) = rxt(189)*y(23)
         mat(481) = rxt(185)*y(2) + rxt(190)*y(6) + rxt(186)*y(82) + rxt(189)*y(13) + ( &
                      + 4.000_r8*rxt(192)+2.000_r8*rxt(194))*y(23) + rxt(214)*y(30) &
                      + rxt(259)*y(59)
         mat(528) = rxt(198)*y(2) + rxt(134)*y(93) + rxt(197)*y(82)
         mat(316) = rxt(214)*y(23)
         mat(36) = rxt(129)*y(93)
         mat(266) = rxt(259)*y(23)


      end subroutine nlnmat02

      subroutine nlnmat03( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------



         mat(668) = rxt(204)*y(28)
         mat(457) = 2.000_r8*rxt(193)*y(23)
         mat(507) = (rxt(271)+rxt(276)+rxt(282))*y(27) + (rxt(270)+rxt(275)+rxt(281)) &
                      *y(28)
         mat(214) = (rxt(271)+rxt(276)+rxt(282))*y(26)
         mat(229) = rxt(204)*y(80) + (rxt(270)+rxt(275)+rxt(281))*y(26)

         mat(473) = -(rxt(185)*y(2) + (rxt(186) + rxt(187)) * y(82) + rxt(188)*y(83) &
                      + rxt(189)*y(13) + rxt(190)*y(6) + rxt(191)*y(7) + (4._r8*rxt(192) &
                      + 4._r8*rxt(193) + 4._r8*rxt(194) + 4._r8*rxt(195)) * y(23) &
                      + (rxt(213) + rxt(214) + rxt(215)) * y(30) + rxt(259)*y(59))
         mat(389) = -rxt(185)*y(23)
         mat(632) = -(rxt(186) + rxt(187)) * y(23)
         mat(445) = -rxt(188)*y(23)
         mat(721) = -rxt(189)*y(23)
         mat(568) = -rxt(190)*y(23)
         mat(595) = -rxt(191)*y(23)
         mat(308) = -(rxt(213) + rxt(214) + rxt(215)) * y(23)
         mat(260) = -rxt(259)*y(23)

         mat(496) = rxt(178)*y(80)
         mat(389) = mat(389) + rxt(199)*y(27) + rxt(202)*y(28)
         mat(632) = mat(632) + rxt(201)*y(27)
         mat(445) = mat(445) + rxt(182)*y(80)
         mat(681) = rxt(178)*y(1) + rxt(182)*y(83) + rxt(200)*y(27)
         mat(64) = rxt(261)*y(59)
         mat(217) = rxt(199)*y(2) + rxt(201)*y(82) + rxt(200)*y(80)
         mat(234) = rxt(202)*y(2)
         mat(260) = mat(260) + rxt(261)*y(24)

         mat(60) = -(rxt(261)*y(59))
         mat(251) = -rxt(261)*y(24)

         mat(459) = 2.000_r8*rxt(194)*y(23) + rxt(213)*y(30)
         mat(296) = rxt(213)*y(23)


         mat(456) = 2.000_r8*rxt(195)*y(23)

         mat(522) = -(rxt(134)*y(93) + rxt(197)*y(82) + rxt(198)*y(2) + (rxt(270) &
                      + rxt(275) + rxt(281)) * y(28) + (rxt(271) + rxt(276) + rxt(282) &
                      ) * y(27) + (rxt(272) + rxt(283)) * y(32))
         mat(350) = -rxt(134)*y(26)
         mat(634) = -rxt(197)*y(26)
         mat(391) = -rxt(198)*y(26)
         mat(235) = -(rxt(270) + rxt(275) + rxt(281)) * y(26)
         mat(218) = -(rxt(271) + rxt(276) + rxt(282)) * y(26)
         mat(188) = -(rxt(272) + rxt(283)) * y(26)

         mat(329) = rxt(179)*y(80)
         mat(634) = mat(634) + rxt(187)*y(23)
         mat(287) = rxt(183)*y(80)
         mat(447) = rxt(181)*y(80)
         mat(113) = rxt(180)*y(80)
         mat(683) = rxt(179)*y(17) + rxt(183)*y(15) + rxt(181)*y(83) + rxt(180)*y(19) &
                      + rxt(200)*y(27)
         mat(475) = rxt(187)*y(82)
         mat(218) = mat(218) + rxt(200)*y(80)

         mat(215) = -(rxt(199)*y(2) + rxt(200)*y(80) + rxt(201)*y(82) + (rxt(271) &
                      + rxt(276) + rxt(282)) * y(26))
         mat(376) = -rxt(199)*y(27)
         mat(671) = -rxt(200)*y(27)
         mat(619) = -rxt(201)*y(27)
         mat(510) = -(rxt(271) + rxt(276) + rxt(282)) * y(27)

         mat(619) = mat(619) + rxt(203)*y(28)
         mat(436) = rxt(188)*y(23)
         mat(460) = rxt(188)*y(83)
         mat(230) = rxt(203)*y(82)

         mat(231) = -(rxt(202)*y(2) + rxt(203)*y(82) + rxt(204)*y(80) + (rxt(270) &
                      + rxt(275) + rxt(281)) * y(26))
         mat(378) = -rxt(202)*y(28)
         mat(621) = -rxt(203)*y(28)
         mat(672) = -rxt(204)*y(28)
         mat(511) = -(rxt(270) + rxt(275) + rxt(281)) * y(28)

         mat(585) = rxt(191)*y(23)
         mat(462) = rxt(191)*y(7)


         mat(458) = rxt(215)*y(30)
         mat(508) = (rxt(272)+rxt(283))*y(32)
         mat(295) = rxt(215)*y(23)
         mat(184) = (rxt(272)+rxt(283))*y(26)

         mat(711) = -(rxt(205)*y(1) + rxt(206)*y(83) + rxt(207)*y(15))
         mat(505) = -rxt(205)*y(81)
         mat(454) = -rxt(206)*y(81)
         mat(293) = -rxt(207)*y(81)

         mat(398) = rxt(208)*y(30) + rxt(218)*y(31)
         mat(357) = rxt(135)*y(31)
         mat(577) = rxt(211)*y(30)
         mat(641) = rxt(209)*y(30) + rxt(217)*y(31)
         mat(482) = (rxt(213)+rxt(214))*y(30)
         mat(317) = rxt(208)*y(2) + rxt(211)*y(6) + rxt(209)*y(82) + (rxt(213) &
                       +rxt(214))*y(23) + 4.000_r8*rxt(216)*y(30) + rxt(260)*y(59)
         mat(199) = rxt(218)*y(2) + rxt(135)*y(93) + rxt(217)*y(82)
         mat(267) = rxt(260)*y(30)

         mat(303) = -(rxt(208)*y(2) + rxt(209)*y(82) + rxt(210)*y(83) + rxt(211)*y(6) &
                      + rxt(212)*y(7) + (rxt(213) + rxt(214) + rxt(215)) * y(23) &
                      + 4._r8*rxt(216)*y(30) + rxt(260)*y(59))
         mat(383) = -rxt(208)*y(30)
         mat(626) = -rxt(209)*y(30)
         mat(439) = -rxt(210)*y(30)
         mat(562) = -rxt(211)*y(30)
         mat(589) = -rxt(212)*y(30)
         mat(467) = -(rxt(213) + rxt(214) + rxt(215)) * y(30)
         mat(256) = -rxt(260)*y(30)

         mat(490) = rxt(205)*y(81)
         mat(383) = mat(383) + rxt(219)*y(32) + rxt(220)*y(33)
         mat(696) = rxt(205)*y(1)
         mat(186) = rxt(219)*y(2)
         mat(101) = rxt(220)*y(2)

         mat(193) = -(rxt(135)*y(93) + rxt(217)*y(82) + rxt(218)*y(2))
         mat(340) = -rxt(135)*y(31)
         mat(617) = -rxt(217)*y(31)
         mat(374) = -rxt(218)*y(31)

         mat(277) = rxt(207)*y(81)
         mat(435) = rxt(206)*y(81)
         mat(693) = rxt(207)*y(15) + rxt(206)*y(83)

         mat(185) = -(rxt(219)*y(2) + (rxt(272) + rxt(283)) * y(26))
         mat(373) = -rxt(219)*y(32)
         mat(509) = -(rxt(272) + rxt(283)) * y(32)

         mat(434) = rxt(210)*y(30)
         mat(298) = rxt(210)*y(83)

         mat(98) = -(rxt(220)*y(2))
         mat(364) = -rxt(220)*y(33)

         mat(581) = rxt(212)*y(30)
         mat(297) = rxt(212)*y(7)

         mat(118) = -((rxt(286) + rxt(287)) * y(2) + rxt(294)*y(3) + rxt(298)*y(89))
         mat(367) = -(rxt(286) + rxt(287)) * y(84)
         mat(404) = -rxt(294)*y(84)
         mat(171) = -rxt(298)*y(84)

         mat(154) = -(rxt(289)*y(5) + rxt(290)*y(6) + rxt(297)*y(89))
         mat(202) = -rxt(289)*y(85)
         mat(556) = -rxt(290)*y(85)
         mat(173) = -rxt(297)*y(85)

         mat(407) = rxt(294)*y(84) + rxt(291)*y(86) + rxt(284)*y(87)
         mat(120) = rxt(294)*y(3)
         mat(81) = rxt(291)*y(3)
         mat(137) = rxt(284)*y(3)

         mat(79) = -((rxt(291) + rxt(292)) * y(3) + rxt(293)*y(2))
         mat(402) = -(rxt(291) + rxt(292)) * y(86)
         mat(362) = -rxt(293)*y(86)

         mat(136) = -(rxt(284)*y(3))
         mat(406) = -rxt(284)*y(87)

         mat(369) = rxt(287)*y(84) + rxt(293)*y(86)
         mat(119) = rxt(287)*y(2)
         mat(80) = rxt(293)*y(2)

         mat(163) = -(rxt(296)*y(89))
         mat(174) = -rxt(296)*y(88)

         mat(371) = rxt(286)*y(84)
         mat(408) = rxt(292)*y(86)
         mat(203) = rxt(289)*y(85)
         mat(557) = rxt(290)*y(85)
         mat(121) = rxt(286)*y(2)
         mat(155) = rxt(289)*y(5) + rxt(290)*y(6)
         mat(82) = rxt(292)*y(3)

         mat(93) = -(rxt(153)*y(3) + rxt(154)*y(2))
         mat(403) = -rxt(153)*y(90)
         mat(363) = -rxt(154)*y(90)

         mat(363) = mat(363) + rxt(286)*y(84)
         mat(117) = rxt(286)*y(2) + .900_r8*rxt(298)*y(89)
         mat(162) = .800_r8*rxt(296)*y(89)
         mat(170) = .900_r8*rxt(298)*y(84) + .800_r8*rxt(296)*y(88)

         mat(175) = -(rxt(296)*y(88) + rxt(297)*y(85) + rxt(298)*y(84))
         mat(164) = -rxt(296)*y(89)
         mat(156) = -rxt(297)*y(89)
         mat(122) = -rxt(298)*y(89)

         mat(20) = -(rxt(128)*y(93))
         mat(336) = -rxt(128)*y(52)

         mat(33) = -(rxt(129)*y(93))
         mat(338) = -rxt(129)*y(53)


         mat(319) = rxt(222)*y(55)
         mat(241) = rxt(224)*y(55)
         mat(532) = rxt(221)*y(55)
         mat(145) = rxt(222)*y(17) + rxt(224)*y(9) + rxt(221)*y(94)

         mat(146) = -(rxt(221)*y(94) + rxt(222)*y(17) + rxt(224)*y(9))
         mat(534) = -rxt(221)*y(55)
         mat(320) = -rxt(222)*y(55)
         mat(242) = -rxt(224)*y(55)

         mat(339) = 2.000_r8*rxt(128)*y(52) + rxt(129)*y(53)
         mat(21) = 2.000_r8*rxt(128)*y(93)
         mat(34) = rxt(129)*y(93)

         mat(65) = -(rxt(250)*y(2) + rxt(251)*y(82))
         mat(361) = -rxt(250)*y(57)
         mat(610) = -rxt(251)*y(57)

         mat(128) = -(rxt(252)*y(82) + rxt(253)*y(3) + rxt(254)*y(1))
         mat(615) = -rxt(252)*y(58)
         mat(405) = -rxt(253)*y(58)
         mat(486) = -rxt(254)*y(58)

         mat(254) = -(rxt(255)*y(82) + rxt(256)*y(3) + rxt(257)*y(1) + rxt(258)*y(7) &
                      + rxt(259)*y(23) + rxt(260)*y(30) + rxt(261)*y(24))
         mat(623) = -rxt(255)*y(59)
         mat(412) = -rxt(256)*y(59)
         mat(488) = -rxt(257)*y(59)
         mat(587) = -rxt(258)*y(59)
         mat(464) = -rxt(259)*y(59)
         mat(301) = -rxt(260)*y(59)
         mat(62) = -rxt(261)*y(59)

         mat(488) = mat(488) + rxt(254)*y(58)
         mat(380) = rxt(250)*y(57)
         mat(412) = mat(412) + rxt(253)*y(58)
         mat(623) = mat(623) + rxt(252)*y(58)
         mat(69) = rxt(250)*y(2)
         mat(129) = rxt(254)*y(1) + rxt(253)*y(3) + rxt(252)*y(82)

         mat(223) = -(rxt(262)*y(82))
         mat(620) = -rxt(262)*y(60)

         mat(487) = rxt(257)*y(59)
         mat(411) = rxt(256)*y(59)
         mat(584) = rxt(258)*y(59)
         mat(620) = mat(620) + rxt(251)*y(57) + rxt(255)*y(59) + (.500_r8*rxt(264) &
                       +rxt(265))*y(63)
         mat(646) = rxt(266)*y(63)
         mat(461) = rxt(259)*y(59)
         mat(61) = rxt(261)*y(59)
         mat(299) = rxt(260)*y(59)
         mat(68) = rxt(251)*y(82)
         mat(253) = rxt(257)*y(1) + rxt(256)*y(3) + rxt(258)*y(7) + rxt(255)*y(82) &
                      + rxt(259)*y(23) + rxt(261)*y(24) + rxt(260)*y(30)
         mat(49) = (.500_r8*rxt(264)+rxt(265))*y(82) + rxt(266)*y(8)


      end subroutine nlnmat03

      subroutine nlnmat04( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------


         mat(44) = -(rxt(263)*y(94))
         mat(533) = -rxt(263)*y(61)

         mat(608) = rxt(262)*y(60)
         mat(222) = rxt(262)*y(82)


         mat(531) = rxt(263)*y(61)
         mat(43) = rxt(263)*y(94)

         mat(48) = -((rxt(264) + rxt(265)) * y(82) + rxt(266)*y(8))
         mat(609) = -(rxt(264) + rxt(265)) * y(63)
         mat(643) = -rxt(266)*y(63)
      end subroutine nlnmat04
      subroutine nlnmat_finit( mat, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: dti
      real(r8), intent(in) :: lmat(nzcnt)
      real(r8), intent(inout) :: mat(nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
         mat( 1) = lmat( 1)
         mat( 2) = lmat( 2)
         mat( 3) = lmat( 3)
         mat( 4) = lmat( 4)
         mat( 5) = lmat( 5)
         mat( 6) = lmat( 6)
         mat( 7) = lmat( 7)
         mat( 8) = lmat( 8)
         mat( 9) = lmat( 9)
         mat( 10) = lmat( 10)
         mat( 11) = lmat( 11)
         mat( 12) = lmat( 12)
         mat( 13) = lmat( 13)
         mat( 14) = lmat( 14)
         mat( 15) = lmat( 15)
         mat( 16) = lmat( 16)
         mat( 17) = lmat( 17)
         mat( 18) = lmat( 18)
         mat( 19) = lmat( 19)
         mat( 20) = mat( 20) + lmat( 20)
         mat( 21) = mat( 21) + lmat( 21)
         mat( 23) = lmat( 23)
         mat( 24) = lmat( 24)
         mat( 25) = lmat( 25)
         mat( 26) = mat( 26) + lmat( 26)
         mat( 27) = mat( 27) + lmat( 27)
         mat( 28) = mat( 28) + lmat( 28)
         mat( 29) = mat( 29) + lmat( 29)
         mat( 30) = lmat( 30)
         mat( 31) = lmat( 31)
         mat( 32) = lmat( 32)
         mat( 33) = mat( 33) + lmat( 33)
         mat( 34) = mat( 34) + lmat( 34)
         mat( 36) = mat( 36) + lmat( 36)
         mat( 37) = lmat( 37)
         mat( 38) = lmat( 38)
         mat( 39) = lmat( 39)
         mat( 40) = lmat( 40)
         mat( 41) = lmat( 41)
         mat( 42) = lmat( 42)
         mat( 44) = mat( 44) + lmat( 44)
         mat( 45) = lmat( 45)
         mat( 46) = lmat( 46)
         mat( 48) = mat( 48) + lmat( 48)
         mat( 54) = lmat( 54)
         mat( 55) = lmat( 55)
         mat( 56) = lmat( 56)
         mat( 57) = lmat( 57)
         mat( 58) = lmat( 58)
         mat( 59) = lmat( 59)
         mat( 60) = mat( 60) + lmat( 60)
         mat( 63) = lmat( 63)
         mat( 64) = mat( 64) + lmat( 64)
         mat( 65) = mat( 65) + lmat( 65)
         mat( 66) = mat( 66) + lmat( 66)
         mat( 67) = lmat( 67)
         mat( 73) = mat( 73) + lmat( 73)
         mat( 74) = lmat( 74)
         mat( 75) = lmat( 75)
         mat( 77) = mat( 77) + lmat( 77)
         mat( 79) = mat( 79) + lmat( 79)
         mat( 86) = mat( 86) + lmat( 86)
         mat( 88) = lmat( 88)
         mat( 90) = mat( 90) + lmat( 90)
         mat( 91) = mat( 91) + lmat( 91)
         mat( 92) = lmat( 92)
         mat( 93) = mat( 93) + lmat( 93)
         mat( 98) = mat( 98) + lmat( 98)
         mat( 99) = lmat( 99)
         mat( 100) = lmat( 100)
         mat( 101) = mat( 101) + lmat( 101)
         mat( 103) = lmat( 103)
         mat( 104) = mat( 104) + lmat( 104)
         mat( 105) = lmat( 105)
         mat( 106) = mat( 106) + lmat( 106)
         mat( 110) = mat( 110) + lmat( 110)
         mat( 115) = mat( 115) + lmat( 115)
         mat( 118) = mat( 118) + lmat( 118)
         mat( 128) = mat( 128) + lmat( 128)
         mat( 135) = lmat( 135)
         mat( 136) = mat( 136) + lmat( 136)
         mat( 137) = mat( 137) + lmat( 137)
         mat( 138) = lmat( 138)
         mat( 139) = lmat( 139)
         mat( 145) = mat( 145) + lmat( 145)
         mat( 146) = mat( 146) + lmat( 146)
         mat( 153) = lmat( 153)
         mat( 154) = mat( 154) + lmat( 154)
         mat( 155) = mat( 155) + lmat( 155)
         mat( 161) = mat( 161) + lmat( 161)
         mat( 163) = mat( 163) + lmat( 163)
         mat( 175) = mat( 175) + lmat( 175)
         mat( 185) = mat( 185) + lmat( 185)
         mat( 190) = mat( 190) + lmat( 190)
         mat( 192) = lmat( 192)
         mat( 193) = mat( 193) + lmat( 193)
         mat( 194) = lmat( 194)
         mat( 199) = mat( 199) + lmat( 199)
         mat( 200) = lmat( 200)
         mat( 204) = lmat( 204)
         mat( 205) = mat( 205) + lmat( 205)
         mat( 215) = mat( 215) + lmat( 215)
         mat( 220) = mat( 220) + lmat( 220)
         mat( 221) = mat( 221) + lmat( 221)
         mat( 223) = mat( 223) + lmat( 223)
         mat( 224) = lmat( 224)
         mat( 225) = lmat( 225)
         mat( 230) = mat( 230) + lmat( 230)
         mat( 231) = mat( 231) + lmat( 231)
         mat( 232) = mat( 232) + lmat( 232)
         mat( 234) = mat( 234) + lmat( 234)
         mat( 237) = lmat( 237)
         mat( 239) = mat( 239) + lmat( 239)
         mat( 240) = mat( 240) + lmat( 240)
         mat( 243) = mat( 243) + lmat( 243)
         mat( 247) = lmat( 247)
         mat( 248) = mat( 248) + lmat( 248)
         mat( 252) = lmat( 252)
         mat( 254) = mat( 254) + lmat( 254)
         mat( 257) = mat( 257) + lmat( 257)
         mat( 268) = mat( 268) + lmat( 268)
         mat( 276) = mat( 276) + lmat( 276)
         mat( 279) = mat( 279) + lmat( 279)
         mat( 280) = mat( 280) + lmat( 280)
         mat( 281) = lmat( 281)
         mat( 303) = mat( 303) + lmat( 303)
         mat( 305) = mat( 305) + lmat( 305)
         mat( 317) = mat( 317) + lmat( 317)
         mat( 323) = mat( 323) + lmat( 323)
         mat( 336) = mat( 336) + lmat( 336)
         mat( 338) = mat( 338) + lmat( 338)
         mat( 339) = mat( 339) + lmat( 339)
         mat( 342) = mat( 342) + lmat( 342)
         mat( 343) = lmat( 343)
         mat( 344) = mat( 344) + lmat( 344)
         mat( 345) = mat( 345) + lmat( 345)
         mat( 346) = mat( 346) + lmat( 346)
         mat( 347) = mat( 347) + lmat( 347)
         mat( 348) = lmat( 348)
         mat( 352) = lmat( 352)
         mat( 354) = mat( 354) + lmat( 354)
         mat( 356) = mat( 356) + lmat( 356)
         mat( 357) = mat( 357) + lmat( 357)
         mat( 358) = lmat( 358)
         mat( 369) = mat( 369) + lmat( 369)
         mat( 372) = lmat( 372)
         mat( 386) = mat( 386) + lmat( 386)
         mat( 406) = mat( 406) + lmat( 406)
         mat( 407) = mat( 407) + lmat( 407)
         mat( 409) = lmat( 409)
         mat( 416) = mat( 416) + lmat( 416)
         mat( 417) = mat( 417) + lmat( 417)
         mat( 418) = mat( 418) + lmat( 418)
         mat( 433) = mat( 433) + lmat( 433)
         mat( 444) = mat( 444) + lmat( 444)
         mat( 470) = mat( 470) + lmat( 470)
         mat( 473) = mat( 473) + lmat( 473)
         mat( 481) = mat( 481) + lmat( 481)
         mat( 484) = mat( 484) + lmat( 484)
         mat( 492) = mat( 492) + lmat( 492)
         mat( 493) = mat( 493) + lmat( 493)
         mat( 494) = mat( 494) + lmat( 494)
         mat( 497) = mat( 497) + lmat( 497)
         mat( 513) = lmat( 513)
         mat( 522) = mat( 522) + lmat( 522)
         mat( 528) = mat( 528) + lmat( 528)
         mat( 538) = lmat( 538)
         mat( 540) = lmat( 540)
         mat( 541) = mat( 541) + lmat( 541)
         mat( 542) = lmat( 542)
         mat( 548) = mat( 548) + lmat( 548)
         mat( 551) = mat( 551) + lmat( 551)
         mat( 557) = mat( 557) + lmat( 557)
         mat( 558) = lmat( 558)
         mat( 559) = mat( 559) + lmat( 559)
         mat( 565) = mat( 565) + lmat( 565)
         mat( 572) = mat( 572) + lmat( 572)
         mat( 586) = mat( 586) + lmat( 586)
         mat( 592) = mat( 592) + lmat( 592)
         mat( 599) = mat( 599) + lmat( 599)
         mat( 600) = mat( 600) + lmat( 600)
         mat( 601) = mat( 601) + lmat( 601)
         mat( 606) = lmat( 606)
         mat( 607) = lmat( 607)
         mat( 631) = mat( 631) + lmat( 631)
         mat( 635) = mat( 635) + lmat( 635)
         mat( 638) = mat( 638) + lmat( 638)
         mat( 640) = mat( 640) + lmat( 640)
         mat( 641) = mat( 641) + lmat( 641)
         mat( 642) = mat( 642) + lmat( 642)
         mat( 647) = mat( 647) + lmat( 647)
         mat( 654) = mat( 654) + lmat( 654)
         mat( 655) = mat( 655) + lmat( 655)
         mat( 661) = mat( 661) + lmat( 661)
         mat( 662) = mat( 662) + lmat( 662)
         mat( 664) = mat( 664) + lmat( 664)
         mat( 669) = mat( 669) + lmat( 669)
         mat( 680) = mat( 680) + lmat( 680)
         mat( 683) = mat( 683) + lmat( 683)
         mat( 689) = mat( 689) + lmat( 689)
         mat( 690) = lmat( 690)
         mat( 691) = lmat( 691)
         mat( 711) = mat( 711) + lmat( 711)
         mat( 731) = mat( 731) + lmat( 731)
         mat( 124) = 0._r8
         mat( 127) = 0._r8
         mat( 140) = 0._r8
         mat( 143) = 0._r8
         mat( 144) = 0._r8
         mat( 166) = 0._r8
         mat( 168) = 0._r8
         mat( 169) = 0._r8
         mat( 172) = 0._r8
         mat( 177) = 0._r8
         mat( 180) = 0._r8
         mat( 181) = 0._r8
         mat( 182) = 0._r8
         mat( 183) = 0._r8
         mat( 191) = 0._r8
         mat( 201) = 0._r8
         mat( 207) = 0._r8
         mat( 210) = 0._r8
         mat( 227) = 0._r8
         mat( 236) = 0._r8
         mat( 244) = 0._r8
         mat( 245) = 0._r8
         mat( 250) = 0._r8
         mat( 259) = 0._r8
         mat( 262) = 0._r8
         mat( 282) = 0._r8
         mat( 284) = 0._r8
         mat( 286) = 0._r8
         mat( 289) = 0._r8
         mat( 294) = 0._r8
         mat( 300) = 0._r8
         mat( 302) = 0._r8
         mat( 304) = 0._r8
         mat( 309) = 0._r8
         mat( 310) = 0._r8
         mat( 311) = 0._r8
         mat( 315) = 0._r8
         mat( 318) = 0._r8
         mat( 321) = 0._r8
         mat( 326) = 0._r8
         mat( 327) = 0._r8
         mat( 328) = 0._r8
         mat( 331) = 0._r8
         mat( 333) = 0._r8
         mat( 335) = 0._r8
         mat( 341) = 0._r8
         mat( 353) = 0._r8
         mat( 355) = 0._r8
         mat( 368) = 0._r8
         mat( 370) = 0._r8
         mat( 377) = 0._r8
         mat( 379) = 0._r8
         mat( 385) = 0._r8
         mat( 392) = 0._r8
         mat( 399) = 0._r8
         mat( 414) = 0._r8
         mat( 415) = 0._r8
         mat( 420) = 0._r8
         mat( 422) = 0._r8
         mat( 423) = 0._r8
         mat( 425) = 0._r8
         mat( 426) = 0._r8
         mat( 427) = 0._r8
         mat( 428) = 0._r8
         mat( 429) = 0._r8
         mat( 430) = 0._r8
         mat( 438) = 0._r8
         mat( 441) = 0._r8
         mat( 463) = 0._r8
         mat( 465) = 0._r8
         mat( 468) = 0._r8
         mat( 469) = 0._r8
         mat( 474) = 0._r8
         mat( 476) = 0._r8
         mat( 480) = 0._r8
         mat( 491) = 0._r8
         mat( 498) = 0._r8
         mat( 499) = 0._r8
         mat( 506) = 0._r8
         mat( 514) = 0._r8
         mat( 515) = 0._r8
         mat( 518) = 0._r8
         mat( 519) = 0._r8
         mat( 520) = 0._r8
         mat( 521) = 0._r8
         mat( 524) = 0._r8
         mat( 525) = 0._r8
         mat( 527) = 0._r8
         mat( 529) = 0._r8
         mat( 530) = 0._r8
         mat( 535) = 0._r8
         mat( 536) = 0._r8
         mat( 537) = 0._r8
         mat( 539) = 0._r8
         mat( 543) = 0._r8
         mat( 544) = 0._r8
         mat( 545) = 0._r8
         mat( 546) = 0._r8
         mat( 547) = 0._r8
         mat( 549) = 0._r8
         mat( 550) = 0._r8
         mat( 552) = 0._r8
         mat( 553) = 0._r8
         mat( 554) = 0._r8
         mat( 555) = 0._r8
         mat( 560) = 0._r8
         mat( 563) = 0._r8
         mat( 564) = 0._r8
         mat( 570) = 0._r8
         mat( 571) = 0._r8
         mat( 582) = 0._r8
         mat( 588) = 0._r8
         mat( 590) = 0._r8
         mat( 591) = 0._r8
         mat( 597) = 0._r8
         mat( 598) = 0._r8
         mat( 603) = 0._r8
         mat( 604) = 0._r8
         mat( 605) = 0._r8
         mat( 616) = 0._r8
         mat( 628) = 0._r8
         mat( 648) = 0._r8
         mat( 649) = 0._r8
         mat( 651) = 0._r8
         mat( 652) = 0._r8
         mat( 653) = 0._r8
         mat( 657) = 0._r8
         mat( 658) = 0._r8
         mat( 659) = 0._r8
         mat( 660) = 0._r8
         mat( 665) = 0._r8
         mat( 666) = 0._r8
         mat( 667) = 0._r8
         mat( 673) = 0._r8
         mat( 677) = 0._r8
         mat( 678) = 0._r8
         mat( 684) = 0._r8
         mat( 685) = 0._r8
         mat( 686) = 0._r8
         mat( 694) = 0._r8
         mat( 697) = 0._r8
         mat( 698) = 0._r8
         mat( 699) = 0._r8
         mat( 702) = 0._r8
         mat( 704) = 0._r8
         mat( 705) = 0._r8
         mat( 706) = 0._r8
         mat( 707) = 0._r8
         mat( 708) = 0._r8
         mat( 709) = 0._r8
         mat( 710) = 0._r8
         mat( 712) = 0._r8
         mat( 714) = 0._r8
         mat( 716) = 0._r8
         mat( 717) = 0._r8
         mat( 718) = 0._r8
         mat( 722) = 0._r8
         mat( 723) = 0._r8
         mat( 724) = 0._r8
         mat( 727) = 0._r8
         mat( 728) = 0._r8
         mat( 730) = 0._r8
         mat( 1) = mat( 1) - dti
         mat( 2) = mat( 2) - dti
         mat( 3) = mat( 3) - dti
         mat( 4) = mat( 4) - dti
         mat( 5) = mat( 5) - dti
         mat( 6) = mat( 6) - dti
         mat( 7) = mat( 7) - dti
         mat( 8) = mat( 8) - dti
         mat( 9) = mat( 9) - dti
         mat( 10) = mat( 10) - dti
         mat( 11) = mat( 11) - dti
         mat( 12) = mat( 12) - dti
         mat( 13) = mat( 13) - dti
         mat( 14) = mat( 14) - dti
         mat( 15) = mat( 15) - dti
         mat( 16) = mat( 16) - dti
         mat( 17) = mat( 17) - dti
         mat( 20) = mat( 20) - dti
         mat( 23) = mat( 23) - dti
         mat( 26) = mat( 26) - dti
         mat( 29) = mat( 29) - dti
         mat( 31) = mat( 31) - dti
         mat( 33) = mat( 33) - dti
         mat( 37) = mat( 37) - dti
         mat( 40) = mat( 40) - dti
         mat( 44) = mat( 44) - dti
         mat( 48) = mat( 48) - dti
         mat( 54) = mat( 54) - dti
         mat( 60) = mat( 60) - dti
         mat( 65) = mat( 65) - dti
         mat( 73) = mat( 73) - dti
         mat( 79) = mat( 79) - dti
         mat( 86) = mat( 86) - dti
         mat( 93) = mat( 93) - dti
         mat( 98) = mat( 98) - dti
         mat( 106) = mat( 106) - dti
         mat( 110) = mat( 110) - dti
         mat( 118) = mat( 118) - dti
         mat( 128) = mat( 128) - dti
         mat( 136) = mat( 136) - dti
         mat( 146) = mat( 146) - dti
         mat( 154) = mat( 154) - dti
         mat( 163) = mat( 163) - dti
         mat( 175) = mat( 175) - dti
         mat( 185) = mat( 185) - dti
         mat( 193) = mat( 193) - dti
         mat( 205) = mat( 205) - dti
         mat( 215) = mat( 215) - dti
         mat( 223) = mat( 223) - dti
         mat( 231) = mat( 231) - dti
         mat( 243) = mat( 243) - dti
         mat( 254) = mat( 254) - dti
         mat( 268) = mat( 268) - dti
         mat( 280) = mat( 280) - dti
         mat( 303) = mat( 303) - dti
         mat( 323) = mat( 323) - dti
         mat( 345) = mat( 345) - dti
         mat( 386) = mat( 386) - dti
         mat( 418) = mat( 418) - dti
         mat( 444) = mat( 444) - dti
         mat( 473) = mat( 473) - dti
         mat( 497) = mat( 497) - dti
         mat( 522) = mat( 522) - dti
         mat( 548) = mat( 548) - dti
         mat( 572) = mat( 572) - dti
         mat( 600) = mat( 600) - dti
         mat( 638) = mat( 638) - dti
         mat( 664) = mat( 664) - dti
         mat( 689) = mat( 689) - dti
         mat( 711) = mat( 711) - dti
         mat( 731) = mat( 731) - dti
      end subroutine nlnmat_finit
      subroutine nlnmat( mat, y, rxt, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: dti
      real(r8), intent(in) :: lmat(nzcnt)
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)
      call nlnmat01( mat, y, rxt )
      call nlnmat02( mat, y, rxt )
      call nlnmat03( mat, y, rxt )
      call nlnmat04( mat, y, rxt )
      call nlnmat_finit( mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix

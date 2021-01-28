




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

         mat(497) = -( rxt(3) + rxt(4) + het_rates(1) )

         mat(386) = -( rxt(66) + rxt(67) + rxt(68) + rxt(79) + rxt(80) + rxt(81) &
                 + het_rates(2) )
         mat(417) = rxt(1) + 2.000_r8*rxt(2) + rxt(72) + rxt(73) + rxt(74) &
                      + 2.000_r8*rxt(77) + rxt(84) + rxt(85) + rxt(86) + 2.000_r8*rxt(89)
         mat(493) = rxt(4)
         mat(565) = rxt(6)
         mat(592) = rxt(8)
         mat(56) = rxt(10)
         mat(654) = rxt(12)
         mat(542) = rxt(21)
         mat(470) = rxt(24)
         mat(63) = rxt(25)
         mat(305) = rxt(32)
         mat(225) = rxt(62)
         mat(46) = rxt(63)
         mat(257) = rxt(65)
         mat(346) = rxt(105)

         mat(345) = -( rxt(105) + rxt(109)*y(4) + rxt(110)*y(4) + rxt(112)*y(36) &
                      + rxt(113)*y(37) + rxt(114)*y(38) + rxt(115)*y(46) + rxt(116)*y(47) &
                      + rxt(117)*y(39) + rxt(118)*y(44) + rxt(119)*y(45) + rxt(120)*y(40) &
                      + rxt(121)*y(35) + rxt(122)*y(43) + rxt(123)*y(42) + rxt(124)*y(48) &
                      + rxt(125)*y(49) + rxt(126)*y(50) + rxt(127)*y(51) + rxt(130)*y(12) &
                      + rxt(131)*y(12) + rxt(132)*y(12) + het_rates(93) )
         mat(416) = rxt(1)
         mat(492) = rxt(3)
         mat(541) = rxt(20)

         mat(418) = -( rxt(1) + rxt(2) + rxt(70) + rxt(72) + rxt(73) + rxt(74) + rxt(77) &
                      + rxt(82) + rxt(84) + rxt(85) + rxt(86) + rxt(89) + het_rates(3) )
         mat(494) = rxt(4)
         mat(655) = rxt(13)
         mat(30) = rxt(100)
         mat(27) = rxt(103) + rxt(104)
         mat(347) = rxt(110)*y(4)

         mat(29) = -( rxt(97) + rxt(100) + rxt(99)*y(56) + het_rates(91) )

         mat(26) = -( rxt(103) + rxt(104) + het_rates(92) )
         mat(484) = rxt(3)
         mat(28) = rxt(97) + rxt(99)*y(56)

         mat(323) = -( het_rates(17) )
         mat(281) = rxt(18)
         mat(540) = rxt(20)
         mat(344) = rxt(132)*y(12)

         mat(106) = -( het_rates(16) )
         mat(276) = rxt(17) + rxt(18)
         mat(66) = rxt(64)
         mat(669) = rxt(225)*y(34)
         mat(135) = rxt(288)*y(56)

         mat(205) = -( rxt(69) + het_rates(5) )
         mat(559) = rxt(6)
         mat(139) = rxt(285)

         mat(572) = -( rxt(6) + rxt(7) + het_rates(6) )
         mat(599) = rxt(8) + .500_r8*rxt(248)
         mat(57) = rxt(10)
         mat(661) = rxt(13)
         mat(161) = rxt(295)
         mat(352) = 2.000_r8*rxt(109)*y(4)

         mat(600) = -( rxt(8) + rxt(248) + het_rates(7) )
         mat(58) = rxt(9) + rxt(168)
         mat(247) = rxt(11)
         mat(662) = rxt(12)
         mat(90) = rxt(15) + rxt(177)
         mat(237) = rxt(30)
         mat(103) = rxt(36)

         mat(638) = -( rxt(226)*y(34) + rxt(227)*y(41) + rxt(228)*y(39) + rxt(229)*y(35) &
                      + rxt(231)*y(44) + rxt(232)*y(45) + rxt(233)*y(51) + rxt(234)*y(50) &
                      + rxt(237)*y(12) + het_rates(82) )
         mat(248) = rxt(11)
         mat(91) = rxt(14)
         mat(77) = rxt(16)
         mat(551) = rxt(19)
         mat(115) = 2.000_r8*rxt(22)
         mat(220) = rxt(27)
         mat(190) = rxt(33)
         mat(601) = .500_r8*rxt(248)
         mat(354) = rxt(130)*y(12)

         mat(664) = -( rxt(12) + rxt(13) + rxt(247) + het_rates(8) )
         mat(59) = rxt(9) + rxt(10) + rxt(168)
         mat(92) = rxt(14)
         mat(239) = rxt(29)
         mat(104) = rxt(35)

         mat(243) = -( rxt(11) + het_rates(9) )
         mat(55) = 2.000_r8*rxt(246) + 2.000_r8*rxt(267) + 2.000_r8*rxt(273) &
                      + 2.000_r8*rxt(278)
         mat(647) = rxt(247)
         mat(586) = .500_r8*rxt(248)
         mat(232) = rxt(268) + rxt(274) + rxt(279)
         mat(100) = rxt(269) + rxt(277) + rxt(280)

         mat(86) = -( rxt(14) + rxt(15) + rxt(177) + het_rates(10) )

         mat(54) = -( rxt(9) + rxt(10) + rxt(168) + rxt(246) + rxt(267) + rxt(273) &
                      + rxt(278) + het_rates(11) )

         mat(731) = -( het_rates(13) )
         mat(358) = rxt(130)*y(12)
         mat(691) = rxt(184)*y(12)
         mat(153) = rxt(223)*y(12)
         mat(642) = rxt(237)*y(12)

         mat(73) = -( rxt(16) + het_rates(14) )

         mat(280) = -( rxt(17) + rxt(18) + het_rates(15) )
         mat(75) = rxt(16)
         mat(343) = rxt(131)*y(12) + rxt(132)*y(12)

         mat(268) = -( het_rates(18) )
         mat(74) = rxt(16)
         mat(279) = 2.000_r8*rxt(17)
         mat(538) = rxt(19) + 2.000_r8*rxt(21)
         mat(513) = rxt(28)
         mat(194) = rxt(34)
         mat(42) = rxt(57)
         mat(342) = rxt(131)*y(12)

         mat(444) = -( rxt(249) + het_rates(83) )
         mat(88) = rxt(15) + rxt(177)
         mat(348) = rxt(131)*y(12)
         mat(680) = rxt(225)*y(34) + rxt(230)*y(35)
         mat(631) = rxt(226)*y(34) + rxt(229)*y(35)

         mat(110) = -( rxt(22) + het_rates(19) )
         mat(433) = .500_r8*rxt(249)

         mat(548) = -( rxt(19) + rxt(20) + rxt(21) + het_rates(94) )
         mat(25) = rxt(61)
         mat(635) = rxt(226)*y(34) + rxt(227)*y(41) + rxt(228)*y(39) + rxt(229)*y(35) &
                      + rxt(233)*y(51) + rxt(237)*y(12)

         mat(689) = -( rxt(184)*y(12) + rxt(225)*y(34) + rxt(230)*y(35) + rxt(235)*y(51) &
                      + rxt(236)*y(50) + het_rates(80) )
         mat(32) = 2.000_r8*rxt(23)
         mat(481) = rxt(24)
         mat(19) = 2.000_r8*rxt(26)
         mat(221) = rxt(27)
         mat(528) = rxt(28)
         mat(240) = rxt(29)
         mat(38) = rxt(31)
         mat(36) = rxt(56)
         mat(356) = 2.000_r8*rxt(112)*y(36) + 2.000_r8*rxt(113)*y(37) &
                      + 2.000_r8*rxt(114)*y(38) + 2.000_r8*rxt(115)*y(46) + rxt(116)*y(47) &
                      + rxt(117)*y(39) + rxt(118)*y(44) + rxt(119)*y(45) &
                      + 4.000_r8*rxt(120)*y(40) + rxt(122)*y(43)
         mat(640) = rxt(226)*y(34) + 3.000_r8*rxt(227)*y(41) + rxt(228)*y(39) &
                      + rxt(231)*y(44) + rxt(232)*y(45)

         mat(31) = -( rxt(23) + het_rates(22) )

         mat(473) = -( rxt(24) + het_rates(23) )
         mat(64) = rxt(25)
         mat(234) = rxt(30)
         mat(18) = 2.000_r8*rxt(196)

         mat(60) = -( rxt(25) + het_rates(24) )

         mat(17) = -( rxt(26) + rxt(196) + het_rates(25) )

         mat(522) = -( rxt(28) + het_rates(26) )
         mat(683) = rxt(184)*y(12) + 2.000_r8*rxt(225)*y(34) + rxt(230)*y(35) &
                      + rxt(235)*y(51) + rxt(236)*y(50)

         mat(215) = -( rxt(27) + het_rates(27) )
         mat(230) = rxt(268) + rxt(274) + rxt(279)

         mat(231) = -( rxt(29) + rxt(30) + rxt(268) + rxt(274) + rxt(279) + het_rates(28) &
       )

         mat(37) = -( rxt(31) + het_rates(29) )

         mat(711) = -( het_rates(81) )
         mat(39) = rxt(31)
         mat(317) = rxt(32)
         mat(192) = rxt(33)
         mat(199) = rxt(34)
         mat(105) = rxt(35)
         mat(357) = rxt(121)*y(35) + rxt(122)*y(43) + rxt(123)*y(42) &
                      + 2.000_r8*rxt(124)*y(48) + 2.000_r8*rxt(125)*y(49) &
                      + 3.000_r8*rxt(126)*y(50) + 2.000_r8*rxt(127)*y(51)
         mat(641) = rxt(229)*y(35) + 2.000_r8*rxt(233)*y(51) + 3.000_r8*rxt(234)*y(50)
         mat(690) = rxt(230)*y(35) + 2.000_r8*rxt(235)*y(51) + 3.000_r8*rxt(236)*y(50)

         mat(303) = -( rxt(32) + het_rates(30) )
         mat(101) = rxt(36)

         mat(193) = -( rxt(34) + het_rates(31) )

         mat(185) = -( rxt(33) + het_rates(32) )
         mat(99) = rxt(269) + rxt(277) + rxt(280)

         mat(98) = -( rxt(35) + rxt(36) + rxt(269) + rxt(277) + rxt(280) + het_rates(33) &
       )

         mat(118) = -( het_rates(84) )

         mat(154) = -( rxt(295) + het_rates(85) )
         mat(407) = rxt(70) + rxt(82)
         mat(137) = rxt(288)*y(56)

         mat(79) = -( het_rates(86) )
         mat(200) = rxt(69)

         mat(136) = -( rxt(285) + rxt(288)*y(56) + het_rates(87) )
         mat(369) = rxt(66) + rxt(67) + rxt(68) + rxt(79) + rxt(80) + rxt(81)
         mat(406) = rxt(72) + rxt(73) + rxt(74) + rxt(84) + rxt(85) + rxt(86)

         mat(163) = -( het_rates(88) )
         mat(557) = rxt(7)
         mat(138) = rxt(285)
         mat(155) = rxt(295)

         mat(93) = -( het_rates(90) )

         mat(175) = -( het_rates(89) )
         mat(558) = rxt(7)
         mat(372) = rxt(66) + rxt(67) + rxt(68) + rxt(79) + rxt(80) + rxt(81)
         mat(204) = rxt(69)
         mat(409) = rxt(70) + rxt(72) + rxt(73) + rxt(74) + rxt(82) + rxt(84) + rxt(85) &
                      + rxt(86)

         mat(20) = -( rxt(55) + het_rates(52) )
         mat(336) = rxt(113)*y(37) + rxt(114)*y(38) + 2.000_r8*rxt(115)*y(46) &
                      + 2.000_r8*rxt(116)*y(47) + rxt(117)*y(39) + rxt(119)*y(45) &
                      + rxt(122)*y(43) + rxt(123)*y(42) + rxt(124)*y(48) &
                      + 2.000_r8*rxt(125)*y(49)
         mat(606) = rxt(228)*y(39) + rxt(232)*y(45)

         mat(33) = -( rxt(56) + het_rates(53) )
         mat(338) = rxt(112)*y(36) + rxt(114)*y(38) + rxt(118)*y(44)
         mat(607) = rxt(231)*y(44)

         mat(40) = -( rxt(57) + het_rates(54) )
         mat(145) = rxt(223)*y(12)

         mat(146) = -( rxt(223)*y(12) + het_rates(55) )
         mat(21) = 2.000_r8*rxt(55)
         mat(34) = rxt(56)
         mat(41) = rxt(57)
         mat(339) = rxt(116)*y(47) + rxt(123)*y(42)


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

         mat(65) = -( rxt(64) + het_rates(57) )

         mat(128) = -( het_rates(58) )
         mat(67) = rxt(64)
         mat(252) = rxt(65)

         mat(254) = -( rxt(65) + het_rates(59) )
         mat(224) = rxt(62)

         mat(223) = -( rxt(62) + het_rates(60) )
         mat(45) = rxt(63)

         mat(44) = -( rxt(63) + het_rates(61) )
         mat(24) = rxt(61)

         mat(23) = -( rxt(61) + het_rates(62) )

         mat(48) = -( het_rates(63) )

         mat(1) = -( het_rates(64) )

         mat(2) = -( het_rates(65) )

         mat(3) = -( het_rates(66) )

         mat(4) = -( het_rates(67) )

         mat(5) = -( het_rates(68) )

         mat(6) = -( het_rates(69) )

         mat(7) = -( het_rates(70) )

         mat(8) = -( het_rates(71) )

         mat(9) = -( het_rates(72) )

         mat(10) = -( het_rates(73) )

         mat(11) = -( het_rates(74) )

         mat(12) = -( het_rates(75) )

         mat(13) = -( het_rates(76) )

         mat(14) = -( het_rates(77) )

         mat(15) = -( het_rates(78) )

         mat(16) = -( het_rates(79) )


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

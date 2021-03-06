



      module mo_prod_loss

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: exp_prod_loss
      public :: imp_prod_loss

      contains

      subroutine exp_prod_loss( prod, loss, y, rxt, het_rates )

      use ppgrid, only : pver

      implicit none

!--------------------------------------------------------------------
! ... dummy args
!--------------------------------------------------------------------
      real(r8), dimension(:,:,:), intent(out) :: &
            prod, &
            loss
      real(r8), intent(in) :: y(:,:,:)
      real(r8), intent(in) :: rxt(:,:,:)
      real(r8), intent(in) :: het_rates(:,:,:)



!--------------------------------------------------------------------
! ... loss and production for Explicit method
!--------------------------------------------------------------------


         loss(:,:,1) = ((rxt(:,:,55) +rxt(:,:,56) +rxt(:,:,57))* y(:,:,3) +rxt(:,:,95) &
                 * y(:,:,12) + het_rates(:,:,16))* y(:,:,16)
         prod(:,:,1) = 0._r8
         loss(:,:,2) = ((rxt(:,:,53) +rxt(:,:,54))* y(:,:,3) + rxt(:,:,4) &
                  + het_rates(:,:,4))* y(:,:,4)
         prod(:,:,2) = 0._r8
         loss(:,:,3) = ( + rxt(:,:,229) + het_rates(:,:,115))* y(:,:,115)
         prod(:,:,3) = 0._r8
         loss(:,:,4) = ( + het_rates(:,:,116))* y(:,:,116)
         prod(:,:,4) =rxt(:,:,229)*y(:,:,115)

      end subroutine exp_prod_loss

      subroutine imp_prod_loss( prod, loss, y, rxt, het_rates )

      use ppgrid, only : pver

      implicit none

!--------------------------------------------------------------------
! ... dummy args
!--------------------------------------------------------------------
      real(r8), dimension(:), intent(out) :: &
            prod, &
            loss
      real(r8), intent(in) :: y(:)
      real(r8), intent(in) :: rxt(:)
      real(r8), intent(in) :: het_rates(:)



!--------------------------------------------------------------------
! ... loss and production for Implicit method
!--------------------------------------------------------------------


         loss(124) = (rxt(47)* y(2) +rxt(50)* y(3) +rxt(80)* y(5) +rxt(83)* y(6) &
                  +rxt(65)* y(12) +rxt(73)* y(13) +rxt(60)* y(15) +rxt(119)* y(23) &
                  +rxt(140)* y(35) +rxt(186)* y(52) +rxt(162)* y(55) +rxt(168)* y(56) &
                  +rxt(224)* y(65) + rxt(2) + rxt(3) + het_rates(1))* y(1)
         prod(124) = (.200_r8*rxt(162)*y(55) +.200_r8*rxt(168)*y(56) + &
                 .100_r8*rxt(186)*y(52))*y(1) + (.250_r8*rxt(130)*y(33) + &
                 .250_r8*rxt(178)*y(59))*y(13) +rxt(46)*y(2)
         loss(113) = (rxt(47)* y(1) +rxt(78)* y(5) + (rxt(81) +rxt(82))* y(6) +rxt(89) &
                 * y(7) +rxt(71)* y(11) +rxt(64)* y(12) +rxt(72)* y(13) +rxt(75) &
                 * y(14) +rxt(100)* y(20) +rxt(241)* y(100) + rxt(46) + het_rates(2)) &
                 * y(2)
         prod(113) = (rxt(48) +rxt(49))*y(3) +rxt(3)*y(1) +rxt(5)*y(6) +rxt(9)*y(7) &
                  +rxt(7)*y(10) +rxt(67)*y(12)*y(12) +rxt(63)*y(15)*y(13)
         loss(104) = (rxt(50)* y(1) + (rxt(53) +rxt(54))* y(4) +rxt(52)* y(11) &
                  + (rxt(55) +rxt(56) +rxt(57))* y(16) +rxt(58)* y(117) + rxt(48) &
                  + rxt(49) + rxt(51) + het_rates(3))* y(3)
         prod(104) =rxt(2)*y(1)
         loss(118) = ((rxt(96) +rxt(97))* y(12) + het_rates(17))* y(17)
         prod(118) = (.500_r8*rxt(119)*y(23) +.560_r8*rxt(140)*y(35) + &
                 .050_r8*rxt(162)*y(55) +.200_r8*rxt(168)*y(56) + &
                 .300_r8*rxt(186)*y(52))*y(1) + (rxt(99)*y(20) + &
                 .350_r8*rxt(112)*y(119) +rxt(135)*y(80) +rxt(156)*y(62) + &
                 rxt(242)*y(100))*y(12) + (rxt(98)*y(20) +rxt(157)*y(62) + &
                 .220_r8*rxt(171)*y(57) +.500_r8*rxt(205)*y(63))*y(7) + (rxt(14) + &
                 rxt(15) +rxt(100)*y(2))*y(20) + (.220_r8*rxt(169)*y(5) + &
                 .110_r8*rxt(173)*y(18) +.220_r8*rxt(174)*y(33))*y(57) &
                  + (.500_r8*rxt(204)*y(5) +.200_r8*rxt(207)*y(18) + &
                 .500_r8*rxt(208)*y(33))*y(63) + (rxt(45) +rxt(241)*y(2))*y(100) &
                  +rxt(35)*y(27) +rxt(17)*y(32) +.700_r8*rxt(24)*y(55) &
                  +1.340_r8*rxt(23)*y(56) +rxt(30)*y(62) +.450_r8*rxt(37)*y(79) &
                  +2.000_r8*rxt(38)*y(80) +rxt(32)*y(85)
         loss(88) = (rxt(71)* y(2) +rxt(52)* y(3) +rxt(69)* y(12) + het_rates(11)) &
                 * y(11)
         prod(88) =rxt(57)*y(16)*y(3) +rxt(62)*y(15)*y(13) +rxt(15)*y(20)
         loss(52) = (rxt(58)* y(3) +rxt(76)* y(12) + het_rates(117))* y(117)
         prod(52) = 0._r8
         loss(43) = (rxt(77)* y(12) + het_rates(118))* y(118)
         prod(43) = 0._r8
         loss(101) = (rxt(60)* y(1) + (rxt(61) +rxt(62) +rxt(63))* y(13) + rxt(59) &
                  + het_rates(15))* y(15)
         prod(101) = (rxt(64)*y(2) +rxt(69)*y(11) +rxt(96)*y(17) +rxt(99)*y(20) + &
                 rxt(242)*y(100))*y(12) + (rxt(56)*y(16) +rxt(52)*y(11))*y(3) &
                  +rxt(71)*y(11)*y(2) +rxt(13)*y(19)
         loss(126) = (rxt(80)* y(1) +rxt(78)* y(2) +rxt(88)* y(7) +rxt(79)* y(13) &
                  +rxt(102)* y(18) +rxt(115)* y(25) +rxt(121)* y(30) +rxt(128)* y(33) &
                  +rxt(142)* y(37) +rxt(147)* y(39) +rxt(151)* y(42) +rxt(160)* y(45) &
                  +rxt(164)* y(47) +rxt(201)* y(50) +rxt(188)* y(53) + (rxt(169) + &
                 rxt(170))* y(57) +rxt(176)* y(59) +rxt(204)* y(63) +rxt(226)* y(66) &
                  +rxt(212)* y(70) +rxt(219)* y(73) +rxt(222)* y(76) +rxt(194)* y(84) &
                  +rxt(110)* y(121) + het_rates(5))* y(5)
         prod(126) = (rxt(5) +.500_r8*rxt(232) +rxt(81)*y(2))*y(6) &
                  +2.000_r8*rxt(53)*y(4)*y(3) +rxt(10)*y(7) +rxt(7)*y(10)
         loss(120) = (rxt(83)* y(1) + (rxt(81) +rxt(82))* y(2) +rxt(84)* y(7) +rxt(86) &
                 * y(12) +rxt(92)* y(13) +rxt(129)* y(33) +rxt(182)* y(59) +rxt(216) &
                 * y(78) + rxt(5) + rxt(232) + het_rates(6))* y(6)
         prod(120) = (rxt(78)*y(2) +rxt(79)*y(13) +rxt(80)*y(1) + &
                 2.000_r8*rxt(88)*y(7) +rxt(102)*y(18) +rxt(110)*y(121) + &
                 rxt(115)*y(25) +rxt(121)*y(30) +rxt(128)*y(33) +rxt(142)*y(37) + &
                 rxt(147)*y(39) +rxt(151)*y(42) +rxt(160)*y(45) +rxt(164)*y(47) + &
                 rxt(169)*y(57) +rxt(176)*y(59) +.920_r8*rxt(188)*y(53) + &
                 1.206_r8*rxt(194)*y(84) +.900_r8*rxt(201)*y(50) +rxt(204)*y(63) + &
                 .900_r8*rxt(212)*y(70) +.900_r8*rxt(219)*y(73) + &
                 .900_r8*rxt(222)*y(76) +rxt(226)*y(66))*y(5) + (rxt(9) + &
                 rxt(89)*y(2) +rxt(90)*y(12) +rxt(91)*y(13) +rxt(171)*y(57) + &
                 rxt(177)*y(59) +rxt(189)*y(53) +1.206_r8*rxt(195)*y(84) + &
                 rxt(199)*y(85) +rxt(205)*y(63) +rxt(225)*y(65))*y(7) + (rxt(12) + &
                 rxt(94) +rxt(93)*y(12))*y(9) + (rxt(6) +rxt(85))*y(10) &
                  + (rxt(158)*y(82) +.400_r8*rxt(198)*y(85))*y(12) &
                  + (.600_r8*rxt(20) +rxt(137))*y(81) + (rxt(21) +rxt(183))*y(83) &
                  +.700_r8*rxt(216)*y(78)*y(6) +rxt(8)*y(8) +.206_r8*rxt(196)*y(84) &
                 *y(13) +rxt(32)*y(85)
         loss(122) = (rxt(89)* y(2) +rxt(88)* y(5) +rxt(84)* y(6) +rxt(90)* y(12) &
                  +rxt(91)* y(13) +rxt(98)* y(20) +rxt(127)* y(32) +rxt(141)* y(35) &
                  +rxt(187)* y(52) +rxt(189)* y(53) +rxt(171)* y(57) +rxt(177)* y(59) &
                  +rxt(157)* y(62) +rxt(205)* y(63) +rxt(225)* y(65) +rxt(195)* y(84) &
                  +rxt(199)* y(85) +rxt(237)* y(102) + rxt(9) + rxt(10) + rxt(231) &
                  + het_rates(7))* y(7)
         prod(122) = (rxt(6) +rxt(7) +rxt(85))*y(10) + (rxt(87)*y(8) +rxt(138)*y(81) + &
                 .500_r8*rxt(184)*y(83))*y(12) + (rxt(82)*y(2) +rxt(83)*y(1))*y(6) &
                  +rxt(11)*y(9) +.400_r8*rxt(20)*y(81)
         loss(80) = (rxt(87)* y(12) + rxt(8) + het_rates(8))* y(8)
         prod(80) = (rxt(231) +rxt(98)*y(20) +rxt(127)*y(32) +rxt(157)*y(62) + &
                 rxt(237)*y(102))*y(7) + (.500_r8*rxt(232) +rxt(86)*y(12))*y(6) &
                  +2.000_r8*rxt(230)*y(10)
         loss(63) = (rxt(93)* y(12) + rxt(11) + rxt(12) + rxt(94) + het_rates(9)) &
                 * y(9)
         prod(63) =rxt(92)*y(13)*y(6)
         loss(59) = ( + rxt(6) + rxt(7) + rxt(85) + rxt(230) + het_rates(10))* y(10)
         prod(59) =rxt(84)*y(7)*y(6)
         loss(121) = (rxt(65)* y(1) +rxt(64)* y(2) +rxt(86)* y(6) +rxt(90)* y(7) &
                  +rxt(87)* y(8) +rxt(93)* y(9) +rxt(69)* y(11) + 2._r8*(rxt(67) + &
                 rxt(68))* y(12) +rxt(66)* y(13) +rxt(70)* y(14) +rxt(95)* y(16) &
                  + (rxt(96) +rxt(97))* y(17) +rxt(107)* y(19) +rxt(99)* y(20) &
                  +rxt(106)* y(21) +rxt(136)* y(22) +rxt(114)* y(23) +rxt(120)* y(26) &
                  +rxt(134)* y(27) +rxt(113)* y(29) +rxt(125)* y(31) +rxt(126)* y(32) &
                  +rxt(133)* y(34) +rxt(139)* y(35) +rxt(146)* y(36) +rxt(145)* y(38) &
                  +rxt(149)* y(40) +rxt(150)* y(41) +rxt(154)* y(43) +rxt(159)* y(44) &
                  +rxt(163)* y(46) +rxt(166)* y(48) +rxt(197)* y(49) +rxt(203)* y(51) &
                  +rxt(185)* y(52) +rxt(191)* y(54) +rxt(161)* y(55) +rxt(167)* y(56) &
                  +rxt(175)* y(58) +rxt(200)* y(60) +rxt(155)* y(61) +rxt(156)* y(62) &
                  +rxt(209)* y(64) +rxt(223)* y(65) +rxt(228)* y(67) +rxt(211)* y(68) &
                  +rxt(215)* y(69) +rxt(214)* y(71) +rxt(217)* y(72) +rxt(220)* y(75) &
                  +rxt(135)* y(80) +rxt(138)* y(81) +rxt(158)* y(82) +rxt(184)* y(83) &
                  +rxt(198)* y(85) +rxt(242)* y(100) +rxt(234)* y(101) + (rxt(235) + &
                 rxt(236))* y(102) +rxt(238)* y(104) +rxt(76)* y(117) +rxt(77)* y(118) &
                  +rxt(112)* y(119) +rxt(108)* y(120) + het_rates(12))* y(12)
         prod(121) = (rxt(60)*y(15) +rxt(73)*y(13) +.120_r8*rxt(119)*y(23) + &
                 .330_r8*rxt(140)*y(35) +.080_r8*rxt(162)*y(55) + &
                 .215_r8*rxt(168)*y(56) +.270_r8*rxt(186)*y(52) + &
                 .700_r8*rxt(224)*y(65))*y(1) + (.300_r8*rxt(107)*y(19) + &
                 .650_r8*rxt(112)*y(119) +.500_r8*rxt(125)*y(31) + &
                 .500_r8*rxt(149)*y(40) +.100_r8*rxt(175)*y(58))*y(12) &
                  + (rxt(71)*y(11) +rxt(72)*y(13) +rxt(75)*y(14) +rxt(100)*y(20))*y(2) &
                  + (2.000_r8*rxt(51) +rxt(55)*y(16) +rxt(52)*y(11) +rxt(58)*y(117)) &
                 *y(3) + (2.000_r8*rxt(61)*y(15) +rxt(79)*y(5) +rxt(91)*y(7))*y(13) &
                  +.500_r8*rxt(232)*y(6) +rxt(8)*y(8) +rxt(11)*y(9) +2.000_r8*rxt(16) &
                 *y(14) +rxt(13)*y(19) +rxt(26)*y(28) +rxt(25)*y(31) +rxt(19)*y(34) &
                  +rxt(27)*y(38) +rxt(18)*y(40) +rxt(28)*y(43) +rxt(40)*y(48) +rxt(39) &
                 *y(51) +rxt(31)*y(64) +rxt(42)*y(67) +rxt(41)*y(71)
         loss(125) = (rxt(73)* y(1) +rxt(72)* y(2) +rxt(79)* y(5) +rxt(92)* y(6) &
                  +rxt(91)* y(7) +rxt(66)* y(12) + 2._r8*rxt(74)* y(13) + (rxt(61) + &
                 rxt(62) +rxt(63))* y(15) +rxt(103)* y(18) +rxt(101)* y(20) +rxt(116) &
                 * y(25) +rxt(122)* y(30) +rxt(130)* y(33) +rxt(143)* y(37) +rxt(148) &
                 * y(39) +rxt(152)* y(42) +rxt(165)* y(47) +rxt(202)* y(50) +rxt(190) &
                 * y(53) +rxt(172)* y(57) +rxt(178)* y(59) +rxt(206)* y(63) +rxt(227) &
                 * y(66) +rxt(213)* y(70) +rxt(218)* y(73) +rxt(221)* y(76) +rxt(196) &
                 * y(84) +rxt(111)* y(121) + rxt(240) + het_rates(13))* y(13)
         prod(125) = (rxt(65)*y(1) +rxt(70)*y(14) +rxt(76)*y(117) +rxt(77)*y(118) + &
                 rxt(90)*y(7) +rxt(97)*y(17) +rxt(106)*y(21) +rxt(108)*y(120) + &
                 .350_r8*rxt(112)*y(119) +rxt(134)*y(27) +rxt(135)*y(80) + &
                 rxt(136)*y(22) +rxt(155)*y(61) +.200_r8*rxt(175)*y(58) + &
                 .500_r8*rxt(184)*y(83) +rxt(198)*y(85) +.250_r8*rxt(211)*y(68) + &
                 .500_r8*rxt(236)*y(102))*y(12) + (rxt(102)*y(18) +rxt(110)*y(121) + &
                 .250_r8*rxt(115)*y(25) +rxt(121)*y(30) +rxt(142)*y(37) + &
                 rxt(147)*y(39) +rxt(160)*y(45) +.470_r8*rxt(169)*y(57) + &
                 rxt(188)*y(53) +.794_r8*rxt(194)*y(84) +.900_r8*rxt(201)*y(50) + &
                 rxt(204)*y(63) +.900_r8*rxt(212)*y(70) +.900_r8*rxt(219)*y(73) + &
                 .900_r8*rxt(222)*y(76) +rxt(226)*y(66))*y(5) &
                  + (2.000_r8*rxt(104)*y(18) +rxt(123)*y(30) +.900_r8*rxt(131)*y(33) + &
                 rxt(144)*y(37) +.300_r8*rxt(153)*y(42) +.730_r8*rxt(173)*y(57) + &
                 rxt(179)*y(59) +rxt(192)*y(53) +.800_r8*rxt(207)*y(63))*y(18) &
                  + (.120_r8*rxt(119)*y(23) +.190_r8*rxt(140)*y(35) + &
                 .060_r8*rxt(162)*y(55) +.275_r8*rxt(168)*y(56) + &
                 .060_r8*rxt(186)*y(52) +rxt(224)*y(65))*y(1) + (rxt(98)*y(20) + &
                 .470_r8*rxt(171)*y(57) +rxt(189)*y(53) +.794_r8*rxt(195)*y(84) + &
                 rxt(199)*y(85) +rxt(205)*y(63))*y(7) + (.470_r8*rxt(174)*y(57) + &
                 rxt(193)*y(53) +rxt(208)*y(63))*y(33) + (rxt(75)*y(14) + &
                 rxt(100)*y(20))*y(2) + (rxt(12) +rxt(94))*y(9) + (rxt(117) +rxt(118)) &
                 *y(24) + (1.340_r8*rxt(22) +.660_r8*rxt(23))*y(56) +rxt(56)*y(16) &
                 *y(3) +.700_r8*rxt(216)*y(78)*y(6) +.794_r8*rxt(196)*y(84)*y(13) &
                  +rxt(59)*y(15) +2.000_r8*rxt(35)*y(27) +1.200_r8*rxt(124)*y(30) &
                 *y(30) +rxt(25)*y(31) +rxt(17)*y(32) +rxt(27)*y(38) +rxt(18)*y(40) &
                  +.900_r8*rxt(39)*y(51) +rxt(33)*y(54) +rxt(34)*y(61) +rxt(30)*y(62) &
                  +rxt(42)*y(67) +.560_r8*rxt(37)*y(79) +2.000_r8*rxt(38)*y(80) &
                  +rxt(32)*y(85) +rxt(109)*y(121)
         loss(56) = (rxt(75)* y(2) +rxt(70)* y(12) + rxt(16) + het_rates(14))* y(14)
         prod(56) = (.500_r8*rxt(240) +rxt(74)*y(13))*y(13) +rxt(68)*y(12)*y(12)
         loss(127) = (rxt(102)* y(5) +rxt(103)* y(13) + 2._r8*(rxt(104) +rxt(105)) &
                 * y(18) +rxt(123)* y(30) +rxt(131)* y(33) +rxt(144)* y(37) +rxt(153) &
                 * y(42) +rxt(192)* y(53) +rxt(173)* y(57) +rxt(179)* y(59) +rxt(207) &
                 * y(63) + het_rates(18))* y(18)
         prod(127) = (rxt(128)*y(5) +.900_r8*rxt(131)*y(18) +2.000_r8*rxt(132)*y(33) + &
                 rxt(174)*y(57) +rxt(180)*y(59) +rxt(193)*y(53) +rxt(208)*y(63))*y(33) &
                  + (rxt(95)*y(16) +.700_r8*rxt(107)*y(19) +rxt(120)*y(26))*y(12) &
                  +.310_r8*rxt(140)*y(35)*y(1) +rxt(55)*y(16)*y(3) +rxt(17)*y(32) &
                  +rxt(19)*y(34) +rxt(29)*y(41) +.300_r8*rxt(24)*y(55) &
                  +.400_r8*rxt(20)*y(81)
         loss(64) = (rxt(107)* y(12) + rxt(13) + het_rates(19))* y(19)
         prod(64) =rxt(103)*y(18)*y(13)
         loss(117) = (rxt(100)* y(2) +rxt(98)* y(7) +rxt(99)* y(12) +rxt(101)* y(13) &
                  + rxt(14) + rxt(15) + het_rates(20))* y(20)
         prod(117) = (rxt(102)*y(5) +2.000_r8*rxt(104)*y(18) +rxt(105)*y(18) + &
                 .700_r8*rxt(123)*y(30) +rxt(131)*y(33) +rxt(144)*y(37) + &
                 .800_r8*rxt(153)*y(42) +.880_r8*rxt(173)*y(57) + &
                 2.000_r8*rxt(179)*y(59) +1.200_r8*rxt(192)*y(53) + &
                 .700_r8*rxt(207)*y(63))*y(18) + (.500_r8*rxt(115)*y(25) + &
                 rxt(147)*y(39) +rxt(151)*y(42) +.500_r8*rxt(160)*y(45) + &
                 .250_r8*rxt(169)*y(57) +rxt(176)*y(59) +.510_r8*rxt(188)*y(53) + &
                 .072_r8*rxt(194)*y(84) +.100_r8*rxt(201)*y(50))*y(5) &
                  + (rxt(106)*y(21) +.300_r8*rxt(107)*y(19) +.500_r8*rxt(133)*y(34) + &
                 .800_r8*rxt(134)*y(27) +rxt(138)*y(81) +.500_r8*rxt(184)*y(83))*y(12) &
                  + (rxt(119)*y(23) +.540_r8*rxt(140)*y(35) +.800_r8*rxt(162)*y(55) + &
                 .700_r8*rxt(168)*y(56) +.600_r8*rxt(186)*y(52))*y(1) &
                  + (.250_r8*rxt(171)*y(57) +rxt(177)*y(59) +.600_r8*rxt(189)*y(53) + &
                 .072_r8*rxt(195)*y(84))*y(7) + (.250_r8*rxt(174)*y(57) + &
                 rxt(180)*y(59) +.600_r8*rxt(193)*y(53))*y(33) + (rxt(56)*y(16) + &
                 rxt(57)*y(16))*y(3) +.008_r8*rxt(196)*y(84)*y(13) +rxt(13)*y(19) &
                  +2.000_r8*rxt(118)*y(24) +rxt(35)*y(27) +rxt(18)*y(40) +rxt(28) &
                 *y(43) +.100_r8*rxt(39)*y(51) +.690_r8*rxt(33)*y(54) &
                  +1.340_r8*rxt(22)*y(56) +2.000_r8*rxt(181)*y(59)*y(59) +rxt(34) &
                 *y(61) +rxt(32)*y(85) +rxt(109)*y(121)
         loss(85) = (rxt(106)* y(12) + het_rates(21))* y(21)
         prod(85) = (rxt(105)*y(18) +.300_r8*rxt(123)*y(30) +.500_r8*rxt(153)*y(42) + &
                 .250_r8*rxt(173)*y(57) +.250_r8*rxt(192)*y(53) + &
                 .300_r8*rxt(207)*y(63))*y(18)
         loss(57) = (rxt(136)* y(12) + het_rates(22))* y(22)
         prod(57) = (.200_r8*rxt(123)*y(18) +.400_r8*rxt(124)*y(30))*y(30)
         loss(49) = ( + rxt(26) + het_rates(28))* y(28)
         prod(49) =rxt(116)*y(25)*y(13)
         loss(66) = (rxt(119)* y(1) +rxt(114)* y(12) + het_rates(23))* y(23)
         prod(66) = 0._r8
         loss(58) = ( + rxt(117) + rxt(118) + het_rates(24))* y(24)
         prod(58) =.750_r8*rxt(115)*y(25)*y(5) +rxt(26)*y(28)
         loss(90) = (rxt(115)* y(5) +rxt(116)* y(13) + het_rates(25))* y(25)
         prod(90) =rxt(114)*y(23)*y(12)
         loss(76) = (rxt(120)* y(12) + het_rates(26))* y(26)
         prod(76) = (.250_r8*rxt(140)*y(35) +.200_r8*rxt(186)*y(52))*y(1) &
                  + (.250_r8*rxt(130)*y(33) +.250_r8*rxt(178)*y(59))*y(13) &
                  +.100_r8*rxt(131)*y(33)*y(18)
         loss(98) = (rxt(134)* y(12) + rxt(35) + het_rates(27))* y(27)
         prod(98) = (.530_r8*rxt(169)*y(5) +.530_r8*rxt(171)*y(7) + &
                 .260_r8*rxt(173)*y(18) +.530_r8*rxt(174)*y(33))*y(57) &
                  + (.250_r8*rxt(204)*y(5) +.250_r8*rxt(205)*y(7) + &
                 .100_r8*rxt(207)*y(18) +.250_r8*rxt(208)*y(33))*y(63) +rxt(117)*y(24)
         loss(44) = (rxt(113)* y(12) + het_rates(29))* y(29)
         prod(44) = 0._r8
         loss(97) = (rxt(121)* y(5) +rxt(122)* y(13) +rxt(123)* y(18) + 2._r8*rxt(124) &
                 * y(30) + het_rates(30))* y(30)
         prod(97) = (rxt(113)*y(29) +.500_r8*rxt(125)*y(31))*y(12) +rxt(36)*y(46)
         loss(68) = (rxt(125)* y(12) + rxt(25) + het_rates(31))* y(31)
         prod(68) =rxt(122)*y(30)*y(13)
         loss(106) = (rxt(127)* y(7) +rxt(126)* y(12) + rxt(17) + het_rates(32)) &
                 * y(32)
         prod(106) = (rxt(121)*y(30) +.270_r8*rxt(142)*y(37) +rxt(147)*y(39) + &
                 rxt(160)*y(45) +rxt(164)*y(47) +.400_r8*rxt(201)*y(50))*y(5) &
                  + (.500_r8*rxt(140)*y(35) +.040_r8*rxt(162)*y(55))*y(1) &
                  + (.500_r8*rxt(125)*y(31) +rxt(136)*y(22))*y(12) &
                  + (.800_r8*rxt(123)*y(18) +1.600_r8*rxt(124)*y(30))*y(30) +rxt(25) &
                 *y(31) +rxt(18)*y(40) +rxt(40)*y(48) +.400_r8*rxt(39)*y(51)
         loss(123) = (rxt(128)* y(5) +rxt(129)* y(6) +rxt(130)* y(13) +rxt(131)* y(18) &
                  + 2._r8*rxt(132)* y(33) +rxt(193)* y(53) +rxt(174)* y(57) +rxt(208) &
                 * y(63) + het_rates(33))* y(33)
         prod(123) = (rxt(151)*y(42) +rxt(164)*y(47) +.530_r8*rxt(169)*y(57) + &
                 rxt(176)*y(59))*y(5) + (rxt(127)*y(32) +rxt(157)*y(62) + &
                 .530_r8*rxt(171)*y(57) +rxt(177)*y(59))*y(7) + (rxt(126)*y(32) + &
                 .500_r8*rxt(133)*y(34) +rxt(156)*y(62))*y(12) &
                  + (.300_r8*rxt(153)*y(42) +.260_r8*rxt(173)*y(57) +rxt(179)*y(59)) &
                 *y(18) + (.600_r8*rxt(20) +rxt(137))*y(81) +.530_r8*rxt(174)*y(57) &
                 *y(33) +rxt(29)*y(41) +rxt(28)*y(43) +rxt(36)*y(46) +rxt(40)*y(48) &
                  +.300_r8*rxt(24)*y(55) +1.340_r8*rxt(22)*y(56) &
                  +2.000_r8*rxt(181)*y(59)*y(59) +rxt(34)*y(61) +rxt(30)*y(62) &
                  +.130_r8*rxt(37)*y(79)
         loss(77) = (rxt(133)* y(12) + rxt(19) + het_rates(34))* y(34)
         prod(77) = (.750_r8*rxt(130)*y(33) +.750_r8*rxt(178)*y(59))*y(13)
         loss(103) = (rxt(140)* y(1) +rxt(141)* y(7) +rxt(139)* y(12) + het_rates(35)) &
                 * y(35)
         prod(103) =.070_r8*rxt(186)*y(52)*y(1) +.700_r8*rxt(24)*y(55)
         loss(45) = (rxt(146)* y(12) + het_rates(36))* y(36)
         prod(45) = 0._r8
         loss(99) = (rxt(142)* y(5) +rxt(143)* y(13) +rxt(144)* y(18) + het_rates(37)) &
                 * y(37)
         prod(99) = (rxt(145)*y(38) +rxt(146)*y(36))*y(12)
         loss(61) = (rxt(145)* y(12) + rxt(27) + het_rates(38))* y(38)
         prod(61) =rxt(143)*y(37)*y(13)
         loss(95) = (rxt(147)* y(5) +rxt(148)* y(13) + het_rates(39))* y(39)
         prod(95) = (rxt(139)*y(35) +.500_r8*rxt(149)*y(40))*y(12)
         loss(82) = (rxt(149)* y(12) + rxt(18) + het_rates(40))* y(40)
         prod(82) =rxt(148)*y(39)*y(13)
         loss(94) = (rxt(150)* y(12) + rxt(29) + het_rates(41))* y(41)
         prod(94) = (.820_r8*rxt(142)*y(37) +.500_r8*rxt(160)*y(45) + &
                 .250_r8*rxt(201)*y(50) +.100_r8*rxt(226)*y(66))*y(5) &
                  +.820_r8*rxt(144)*y(37)*y(18) +.820_r8*rxt(27)*y(38) &
                  +.250_r8*rxt(39)*y(51) +.100_r8*rxt(42)*y(67)
         loss(108) = (rxt(151)* y(5) +rxt(152)* y(13) +rxt(153)* y(18) &
                  + het_rates(42))* y(42)
         prod(108) = (rxt(150)*y(41) +rxt(154)*y(43))*y(12)
         loss(62) = (rxt(154)* y(12) + rxt(28) + het_rates(43))* y(43)
         prod(62) =rxt(152)*y(42)*y(13)
         loss(39) = (rxt(159)* y(12) + het_rates(44))* y(44)
         prod(39) = 0._r8
         loss(69) = (rxt(160)* y(5) + het_rates(45))* y(45)
         prod(69) =rxt(159)*y(44)*y(12)
         loss(40) = (rxt(197)* y(12) + het_rates(49))* y(49)
         prod(40) = 0._r8
         loss(96) = (rxt(201)* y(5) +rxt(202)* y(13) + het_rates(50))* y(50)
         prod(96) = (rxt(197)*y(49) +rxt(203)*y(51))*y(12)
         loss(92) = (rxt(203)* y(12) + rxt(39) + het_rates(51))* y(51)
         prod(92) =rxt(202)*y(50)*y(13)
         loss(75) = (rxt(163)* y(12) + rxt(36) + het_rates(46))* y(46)
         prod(75) =.800_r8*rxt(201)*y(50)*y(5) +.800_r8*rxt(39)*y(51)
         loss(93) = (rxt(164)* y(5) +rxt(165)* y(13) + het_rates(47))* y(47)
         prod(93) = (rxt(163)*y(46) +rxt(166)*y(48))*y(12)
         loss(60) = (rxt(166)* y(12) + rxt(40) + het_rates(48))* y(48)
         prod(60) =rxt(165)*y(47)*y(13)
         loss(100) = (rxt(186)* y(1) +rxt(187)* y(7) +rxt(185)* y(12) + het_rates(52)) &
                 * y(52)
         prod(100) = 0._r8
         loss(116) = (rxt(188)* y(5) +rxt(189)* y(7) +rxt(190)* y(13) +rxt(192)* y(18) &
                  +rxt(193)* y(33) + het_rates(53))* y(53)
         prod(116) = (rxt(185)*y(52) +.200_r8*rxt(191)*y(54))*y(12)
         loss(84) = (rxt(191)* y(12) + rxt(33) + het_rates(54))* y(54)
         prod(84) =rxt(190)*y(53)*y(13)
         loss(114) = (rxt(162)* y(1) +rxt(161)* y(12) + rxt(24) + het_rates(55)) &
                 * y(55)
         prod(114) = (.320_r8*rxt(188)*y(5) +.350_r8*rxt(189)*y(7) + &
                 .260_r8*rxt(192)*y(18) +.350_r8*rxt(193)*y(33))*y(53) &
                  + (.039_r8*rxt(194)*y(5) +.039_r8*rxt(195)*y(7) + &
                 .039_r8*rxt(196)*y(13))*y(84) + (.200_r8*rxt(186)*y(52) + &
                 rxt(224)*y(65))*y(1) +rxt(226)*y(66)*y(5) +.402_r8*rxt(33)*y(54) &
                  +rxt(42)*y(67)
         loss(110) = (rxt(168)* y(1) +rxt(167)* y(12) + rxt(22) + rxt(23) &
                  + het_rates(56))* y(56)
         prod(110) = (.230_r8*rxt(188)*y(5) +.250_r8*rxt(189)*y(7) + &
                 .190_r8*rxt(192)*y(18) +.250_r8*rxt(193)*y(33))*y(53) &
                  + (.167_r8*rxt(194)*y(5) +.167_r8*rxt(195)*y(7) + &
                 .167_r8*rxt(196)*y(13))*y(84) + (.400_r8*rxt(186)*y(52) + &
                 rxt(224)*y(65))*y(1) +rxt(226)*y(66)*y(5) +.288_r8*rxt(33)*y(54) &
                  +rxt(42)*y(67)
         loss(115) = ((rxt(169) +rxt(170))* y(5) +rxt(171)* y(7) +rxt(172)* y(13) &
                  +rxt(173)* y(18) +rxt(174)* y(33) + het_rates(57))* y(57)
         prod(115) = (rxt(161)*y(55) +.500_r8*rxt(167)*y(56) +.200_r8*rxt(175)*y(58)) &
                 *y(12)
         loss(67) = (rxt(175)* y(12) + het_rates(58))* y(58)
         prod(67) =rxt(172)*y(57)*y(13)
         loss(119) = (rxt(176)* y(5) +rxt(182)* y(6) +rxt(177)* y(7) +rxt(178)* y(13) &
                  +rxt(179)* y(18) +rxt(180)* y(33) + 2._r8*rxt(181)* y(59) &
                  + het_rates(59))* y(59)
         prod(119) = (.500_r8*rxt(167)*y(56) +.500_r8*rxt(175)*y(58))*y(12) &
                  + (rxt(21) +rxt(183))*y(83) +.200_r8*rxt(186)*y(52)*y(1) &
                  +.660_r8*rxt(22)*y(56)
         loss(70) = (rxt(200)* y(12) + het_rates(60))* y(60)
         prod(70) = (.370_r8*rxt(188)*y(5) +.400_r8*rxt(189)*y(7) + &
                 .300_r8*rxt(192)*y(18) +.400_r8*rxt(193)*y(33))*y(53) &
                  + (rxt(198)*y(12) +rxt(199)*y(7))*y(85)
         loss(109) = (rxt(155)* y(12) + rxt(34) + het_rates(61))* y(61)
         prod(109) = (.220_r8*rxt(169)*y(5) +.220_r8*rxt(171)*y(7) + &
                 .230_r8*rxt(173)*y(18) +.220_r8*rxt(174)*y(33))*y(57) &
                  + (.250_r8*rxt(204)*y(5) +.250_r8*rxt(205)*y(7) + &
                 .100_r8*rxt(207)*y(18) +.250_r8*rxt(208)*y(33))*y(63) &
                  + (.500_r8*rxt(149)*y(40) +.500_r8*rxt(184)*y(83))*y(12) &
                  +.200_r8*rxt(153)*y(42)*y(18)
         loss(111) = (rxt(157)* y(7) +rxt(156)* y(12) + rxt(30) + het_rates(62)) &
                 * y(62)
         prod(111) = (.250_r8*rxt(169)*y(57) +.250_r8*rxt(204)*y(63) + &
                 .450_r8*rxt(212)*y(70) +.540_r8*rxt(222)*y(76))*y(5) &
                  + (.500_r8*rxt(153)*y(42) +.240_r8*rxt(173)*y(57) + &
                 .100_r8*rxt(207)*y(63))*y(18) + (.950_r8*rxt(162)*y(55) + &
                 .800_r8*rxt(168)*y(56))*y(1) + (.250_r8*rxt(171)*y(57) + &
                 .250_r8*rxt(205)*y(63))*y(7) + (rxt(155)*y(61) +rxt(158)*y(82))*y(12) &
                  + (.250_r8*rxt(174)*y(57) +.250_r8*rxt(208)*y(63))*y(33) &
                  +.450_r8*rxt(41)*y(71) +.180_r8*rxt(37)*y(79)
         loss(112) = (rxt(204)* y(5) +rxt(205)* y(7) +rxt(206)* y(13) +rxt(207)* y(18) &
                  +rxt(208)* y(33) + het_rates(63))* y(63)
         prod(112) = (.800_r8*rxt(191)*y(54) +rxt(200)*y(60) +rxt(209)*y(64))*y(12)
         loss(53) = ((rxt(209) +rxt(210))* y(12) + rxt(31) + het_rates(64))* y(64)
         prod(53) = (rxt(196)*y(84) +rxt(206)*y(63))*y(13)
         loss(89) = (rxt(224)* y(1) +rxt(225)* y(7) +rxt(223)* y(12) + het_rates(65)) &
                 * y(65)
         prod(89) = 0._r8
         loss(102) = (rxt(226)* y(5) +rxt(227)* y(13) + het_rates(66))* y(66)
         prod(102) = (rxt(223)*y(65) +rxt(228)*y(67))*y(12) +rxt(225)*y(65)*y(7)
         loss(81) = (rxt(228)* y(12) + rxt(42) + het_rates(67))* y(67)
         prod(81) =rxt(227)*y(66)*y(13)
         loss(46) = (rxt(211)* y(12) + het_rates(68))* y(68)
         prod(46) = 0._r8
         loss(47) = (rxt(215)* y(12) + het_rates(69))* y(69)
         prod(47) =.250_r8*rxt(211)*y(68)*y(12)
         loss(86) = (rxt(212)* y(5) +rxt(213)* y(13) + het_rates(70))* y(70)
         prod(86) = (.700_r8*rxt(211)*y(68) +rxt(214)*y(71))*y(12)
         loss(71) = (rxt(214)* y(12) + rxt(41) + het_rates(71))* y(71)
         prod(71) =rxt(213)*y(70)*y(13)
         loss(51) = (rxt(216)* y(6) + het_rates(78))* y(78)
         prod(51) =rxt(215)*y(69)*y(12)
         loss(87) = ( + rxt(37) + het_rates(79))* y(79)
         prod(87) = (.900_r8*rxt(212)*y(70) +.900_r8*rxt(219)*y(73) + &
                 .620_r8*rxt(222)*y(76))*y(5) +.700_r8*rxt(216)*y(78)*y(6) &
                  +.900_r8*rxt(41)*y(71)
         loss(91) = (rxt(135)* y(12) + rxt(38) + het_rates(80))* y(80)
         prod(91) = (.250_r8*rxt(204)*y(63) +.450_r8*rxt(212)*y(70) + &
                 .900_r8*rxt(219)*y(73) +.340_r8*rxt(222)*y(76))*y(5) &
                  + (.250_r8*rxt(205)*y(7) +.100_r8*rxt(207)*y(18) + &
                 .250_r8*rxt(208)*y(33))*y(63) + (.650_r8*rxt(112)*y(119) + &
                 .200_r8*rxt(134)*y(27))*y(12) +.450_r8*rxt(41)*y(71) +.130_r8*rxt(37) &
                 *y(79)
         loss(41) = (rxt(217)* y(12) + het_rates(72))* y(72)
         prod(41) = 0._r8
         loss(72) = (rxt(219)* y(5) +rxt(218)* y(13) + het_rates(73))* y(73)
         prod(72) =rxt(217)*y(72)*y(12)
         loss(1) = ( + het_rates(74))* y(74)
         prod(1) =rxt(218)*y(73)*y(13)
         loss(42) = (rxt(220)* y(12) + het_rates(75))* y(75)
         prod(42) = 0._r8
         loss(78) = (rxt(222)* y(5) +rxt(221)* y(13) + het_rates(76))* y(76)
         prod(78) =rxt(220)*y(75)*y(12)
         loss(2) = ( + het_rates(77))* y(77)
         prod(2) =rxt(221)*y(76)*y(13)
         loss(79) = (rxt(138)* y(12) + rxt(20) + rxt(137) + het_rates(81))* y(81)
         prod(79) =rxt(129)*y(33)*y(6)
         loss(73) = (rxt(158)* y(12) + het_rates(82))* y(82)
         prod(73) =.100_r8*rxt(201)*y(50)*y(5) +rxt(141)*y(35)*y(7)
         loss(83) = (rxt(184)* y(12) + rxt(21) + rxt(183) + het_rates(83))* y(83)
         prod(83) =rxt(182)*y(59)*y(6)
         loss(107) = (rxt(194)* y(5) +rxt(195)* y(7) +rxt(196)* y(13) + het_rates(84)) &
                 * y(84)
         prod(107) =rxt(187)*y(52)*y(7)
         loss(105) = (rxt(199)* y(7) +rxt(198)* y(12) + rxt(32) + het_rates(85)) &
                 * y(85)
         prod(105) = (.800_r8*rxt(170)*y(57) +.080_r8*rxt(188)*y(53) + &
                 .794_r8*rxt(194)*y(84))*y(5) + (.794_r8*rxt(195)*y(7) + &
                 .794_r8*rxt(196)*y(13))*y(84)
         loss(54) = (rxt(241)* y(2) +rxt(242)* y(12) + rxt(45) + het_rates(100)) &
                 * y(100)
         prod(54) = 0._r8
         loss(48) = (rxt(234)* y(12) + het_rates(101))* y(101)
         prod(48) = (rxt(235)*y(102) +.500_r8*rxt(236)*y(102) +rxt(242)*y(100))*y(12) &
                  +rxt(237)*y(102)*y(7)
         loss(55) = (rxt(237)* y(7) + (rxt(235) +rxt(236))* y(12) + het_rates(102)) &
                 * y(102)
         prod(55) = 0._r8
         loss(3) = ( + het_rates(103))* y(103)
         prod(3) =rxt(234)*y(101)*y(12)
         loss(38) = (rxt(238)* y(12) + het_rates(104))* y(104)
         prod(38) = 0._r8
         loss(4) = ( + het_rates(105))* y(105)
         prod(4) = 0._r8
         loss(5) = ( + het_rates(106))* y(106)
         prod(5) = 0._r8
         loss(6) = ( + het_rates(90))* y(90)
         prod(6) = 0._r8
         loss(7) = ( + het_rates(91))* y(91)
         prod(7) = 0._r8
         loss(8) = ( + het_rates(92))* y(92)
         prod(8) = 0._r8
         loss(9) = ( + het_rates(93))* y(93)
         prod(9) = 0._r8
         loss(10) = ( + het_rates(94))* y(94)
         prod(10) = 0._r8
         loss(11) = ( + het_rates(95))* y(95)
         prod(11) = 0._r8
         loss(12) = ( + het_rates(96))* y(96)
         prod(12) = 0._r8
         loss(13) = ( + het_rates(97))* y(97)
         prod(13) = 0._r8
         loss(14) = ( + het_rates(98))* y(98)
         prod(14) = 0._r8
         loss(15) = ( + het_rates(99))* y(99)
         prod(15) = 0._r8
         loss(16) = ( + rxt(233) + het_rates(86))* y(86)
         prod(16) = 0._r8
         loss(17) = ( + het_rates(87))* y(87)
         prod(17) =rxt(233)*y(86)
         loss(18) = ( + rxt(239) + het_rates(88))* y(88)
         prod(18) = 0._r8
         loss(19) = ( + het_rates(89))* y(89)
         prod(19) =rxt(239)*y(88)
         loss(50) = (rxt(112)* y(12) + het_rates(119))* y(119)
         prod(50) = 0._r8
         loss(65) = (rxt(108)* y(12) + het_rates(120))* y(120)
         prod(65) = (rxt(110)*y(5) +rxt(111)*y(13))*y(121) +.500_r8*rxt(119)*y(23) &
                 *y(1) +.350_r8*rxt(112)*y(119)*y(12)
         loss(74) = (rxt(110)* y(5) +rxt(111)* y(13) + rxt(109) + het_rates(121)) &
                 * y(121)
         prod(74) =rxt(101)*y(20)*y(13)
         loss(20) = ( + het_rates(107))* y(107)
         prod(20) = 0._r8
         loss(21) = ( + het_rates(108))* y(108)
         prod(21) = 0._r8
         loss(22) = ( + het_rates(109))* y(109)
         prod(22) = 0._r8
         loss(23) = ( + het_rates(110))* y(110)
         prod(23) = 0._r8
         loss(24) = ( + het_rates(111))* y(111)
         prod(24) = 0._r8
         loss(25) = ( + het_rates(112))* y(112)
         prod(25) = 0._r8
         loss(26) = ( + het_rates(113))* y(113)
         prod(26) = 0._r8
         loss(27) = ( + het_rates(114))* y(114)
         prod(27) = 0._r8
         loss(28) = ( + rxt(243) + het_rates(122))* y(122)
         prod(28) = 0._r8
         loss(29) = ( + rxt(244) + het_rates(123))* y(123)
         prod(29) = 0._r8
         loss(30) = ( + rxt(245) + het_rates(124))* y(124)
         prod(30) = 0._r8
         loss(31) = ( + het_rates(125))* y(125)
         prod(31) = 0._r8
         loss(32) = ( + rxt(246) + het_rates(126))* y(126)
         prod(32) = 0._r8
         loss(33) = ( + rxt(247) + het_rates(127))* y(127)
         prod(33) = 0._r8
         loss(34) = ( + rxt(248) + het_rates(128))* y(128)
         prod(34) = 0._r8
         loss(35) = ( + het_rates(129))* y(129)
         prod(35) = 0._r8
         loss(36) = ( + rxt(43) + het_rates(130))* y(130)
         prod(36) = 0._r8
         loss(37) = ( + rxt(44) + het_rates(131))* y(131)
         prod(37) = 0._r8

      end subroutine imp_prod_loss

      end module mo_prod_loss

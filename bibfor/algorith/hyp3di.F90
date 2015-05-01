subroutine hyp3di(c11, c22, c33, c12, c13,&
                  c23, c10, c01, c20, ciso,&
                  codret)
!
! ======================================================================
! COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
! COPYRIGHT (C) 2007 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    real(kind=8) :: c11, c22, c33
    real(kind=8) :: c12, c13, c23
    real(kind=8) :: c10, c01, c20
    real(kind=8) :: ciso(6, 6)
    integer :: codret
!
! ----------------------------------------------------------------------
!
! LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
!
! 3D - MATRICE TANGENTE - PARTIE ISOTROPIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  C11,C22,C33,C12,C13,C23 : ELONGATIONS
! IN  C10,C01,C20             : CARACTERISTIQUES MATERIAUX
! OUT CISO : MATRICE TANGENTE ISOTROPIQUE
! OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: df(33)
    real(kind=8) :: dfr0(33)
    real(kind=8) :: dfr1(33)
    real(kind=8) :: dfr2(33)
    real(kind=8) :: dfr3(33)
    real(kind=8) :: dfr4(33)
    real(kind=8) :: grd(6, 6)
    real(kind=8) :: t1, t3, t5, t7
    real(kind=8) :: t10, t12, t13, t14, t15, t16, t17, t18, t19
    real(kind=8) :: t20, t23, t24, t26, t27, t29, t32, t33
    real(kind=8) :: t40, t43, t46, t47, t56, t59
    real(kind=8) :: t60, t69, t72, t73, t82, t89
    real(kind=8) :: t104, t114, t116, t117, t118, t119
    real(kind=8) :: t125, t126, t129, t132, t134
    real(kind=8) :: t141, t142, t147, t148, t149, t156
    real(kind=8) :: t161, t166, t167, t169
    real(kind=8) :: t170, t176, t178, t185, t196
    real(kind=8) :: t201, t206, t208, t214, t216, t223, t234, t248, t252
    real(kind=8) :: t260, t265, t271, t282, t293, t297
    real(kind=8) :: t305, t310, t316, t327
    real(kind=8) :: t338, t342, t350, t355
    real(kind=8) :: t361, t372, t374, t379, t381, t383, t385, t386, t393
    real(kind=8) :: t406, t408, t410, t412, t419
    real(kind=8) :: t433, t435, t437, t444, t458
    real(kind=8) :: t460, t462, t471, t474, t491, t493, t497
    real(kind=8) :: t506, t525, t527, t529, t536
!
! ----------------------------------------------------------------------
!
    t1 = c11*c22
    t3 = c23**2
    t5 = c12**2
    t7 = c12*c13
    t10 = c13**2
    t12 = t1*c33-c11*t3-t5*c33+2*t7*c23-t10*c22
    t13 = t12**(1.d0/3.d0)
    if ((t13.eq.0.d0) .or. (t12.eq.0.d0)) then
        codret=1
        goto 99
    endif
    t14 = 1.d0/t13
    t15 = c11+c22+c33
    t16 = 1.d0/t12
    t17 = t14*t16
    t18 = t15*t17
    t19 = c22*c33
    t20 = t19-t3
    t23 = t14-t18*t20/3.d0
    t26 = t13**2.d0
    t27 = 1.d0/t26
!
    t29 = c11*c33
    t24 = t1+t29+t19-t5-t10-t3
    t33 = t24*t27*t16
    t40 = c20*(t15*t14-3.d0)
    t32 = c22+c33
    t43 = t29-t10
    t46 = t14-t18*t43/3.d0
    t47 = c11+c33
    t56 = t1-t5
    t59 = t14-t18*t56/3.d0
    t60 = c11+c22
    t69 = c10*t15
    t73 = -2.d0*c12*c33+2.d0*c13*c23
    t72 = t69*t17
    t82 = t40*t18
    t89 = 2.d0*c12*c23-2.d0*c13*c22
!
    t104 = -2.d0*c11*c23+2.d0*t7
    df(19) = 2.d0*t23
    df(18) = -2.d0/3.d0*c01*t20
    t114 = df(18)
    df(17) = t114*t27*t16
    df(16) = c01*t32
    t116 = t114*t24
    t117 = t26**2.d0
    t118 = 1.d0/t117
    t119 = t118*t16
    df(15) = -t116*t119-df(16)*t118
    df(14) = c10+2.d0*t40
    t125 = 2.d0/3.d0*c01*t33
    t126 = df(14)
    df(13) = -t125-t126*t18/3.d0
    t129 = df(13)
    df(12) = df(17)+t129
    df(11) = -t126*t20/3.d0
!
    t132 = df(11)
    df(10) = t132*t15
    t134 = df(19)*c20
    df(9) = t134*t14+t132*t17
    df(8) = t134*t15+t126
    t141 = df(10)
    t142 = 1.d0/t26
    df(7) = 2.d0*df(15)*t13-t141*t142*t16-df(8)*t142
    t147 = t12**2.d0
    t148 = 1.d0/t147
    t149 = t27*t148
    df(6) = -t116*t149-t141*t14*t148+df(7)*t142/3.d0
    t156 = df(6)
    df(5) = -df(17)-t156*c22
    df(4) = 2.d0*t156*c23
    df(3) = -df(17)-t156*c33
    df(2) = -df(17)-t129-t156*c11
    df(1) = -df(3)
!
    dfr0(22) = df(14)
    t161 = dfr0(22)
    dfr0(21) = -t125-t161*t18/3.d0
    dfr0(19) = 2.d0*t46
    dfr0(18) = -2.d0/3.d0*c01*t43
    t166 = dfr0(21)
    t167 = dfr0(18)
    t169 = t167*t27*t16
    dfr0(17) = t166+t169
    dfr0(16) = c01*t47
    t170 = t167*t24
    dfr0(15) = -t170*t119-dfr0(16)*t118
    dfr0(12) = t169
    dfr0(11) = -t161*t43/3.d0
    t176 = dfr0(11)
    dfr0(10) = t176*t15
    t178 = dfr0(19)*c20
    dfr0(9) = t178*t14+t176*t17
    dfr0(8) = t161+t178*t15
    t185 = dfr0(10)
    dfr0(7) = 2.d0*dfr0(15)*t13-t185*t142*t16-dfr0(8)*t142
    dfr0(6) = -t170*t149-t185*t14*t148+dfr0(7)*t142/3.d0
    t196 = dfr0(6)
    dfr0(5) = -t166-dfr0(12)-t196*c22
    dfr0(4) = 2.d0*t196*c23
    dfr0(3) = -dfr0(12)-t196*c33
    dfr0(2) = -dfr0(12)-t196*c11
    dfr0(1) = -dfr0(3)
    dfr1(25) = dfr0(22)
    t201 = dfr1(25)
    dfr1(24) = -t125-t201*t18/3.d0
    dfr1(19) = 2.d0*t59
    dfr1(18) = -2.d0/3.d0*c01*t56
    t206 = dfr1(18)
    dfr1(17) = t206*t27*t16
    dfr1(16) = c01*t60
    t208 = t206*t24
    dfr1(15) = -t208*t119-dfr1(16)*t118
    dfr1(12) = dfr1(17)
    dfr1(11) = -t201*t56/3.d0
    t214 = dfr1(11)
    dfr1(10) = t214*t15
    t216 = dfr1(19)*c20
    dfr1(9) = t216*t14+t214*t17
    dfr1(8) = t201+t216*t15
    t223 = dfr1(10)
    dfr1(7) = 2.d0*dfr1(15)*t13-t223*t142*t16-dfr1(8)*t142
    dfr1(6) = -t208*t149-t223*t14*t148+dfr1(7)*t142/3.d0
    t234 = dfr1(6)
    dfr1(5) = -dfr1(12)-t234*c22
    dfr1(4) = 2.d0*t234*c23
    dfr1(3) = -dfr1(24)-dfr1(12)-t234*c33
    dfr1(2) = -dfr1(12)-t234*c11
    dfr1(1) = -dfr1(3)
    dfr2(28) = -t72/3.d0-t125-2.d0/3.d0*t82
    dfr2(27) = -t17*t73/3.d0
    dfr2(19) = -2.d0/3.d0*t18*t73
    dfr2(18) = -2.d0/3.d0*c01*t73
    t248 = dfr2(18)
    dfr2(17) = t248*t27*t16
    dfr2(16) = -2.d0*c01*c12
    t252 = t248*t24
    dfr2(15) = -t252*t119-dfr2(16)*t118
    dfr2(12) = dfr2(17)
    dfr2(11) = -2.d0/3.d0*t40*t73
    t260 = dfr2(11)
    dfr2(10) = -t69*t73/3+t260*t15
    t265 = dfr2(19)*c20
    dfr2(9) = dfr2(27)*c10+t265*t14+t260*t17
    dfr2(8) = t265*t15
    t271 = dfr2(10)
    dfr2(7) = 2.d0*dfr2(15)*t13-t271*t142*t16-dfr2(8)*t142
    dfr2(6) = -t252*t149-t271*t14*t148+dfr2(7)*t142/3.d0
    t282 = dfr2(6)
    dfr2(5) = -dfr2(12)-t282*c22
    dfr2(4) = 2.d0*t282*c23
    dfr2(3) = -dfr2(12)-t282*c33
    dfr2(2) = -dfr2(12)-t282*c11
    dfr2(1) = -dfr2(3)
    dfr3(30) = dfr2(28)
    dfr3(27) = -t17*t89/3.d0
    dfr3(19) = -2.d0/3.d0*t18*t89
    dfr3(18) = -2.d0/3.d0*c01*t89
    t293 = dfr3(18)
    dfr3(17) = t293*t27*t16
    dfr3(16) = -2.d0*c01*c13
    t297 = t293*t24
    dfr3(15) = -t297*t119-dfr3(16)*t118
    dfr3(12) = dfr3(17)
    dfr3(11) = -2.d0/3.d0*t40*t89
    t305 = dfr3(11)
    dfr3(10) = -t69*t89/3.d0+t305*t15
    t310 = dfr3(19)*c20
    dfr3(9) = dfr3(27)*c10+t310*t14+t305*t17
    dfr3(8) = t310*t15
    t316 = dfr3(10)
    dfr3(7) = 2.d0*dfr3(15)*t13-t316*t142*t16-dfr3(8)*t142
    dfr3(6) = -t297*t149-t316*t14*t148+dfr3(7)*t142/3.d0
    t327 = dfr3(6)
    dfr3(5) = -dfr3(12)-t327*c22
    dfr3(4) = 2.d0*t327*c23
    dfr3(3) = -dfr3(12)-t327*c33
    dfr3(2) = -dfr3(12)-t327*c11
    dfr3(1) = -dfr3(3)
    dfr4(32) = dfr3(30)
    dfr4(27) = -t17*t104/3.d0
    dfr4(19) = -2.d0/3.d0*t18*t104
    dfr4(18) = -2.d0/3.d0*c01*t104
    t338 = dfr4(18)
    dfr4(17) = t338*t27*t16
    dfr4(16) = -2.d0*c01*c23
    t342 = t338*t24
    dfr4(15) = -t342*t119-dfr4(16)*t118
    dfr4(12) = dfr4(17)
    dfr4(11) = -2.d0/3.d0*t40*t104
    t350 = dfr4(11)
    dfr4(10) = -t69*t104/3+t350*t15
    t355 = dfr4(19)*c20
    dfr4(9) = dfr4(27)*c10+t355*t14+t350*t17
    dfr4(8) = t355*t15
    t361 = dfr4(10)
    dfr4(7) = 2.d0*dfr4(15)*t13-t361*t142*t16-dfr4(8)*t142
    dfr4(6) = -t342*t149-t361*t14*t148+dfr4(7)*t142/3.d0
    t372 = dfr4(6)
    dfr4(5) = -dfr4(12)-t372*c22
    t374 = dfr4(32)
    dfr4(4) = 2.d0*t374+2*t372*c23
    dfr4(3) = -dfr4(12)-t372*c33
    dfr4(2) = -dfr4(12)-t372*c11
    dfr4(1) = -dfr4(3)
    t379 = df(17)
    t381 = df(9)
    t383 = df(1)
    grd(1,1) = t379*c33+t381-t156*t3+t383*c22
    t385 = c01*t27
    t386 = df(12)
    grd(1,2) = t385+t386*c33+t381-t156*t10+t383*c11
    grd(1,3) = t385+t379*c11+t386*c22+t381+t156*t56
    t393 = df(4)
    grd(1,4) = t393*c13+2*df(3)*c12
    grd(1,5) = 2*df(5)*c13+t393*c12
    grd(1,6) = 2*t156*t7+2*df(2)*c23
    t406 = dfr0(17)
    t408 = dfr0(9)
    t410 = dfr0(1)
    grd(2,1) = t385+t406*c33+t408-t196*t3+t410*c22
    t412 = dfr0(12)
    grd(2,2) = t412*c33+t408-t196*t10+t410*c11
    grd(2,3) = t385+t406*c11+t412*c22+t408+t196*t56
    t419 = dfr0(4)
    grd(2,4) = t419*c13+2*dfr0(3)*c12
    grd(2,5) = 2*dfr0(5)*c13+t419*c12
    grd(2,6) = 2*t196*t7+2*dfr0(2)*c23
    t433 = dfr1(9)
    t435 = dfr1(1)
    grd(3,1) = t385+dfr1(12)*c33+t433-t234*t3+t435*c22
    t437 = dfr1(12)
    grd(3,2) = t385+t437*c33+t433-t234*t10+t435*c11
    grd(3,3) = dfr1(12)*c11+t437*c22+t433+t234*t56
    t444 = dfr1(4)
    grd(3,4) = t444*c13+2*dfr1(3)*c12
    grd(3,5) = 2*dfr1(5)*c13+t444*c12
    grd(3,6) = 2*t234*t7+2*dfr1(2)*c23
    t458 = dfr2(9)
    t460 = dfr2(1)
    grd(4,1) = dfr2(12)*c33+t458-t282*t3+t460*c22
    t462 = dfr2(12)
    grd(4,2) = t462*c33+t458-t282*t10+t460*c11
    grd(4,3) = -2*dfr3(30)*c12+dfr2(12)*c11+t462*c22+t458+t282*t56
    t471 = 2*t385
    t474 = dfr2(4)
    grd(4,4) = -t471-2*dfr3(30)*c33+t474*c13+2*dfr2(3)*c12
    grd(4,5) = 2*dfr3(30)*c23+2*dfr2(5)*c13+t474*c12
    grd(4,6) = 2*dfr3(30)*c13+2*t282*t7+2*dfr2(2)*c23
    t491 = dfr3(9)
    t493 = dfr3(1)
    grd(5,1) = dfr3(12)*c33+t491-t327*t3+t493*c22
    t497 = dfr3(12)
    grd(5,2) = -2*dfr4(32)*c13+t497*c33+t491-t327*t10+t493*c11
    grd(5,3) = dfr3(12)*c11+t497*c22+t491+t327*t56
    t506 = dfr3(4)
    grd(5,4) = 2*dfr4(32)*c23+t506*c13+2*dfr3(3)*c12
    grd(5,5) = -t471-2*dfr4(32)*c22+2*dfr3(5)*c13+t506*c12
    grd(5,6) = 2*dfr4(32)*c12+2*t327*t7+2*dfr3(2)*c23
    t525 = dfr4(9)
    t527 = dfr4(1)
    grd(6,1) = -2*t374*c23+dfr4(12)*c33+t525-t372*t3+t527*c22
    t529 = dfr4(12)
    grd(6,2) = t529*c33+t525-t372*t10+t527*c11
    grd(6,3) = dfr4(12)*c11+t529*c22+t525+t372*t56
    t536 = dfr4(4)
    grd(6,4) = t536*c13+2.d0*dfr4(3)*c12
    grd(6,5) = 2.d0*dfr4(5)*c13+t536*c12
    grd(6,6) = -2.d0*t385-2.d0*t374*c11+2.d0*t372*t7+2.d0*dfr4(2)*c23
!
    ciso(1,1) = 4.d0*grd(1,1)
    ciso(1,2) = 4.d0*grd(1,2)
    ciso(1,3) = 4.d0*grd(1,3)
    ciso(1,4) = 4.d0*grd(1,4)
    ciso(1,5) = 4.d0*grd(1,5)
    ciso(1,6) = 4.d0*grd(1,6)
    ciso(2,1) = 4.d0*grd(2,1)
    ciso(2,2) = 4.d0*grd(2,2)
    ciso(2,3) = 4.d0*grd(2,3)
    ciso(2,4) = 4.d0*grd(2,4)
    ciso(2,5) = 4.d0*grd(2,5)
    ciso(2,6) = 4.d0*grd(2,6)
    ciso(3,1) = 4.d0*grd(3,1)
    ciso(3,2) = 4.d0*grd(3,2)
    ciso(3,3) = 4.d0*grd(3,3)
    ciso(3,4) = 4.d0*grd(3,4)
    ciso(3,5) = 4.d0*grd(3,5)
    ciso(3,6) = 4.d0*grd(3,6)
    ciso(4,1) = 4.d0*grd(4,1)
    ciso(4,2) = 4.d0*grd(4,2)
    ciso(4,3) = 4.d0*grd(4,3)
    ciso(4,4) = 4.d0*grd(4,4)
    ciso(4,5) = 4.d0*grd(4,5)
    ciso(4,6) = 4.d0*grd(4,6)
    ciso(5,1) = 4.d0*grd(5,1)
    ciso(5,2) = 4.d0*grd(5,2)
    ciso(5,3) = 4.d0*grd(5,3)
    ciso(5,4) = 4.d0*grd(5,4)
    ciso(5,5) = 4.d0*grd(5,5)
    ciso(5,6) = 4.d0*grd(5,6)
    ciso(6,1) = 4.d0*grd(6,1)
    ciso(6,2) = 4.d0*grd(6,2)
    ciso(6,3) = 4.d0*grd(6,3)
    ciso(6,4) = 4.d0*grd(6,4)
    ciso(6,5) = 4.d0*grd(6,5)
    ciso(6,6) = 4.d0*grd(6,6)
!
99  continue
end subroutine

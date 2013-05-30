subroutine hyp3dv(c11, c22, c33, c12, c13,&
                  c23, k, cvol, codret)
!
! ======================================================================
! COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    real(kind=8) :: k
    real(kind=8) :: cvol(6, 6)
    integer :: codret
!
! ----------------------------------------------------------------------
!
! LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
!
! 3D - MATRICE TANGENTE - PARTIE VOLUMIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  C11,C22,C33,C12,C13,C23 : ELONGATIONS
! IN  K      : MODULE DE COMPRESSIBILITE
! OUT CVOL   : MATRICE TANGENTE VOLUMIQUE
! OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: df(14)
    real(kind=8) :: dff0(14)
    real(kind=8) :: dff1(14)
    real(kind=8) :: dff2(14)
    real(kind=8) :: dff3(14)
    real(kind=8) :: dff4(14)
    real(kind=8) :: grd(6, 6)
    real(kind=8) :: t1, t3, t5, t7
    real(kind=8) :: t10, t13, t15, t16, t17, t19
    real(kind=8) :: t22, t24, t29, t34, t38, t43, t44, t49
    real(kind=8) :: t50, t54, t64, t68, t78, t92, t96
    real(kind=8) :: t106, t110, t120, t125
    real(kind=8) :: t130, t132, t133, t137, t140
    real(kind=8) :: t155, t160, t163, t178, t184, t199
    real(kind=8) :: t204, t207, t212, t217, t224, t230, t245, t251
!
!-----------------------------------------------------------------------
!
    t1 = c11*c22
    t3 = c23**2
    t5 = c12**2
    t7 = c12*c13
    t10 = c13**2
    t13 = sqrt(t1*c33-c11*t3-t5*c33+2*t7*c23-t10*c22)
    if ((t13.eq.0.d0)) then
        codret=1
        goto 99
    endif
    t15 = k*(-1+t13)
    t16 = 1/t13
    t17 = t15*t16
    t19 = c22*c33-t3
    t22 = c11*c33-t10
    t24 = t1-t5
    t29 = -2*c12*c33+2*c13*c23
    t34 = 2*c12*c23-2*c13*c22
    t38 = -2*c11*c23+2*t7
    df(8) = t15*t19/2
    df(7) = t16*t19/2
    t43 = t13**2
    t44 = 1/t43
    df(6) = -df(8)*t44+df(7)*k
    t49 = 1/t13
    t50 = df(6)*t49
    df(5) = -t50*c22/2
    df(4) = t50*c23
    t54 = t50*c33/2
    df(3) = -t54
    df(2) = -t17/2-t50*c11/2
    df(1) = t54
    dff0(8) = t15*t22/2
    dff0(7) = t16*t22/2
    dff0(6) = -dff0(8)*t44+dff0(7)*k
    t64 = dff0(6)*t49
    dff0(5) = -t17/2-t64*c22/2
    dff0(4) = t64*c23
    t68 = t64*c33/2
    dff0(3) = -t68
    dff0(2) = -t64*c11/2
    dff0(1) = t68
    dff1(8) = t15*t24/2
    dff1(7) = t16*t24/2
    dff1(6) = -dff1(8)*t44+dff1(7)*k
    t78 = dff1(6)*t49
    dff1(5) = -t78*c22/2
    dff1(4) = t78*c23
    dff1(3) = -t17/2-t78*c33/2
    dff1(2) = -t78*c11/2
    dff1(1) = -dff1(3)
    dff2(8) = t15*t29/2
    dff2(7) = t16*t29/2
    dff2(6) = -dff2(8)*t44+dff2(7)*k
    t92 = dff2(6)*t49
    dff2(5) = -t92*c22/2
    dff2(4) = t92*c23
    t96 = t92*c33/2
    dff2(3) = -t96
    dff2(2) = -t92*c11/2
    dff2(1) = t96
    dff3(8) = t15*t34/2
    dff3(7) = t16*t34/2
    dff3(6) = -dff3(8)*t44+dff3(7)*k
    t106 = dff3(6)*t49
    dff3(5) = -t106*c22/2
    dff3(4) = t106*c23
    t110 = t106*c33/2
    dff3(3) = -t110
    dff3(2) = -t106*c11/2
    dff3(1) = t110
    dff4(8) = t15*t38/2
    dff4(7) = t16*t38/2
    dff4(6) = -dff4(8)*t44+dff4(7)*k
    t120 = dff4(6)*t49
    dff4(5) = -t120*c22/2
    dff4(4) = t17+t120*c23
    t125 = t120*c33/2
    dff4(3) = -t125
    dff4(2) = -t120*c11/2
    dff4(1) = t125
    t130 = df(1)
    grd(1,1) = -t50*t3/2+t130*c22
    t132 = t17*c33
    t133 = t132/2
    grd(1,2) = t133-t50*t10/2+t130*c11
    t137 = t17*c22
    grd(1,3) = t137/2+t50*t24/2
    t140 = df(4)
    grd(1,4) = t140*c13+2*df(3)*c12
    grd(1,5) = 2*df(5)*c13+t140*c12
    grd(1,6) = t50*t7+2*df(2)*c23
    t155 = dff0(1)
    grd(2,1) = t133-t64*t3/2+t155*c22
    grd(2,2) = -t64*t10/2+t155*c11
    t160 = t17*c11
    grd(2,3) = t160/2+t64*t24/2
    t163 = dff0(4)
    grd(2,4) = t163*c13+2*dff0(3)*c12
    grd(2,5) = 2*dff0(5)*c13+t163*c12
    grd(2,6) = t64*t7+2*dff0(2)*c23
    t178 = dff1(1)
    grd(3,1) = -t78*t3/2+t178*c22
    grd(3,2) = -t78*t10/2+t178*c11
    grd(3,3) = t78*t24/2
    t184 = dff1(4)
    grd(3,4) = t184*c13+2*dff1(3)*c12
    grd(3,5) = 2*dff1(5)*c13+t184*c12
    grd(3,6) = t78*t7+2*dff1(2)*c23
    t199 = dff2(1)
    grd(4,1) = -t92*t3/2+t199*c22
    grd(4,2) = -t92*t10/2+t199*c11
    t204 = t17*c12
    grd(4,3) = -t204+t92*t24/2
    t207 = dff2(4)
    grd(4,4) = -t132+t207*c13+2*dff2(3)*c12
    t212 = t17*c23
    grd(4,5) = t212+2*dff2(5)*c13+t207*c12
    t217 = t17*c13
    grd(4,6) = t217+t92*t7+2*dff2(2)*c23
    t224 = dff3(1)
    grd(5,1) = -t106*t3/2+t224*c22
    grd(5,2) = -t217-t106*t10/2+t224*c11
    grd(5,3) = t106*t24/2
    t230 = dff3(4)
    grd(5,4) = t212+t230*c13+2*dff3(3)*c12
    grd(5,5) = -t137+2*dff3(5)*c13+t230*c12
    grd(5,6) = t204+t106*t7+2*dff3(2)*c23
    t245 = dff4(1)
    grd(6,1) = -t212-t120*t3/2+t245*c22
    grd(6,2) = -t120*t10/2+t245*c11
    grd(6,3) = t120*t24/2
    t251 = dff4(4)
    grd(6,4) = t251*c13+2*dff4(3)*c12
    grd(6,5) = 2*dff4(5)*c13+t251*c12
    grd(6,6) = -t160+t120*t7+2*dff4(2)*c23
!
    cvol(1,1) = 4.d0*grd(1,1)
    cvol(1,2) = 4.d0*grd(1,2)
    cvol(1,3) = 4.d0*grd(1,3)
    cvol(1,4) = 4.d0*grd(1,4)
    cvol(1,5) = 4.d0*grd(1,5)
    cvol(1,6) = 4.d0*grd(1,6)
    cvol(2,1) = 4.d0*grd(2,1)
    cvol(2,2) = 4.d0*grd(2,2)
    cvol(2,3) = 4.d0*grd(2,3)
    cvol(2,4) = 4.d0*grd(2,4)
    cvol(2,5) = 4.d0*grd(2,5)
    cvol(2,6) = 4.d0*grd(2,6)
    cvol(3,1) = 4.d0*grd(3,1)
    cvol(3,2) = 4.d0*grd(3,2)
    cvol(3,3) = 4.d0*grd(3,3)
    cvol(3,4) = 4.d0*grd(3,4)
    cvol(3,5) = 4.d0*grd(3,5)
    cvol(3,6) = 4.d0*grd(3,6)
    cvol(4,1) = 4.d0*grd(4,1)
    cvol(4,2) = 4.d0*grd(4,2)
    cvol(4,3) = 4.d0*grd(4,3)
    cvol(4,4) = 4.d0*grd(4,4)
    cvol(4,5) = 4.d0*grd(4,5)
    cvol(4,6) = 4.d0*grd(4,6)
    cvol(5,1) = 4.d0*grd(5,1)
    cvol(5,2) = 4.d0*grd(5,2)
    cvol(5,3) = 4.d0*grd(5,3)
    cvol(5,4) = 4.d0*grd(5,4)
    cvol(5,5) = 4.d0*grd(5,5)
    cvol(5,6) = 4.d0*grd(5,6)
    cvol(6,1) = 4.d0*grd(6,1)
    cvol(6,2) = 4.d0*grd(6,2)
    cvol(6,3) = 4.d0*grd(6,3)
    cvol(6,4) = 4.d0*grd(6,4)
    cvol(6,5) = 4.d0*grd(6,5)
    cvol(6,6) = 4.d0*grd(6,6)
!
99  continue
end subroutine

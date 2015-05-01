subroutine fcepai(zr)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    real(kind=8) :: zr(*), vf(3)
!
!---  DEFINITION DES 3 POINTS DE D'INTEGRATION SUR L'EPAISSEUR DE LA
!---  COUCHE
!
!-----------------------------------------------------------------------
    integer :: i, i1, j, l, ll
    real(kind=8) :: xi3
!-----------------------------------------------------------------------
    j=1500
!
    zr(j+1)= -1.d0
    zr(j+2)=  0.d0
    zr(j+3)=  1.d0
!
!     DEFINITION DES 3 POIDS DE NEWTON-COTES CORRESPONDANTS
!
    zr(j+4)=  0.333333333333333d0
    zr(j+5)=  1.333333333333333d0
    zr(j+6)=  0.333333333333333d0
!
!     VALEURS DES 3 PARABOLES (POUR LA DISTRIBUTION DE LA TEMPERATURE)
!     AUX 3 PTS DE D'INTEGRATION PRECEDANTS
!
    do 100 i = 1, 3
        xi3=zr(j+i)
!
        vf(1)= 1-xi3*xi3
        vf(2)=-xi3*(1-xi3)/2.d0
        vf(3)= xi3*(1+xi3)/2.d0
!
        ll=3*(i-1)
        do 110 l = 1, 3
            i1=6+ll+l
            zr(j+i1)=vf(l)
110      continue
100  continue
!
end subroutine

subroutine depsh6(loop, bloc, ueloc, deps, d)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!      CALCUL DES DERIVEES DUX/DX DVX/DY ETC....     H.  BUNG   FEV 83
!          (ELEMENTS CUB6 )
!
! ENTREES :
!       B     :  MATRICE (B)
!      U     :  TABLEAU DES VALEURS NODALES
!    LOOP    =  2 ON CALCULE LES DEFORMATIONS JUSQU'AU 2E ORDRE
!               SUR LA CONFIGURATION (N+1)
!
! SORTIES :
!      D       : D(1)=DU1/DX1 ;  D(2)=DU2/DX1 ; D(3)=DU3/DX1 ;
!              : D(4)=DU1/DX2 ;  D(5)=DU2/DX2 ; D(6)=DU3/DX2 ;
!              : D(7)=DU1/DX3 ;  D(8)=DU2/DX3 ; D(9)=DU3/DX3
!      DEPS  : TENSEUR DE DEFORMATION ASSOCIE A   <U>
!               DEPS(1)=D(1)     ; DEPS(2)=D(5)    ; DEPS(3)=D(9)
!               DEPS(4)=D(2)+D(4); DEPS(5)=D(6)+D(8); DEPS(6)=D(3)+D(7)
!
!      AVEC EVENTUELLEMENT LES TERMES DU 2EME  ORDRE
!
    implicit none
#include "asterfort/r8inir.h"
    integer :: loop, i
    real(kind=8) :: bloc(6, 18), ueloc(3, 6)
    real(kind=8) :: d(9), deps(6), utemp(18)
!
    call r8inir(9, 0.d0, d, 1)
    call r8inir(18, 0.d0, utemp, 1)
    do 20 i = 1, 6
        utemp(i) = ueloc(1,i)
        utemp(i+6) = ueloc(2,i)
        utemp(i+12) = ueloc(3,i)
20  end do
!
    call r8inir(6, 0.d0, deps, 1)
    do 30 i = 1, 18
        deps(1) = deps(1) + bloc(1,i)*utemp(i)
        deps(2) = deps(2) + bloc(2,i)*utemp(i)
        deps(3) = deps(3) + bloc(3,i)*utemp(i)
        deps(4) = deps(4) + bloc(4,i)*utemp(i)
        deps(5) = deps(5) + bloc(5,i)*utemp(i)
        deps(6) = deps(6) + bloc(6,i)*utemp(i)
30  end do
!
    d(1) = deps(1)
    d(2) = deps(4)/0.5d0
    d(3) = deps(6)/0.5d0
    d(4) = deps(4)/0.5d0
    d(5) = deps(2)
    d(6) = deps(5)/0.5d0
    d(7) = deps(6)/0.5d0
    d(8) = deps(5)/0.5d0
    d(9) = deps(3)
!
    if (loop .eq. 2) then
        deps(1) = deps(1) - 0.5d0*(d(1)*d(1) + d(2)*d(2) + d(3)*d(3))
        deps(2) = deps(2) - 0.5d0*(d(4)*d(4) + d(5)*d(5) + d(6)*d(6))
        deps(3) = deps(3) - 0.5d0*(d(7)*d(7) + d(8)*d(8) + d(9)*d(9))
!
        deps(4) = deps(4) - d(1)*d(4) - d(2)*d(5) - d(3)*d(6)
        deps(5) = deps(5) - d(4)*d(7) - d(5)*d(8) - d(6)*d(9)
        deps(6) = deps(6) - d(1)*d(7) - d(2)*d(8) - d(3)*d(9)
    endif
!
end subroutine

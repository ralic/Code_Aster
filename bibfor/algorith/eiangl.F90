subroutine eiangl(ndim, nno2, angnau, ang)
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
    implicit none
#include "asterc/r8dgrd.h"
    integer :: ndim, nno2
    real(kind=8) :: angnau(3), ang(24)
!
!--------------------------------------------------
!  DEFINITION DES ANGLES NAUTIQUES AUX NOEUDS
!  EN RADIAN POUR L'ELEMENT D'INTERFACE
!
!  IN  : NDIM,NNO2
!        ANGNAU : ANGLES NAUTIQUES EN DEGRES
!  OUT :
!        ANG : ANGLES NAUTIQUES AUX NOEUDS EN RADIAN
!--------------------------------------------------
    integer :: n
!
    if (ndim .eq. 2) then
        do 10 n = 1, nno2
            ang(n) = angnau(1)*r8dgrd()
10      continue
    else
        do 20 n = 1, nno2
            ang(1 + (n-1)*3) = angnau(1)*r8dgrd()
            ang(2 + (n-1)*3) = angnau(2)*r8dgrd()
            ang(3 + (n-1)*3) = angnau(3)*r8dgrd()
20      continue
    endif
!
end subroutine

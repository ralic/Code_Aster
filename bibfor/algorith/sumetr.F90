subroutine sumetr(cova, metr, jac)
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
!
    implicit none
!
#include "blas/ddot.h"
    real(kind=8) :: cova(3, 3), metr(2, 2), jac
!
!.......................................................................
!     CALCUL DU TENSEUR METRIQUE (2X2) ET DE SON JACOBIEN
!.......................................................................
! IN  COVA    COORDONNEES DES VECTEURS DE LA BASE COVARAINTE
! OUT METR    TENSEUR METRIQUE (2X2)
! OUT JAC     JACOBIEN DE LA METRIQUE
!.......................................................................
!
    integer :: i, j
!
!
!    CALCUL DE LA METRIQUE
    do 10 i = 1, 2
        do 20 j = 1, 2
            metr(i,j) = ddot(3,cova(1,i),1,cova(1,j),1)
20      continue
10  end do
!
!
!    CALCUL DU JACOBIEN
    jac = sqrt(abs( metr(1,1)*metr(2,2) - metr(1,2)*metr(2,1) ))
!
end subroutine

subroutine rvdet3(t, d)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/rvdet2.h"
    real(kind=8) :: t(*), d
!
!*********************************************************************
!
!   OPERATION REALISEE
!   ------------------
!
!     CALCUL DU DETERMINANT DU TENSEUR 3X3 SYMETRIQUE T
!
!     T EST REPRESENTE PAR LA TABLE DE SES COMPOSANTES DANS L' ORDRE :
!
!        XX, YY, ZZ, XY, XZ, YZ
!
!
!*********************************************************************
!
    real(kind=8) :: aux
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    d = 0.0d0
    aux = 0.0d0
!
    call rvdet2(t(2), t(6), t(6), t(3), aux)
!
    d = t(1)*aux
!
    call rvdet2(t(4), t(6), t(5), t(3), aux)
!
    d = d - t(4)*aux
!
    call rvdet2(t(4), t(2), t(5), t(6), aux)
!
    d = d + t(5)*aux
!
end subroutine

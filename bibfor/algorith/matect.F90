subroutine matect(materd, materf, nmat, macst)
    implicit none
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
!     ROUTINE GENERIQUE DE RECUPERATION DU MATERIAU A T ET T+DT
!     ----------------------------------------------------------------
!     IN  MATERD  :  COEFFICIENTS MATERIAU A T-
!     IN  MATERF  :  COEFFICIENTS MATERIAU A T+
!         NMAT   :  DIMENSION  DE MATER
!     OUT MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                   'NON' SINON
!     ----------------------------------------------------------------
#include "asterc/r8prem.h"
    integer :: nmat, i
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), epsi
    character(len=3) :: macst
!     ----------------------------------------------------------------
    epsi=r8prem()
!
! -   MATERIAU CONSTANT ?
    macst = 'OUI'
    do 30 i = 1, nmat
        if (abs(materd(i,1)-materf(i,1)) .gt. epsi*materd(i,1)) then
            macst = 'NON'
            goto 9999
        endif
30  end do
    do 40 i = 1, nmat
        if (abs(materd(i,2)-materf(i,2)) .gt. epsi*materd(i,2)) then
            macst = 'NON'
            goto 9999
        endif
40  end do
9999  continue
end subroutine

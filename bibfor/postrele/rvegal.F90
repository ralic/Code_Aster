subroutine rvegal(epsi, criter, x, y, ok,&
                  eccart)
    implicit none
#include "asterf_types.h"
!
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
    character(len=1) :: criter
    aster_logical :: ok
    real(kind=8) :: epsi, x, y, eccart
!
!********************************************************************
!
!     OPERATION REALISEE
!     ------------------
!
!       TEST DE L' EGALITE " X = Y "
!
!********************************************************************
!
    real(kind=8) :: seuil, z
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    eccart = x - y
    seuil = epsi
    z = abs(x)
!
    if ((criter .eq. 'R') .and. (z .ge. epsi)) then
!
        seuil = seuil*z
!
    endif
!
    ok = ( abs(eccart) .lt. seuil)
!
end subroutine

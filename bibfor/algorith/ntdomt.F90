subroutine ntdomt(parmer)
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
    implicit none
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
    real(kind=8) :: parmer(1)
! ----------------------------------------------------------------------
! SAISIE DES PARAMETRES DE LA METHODE DE RESOLUTION
!
! OUT PARMER  : PARAMETRES REELS DE LA METHODE
!               PARMER(1) : THETA
!
! ----------------------------------------------------------------------
    integer :: n1
    real(kind=8) :: theta
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call getvr8(' ', 'PARM_THETA', scal=theta, nbret=n1)
    if ((theta.lt.0.0d0) .or. (theta.gt.1.0d0)) then
        call utmess('F', 'ALGORITH9_4')
    endif
    parmer(1) = theta
!
! FIN ------------------------------------------------------------------
end subroutine

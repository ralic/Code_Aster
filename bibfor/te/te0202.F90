subroutine te0202(option, nomte)
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
!
!
! ======================================================================
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmfifi.h"
    character(len=16) :: nomte, option
!
!-----------------------------------------------------------------------
!
!     BUT : CALCUL DES OPTIONS NON LINEAIRES DES ELEMENTS DE
!          FISSURE JOINT
!
!     OPTION : FORC_NODA
!
!-----------------------------------------------------------------------
!
!
    integer :: igeom, icont, ivect, npg
    character(len=8) :: typmod(2)
!
!
    if (lteatt('AXIS','OUI')) then
        typmod(1) = 'AXIS'
    else
        typmod(1) = 'PLAN'
    endif
    typmod(2) = 'ELEMJOIN'
!
    npg=2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTMR', 'L', icont)
    call jevech('PVECTUR', 'E', ivect)
!
    call nmfifi(npg, typmod, zr(igeom), zr(icont), zr(ivect))
!
end subroutine

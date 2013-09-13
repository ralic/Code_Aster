subroutine op0136()
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COMMANDE POST_FATIGUE
!     ------------------------------------------------------------------
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/pofape.h"
#include "asterfort/pofaqu.h"
#include "asterfort/pofaun.h"
#include "asterfort/titre.h"
    integer :: n1
    character(len=8) :: typcha
!     ------------------------------------------------------------------
!
    call infmaj()
    call getvtx(' ', 'CHARGEMENT', scal=typcha, nbret=n1)
!
!     --- CHARGEMENT PUREMENT UNAXIAL ---
!
    if (typcha .eq. 'UNIAXIAL') then
        call pofaun()
!
!     --- CHARGEMENT MULTIAXIAL ---
!
    else if (typcha .eq. 'MULTIAXI') then
        call pofape()
!
!     --- CHARGEMENT QUELCONQUE (ENDOMMAGEMENT DE LEMAITRE) ---
!
    else if (typcha .eq. 'QUELCONQ') then
        call pofaqu()
    endif
    call titre()
!
end subroutine

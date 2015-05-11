subroutine op0028()
!
implicit none
!
#include "asterc/getres.h"
#include "asterfort/dfllad.h"
#include "asterfort/dflldb.h"
#include "asterfort/dfllec.h"
#include "asterfort/dfllty.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: sdlist
    character(len=16) :: k16bid
    character(len=16) :: metlis
    real(kind=8) :: dtmin
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infmaj()
    call infniv(ifm, niv)
!
! --- NOM DU CONCEPT
!
    call getres(sdlist, k16bid, k16bid)
!
! --- LECTURE DU TYPE DE CONSTRUCTION DE LA LISTE D'INSTANTS
!
    call dfllty(sdlist, metlis, dtmin)
!
! --- LECTURE DES ECHECS
!
    call dfllec(sdlist, dtmin)
!
! --- ADAPTATION SEULEMENT SI METHODE AUTO
!
    if (metlis .eq. 'AUTO') then
        call dfllad(sdlist)
    endif
!
! --- DEBUG
!
    if (niv .ge. 2) then
        call dflldb(sdlist, ifm)
    endif

end subroutine

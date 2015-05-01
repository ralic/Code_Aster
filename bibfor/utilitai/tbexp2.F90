subroutine tbexp2(nomta, para)
    implicit none
#include "asterf_types.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomta, para
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
! ======================================================================
!      EXISTENCE D'UN PARAMETRE DANS UNE TABLE.
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN  : PARA   : PARAMETRE A CHERCHER
! ----------------------------------------------------------------------
    character(len=4) :: typpar
    character(len=24) :: valk(2)
    aster_logical :: exist
! DEB------------------------------------------------------------------
!
    call jemarq()
!
    call tbexip(nomta, para, exist, typpar)
!
    if (.not.exist) then
        valk (1) = para
        valk (2) = nomta
        call utmess('F', 'UTILITAI6_93', nk=2, valk=valk)
    endif
!
    call jedema()
end subroutine

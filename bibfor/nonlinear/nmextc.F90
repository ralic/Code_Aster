subroutine nmextc(sdieto, motfac, iocc, nomcha, lextr)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmetob.h"
    character(len=16) :: motfac
    integer :: iocc
    character(len=24) :: nomcha, sdieto
    logical :: lextr
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
!
! LECTURE DU NOM DU CHAMP
! VERIFICATION CHAMP OK POUR PHENOMENE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! OUT NOMCHA : NOM DU CHAMP
! OUT LEXTR  : .TRUE. SI LE CHAMP EST EXTRACTABLE (COMPATIBLE AVEC
!               PHENOMENE)
!
!
!
!
    character(len=8) :: k8bid
    integer :: nchp, n1
    integer :: icham
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lextr = .true.
!
! --- LECTURE: IL FAUT UN CHAMP ET UN SEUL
!
    call getvtx(motfac, 'NOM_CHAM', iocc, iarg, 0,&
                k8bid, n1)
    nchp = -n1
    ASSERT(nchp.eq.1)
!
! --- NOM DU CHAMP
!
    call getvtx(motfac, 'NOM_CHAM', iocc, iarg, 1,&
                nomcha, nchp)
!
! --- INDICE DU CHAMP
!
    call nmetob(sdieto, nomcha, icham)
!
! --- OBSERVABLE ?
!
    if (icham .eq. 0) then
        lextr = .false.
    else
        lextr = .true.
    endif
!
    call jedema()
!
end subroutine

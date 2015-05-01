subroutine nmcrpa(motfaz, iocc, sdlist, base, nbinst,&
                  dtmin)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrpm.h"
#include "asterfort/wkvect.h"
    character(len=24) :: sdlist
    character(len=*) :: motfaz
    character(len=1) :: base
    integer :: iocc
    real(kind=8) :: dtmin
    integer :: nbinst
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
!
! LECTURE LISTE INSTANTS
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA SD DANS LAQUELLE ON CONSERVERA LA LISTE
!               ON CREE UN VECTEUR DE LONGUEUR NBINST SUR BASE
! NB: LA LISTE N'EST PAS FORCEMENT CROISSANTE
! IN  BASE   : NOM DE LA BASE POUR CREATION SD
! IN  MOTFAC : MOT-FACTEUR POUR LIRE (LIST_INST/INST)
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! OUT NBINST : NOMBRE D'INSTANTS DANS LA LISTE
! OUT DTMIN  : INCREMENT DE TEMPS MINIMUM DANS LA LISTE
!
!
!
!
    integer :: n2, n3, i, iret
    character(len=19) :: list
    integer ::  jslist
    character(len=16) :: motfac
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbinst = 0
    motfac = motfaz
    dtmin = 0.d0
!
! --- CREATION ET INITIALISATION SD
!
    call getvid(motfac, 'LIST_INST', iocc=iocc, scal=list, nbret=n2)
    call getvr8(motfac, 'INST', iocc=iocc, nbval=0, nbret=n3)
    n3 = -n3
!
! --- RECUPERATION DU NOMBRE D'INSTANTS
!
    if ((n2.ge.1) .and. (n3.ge.1)) then
        ASSERT(.false.)
    endif
!
    if (n3 .ge. 1) then
        nbinst = n3
    else if (n2.ge.1) then
        call jelira(list//'.VALE', 'LONMAX', ival=nbinst)
    else
        nbinst = 0
        goto 99
    endif
!
! --- CREATION DE LA LISTE
!
    call wkvect(sdlist, base//' V R', nbinst, jslist)
!
! --- REMPLISSAGE DE LA LISTE
!
    if (n3 .ge. 1) then
        call getvr8(motfac, 'INST', iocc=iocc, nbval=nbinst, vect=zr(jslist),&
                    nbret=iret)
    else
        call jeveuo(list//'.VALE', 'L', vr=vale)
        do 43 i = 1, nbinst
            zr(jslist+i-1) = vale(i)
43      continue
    endif
!
! --- CALCUL DU DELTA MINIMUM DE LA LISTE
!
    call nmcrpm(zr(jslist), nbinst, dtmin)
!
99  continue
!
    call jedema()
!
end subroutine

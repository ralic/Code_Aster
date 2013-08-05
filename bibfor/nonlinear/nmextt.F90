subroutine nmextt(sdieto, nomcha, typcha)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetob.h"
    character(len=24) :: sdieto, nomcha
    character(len=4) :: typcha
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! TYPE DU CHAMP (NOEU OU ELGA)
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  NOMCHA : NOM DU CHAMP
! OUT TYPCHA : TYPE DU CHAMP
!             'NOEU'
!             'ELGA'
!
!
!
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: zioch
    integer :: icham
    character(len=24) :: loccha
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typcha = 'XXXX'
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'L', jiolch)
    zioch = zi(jioinf+4-1)
!
! --- INDICE DU CHAMP
!
    call nmetob(sdieto, nomcha, icham)
!
! --- LOCALISATION DU CHAMP
!
    loccha = zk24(jiolch+zioch*(icham-1)+5-1)
!
! --- TYPE DE CHAMP
!
    if (loccha .eq. 'ELGA') then
        typcha = 'ELGA'
    else if (loccha.eq.'NOEU') then
        typcha = 'NOEU'
    else
        write(6,*) 'LOCALISATION INTERDITE POUR OBSV: ',loccha
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine

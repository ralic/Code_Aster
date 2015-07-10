subroutine apnomk(sdappa, questi, rnomsd)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: sdappa
    character(len=*) :: questi
    character(len=24) :: rnomsd
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! ROUTINE D'INTERROGATION DE LA SD NOMSD
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  QUISTI : QUEL NOM DE SD
!               'NEWGEO' - OBJET GEOMETRIE ACTUALISEE
!               'NOMA'   - OBJET MAILLAGE
!               'DEFICO' - OBJET DEFINTION DU CONTACT
! OUT RNOMSD : NOM DE LA SD INTERROGEE
!
!
!
!
    character(len=24) :: nomsd
    integer :: jnomsd
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    rnomsd = ' '
!
! --- ACCES SDAPPA
!
    nomsd = sdappa(1:19)//'.NOSD'
    call jeveuo(nomsd, 'L', jnomsd)
!
! --- REPONSE
!
    if (questi(1:4) .eq. 'NOMA') then
        rnomsd = zk24(jnomsd+1 -1)
!
    else if (questi(1:6).eq.'NEWGEO') then
        rnomsd = zk24(jnomsd+2 -1)
!
    else if (questi(1:6).eq.'DEFICO') then
        rnomsd = zk24(jnomsd+3 -1)
!
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine

subroutine nmetcv(nomchs, chrefe, lochin, locout, chain,&
                  chaout)
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
    implicit none
#include "jeveux.h"
#include "asterfort/chpchd.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    character(len=24) :: chain, chaout
    character(len=24) :: nomchs, chrefe
    character(len=24) :: lochin, locout
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! CONVERSION D'UN CHAMP
!
! ----------------------------------------------------------------------
!
!
! IN  NOMCHS : NOM DU CHAMP DANS SD RESULTAT
! IN  CHREFE : CHAM_ELEM DE REFERENCE -
!              SERT A LA CONVERSION CART -> ELGA
! IN  CHAIN  : CHAMP D'ENTREE A CONVERTIR
! IN  LOCHIN : TYPE DE LOCALISATION DU CHAMP
! IN  LOCOUT : TYPE DE LOCALISATION DU CHAMP DE SORTIE DEMANDE
! OUT CHAOUT : CHAMP DE SORTIE CONVERTI
!
!
!
!
    integer :: iret, ibid
    character(len=24) :: valk(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LOCALISATION DU CHAMP EN ENTREE
!
    call dismoi('C', 'TYPE_CHAMP', chain, 'CHAMP', ibid,&
                lochin, iret)
    if (iret .eq. 1) then
        call utmess('F', 'ETATINIT_50', sk=nomchs)
    endif
!
! --- PAS DE CONVERSION SI BONS TYPES
!
    if (lochin .eq. locout) then
        call copisd('CHAMP_GD', 'V', chain, chaout)
        goto 99
    endif
!
! --- CONVERSION POSSIBLE ?
!
    valk(1) = chain
    valk(2) = lochin
    valk(3) = locout
    if (locout .eq. 'ELGA') then
        if (chrefe .eq. ' ') then
            call utmess('F', 'ETATINIT_52', nk=3, valk=valk)
        else
            call utmess('I', 'ETATINIT_51', nk=3, valk=valk)
        endif
    else
        call utmess('F', 'ETATINIT_52', nk=3, valk=valk)
    endif
!
! --- TRANSFORMER LE CHAM_ELEM EN CHAM_ELGA
!
    call chpchd(chain, locout, chrefe, 'NON', 'V',&
                chaout)
!
99  continue
!
    call jedema()
end subroutine

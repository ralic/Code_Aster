function dinins(sddisc, numins)
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
    integer :: dinins
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
    integer :: numins
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE - DISCRETISATION)
!
! ACCES AU NIVEAU DE SUBDIVISION D'UN INSTANT
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NUMINS : NUMERO DE L'INSTANT
! OUT DININS : NIVEAU DE SUBDIVISION DE L'INSTANT (1=PAS REDECOUPE)
!
! ----------------------------------------------------------------------
!
    integer :: jnivtp, ibid
    character(len=24) :: tpsdin
    character(len=16) :: metlis
    real(kind=8) :: r8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LA NOTION DE SOUS-NIVEAU N'EXISTE PAS EN GESTION AUTO
!
    call utdidt('L', sddisc, 'LIST', ibid, 'METHODE',&
                r8bid, ibid, metlis)
    if (metlis .eq. 'AUTO') then
        dinins = 1
        goto 999
    endif
!
! --- ACCES SD LISTE D'INSTANTS
!
    tpsdin = sddisc(1:19)//'.DINI'
    call jeveuo(tpsdin, 'L', jnivtp)
    ASSERT(numins.ge.1)
    dinins = zi(jnivtp-1+numins)
!
999  continue
    call jedema()
end function

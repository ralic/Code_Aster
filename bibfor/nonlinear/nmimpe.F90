subroutine nmimpe(modele, limped)
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
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=24) :: modele
    logical(kind=1) :: limped
!
!
!
!
!
    character(len=19) :: nolig, ligrel
    character(len=16) :: nomte
    character(len=24) :: repk
    integer :: nbgrel, igrel, ialiel, nel, itypel
! ----------------------------------------------------------------------
!
!
    call jemarq()
    ligrel = modele(1:8)//'.MODELE'
    nolig = ligrel(1:19)
!
    limped = .true.
!
    call jelira(nolig//'.LIEL', 'NUTIOC', ival=nbgrel)
    repk = 'NON'
    do igrel = 1, nbgrel
        call jeveuo(jexnum(nolig//'.LIEL', igrel), 'L', ialiel)
        call jelira(jexnum(nolig//'.LIEL', igrel), 'LONMAX', ival=nel)
        itypel = zi(ialiel-1+nel)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
        if ((nomte(1:9).eq.'MEFA_FACE') .or. (nomte(1:6).eq.'MEFASE')) then
            repk = 'OUI'
            goto 20
        endif
    end do
!
    if (repk .eq. 'NON') then
        limped = .false.
    endif
!
20  continue
    if (limped) then
        call utmess('I', 'ALGORITH3_23')
    endif
    call jedema()
end subroutine

subroutine detrsd_vide(typesd, nomsd)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/detrsd.h"
    character(len=*), intent(in) :: typesd, nomsd
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  but : detruire une structure de donnee si elle est vide
!
!  in   typesd : type de la structure de donnee a detruire
!          'carte'
!       nomsd   : nom de la structure de donnees a detruire
!
!  Resultat:
!     on detruit tous les objets jeveux correspondant a cette SD.
! ----------------------------------------------------------------------
    integer ::  ngedit, iexi, jdesc
    character(len=19) :: carte
    character(len=24) :: typ2sd
!
! -DEB------------------------------------------------------------------
!
    call jemarq()
    typ2sd = typesd


    if (typ2sd .eq. 'CARTE') then
!   -----------------------------
        carte=nomsd
        call jeexin(carte//'.DESC', iexi)
        if (iexi.eq.0) goto 999
        call jeveuo(carte//'.DESC', 'L', jdesc)
        ngedit=zi(jdesc-1+3)
        if (ngedit.eq.0) then
            call detrsd('CHAMP',carte)
        endif


    else
        call utmess('F', 'UTILITAI_47', sk=typ2sd)
    endif

999 continue
    call jedema()
end subroutine

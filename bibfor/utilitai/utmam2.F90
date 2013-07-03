subroutine utmam2(modele, nbma, nbtrou, tatrou)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nbma, nbtrou, tatrou(nbma)
    character(len=8) :: modele
! ----------------------------------------------------------------------
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
!
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       FILTRER LES MAILLES AFFECTEES PAR LE MODELE
!                   **                       **
!       IDEM QUE UTMAMO MAIS AVEC UNE LISTE DE MAILLE
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MODELE    : NOM DU MODELE
! IN   NBMA      : NOMBRE DE MAILLE DU MAILLAGE
!
!      SORTIE :
!-------------
! OUT  NBTROU    : NOMBRE DE MAILLE TROUVEES
! OUT  LITROU    : LISTE DES MAILLES TROUVEES (OBJET JEVEUX)
!                  SI NBTROU = 0, L'OBJET JEVEUX N'EST PAS CREE
!
!.......................................................................
!
    integer :: nbmail, ima, itrou
    integer :: itempo, jmamo
!
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(modele//'.MAILLE', 'L', jmamo)
    call jelira(modele//'.MAILLE', 'LONMAX', nbmail, k8bid)
    call assert(nbma.eq.nbmail)
!
    call wkvect('&&UTMAM2.LISTE_M_TEMP', 'V V I', nbmail, itempo)
!
    nbtrou = 0
!
    do 10, ima=1,nbmail
    if (zi(jmamo-1+ima) .gt. 0) then
        nbtrou=nbtrou+1
        zi(itempo-1+nbtrou)=ima
    endif
    10 end do
!
    if (nbtrou .eq. 0) goto 9999
!
    do 20 itrou = 1, nbtrou
        tatrou(itrou) = zi(itempo-1+itrou)
20  end do
!
9999  continue
!
    call jedema()
!
    call jedetr('&&UTMAM2.LISTE_M_TEMP')
!
end subroutine

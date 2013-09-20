subroutine utflmd(mailla, limail, nbmail, dim, typmai,&
                  nbtrou, litrou)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/utflm2.h"
#include "asterfort/wkvect.h"
    integer :: nbmail, dim, nbtrou
    character(len=8) :: mailla
    character(len=*) :: litrou, limail, typmai
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       FILTRER UNE LISTE DE MAILLE D'APRES LEUR DIMENSION
!       *           *        *                   *
!       IDEM QUE UTFLM2 MAIS AVEC UN VECTEUR JEVEUX
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MAILLA    : NOM DU MAILLAGE
! IN   LIMAIL    : LISTE DES MAILLES (OBJET JEVEUX)
! IN   NBMA      : NOMBRE DE MAILLES DE LA LISTE
! IN   NDIM      : DIMENSION DES MAILLES A TROUVER (-1,0,1,2,3)
! IN   TYPMAI    : SI DIM=-1, ON FILTRE SUR TYPMAI='QUAD4'/'TRIA3'/...
!                  SINON, ON NE SE SERT PAS DE TYPMAI
!
!      SORTIE :
!-------------
! OUT  NBTROU    : NOMBRE DE MAILLE TROUVEES
! OUT  LITROU    : LISTE DES MAILLES TROUVEES (OBJET JEVEUX)
!                  SI NBTROU = 0, L'OBJET JEVEUX N'EST PAS CREE
!
!.......................................................................
!
!
!
!
!
    integer :: ilimai, lonmax, itrma
!
!
! ----------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(limail, 'L', ilimai)
    call jelira(limail, 'LONMAX', lonmax)
    ASSERT(nbmail.le.lonmax)
    call wkvect(litrou, 'V V I', nbmail, itrma)
!
    call utflm2(mailla, zi(ilimai), nbmail, dim, typmai,&
                nbtrou, zi(itrma))
!
    if (nbtrou .gt. 0) then
        call juveca(litrou, nbtrou)
    else
        call jedetr(litrou)
    endif
    call jedema()
end subroutine

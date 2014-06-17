subroutine utflm2(mailla, tabmai, nbma, dim, typmai,&
                  nbtrou, tatrou)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8), intent(in) :: mailla
    integer, intent(in) :: nbma
    integer, intent(in) :: tabmai(nbma)
    integer, intent(in) :: dim
    character(len=*), intent(in) :: typmai
    integer, intent(out) :: nbtrou
    integer, intent(out) :: tatrou(nbma)

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
!       FILTRER UNE LISTE DE MAILLE D'APRES LEUR DIMENSION VERSION 2
!       *           *        *                                     *
!       IDEM QUE UTFLMD MAIS AVEC UNE LISTE DE MAILLE
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MAILLA    : NOM DU MAILLAGE
! IN   TABMAI    : LISTE DES MAILLES
! IN   NBMA      : LONGUEUR DE LA LISTE
! IN   NDIM      : DIMENSION DES MAILLES A TROUVER (-1,0,1,2,3)
! IN   TYPMAI    : SI DIM=-1, ON FILTRE SUR TYPMAI='QUAD4'/'TRIA3'/...
!                  SINON, ON NE SE SERT PAS DE TYPMAI
!      SORTIE :
!-------------
! OUT  NBTROU    : NOMBRE DE MAILLE TROUVEES
! OUT  TATROU    : LISTE DES MAILLES TROUVEES
!     REMARQUE : TATROU EST SUPPOSE DE LONGUEUR SUFFISANTE (>= NBTROU)
!
!.......................................................................
!
!
!
!
!
    integer :: nbtyp, i, ii, itrou, itych
    integer, pointer :: dime_topo(:) => null()
    integer, pointer :: liste_m_temp(:) => null()
    integer, pointer :: liste_typmai(:) => null()
    character(len=8), pointer :: type_maille(:) => null()
    integer, pointer :: typmail(:) => null()
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
    ASSERT(nbma.gt.0)
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtyp)
!
!     -- SI DIM=-1, ON TRIE SUR TYPMAI :
    if (dim .eq. -1) then
        call jenonu(jexnom('&CATA.TM.NOMTM', typmai), itych)
        if (itych .eq. 0) then
            call utmess('F', 'CALCULEL2_67', sk=typmai)
        endif
!
    else
!
        AS_ALLOCATE(vk8=type_maille, size=nbtyp)
        AS_ALLOCATE(vi=dime_topo, size=nbtyp)
        AS_ALLOCATE(vi=liste_typmai, size=nbma)
!
! ------RECUPERATION DE TOUS LES TYPES DE MAILLE
!        ET DE LEUR DIMENSION TOPOLOGIQUE
!
        do i = 1, nbtyp
            call jenuno(jexnum('&CATA.TM.NOMTM', i), type_maille(i))
            call dismoi('DIM_TOPO', type_maille(i), 'TYPE_MAILLE', repi=dime_topo(i))
        end do
    endif
!
!
! ----RECUPERATION DE LA LISTE DES TYPES DE MAILLE DU MAILLAGE
!
    call jeveuo(mailla//'.TYPMAIL        ', 'L', vi=typmail)
!
    AS_ALLOCATE(vi=liste_m_temp, size=nbma)
!
    nbtrou = 0
    ii = 1
    do i = 1, nbma
!
! ------RECUPERATION DU TYPE DE LA MAILLE I
!
        if (dim .ne. -1) then
!
! --------SI LA DIMENSION TOPOLOGIQUE EST LA BONNE ON GARDE LA MAILLE
!
            liste_typmai(i) = typmail(tabmai(i))
            if (dime_topo(liste_typmai(i)) .eq. dim) then
                liste_m_temp(ii) = tabmai(i)
                nbtrou = nbtrou + 1
                ii = ii + 1
            endif
        else
!
! --------TRI SUR LE TYPE DE LA MAILLE :
!
            if (typmail(tabmai(i)) .eq. itych) then
                liste_m_temp(ii) = tabmai(i)
                nbtrou = nbtrou + 1
                ii = ii + 1
            endif
        endif
    end do
!
    if (nbtrou .eq. 0) goto 999
!
! ----SI LA LISTE N'EST PAS VIDE ON RECOPIE DANS TATROU DE TAILLE NBMA
!
    do itrou = 1, nbtrou
        tatrou(itrou) = liste_m_temp(itrou)
    end do
!
999 continue
!
    AS_DEALLOCATE(vk8=type_maille)
    AS_DEALLOCATE(vi=dime_topo)
    AS_DEALLOCATE(vi=liste_m_temp)
    AS_DEALLOCATE(vi=liste_typmai)
!
    call jedema()
!
end subroutine

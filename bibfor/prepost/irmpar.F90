subroutine irmpar(nomcon, ifichi, nopara)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mprcre.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: nopara
    character(len=*) :: nomcon
    integer :: ifichi
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!
!-----------------------------------------------------------------------
!     CREATION D'UN PARAMETRE DANS UN FICHIER MED
!
! IN  NOMCON : K8  : NOM DU CONCEPT A IMPRIMER
! IN  IFICHI : IS  : UNITE LOGIQUE D'ECRITURE
! IN  NOPARA : K16 : NOM D'UN PARAMATRE A AJOUTER
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: edleaj, idfimd, codret, hdfok, medok
    character(len=1) :: saux01
    character(len=8) :: saux08
    character(len=16) :: saux16
    character(len=64) :: saux64
    character(len=200) :: nofimd, nopar2
    character(len=255) :: kfic
!
    aster_logical :: ficexi
!     ------------------------------------------------------------------
    call jemarq()
!
    call ulisog(ifichi, kfic, saux01)
    if (kfic(1:1) .eq. ' ') then
        call codent(ifichi, 'G', saux08)
        nofimd = 'fort.'//saux08
    else
        nofimd = kfic(1:200)
    endif
    inquire(file=nofimd,exist=ficexi)
    if (ficexi) then
        call as_mficom(nofimd, hdfok, medok, codret)
        if ( medok.eq.0.or.hdfok.eq.0.or.codret.ne.0 ) then
            edleaj = 3
            call as_mfiope(idfimd, nofimd, edleaj, codret)
        else
            edleaj = 1
            call as_mfiope(idfimd, nofimd, edleaj, codret)
        endif
    else
        edleaj = 3
        call as_mfiope(idfimd, nofimd, edleaj, codret)
    endif
    if (codret .ne. 0) then
        saux08='mfiope'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    saux64 = nomcon//nopara
    nopar2 = nopara
    saux16 = ' '
    call as_mprcre(idfimd, saux64, 0, nopar2, saux16, codret)
    if (codret .ne. 0) then
        saux08='mprcre'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    call as_mficlo(idfimd, codret)
    if (codret .ne. 0) then
        saux08='mficlo'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    call jedema()
end subroutine

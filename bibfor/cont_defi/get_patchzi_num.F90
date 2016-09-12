subroutine get_patchzi_num(mesh, nmgrma, num)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"


! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: nmgrma
    integer, intent(out) :: num
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get Patch zone number in DECOUPE_LAC 
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  nmgrma           : name of izone GROUP_MA_ESCL 
! In  num              : corresponding Patch zone number in DECOUPE_LAC
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_patchzi, nb_patchzi
    aster_logical :: find
    character(len=24), pointer:: nm_patchzi(:) => null() 
!
! --------------------------------------------------------------------------------------------------
!
    call jeveuo(mesh//'.PTRNOMPAT', 'L', vk24 = nm_patchzi)
    call jelira(mesh//'.PTRNOMPAT', 'LONMAX', nb_patchzi)
    num = 0
!
    do i_patchzi=1, nb_patchzi
        if (nm_patchzi(i_patchzi).eq. nmgrma) then
            num = i_patchzi
            find=.true.                
        endif
    enddo
    ASSERT(find)
    !Ajouter un message d'erreur la zone de contact esclave "nmgrma" n'a pas était prétraitée
    !avec l'option DECOUPE_LAC de CREA_MAILLAGE
!
end subroutine

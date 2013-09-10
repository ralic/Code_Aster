subroutine char_rcbp_lino(mesh, name_ancr, list_node, nb_node)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: name_ancr
    character(len=24), intent(in) :: list_node
    integer, intent(out) :: nb_node
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! RELA_CINE_BP - Get list of nodes for ancrage
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh         : name of mesh
! In  name_ancr    : name of ancrage
! In  list_node    : list of nodes of ancrage
! Out nb_node      : number of nodes of ancrage
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jgro, jlino_old, jind, jlino_new
    character(len=8) :: k8bid
    integer :: ino, nbno, numnoe, indnoe, ino_1, indlis
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_node = 0
!
! - Acces to nodes
!
    call jeveuo(jexnom(mesh//'.GROUPENO', name_ancr), 'L', jgro)
    call jelira(jexnom(mesh//'.GROUPENO', name_ancr), 'LONUTI', nbno, k8bid)
!
! - Create list of nodes
!
    call wkvect('&&CAPREC.LIST', 'V V I', nbno, jlino_old)
    indnoe = 0
    do ino = 1, nbno
        numnoe = zi(jgro+ino-1)
        indnoe = indnoe + 1
        zi(jlino_old+indnoe-1) = numnoe
    enddo
!
! - No double
!
    call wkvect('&&CAPREC.INDICE', 'V V I', nbno, jind)
    do ino = 1, nbno
        do ino_1 = ino + 1, nbno
            if (zi(jlino_old+ino_1-1) .eq. zi(jlino_old+ino-1)) then
                zi(jind+ino_1-1) = 1
            endif
        enddo
    enddo
    call wkvect(list_node, 'V V I', nbno, jlino_new)
!
! - Re-create list of nodes
!
    indlis = 0
    do ino = 1, nbno
        if (zi(jind+ino-1) .eq. 0) then
            indlis = indlis + 1
            zi(jlino_new+indlis-1) = zi(jlino_old+ino-1)
        endif
    enddo
    nb_node = indlis
!
    call jedetr('&&CAPREC.INDICE')
    call jedetr('&&CAPREC.LIST')
!
    call jedema()
end subroutine

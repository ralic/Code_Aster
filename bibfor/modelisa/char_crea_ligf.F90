subroutine char_crea_ligf(mesh, ligrch, nb_node, nb_list_elem, nb_list_node)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
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
    character(len=19), intent(in) :: ligrch
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_list_elem
    integer, intent(in) :: nb_list_node
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Create <LIGREL> on nodes (for "late" elements on nodes)
!
! --------------------------------------------------------------------------------------------------
!
! In  ligrch       : name of <LIGREL>
! In  mesh         : name of mesh
! In  nb_node      : number of nodes
! In  nb_list_elem : number of list of elements for <LIGREL>
! In  nb_list_node : number of list of nodes for <LIGREL>
!
! --------------------------------------------------------------------------------------------------
!
    integer :: j_lgrf, j_nbno, j_lgns, nb_grel, iret
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    call jeexin(ligrch//'.LIEL', iret)
    if (iret .eq. 0) then
        nb_grel = nb_node
        call wkvect(ligrch//'.LGRF', 'G V K8', 2, j_lgrf)
        zk8(j_lgrf) = mesh
!
        call wkvect(ligrch//'.NBNO', 'G V I', 1, j_nbno)
        zi(j_nbno) = 0
!
        call jecrec(ligrch//'.LIEL', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                     nb_grel)
        call jeecra(ligrch//'.LIEL', 'LONT', nb_list_elem, ' ')
!
        call jecrec(ligrch//'.NEMA', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nb_grel)
        call jeecra(ligrch//'.NEMA', 'LONT', nb_list_node, ' ')

        call wkvect(ligrch//'.LGNS', 'G V I', 2*nb_node, j_lgns)
    endif
!
    call jedema()
end subroutine

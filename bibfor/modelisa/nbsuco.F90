subroutine nbsuco(sdcont      , keywf       , mesh, model, nb_cont_zone,&
                  nb_cont_elem, nb_cont_node)
!
implicit none
!
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lireco.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: nb_cont_zone
    integer, intent(out) :: nb_cont_elem
    integer, intent(out) :: nb_cont_node
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Count nodes and elements
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  model            : name of model
! In  nb_cont_zone     : number of zones of contact
! Out nb_cont_elem     : number of elements of contact
! Out nb_cont_node     : number of nodes of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_zone, i_surf
    integer :: nb_elem_slav, nb_node_slav
    integer :: nb_elem_mast, nb_node_mast
    character(len=24) :: list_elem_slav, list_elem_mast
    character(len=24) :: list_node_slav, list_node_mast
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_psumaco
    integer, pointer :: v_sdcont_psumaco(:) => null()
    character(len=24) :: sdcont_psunoco
    integer, pointer :: v_sdcont_psunoco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_cont_node = 0
    nb_cont_elem = 0
    i_surf       = 1
!
! - Datastructure for contact definition
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    sdcont_psumaco = sdcont_defi(1:16)//'.PSUMACO'
    sdcont_psunoco = sdcont_defi(1:16)//'.PSUNOCO'
    call jeveuo(sdcont_psumaco, 'E', vi = v_sdcont_psumaco)
    call jeveuo(sdcont_psunoco, 'E', vi = v_sdcont_psunoco)
!
! - Temporary datastructures
!
    list_elem_mast = '&&NBSUCO.MAIL.MAIT'
    list_elem_slav = '&&NBSUCO.MAIL.ESCL'
    list_node_mast = '&&NBSUCO.NOEU.MAIT'
    list_node_slav = '&&NBSUCO.NOEU.ESCL'
!
! - Number of nodes/elements
!
    do i_zone = 1, nb_cont_zone
        call lireco(keywf         , mesh          , model         , i_zone      , list_elem_slav,&
                    list_elem_mast, list_node_slav, list_node_mast, nb_elem_slav, nb_node_slav  ,&
                    nb_elem_mast  , nb_node_mast)
        nb_cont_node = nb_cont_node+nb_node_mast+nb_node_slav
        nb_cont_elem = nb_cont_elem+nb_elem_mast+nb_elem_slav
!
! ----- Number of master nodes/elements
!
        v_sdcont_psumaco(i_surf+1) = v_sdcont_psumaco(i_surf) + nb_elem_mast
        v_sdcont_psunoco(i_surf+1) = v_sdcont_psunoco(i_surf) + nb_node_mast
        i_surf = i_surf + 1
!
! ----- Number of slave nodes/elements
!
        v_sdcont_psumaco(i_surf+1) = v_sdcont_psumaco(i_surf) + nb_elem_slav
        v_sdcont_psunoco(i_surf+1) = v_sdcont_psunoco(i_surf) + nb_node_slav
        i_surf = i_surf + 1
    end do
!
    call jedetr(list_elem_slav)
    call jedetr(list_elem_mast)
    call jedetr(list_node_slav)
    call jedetr(list_node_mast)
!
end subroutine

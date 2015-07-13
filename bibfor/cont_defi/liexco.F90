subroutine liexco(sdcont      , keywf       , mesh, model, nb_cont_zone,&
                  nb_cont_elem, nb_cont_node)
!
implicit none
!
#include "asterfort/cfnbsf.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lireco.h"
#include "asterfort/wkvect.h"
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
    integer, intent(in) :: nb_cont_elem
    integer, intent(in) :: nb_cont_node
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Save nodes and elements
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  model            : name of model
! In  nb_cont_zone     : number of zones of contact
! In  nb_cont_elem     : number of elements of contact
! In  nb_cont_node     : number of nodes of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdecma, jdecno
    integer :: nb_elem_slav, nb_node_slav
    integer :: nb_elem_mast, nb_node_mast
    integer :: i_zone, i_surf, i_elem, i_node
    character(len=24) :: list_elem_slav, list_elem_mast
    character(len=24) :: list_node_slav, list_node_mast
    integer, pointer :: v_list(:) => null()
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    i_surf      = 1
!
! - Temporary datastructures
!
    list_elem_mast = '&&LIEXCO.MAIL.MAIT'
    list_elem_slav = '&&LIEXCO.MAIL.ESCL'
    list_node_mast = '&&LIEXCO.NOEU.MAIT'
    list_node_slav = '&&LIEXCO.NOEU.ESCL'
!
! - Datastructure for contact definition
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    sdcont_noeuco = sdcont_defi(1:16)//'.NOEUCO'
!
! - Create datastructure for nodes and elements
!
    call wkvect(sdcont_mailco, 'G V I', nb_cont_elem, vi = v_sdcont_mailco)
    call wkvect(sdcont_noeuco, 'G V I', nb_cont_node, vi = v_sdcont_noeuco)
!
! - Loop on zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Read
!
        call lireco(keywf         , mesh          , model         , i_zone      , list_elem_slav,&
                    list_elem_mast, list_node_slav, list_node_mast, nb_elem_slav, nb_node_slav  ,&
                    nb_elem_mast  , nb_node_mast)
!
! ----- Master elements
!
        call cfnbsf(sdcont_defi, i_surf, 'MAIL', nb_elem_mast, jdecma)
        call jeveuo(list_elem_mast, 'L', vi = v_list)
        do i_elem = 1, nb_elem_mast
            v_sdcont_mailco(jdecma+i_elem) = v_list(i_elem)
        end do
!
! ----- Master nodes
!
        call cfnbsf(sdcont_defi, i_surf, 'NOEU', nb_node_mast, jdecno)
        call jeveuo(list_node_mast, 'L', vi = v_list)
        do i_node = 1, nb_node_mast
            v_sdcont_noeuco(jdecno+i_node) = v_list(i_node)
        end do
!
        i_surf = i_surf + 1
!
! ----- Slave elements
!
        call cfnbsf(sdcont_defi, i_surf, 'MAIL', nb_elem_slav, jdecma)
        call jeveuo(list_elem_slav, 'L', vi = v_list)
        do i_elem = 1, nb_elem_slav
            v_sdcont_mailco(jdecma+i_elem) = v_list(i_elem)
        end do
!
! ----- Slave nodes
!
        call cfnbsf(sdcont_defi, i_surf, 'NOEU', nb_node_slav, jdecno)
        call jeveuo(list_node_slav, 'L', vi = v_list)
        do i_node = 1, nb_node_slav
            v_sdcont_noeuco(jdecno+i_node) = v_list(i_node)
        end do
!
        i_surf = i_surf + 1
    end do
!
    call jedetr(list_elem_slav)
    call jedetr(list_elem_mast)
    call jedetr(list_node_slav)
    call jedetr(list_node_mast)
!
end subroutine

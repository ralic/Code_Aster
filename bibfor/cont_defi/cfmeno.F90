subroutine cfmeno(sdcont_defi , nb_cont_surf, nb_cont_node0, v_list_node, v_poin_node,&
                  nb_cont_node)
!
implicit none
!
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
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
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: nb_cont_surf
    integer, intent(in) :: nb_cont_node0
    integer, intent(in) :: nb_cont_node
    integer, pointer, intent(in) :: v_poin_node(:)
    integer, pointer, intent(in) :: v_list_node(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress multiple nodes - Copy in contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_node0    : number of nodes of contact (before detection of multiple node)
! In  nb_cont_node     : number of nodes of contact (after detection of multiple node)
! In  v_list_node      : pointer to list of non-double nodes
! In  v_poin_node      : pointer to pointer of contact surface
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_surf, i_node
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
    character(len=24) :: sdcont_psunoco
    integer, pointer :: v_sdcont_psunoco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Datastructure for contact definition
!
    sdcont_noeuco  = sdcont_defi(1:16)//'.NOEUCO'
    sdcont_psunoco = sdcont_defi(1:16)//'.PSUNOCO'
    call jeveuo(sdcont_noeuco , 'E', vi = v_sdcont_noeuco)
    call jeveuo(sdcont_psunoco, 'E', vi = v_sdcont_psunoco)

! - PSUNOCO pointer modification
!
    do i_surf = 1, nb_cont_surf
        v_sdcont_psunoco(i_surf+1) = v_sdcont_psunoco(i_surf+1) - v_poin_node(i_surf+1)
    end do
!
! - Copy of nodes
!
    do i_node = 1, nb_cont_node
        v_sdcont_noeuco(i_node) = v_list_node(i_node)
    end do
!
! - New length of NOEUCO
!
    do i_node = nb_cont_node + 1, nb_cont_node0
        v_sdcont_noeuco(i_node) = 0
    end do
    call jeecra(sdcont_noeuco, 'LONUTI', ival=nb_cont_node)
!
end subroutine

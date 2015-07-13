subroutine cfmema(sdcont_defi , nb_cont_surf, nb_cont_elem0, v_list_elem, v_poin_elem,&
                  nb_cont_elem)
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
    integer, intent(in) :: nb_cont_elem0
    integer, intent(in) :: nb_cont_elem
    integer, pointer, intent(in) :: v_poin_elem(:)
    integer, pointer, intent(in) :: v_list_elem(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress multiple elements - Copy in contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_elem0    : number of elements of contact (before detection of multiple element)
! In  nb_cont_elem     : number of elements of contact (after detection of multiple element)
! In  v_list_elem      : pointer to list of non-double elements
! In  v_poin_elem      : pointer to pointer of contact surface
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_surf, i_elem
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_psumaco
    integer, pointer :: v_sdcont_psumaco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Datastructure for contact definition
!
    sdcont_mailco  = sdcont_defi(1:16)//'.MAILCO'
    sdcont_psumaco = sdcont_defi(1:16)//'.PSUMACO'
    call jeveuo(sdcont_mailco , 'E', vi = v_sdcont_mailco)
    call jeveuo(sdcont_psumaco, 'E', vi = v_sdcont_psumaco)
!
! - PSUMACO pointer modification
!
    do i_surf = 1, nb_cont_surf
        v_sdcont_psumaco(i_surf+1) = v_sdcont_psumaco(i_surf+1) - v_poin_elem(i_surf+1)
    end do
!
! - Copy of elements
!
    do i_elem = 1, nb_cont_elem
        v_sdcont_mailco(i_elem) = v_list_elem(i_elem)
    end do
!
! - New length of MAILCO
!
    do i_elem = nb_cont_elem + 1, nb_cont_elem0
        v_sdcont_mailco(i_elem) = 0
    end do
    call jeecra(sdcont_mailco, 'LONUTI', ival=nb_cont_elem)
!
end subroutine

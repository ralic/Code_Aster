subroutine nmext0(field_disc, nb_elem  , nb_node  , nb_poin  , nb_spoi       ,&
                  nb_cmp    , work_node, work_poin, work_elem, type_extr_elem,&
                  type_extr )
!
implicit none
!
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
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
    character(len=4), intent(in) :: field_disc
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_poin
    integer, intent(in) :: nb_spoi
    integer, intent(in) :: nb_cmp
    character(len=8), intent(in) :: type_extr_elem
    character(len=8), intent(in) :: type_extr
    character(len=19), intent(in) :: work_poin
    character(len=19), intent(in) :: work_node
    character(len=19), intent(in) :: work_elem
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Create workink vectors
!
! --------------------------------------------------------------------------------------------------
!
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  nb_node          : number of nodes
! In  nb_elem          : number of elements
! In  nb_poin          : number of points (Gauss)
! In  nb_spoi          : number of subpoints
! In  nb_cmp           : number of components
! In  work_node        : working vector to save node values
! In  work_elem        : working vector to save element values
! In  work_poin        : working vector to save point (Gauss) values
! In  type_extr        : type of extraction
! In  type_extr_elem   : type of extraction by element
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: node_init_vale, elem_init_vale
    integer :: ino, i_elem, i_cmp, i_poin, i_spoi
    real(kind=8), pointer :: v_work_elem(:) => null()
    real(kind=8), pointer :: v_work_poin(:) => null()
    real(kind=8), pointer :: v_work_node(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    if (field_disc .eq. 'NOEU') then
        call wkvect(work_node, 'V V R', nb_node*nb_cmp                , vr = v_work_node)
    else if (field_disc.eq.'ELGA') then
        call wkvect(work_poin, 'V V R', nb_poin*nb_spoi*nb_cmp        , vr = v_work_poin)
        call wkvect(work_elem, 'V V R', nb_elem*nb_poin*nb_spoi*nb_cmp, vr = v_work_elem)
    else
        ASSERT(.false.)
    endif
!
! - Values for initializations
!
    if (type_extr .eq. 'MAX') then
        node_init_vale = -r8maem()
    else if (type_extr.eq.'MIN') then
        node_init_vale = +r8maem()
    else if (type_extr.eq.'MAXI_ABS') then
        node_init_vale = 0.d0
    else if (type_extr.eq.'MINI_ABS') then
        node_init_vale = +r8maem()
    else if (type_extr.eq.'VALE') then
        node_init_vale = 0.d0
    else if (type_extr.eq.'MOY') then
        node_init_vale = 0.d0
    else
        ASSERT(.false.)
    endif
!
    if (field_disc .eq. 'ELGA') then
        if (type_extr_elem .eq. 'MAX') then
            elem_init_vale = -r8maem()
        else if (type_extr_elem.eq.'MIN') then
            elem_init_vale = +r8maem()
        else if (type_extr_elem.eq.'VALE') then
            elem_init_vale = 0.d0
        else if (type_extr_elem.eq.'MOY') then
            elem_init_vale = 0.d0
        else
            ASSERT(.false.)
        endif
    endif
!
! - Set for working vector (nodes)
!
    if (field_disc .eq. 'NOEU') then
        do ino = 1, nb_node
            do i_cmp = 1, nb_cmp
                v_work_node(nb_cmp*(ino-1)+i_cmp) = node_init_vale
            end do
        end do
    endif
!
! - Set for working vector (elements and points)
!
    if (field_disc .eq. 'ELGA') then
        do i_elem = 1, nb_elem
            do i_poin = 1, nb_poin
                do i_spoi = 1, nb_spoi
                    do i_cmp = 1, nb_cmp
                        v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem-1)+&
                                    nb_poin*nb_spoi*(i_cmp-1)+&
                                    nb_spoi*(i_poin-1)+&
                                    (i_spoi-1)) = node_init_vale
                        v_work_poin(1+nb_poin*nb_spoi*(i_cmp-1)+&
                                    nb_spoi*(i_poin-1)+&
                                    (i_spoi-1)) = elem_init_vale
                    end do
                end do
            end do
        end do
    endif
!
end subroutine

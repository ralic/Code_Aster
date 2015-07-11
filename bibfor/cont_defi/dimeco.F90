subroutine dimeco(sdcont      , model_ndim, nb_cont_zone, nb_cont_surf, nb_cont_elem,&
                  nb_cont_node)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/cfnben.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
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
    integer, intent(in) :: model_ndim
    integer, intent(in) :: nb_cont_zone
    integer, intent(in) :: nb_cont_surf
    integer, intent(in) :: nb_cont_elem
    integer, intent(in) :: nb_cont_node
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Save contact counters - Total counters
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! IO  model_ndim       : dimension of model
! In  nb_cont_zone     : number of zones of contact
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_elem     : number of elements of contact
! In  nb_cont_node     : number of nodes of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nt_elem_node
    integer :: nb_cont_point, nb_cont_poinc
    integer :: nt_cont_point, nt_cont_poinc
    integer :: i_zone, i_elem_mast, i_elem_slav
    integer :: nb_node_slav, nb_node_mast, nb_elem_slav, nb_elem_mast
    integer :: nt_node_slav, nt_node_mast, nt_elem_slav, nt_elem_mast
    integer :: nb_node_slavc, nb_node_mastc, nb_elem_slavc, nb_elem_mastc
    integer :: nt_node_esclc, nt_node_mastc, nt_elem_esclc, nt_elem_mastc
    integer :: node_nbelem
    integer :: jdecme, jdecmm, elem_slav_indx, elem_mast_indx
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_ndimco
    integer, pointer :: v_sdcont_ndimco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!

!
! - Datastructure for contact
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    sdcont_ndimco = sdcont_defi(1:16)//'.NDIMCO'
    call jeveuo(sdcont_ndimco, 'E', vi = v_sdcont_ndimco)
!
! - Set global parameters
!
    v_sdcont_ndimco(1) = model_ndim
    v_sdcont_ndimco(2) = nb_cont_zone
    v_sdcont_ndimco(3) = nb_cont_surf
    v_sdcont_ndimco(4) = nb_cont_elem
    v_sdcont_ndimco(5) = nb_cont_node
    v_sdcont_ndimco(6) = 0
    v_sdcont_ndimco(7) = 0
!
! - Total number of nodes and elements for all contact zones
!
    nt_node_slav = 0
    nt_elem_slav = 0
    nt_node_mast = 0
    nt_elem_mast = 0
    do i_zone = 1, nb_cont_zone
        nb_elem_slav = mminfi(sdcont_defi, 'NBMAE', i_zone)
        nb_node_slav = mminfi(sdcont_defi, 'NBNOE', i_zone)
        nb_elem_mast = mminfi(sdcont_defi, 'NBMAM', i_zone)
        nb_node_mast = mminfi(sdcont_defi, 'NBNOM', i_zone)
        nt_node_slav = nt_node_slav + nb_node_slav
        nt_elem_slav = nt_elem_slav + nb_elem_slav
        nt_node_mast = nt_node_mast + nb_node_mast
        nt_elem_mast = nt_elem_mast + nb_elem_mast
    end do
!
    v_sdcont_ndimco(8 ) = nt_node_slav
    v_sdcont_ndimco(9 ) = nt_elem_slav
    v_sdcont_ndimco(10) = nt_node_mast
    v_sdcont_ndimco(11) = nt_elem_mast
!
! - Total number of nodes and elements for computation contact zone
!
    nt_node_esclc = 0
    nt_elem_esclc = 0
    nt_node_mastc = 0
    nt_elem_mastc = 0
    do i_zone = 1, nb_cont_zone
        nb_elem_slavc = mminfi(sdcont_defi, 'NBMAEC', i_zone)
        nb_node_slavc = mminfi(sdcont_defi, 'NBNOEC', i_zone)
        nb_elem_mastc = mminfi(sdcont_defi, 'NBMAMC', i_zone)
        nb_node_mastc = mminfi(sdcont_defi, 'NBNOMC', i_zone)
        nt_node_esclc = nt_node_esclc + nb_node_slavc
        nt_elem_esclc = nt_elem_esclc + nb_elem_slavc
        nt_node_mastc = nt_node_mastc + nb_node_mastc
        nt_elem_mastc = nt_elem_mastc + nb_elem_mastc
    end do
!
    v_sdcont_ndimco(12) = nt_node_esclc
    v_sdcont_ndimco(13) = nt_elem_esclc
    v_sdcont_ndimco(14) = nt_node_mastc
    v_sdcont_ndimco(15) = nt_elem_mastc
!
! - Total number of contact point
!
    nt_cont_point = 0
    do i_zone = 1, nb_cont_zone
        nb_cont_point = mminfi(sdcont_defi, 'NBPT', i_zone)
        nt_cont_point = nt_cont_point + nb_cont_point
    end do
!
    v_sdcont_ndimco(16) = nt_cont_point
!
! - Total number of contact point for computation
!
    nt_cont_poinc = 0
    do i_zone = 1, nb_cont_zone
        nb_cont_poinc = mminfi(sdcont_defi, 'NBPC', i_zone)
        nt_cont_poinc = nt_cont_poinc + nb_cont_poinc
    end do
!
    v_sdcont_ndimco(17) = nt_cont_poinc
!
! - Total number of nodes by element (ELNO)
!
    nt_elem_node = 0
    do i_zone = 1, nb_cont_zone
        jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
        nb_elem_slav = mminfi(sdcont_defi, 'NBMAE' , i_zone)
        do i_elem_slav = 1, nb_elem_slav
            elem_slav_indx = jdecme + i_elem_slav
            call cfnben(sdcont_defi, elem_slav_indx, 'CONNEX', node_nbelem)
            nt_elem_node = nt_elem_node + node_nbelem
        end do
        jdecmm       = mminfi(sdcont_defi, 'JDECMM', i_zone)
        nb_elem_mast = mminfi(sdcont_defi, 'NBMAM' , i_zone)
        do i_elem_mast = 1, nb_elem_mast
            elem_mast_indx = jdecmm + i_elem_mast
            call cfnben(sdcont_defi, elem_mast_indx, 'CONNEX', node_nbelem)
            nt_elem_node = nt_elem_node + node_nbelem
        end do
    end do
!
    v_sdcont_ndimco(18) = nt_elem_node
!
end subroutine

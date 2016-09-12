subroutine aptgno(sdappa, mesh, sdcont_defi)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfcald.h"
#include "asterfort/cfdisi.h"
#include "asterfort/aptgnn.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/infdbg.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents at each node (average)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_cont_zone, model_ndim
    integer :: i_zone, norm_type
    integer :: jdecnm, nb_node_mast
    integer :: jdecne, nb_node_slav
    aster_logical :: apcald
    real(kind=8) :: norm_vect(3)
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> ...... TANGENTES SUR LES NOEUDS'
    endif
!
! - Get parameters
!
    model_ndim   = cfdisi(sdcont_defi,'NDIM'  )
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO' )
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone - Master
!
        nb_node_mast   = mminfi(sdcont_defi, 'NBNOM'    , i_zone)
        jdecnm         = mminfi(sdcont_defi, 'JDECNM'   , i_zone)
        norm_type      = mminfi(sdcont_defi, 'VECT_MAIT', i_zone)
        norm_vect(1:3) = 0.d0
        if (norm_type .ne. 0) then
            norm_vect(1) = mminfr(sdcont_defi, 'VECT_MAIT_DIRX', i_zone)
            norm_vect(2) = mminfr(sdcont_defi, 'VECT_MAIT_DIRY', i_zone)
            norm_vect(3) = mminfr(sdcont_defi, 'VECT_MAIT_DIRZ', i_zone)
        endif      
!
! ----- Compute tangents at each node by smoothing - On current zone/Master
!
        apcald = cfcald(sdcont_defi, i_zone, 'MAIT')
        if (apcald) then
            call aptgnn(sdappa      , mesh     , sdcont_defi, model_ndim, jdecnm,&
                        nb_node_mast, norm_type, norm_vect)
        endif
!
! ----- Parameters on current zone - Slave
!
        nb_node_slav   = mminfi(sdcont_defi, 'NBNOE'    , i_zone)
        jdecne         = mminfi(sdcont_defi, 'JDECNE'   , i_zone)
        norm_type      = mminfi(sdcont_defi, 'VECT_ESCL', i_zone)
        norm_vect(1:3) = 0.d0
        if (norm_type .ne. 0) then
            norm_vect(1) = mminfr(sdcont_defi, 'VECT_ESCL_DIRX', i_zone)
            norm_vect(2) = mminfr(sdcont_defi, 'VECT_ESCL_DIRY', i_zone)
            norm_vect(3) = mminfr(sdcont_defi, 'VECT_ESCL_DIRZ', i_zone)
        endif
!
! ----- Compute tangents at each node by smoothing - On current zone/salve
!
        apcald = cfcald(sdcont_defi, i_zone, 'ESCL')
        if (apcald) then
            call aptgnn(sdappa      , mesh     , sdcont_defi, model_ndim, jdecne,&
                        nb_node_slav, norm_type, norm_vect)
        endif
    end do
!
end subroutine

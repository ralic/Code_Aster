subroutine aptgno(sdappa, mesh, sdcont_defi)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/appari.h"
#include "asterfort/aptgnn.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/apzonv.h"
#include "asterfort/infdbg.h"
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
    call appari(sdappa, 'APPARI_NDIMG', model_ndim)
    call appari(sdappa, 'APPARI_NBZONE', nb_cont_zone)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone - Master
!
        call apzoni(sdappa, i_zone, 'NBNOM'         , nb_node_mast)
        call apzoni(sdappa, i_zone, 'JDECNM'        , jdecnm)
        call apzoni(sdappa, i_zone, 'TYPE_NORM_MAIT', norm_type)
        call apzonv(sdappa, i_zone, 'VECT_MAIT'     , norm_vect)
!
! ----- Compute tangents at each node by smoothing - On current zone/Master
!
        call apzonl(sdappa, i_zone, 'CALC_NORM_MAIT', apcald)
        if (apcald) then
            call aptgnn(sdappa      , mesh     , sdcont_defi, model_ndim, jdecnm,&
                        nb_node_mast, norm_type, norm_vect)
        endif
!
! ----- Parameters on current zone - Slave
!
        call apzoni(sdappa, i_zone, 'NBNOE'         , nb_node_slav)
        call apzoni(sdappa, i_zone, 'JDECNE'        , jdecne)
        call apzoni(sdappa, i_zone, 'TYPE_NORM_ESCL', norm_type)
        call apzonv(sdappa, i_zone, 'VECT_ESCL'     , norm_vect)
!
! ----- Compute tangents at each node by smoothing - On current zone/salve
!
        call apzonl(sdappa, i_zone, 'CALC_NORM_ESCL', apcald)
        if (apcald) then
            call aptgnn(sdappa      , mesh     , sdcont_defi, model_ndim, jdecne,&
                        nb_node_slav, norm_type, norm_vect)
        endif
    end do
!
end subroutine

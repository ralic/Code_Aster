subroutine aptgen(sdappa, mesh, sdcont_defi, newgeo)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisr.h"
#include "asterfort/aptgem.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/cfcald.h"
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
    character(len=19), intent(in) :: newgeo
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents at each node for each element
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_zone, model_ndim
    integer :: ifm, niv
    integer :: i_zone
    integer :: jdecmm, nb_elem_mast
    integer :: jdecme, nb_elem_slav
    character(len=4) :: zone_type
    aster_logical :: apcald
    real(kind=8) :: epsi_maxi
    integer :: iter_maxi
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> ...... TANGENTES SUR LES NOEUDS PAR ELEMENT (ELNO)'
    endif
!
! - Get parameters
!
    epsi_maxi    = cfdisr(sdcont_defi,'PROJ_NEWT_RESI')
    iter_maxi    = cfdisi(sdcont_defi,'PROJ_NEWT_ITER')
    model_ndim   = cfdisi(sdcont_defi,'NDIM'  )
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO' )
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone - Master
!
        nb_elem_mast = mminfi(sdcont_defi, 'NBMAM' , i_zone)
        jdecmm       = mminfi(sdcont_defi, 'JDECMM', i_zone)
        zone_type = 'MAIT'
!
! ----- Compute tangents at each node for each element - Master
!
        apcald = cfcald(sdcont_defi, i_zone, 'MAIT')
        if (apcald) then
            call aptgem(sdappa      , mesh     , newgeo   , sdcont_defi, model_ndim,&
                        i_zone      , zone_type, iter_maxi, epsi_maxi  , jdecmm    ,&
                        nb_elem_mast)
        endif
!
! ----- Parameters on current zone - Slave
!
        nb_elem_slav = mminfi(sdcont_defi, 'NBMAE' , i_zone)
        jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
        zone_type = 'ESCL'
!
! ----- Compute tangents at each node for each element - Slave
!
        apcald = cfcald(sdcont_defi, i_zone, 'ESCL')
        if (apcald) then
            call aptgem(sdappa      , mesh     , newgeo   , sdcont_defi, model_ndim,&
                        i_zone      , zone_type, iter_maxi, epsi_maxi  , jdecme    ,&
                        nb_elem_slav)
        endif
    end do
!
end subroutine

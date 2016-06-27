subroutine apstos(mesh, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/mminfr.h"
#include "asterfort/jecrec.h"
#include "asterfort/jeveuo.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/aplcpb.h"
#include "asterfort/aplcno.h"
#include "asterfort/aplcpg.h"
#include "asterfort/aplcfb.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
#include "asterfort/jerazo.h"
#include "asterfort/apstoc.h"
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
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Pairing - Segment to segment
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: newgeo, sdappa
    character(len=8) :: knuzo
    integer :: nb_elem_mast, nb_elem_slav
    character(len=24) :: sdappa_mast, sdappa_slav
    integer :: i_zone, nb_pair_zone, nb_cont_zone, nt_patch, zmeth
    aster_logical :: l_smooth
    real(kind=8) :: pair_tole
    character(len=24) :: pair_method
    integer, pointer :: list_pair_zone(:) => null()
    character(len=24) :: sdappa_gapi
    integer, pointer :: v_sdappa_mast(:) => null()
    integer, pointer :: v_sdappa_slav(:) => null()
    integer, pointer :: v_sdcont_methco(:) => null()
    character(len=24) :: sdcont_methco
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<Pairing> Segment-to-segment pairing'
    endif
!
! - Initializations
!
    nb_pair_zone   = 0
!
! - Get parameters
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi, 'NZOCO')
    l_smooth     = cfdisl(ds_contact%sdcont_defi, 'LISSAGE')
    nt_patch     = ds_contact%nt_patch
    zmeth        = cfmmvd('ZMETH')
!
! - Updated geometry
!
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
!
! - Access to pairing datastructures
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    call jerazo(sdappa_gapi, nt_patch, 1)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Get parameters
!
        pair_tole    = mminfr(ds_contact%sdcont_defi, 'TOLE_APPA', i_zone)
        sdcont_methco = ds_contact%sdcont_defi(1:16)//'.METHCO'
        call jeveuo(sdcont_methco, 'L', vi = v_sdcont_methco)
        if (v_sdcont_methco(zmeth*(i_zone-1)+7) .eq. 2) then
            pair_method  = 'FORCEE'        
        else if (v_sdcont_methco(zmeth*(i_zone-1)+7) .eq. 3) then 
            pair_method  = 'ROBUSTE'        
        else if (v_sdcont_methco(zmeth*(i_zone-1)+7) .eq. 4) then 
            pair_method  = 'RAPIDE'
        endif
!
! ----- Generate name of objects
!
        ASSERT(i_zone .le. 9)
        call codent(i_zone, 'G', knuzo)
        sdappa_mast = sdappa(1:19)//'.MAS'//knuzo(1:1)
        sdappa_slav = sdappa(1:19)//'.ESC'//knuzo(1:1)
!
! ----- Get objects
!
        call jelira(sdappa_mast, 'LONMAX', nb_elem_mast)
        call jelira(sdappa_slav, 'LONMAX', nb_elem_slav) 
        call jeveuo(sdappa_mast, 'L', vi = v_sdappa_mast)
        call jeveuo(sdappa_slav, 'L', vi = v_sdappa_slav)
!
! ----- Pairing
!
        if (pair_method.eq.'ROBUSTE') then
            call aplcpb(mesh        , newgeo        , sdappa      , i_zone       , pair_tole,&
                        nb_elem_mast, v_sdappa_mast , nb_elem_slav, v_sdappa_slav, &
                        nb_pair_zone, list_pair_zone)
        elseif (pair_method.eq.'RAPIDE') then
            call aplcpg(mesh        , newgeo        , sdappa      , i_zone       , pair_tole,&
                        nb_elem_mast, v_sdappa_mast , nb_elem_slav, v_sdappa_slav, &
                        nb_pair_zone, list_pair_zone)
        elseif (pair_method.eq.'FORCEE') then
            call aplcfb(mesh        , newgeo        , sdappa      , i_zone       , pair_tole,&
                        nb_elem_mast, v_sdappa_mast , nb_elem_slav, v_sdappa_slav, &
                        nb_pair_zone, list_pair_zone)
        else
            ASSERT(.false.)
        endif
    end do
!
! - Save pairing information in sdappa data structure
!
    call apstoc(ds_contact, nb_pair_zone, list_pair_zone)
!
! - Compute smooth normals at nodes
!
    if (l_smooth) then
        call aplcno(mesh, newgeo, ds_contact%sdcont_defi, sdappa)
    end if
!
end subroutine

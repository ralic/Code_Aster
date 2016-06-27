subroutine apalac(mesh, newgeo, ds_contact, sdappa)
!
use NonLin_Datastructure_type
!
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
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: newgeo
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: sdappa
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Pairing on contact zones
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdappa           : name of pairing datastructure
!
! --------------------------------------------------------------------------------------------------
!
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
    nb_pair_zone   = 0
!
! - Get parameters
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi, 'NZOCO')
    l_smooth     = cfdisl(ds_contact%sdcont_defi, 'LISSAGE')
    nt_patch     = ds_contact%nt_patch
    zmeth        = cfmmvd('ZMETH')
!
! - Access to pairing datastructures
!
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
    call apstoc(sdappa, nb_pair_zone, list_pair_zone)
!
! - Compute smooth normals at nodes
!
    if (l_smooth) then
        call aplcno(mesh, newgeo, ds_contact%sdcont_defi, sdappa)
    end if
!
end subroutine        

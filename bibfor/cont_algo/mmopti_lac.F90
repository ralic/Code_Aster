subroutine mmopti_lac(mesh, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/jedema.h"
#include "asterfort/mminfi.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Initial options (*_INIT)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_cont_zone, i_patch, nb_patch, nb_cont_zone
    integer :: j_patch, cont_init
    integer :: indi_cont_curr, indi_cont_prev
    real(kind=8) :: tole_inter, pair_init, gap
    character(len=19) :: sdappa
    character(len=24) :: sdcont_stat
    integer, pointer :: v_sdcont_stat(:) => null()
    character(len=24) :: sdappa_gapi
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    character(len=24) :: sdappa_coef
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    integer, pointer :: v_mesh_patch(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Initial options'
    endif
!
! - Initializations
!
    tole_inter = 1.d-5
!
! - Get parameters
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
!
! - Access to mesh (patch)
!
    call jeveuo(jexnum(mesh//'.PATCH',1), 'L', vi = v_mesh_patch)
!
! - Get pairing datastructure
!
    sdcont_stat = ds_contact%sdcont_solv(1:14)//'.STAT'
    call jeveuo(sdcont_stat, 'E', vi = v_sdcont_stat)
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    sdappa_coef = sdappa(1:19)//'.COEF' 
    call jeveuo(sdappa_gapi, 'L', vr = v_sdappa_gapi)
    call jeveuo(sdappa_coef, 'L', vr = v_sdappa_coef)
!
! - Loop on contact zones
!
    do i_cont_zone = 1, nb_cont_zone
!
! ----- Get access to patch
!
        nb_patch = v_mesh_patch((i_cont_zone-1)*2+2)
        j_patch  = v_mesh_patch((i_cont_zone-1)*2+1)
!
! ----- Get initial pairing
!
        pair_init = 100.d0*r8prem()
        cont_init = mminfi(ds_contact%sdcont_defi, 'CONTACT_INIT', i_cont_zone)
        if (cont_init .eq. 1) then
            pair_init = 0.5               
        end if
!
! ----- Loop on patches
!
        do i_patch=1, nb_patch
!
            gap            = v_sdappa_gapi(j_patch-2+i_patch)
            indi_cont_prev = v_sdcont_stat(j_patch-2+i_patch)
!
! --------- Compute new status
!
            if (gap .ne. gap ) then
                indi_cont_curr = -1
            else
                if (gap .le. pair_init .and.&
                    v_sdappa_coef(j_patch-2+i_patch).ge.tole_inter) then
                    indi_cont_curr = 1
                else
                    indi_cont_curr = 0
                endif            
            endif
!
! --------- Save new status
!
            v_sdcont_stat(j_patch-2+i_patch) = indi_cont_curr
        end do
    end do
!
    call jedema()
!
end subroutine

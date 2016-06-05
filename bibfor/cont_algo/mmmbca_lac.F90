subroutine mmmbca_lac(mesh, hval_incr, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jexnum.h"
#include "asterfort/detrsd.h"
#include "asterfort/mreacg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmchex.h"
#include "asterfort/mmfield_prep.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: hval_incr(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Management of contact loop
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  hval_incr        : hat-variable for incremental values fields
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_cont_zone, i_patch, nb_patch, nb_cont_zone
    integer :: j_patch, node_nume
    integer :: indi_cont_curr, indi_cont_prev, loop_cont_vali
    aster_logical :: loop_cont_conv
    character(len=19) :: depplu, oldgeo, newgeo, cnscon
    real(kind=8) :: tole_inter, gap, lagc, coefint, loop_cont_vale
    character(len=24) :: sdcont_stat
    integer, pointer :: v_sdcont_stat(:) => null()
    character(len=24) :: sdcont_lagc
    real(kind=8), pointer :: v_sdcont_lagc(:) => null()
    character(len=19) :: sdappa
    character(len=24) :: sdappa_gapi
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    character(len=24) :: sdappa_coef
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    integer, pointer :: v_mesh_patch(:) => null()
    integer, pointer :: v_mesh_lpatch(:) => null()
    real(kind=8), pointer :: v_cnscon_cnsv(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... ACTIVATION/DESACTIVATION'
    endif
!
! - Initializations
!
    loop_cont_conv = .true.
    loop_cont_vali = 0
    tole_inter     = 1.d-5
!
! - Get parameters
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi, 'NZOCO')
!
! - Access to mesh (patches)
!
    call jeveuo(jexnum(mesh//'.PATCH',1), 'L', vi = v_mesh_lpatch)
!
! - Acces to contact objects
!
    sdcont_stat = ds_contact%sdcont_solv(1:14)//'.STAT'
    sdcont_lagc = ds_contact%sdcont_solv(1:14)//'.LAGC'
    call jeveuo(sdcont_stat, 'E', vi = v_sdcont_stat)
    call jeveuo(sdcont_lagc, 'E', vr = v_sdcont_lagc)
!
! - Get pairing datastructure
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    sdappa_coef = sdappa(1:19)//'.COEF' 
    call jeveuo(sdappa_gapi, 'L', vr = v_sdappa_gapi)
    call jeveuo(sdappa_coef, 'L', vr = v_sdappa_coef)
!
! - Get hat variables
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! - Geometric update
!
    oldgeo = mesh//'.COORDO'
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
    call mreacg(mesh, ds_contact, field_update_ = depplu)
!
! - Prepare displacement field to get contact Lagrangien multiplier
!
    cnscon = '&&MMMBCA.CNSCON'
    call mmfield_prep(depplu, cnscon,&
                      l_sort_ = .true._1, nb_cmp_ = 1, list_cmp_ = ['LAGS_C  '])
    call jeveuo(cnscon//'.CNSV', 'L', vr = v_cnscon_cnsv)
!
! - Loop on contact zones
!
    do i_cont_zone = 1, nb_cont_zone
!
! ----- Get access to patch
!
        nb_patch = v_mesh_lpatch((i_cont_zone-1)*2+2)
        j_patch  = v_mesh_lpatch((i_cont_zone-1)*2+1)
!
! ----- Loop on patches
!
        do i_patch = 1, nb_patch
!
! --------- Get current patch

            call jeveuo(jexnum(mesh//'.PATCH',j_patch+i_patch-1), 'L', vi = v_mesh_patch)
!
! --------- Get/Set LAGS_C 
!
            node_nume = v_mesh_patch(2)
            lagc      = v_cnscon_cnsv(node_nume)
            v_sdcont_lagc(j_patch-2+i_patch) = lagc
!
! --------- Get previous parameters
!
            coefint        = v_sdappa_coef(j_patch-2+i_patch)
            gap            = v_sdappa_gapi(j_patch-2+i_patch)
            indi_cont_prev = v_sdcont_stat(j_patch-2+i_patch)
!
! --------- Compute new status
!
            if (isnan(gap)) then
                indi_cont_curr = -1
            else
                if ((lagc+gap) .le. r8prem() .and.&
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
!
! --------- Change status ?
!
            if (indi_cont_curr .ne. indi_cont_prev .and.&
                (indi_cont_curr.eq.1 .or. indi_cont_prev.eq.1) ) then
                loop_cont_vali = loop_cont_vali+1
            end if    
        end do
    end do
!
! - Convergence ?
!
    loop_cont_conv = loop_cont_vali .eq. 0
!
! - Set loop values
!
    if (loop_cont_conv) then
        call mmbouc(ds_contact, 'Cont', 'Set_Convergence')
    else
        call mmbouc(ds_contact, 'Cont', 'Set_Divergence')
    endif
    loop_cont_vale = real(loop_cont_vali, kind=8)
    call mmbouc(ds_contact, 'Cont', 'Set_Vale' , loop_vale_ = loop_cont_vale)
!
! - Cleaning
!
    call jedetr(newgeo)
    call detrsd('CHAM_NO_S', cnscon)
!
    call jedema()
end subroutine

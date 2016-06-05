subroutine cfmxr0_lac(mesh, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnum.h"
#include "asterfort/detrsd.h"
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
! Create CONT_ELEM for storing contact results
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: ncmp   = 6
    integer, parameter :: nceld1 = 4
    integer, parameter :: nceld2 = 4
    integer, parameter :: nceld3 = 4 
    integer :: ifm, niv
    integer :: nt_patch, nb_grel, nb_liel
    integer :: i_liel, i_grel, patch_indx, elem_slav_nume, iret, vale_indx, decal
    real(kind=8) :: lagc, gapi, coef
    integer :: indi_cont
    character(len=19) :: celinr
    character(len=24) :: celinr_celv
    integer :: jv_celinr_celv
    character(len=24) :: celinr_celd
    integer, pointer :: v_celinr_celd(:) => null()
    character(len=8)  :: ligrel_link_slav
    integer, pointer :: v_ligrel_liel(:) => null()
    character(len=19) :: sdappa
    character(len=24) :: sdappa_coef
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    character(len=24) :: sdcont_stat
    integer, pointer :: v_sdcont_stat(:) => null()
    character(len=24) :: sdcont_lagc 
    real(kind=8), pointer :: v_sdcont_lagc(:) => null()
    character(len=24) :: sdappa_gapi
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    integer, pointer :: v_mesh_comapa(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Create contact field for post-treatment'
    endif
!
! - Get pairing datastructure
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - Get parameters
!
    nt_patch  = ds_contact%nt_patch
!
! - Access to contact objects
!
    sdcont_stat = ds_contact%sdcont_solv(1:14)//'.STAT'
    sdcont_lagc = ds_contact%sdcont_solv(1:14)//'.LAGC'
    call jeveuo(sdcont_stat, 'L', vi = v_sdcont_stat)
    call jeveuo(sdcont_lagc, 'L', vr = v_sdcont_lagc)
    sdappa_coef = sdappa(1:19)//'.COEF'
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    call jeveuo(sdappa_coef, 'L', vr = v_sdappa_coef)
    call jeveuo(sdappa_gapi, 'L', vr = v_sdappa_gapi)
!
! - Access to mesh
!
    call jeveuo(mesh//'.COMAPA','L', vi = v_mesh_comapa)
!
! - Get list of elements for slave surface (create in DEFI_CONTACT)
!
    ligrel_link_slav = ds_contact%ligrel_elem_slav
!
! - <CHELEM> for CONT_ELEM field
!
    celinr = ds_contact%field_cont_elem
!
! - Create new CONT_ELEM field
!
    call detrsd('CHAM_ELEM', celinr)
    call alchml(ligrel_link_slav//'.CHME.LIGRE', 'CONT_ELEM', 'CT_ELEM', 'V', celinr,&
                iret, ' ')
    ASSERT(iret.eq.0)
!
! - Access to input field
!
    celinr_celd = celinr//'.CELD'
    celinr_celv = celinr//'.CELV'
    call jeveuo(celinr_celd, 'L', vi = v_celinr_celd)
    call jeveuo(celinr_celv, 'E', jv_celinr_celv)
    nb_grel = v_celinr_celd(2)
!
! - Fill input field
!
    do i_grel = 1, nb_grel
        decal   = v_celinr_celd(nceld1+i_grel)
        nb_liel = v_celinr_celd(decal+1)
        if (v_celinr_celd(decal+3) .ne. 0) then
            ASSERT(v_celinr_celd(decal+3).eq.ncmp)
            call jeveuo(jexnum(ligrel_link_slav//'.CHME.LIGRE.LIEL', i_grel),&
                         'L', vi = v_ligrel_liel)
            do i_liel = 1, nb_liel
                elem_slav_nume = v_ligrel_liel(i_liel)
                patch_indx     = v_mesh_comapa(elem_slav_nume)
                vale_indx      = jv_celinr_celv-1+v_celinr_celd(decal+nceld2+nceld3*(i_liel-1)+4)
                if (patch_indx .ne. 0) then
                    lagc      = v_sdcont_lagc(patch_indx)
                    gapi      = v_sdappa_gapi(patch_indx)
                    coef      = v_sdappa_coef(patch_indx)
                    indi_cont = v_sdcont_stat(patch_indx)
                    zr(vale_indx-1+1) = lagc
                    if (gapi .eq. gapi) then
                        zr(vale_indx-1+2) = gapi
                    else
                        zr(vale_indx-1+2) = -1.d0
                    end if
                    zr(vale_indx-1+3) = indi_cont
                    zr(vale_indx-1+4) = coef
                    zr(vale_indx-1+5) = coef*lagc
                end if
            enddo
        endif
    enddo
!
    call jedema()
end subroutine

subroutine mmchml_l(mesh, ds_contact, ligrcf, chmlcf)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/cfdisi.h"
#include "asterfort/mminfi.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: ligrcf
    character(len=19), intent(in) :: chmlcf
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Create and fill input field
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  ligrcf           : name of LIGREL for contact element
! In  chmlcf           : name of CHAM_LEM for input field
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: ncmp   = 28
    integer, parameter :: nceld1 = 4
    integer, parameter :: nceld2 = 4
    integer, parameter :: nceld3 = 4
    integer :: nt_liel, nb_grel, nb_liel, i_grel, i_liel, i_cont_pair, nb_cont_pair, i_zone
    integer :: vale_indx, decal, elem_slav_nume, patch_nume, jacobian_type, nb_cont_zone
    real(kind=8) :: r_axi, r_smooth
    character(len=19) :: sdappa
    character(len=24) :: chmlcf_celv
    integer :: jv_chmlcf_celv
    character(len=24) :: chmlcf_celd
    integer, pointer :: v_chmlcf_celd(:) => null()
    integer, pointer :: v_ligrcf_liel(:) => null()
    integer, pointer :: v_mesh_comapa(:) => null()
    character(len=24) :: sdappa_coef
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    character(len=24) :: sdcont_stat
    integer, pointer :: v_sdcont_stat(:) => null()
    character(len=24) :: sdcont_lagc 
    real(kind=8), pointer :: v_sdcont_lagc(:) => null()
    character(len=24) :: sdappa_apli
    integer, pointer :: v_sdappa_apli(:) => null()
    integer, pointer :: typ_jaco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get parameters
!
    r_smooth     = real(cfdisi(ds_contact%sdcont_defi,'LISSAGE'),kind=8)
    r_axi        = real(cfdisi(ds_contact%sdcont_defi,'AXISYMETRIQUE'),kind=8)
    nb_cont_pair = ds_contact%nb_cont_pair
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi, 'NZOCO')
    AS_ALLOCATE(vi=typ_jaco, size= nb_cont_zone)
    do i_zone=1, nb_cont_zone
        typ_jaco(i_zone)= mminfi(ds_contact%sdcont_defi, 'TYPE_JACOBIEN', i_zone)
    end do
!
! - Access to input field
!
    chmlcf_celd = chmlcf//'.CELD'
    chmlcf_celv = chmlcf//'.CELV'
    call jeveuo(chmlcf_celd, 'L', vi = v_chmlcf_celd)
    call jeveuo(chmlcf_celv, 'E', jv_chmlcf_celv)
    nb_grel = v_chmlcf_celd(2)
!
! - Get pairing datastructure
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - Access to mesh
!
    call jeveuo(mesh//'.COMAPA','L',vi = v_mesh_comapa)
!
! - Access to contact objects
!
    sdcont_stat = ds_contact%sdcont_solv(1:14)//'.STAT'
    sdcont_lagc = ds_contact%sdcont_solv(1:14)//'.LAGC'
    call jeveuo(sdcont_stat, 'L', vi = v_sdcont_stat)
    call jeveuo(sdcont_lagc, 'L', vr = v_sdcont_lagc)
    sdappa_coef = sdappa(1:19)//'.COEF'
    sdappa_apli = sdappa(1:19)//'.APLI'
    call jeveuo(sdappa_coef, 'L', vr = v_sdappa_coef)
    call jeveuo(sdappa_apli, 'L', vi = v_sdappa_apli)
!
! - Fill input field
!
    nt_liel = 0
    do i_grel = 1, nb_grel
        decal   = v_chmlcf_celd(nceld1+i_grel)
        nb_liel = v_chmlcf_celd(decal+1)
        ASSERT(v_chmlcf_celd(decal+3).eq.ncmp)
        call jeveuo(jexnum(ligrcf//'.LIEL', i_grel), 'L', vi = v_ligrcf_liel)
        do i_liel = 1, nb_liel
! --------- Get patch
            i_cont_pair    = -v_ligrcf_liel(i_liel)
            elem_slav_nume = v_sdappa_apli(3*(i_cont_pair-1)+1)
            i_zone         = v_sdappa_apli(3*(i_cont_pair-1)+3)
            patch_nume     = v_mesh_comapa(elem_slav_nume)
            jacobian_type  = typ_jaco(i_zone)
! --------- Adress in CHAM_ELEM
            vale_indx = jv_chmlcf_celv-1+v_chmlcf_celd(decal+nceld2+nceld3*(i_liel-1)+4)
! --------- Set values in CHAM_ELEM
            zr(vale_indx-1+1)  = r_smooth
            zr(vale_indx-1+2)  = jacobian_type
            zr(vale_indx-1+3)  = 0.d0
            zr(vale_indx-1+4)  = 0.d0
            zr(vale_indx-1+5)  = 0.d0
            zr(vale_indx-1+6)  = 0.d0
            zr(vale_indx-1+7)  = 0.d0
            zr(vale_indx-1+8)  = 0.d0
            zr(vale_indx-1+9)  = 0.d0
            zr(vale_indx-1+10) = 0.d0
            zr(vale_indx-1+11) = v_sdappa_coef(patch_nume)
            zr(vale_indx-1+12) = v_sdcont_stat(patch_nume)
            zr(vale_indx-1+13) = v_sdcont_lagc(patch_nume)
            zr(vale_indx-1+14) = r_axi
            zr(vale_indx-1+15) = 0.d0
            zr(vale_indx-1+16) = 0.d0
            zr(vale_indx-1+17) = 0.d0
            zr(vale_indx-1+18) = 0.d0
            zr(vale_indx-1+19) = 0.d0
            zr(vale_indx-1+20) = 0.d0
            zr(vale_indx-1+21) = 0.d0
            zr(vale_indx-1+22) = 0.d0
            zr(vale_indx-1+23) = 0.d0
            zr(vale_indx-1+24) = 0.d0
            zr(vale_indx-1+25) = 1.d0
        end do
        nt_liel = nt_liel + nb_liel
    enddo
    ASSERT(nt_liel .eq. nb_cont_pair)
    AS_DEALLOCATE(vi=typ_jaco)
!
    call jedema()
!
end subroutine

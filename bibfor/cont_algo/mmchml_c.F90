subroutine mmchml_c(ds_contact, ligrcf, chmlcf, sddyna, time_incr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
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
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: ligrcf
    character(len=19), intent(in) :: chmlcf
    character(len=19), intent(in) :: sddyna
    real(kind=8), intent(in) :: time_incr
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Create and fill input field
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  ligrcf           : name of LIGREL for contact element
! In  chmlcf           : name of CHAM_LEM for input field
! In  sddyna           : datastructure for dynamic
! In  time_incr        : time increment
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: ncmp   = 28
    integer, parameter :: nceld1 = 4
    integer, parameter :: nceld2 = 4
    integer, parameter :: nceld3 = 4 
    integer :: ztabf
    integer :: i_cont_poin, i_zone, nt_cont_poin
    integer :: vale_indx, decal
    aster_logical :: l_dyna, l_theta
    real(kind=8) :: theta
    integer :: dyna_form
    real(kind=8) :: coef_fric
    integer :: i_algo_cont, i_algo_fric, i_reso_fric, i_reso_geom
    integer :: nt_liel, nb_grel, nb_liel, i_grel, i_liel
    character(len=24) :: chmlcf_celv
    integer :: jv_chmlcf_celv
    character(len=24) :: chmlcf_celd  
    integer, pointer :: v_chmlcf_celd(:) => null()
    integer, pointer :: v_ligrcf_liel(:) => null()
    character(len=24) :: sdcont_tabfin, sdcont_jsupco
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    real(kind=8), pointer :: v_sdcont_jsupco(:) => null()
    character(len=24) :: sdcont_cychis
    real(kind=8), pointer :: v_sdcont_cychis(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Active functionnalities
!
    l_dyna  = ndynlo(sddyna,'DYNAMIQUE')
    l_theta = ndynlo(sddyna,'THETA_METHODE')
!
! - Access to contact objects
!
    sdcont_jsupco = ds_contact%sdcont_solv(1:14)//'.JSUPCO'
    sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    sdcont_cychis = ds_contact%sdcont_solv(1:14)//'.CYCHIS'
    call jeveuo(sdcont_jsupco, 'L', vr = v_sdcont_jsupco)
    call jeveuo(sdcont_tabfin, 'L', vr = v_sdcont_tabfin)
    call jeveuo(sdcont_cychis, 'L', vr = v_sdcont_cychis)
    ztabf = cfmmvd('ZTABF')
!
! - Get_contact parameters
!
    i_reso_fric  = cfdisi(ds_contact%sdcont_defi,'ALGO_RESO_FROT')
    i_reso_geom  = cfdisi(ds_contact%sdcont_defi,'ALGO_RESO_GEOM')
    nt_cont_poin = nint(v_sdcont_tabfin(1))
!
! - Get dynamic parameters
!
    theta     = 0.d0
    dyna_form = 0
    if (l_dyna) then
        if (l_theta) then
            theta = ndynre(sddyna,'THETA')
            dyna_form = 2
        else
            dyna_form = 1
        endif
    endif
!
! - Access to input field
!
    chmlcf_celd = chmlcf//'.CELD'
    chmlcf_celv = chmlcf//'.CELV'
    call jeveuo(chmlcf_celd, 'L', vi = v_chmlcf_celd)
    call jeveuo(chmlcf_celv, 'E', jv_chmlcf_celv)
    nb_grel = v_chmlcf_celd(2)
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
            i_cont_poin = -v_ligrcf_liel(i_liel)
            i_zone      = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
            coef_fric   = mminfr(ds_contact%sdcont_defi, 'COEF_COULOMB' , i_zone)
            i_algo_cont = mminfi(ds_contact%sdcont_defi, 'ALGO_CONT'    , i_zone)
            i_algo_fric = mminfi(ds_contact%sdcont_defi, 'ALGO_FROT'    , i_zone)
! --------- Adress in CHAM_ELEM
            vale_indx = jv_chmlcf_celv-1+v_chmlcf_celd(decal+nceld2+nceld3*(i_liel-1)+4)
! --------- Set values in CHAM_ELEM
            zr(vale_indx-1+1)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+4 )
            zr(vale_indx-1+2)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+5 )
            zr(vale_indx-1+3)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+6 )
            zr(vale_indx-1+4)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+7 )
            zr(vale_indx-1+5)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+8 )
            zr(vale_indx-1+6)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+9 )
            zr(vale_indx-1+7)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+10)
            zr(vale_indx-1+8)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+11)
            zr(vale_indx-1+9)  = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+12)
            zr(vale_indx-1+10) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+13)
            zr(vale_indx-1+11) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+15)
            zr(vale_indx-1+12) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+23)
            zr(vale_indx-1+13) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+17)
            zr(vale_indx-1+14) = v_sdcont_jsupco(i_cont_poin)
            zr(vale_indx-1+15) = i_algo_cont
            zr(vale_indx-1+16) = v_sdcont_cychis(25*(i_cont_poin-1)+2)
            zr(vale_indx-1+17) = i_reso_fric
            zr(vale_indx-1+25) = i_reso_geom
            zr(vale_indx-1+18) = i_algo_fric
            zr(vale_indx-1+19) = v_sdcont_cychis(25*(i_cont_poin-1)+6)
            zr(vale_indx-1+20) = coef_fric
            zr(vale_indx-1+21) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+20)
            zr(vale_indx-1+22) = dyna_form
            zr(vale_indx-1+23) = time_incr
            zr(vale_indx-1+24) = theta
        enddo
        nt_liel = nt_liel + nb_liel
    enddo
    ASSERT(nt_liel.eq.nt_cont_poin)
!
    call jedema()
end subroutine

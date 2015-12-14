subroutine cfcgeo(mesh, hval_algo, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfverl.h"
#include "asterfort/cnomax.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmchex.h"
#include "asterfort/utmess.h"
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
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact   
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Evaluate geometry loop
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cmp_disp = 3
    character(len=8), parameter :: list_cmp_disp(nb_cmp_disp) = (/'DX','DY','DZ'/)
    integer :: nb_equa, i_equa
    integer :: rea1_node, rea2_node
    integer :: loop_geom_count, nb_iter_geom, iter_geom_maxi
    real(kind=8) :: loop_geom_vale, rea1_maxi, rea2_maxi, geom_epsi_maxi, geom_mini, geom_maxi
    character(len=16) :: loop_geom_node
    aster_logical :: l_geom_sans, l_geom_manu, l_geom_auto
    aster_logical :: l_first_reac
    character(len=19) :: disp_cumu_inst
    character(len=24) :: sdcont_rea1, sdcont_rea2
    aster_logical :: loop_geom_alarm
    real(kind=8), pointer :: v_sdcont_rea1(:) => null()
    real(kind=8), pointer :: v_sdcont_rea2(:) => null()
    real(kind=8), pointer :: v_disp_cumu(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    loop_geom_alarm = .false.
    l_first_reac    = .false.
    rea1_maxi       = 0.d0
    rea2_maxi       = 0.d0
!
! - Get contact parameters
!
    nb_equa = cfdisd(ds_contact%sdcont_solv,'NEQ' )
!
! - Get fields
!
    call nmchex(hval_algo, 'SOLALG', 'DEPDEL', disp_cumu_inst)
    call jeveuo(disp_cumu_inst(1:19)//'.VALE', 'L', vr = v_disp_cumu  )
!
! - Access to contact objects
!
    sdcont_rea1 = ds_contact%sdcont_solv(1:14)//'.REA1'
    sdcont_rea2 = ds_contact%sdcont_solv(1:14)//'.REA2'
    call jeveuo(sdcont_rea1(1:19)//'.VALE', 'E', vr = v_sdcont_rea1)
    call jeveuo(sdcont_rea2(1:19)//'.VALE', 'E', vr = v_sdcont_rea2)
!
! - Get geometry loop parameters
!
    geom_epsi_maxi = cfdisr(ds_contact%sdcont_defi,'RESI_GEOM' )
    iter_geom_maxi = cfdisi(ds_contact%sdcont_defi, 'ITER_GEOM_MAXI')
    nb_iter_geom   = cfdisi(ds_contact%sdcont_defi, 'NB_ITER_GEOM' )
    l_geom_manu    = cfdisl(ds_contact%sdcont_defi, 'REAC_GEOM_MANU')
    l_geom_sans    = cfdisl(ds_contact%sdcont_defi, 'REAC_GEOM_SANS')
    l_geom_auto    = cfdisl(ds_contact%sdcont_defi, 'REAC_GEOM_AUTO')
!
! - New contact iteration
!
    call mmbouc(ds_contact, 'Geom', 'Read_Counter', loop_geom_count)
!
! - Compute displacement criterion
!
    do i_equa = 1, nb_equa
        v_sdcont_rea2(i_equa) = v_sdcont_rea2(i_equa) + v_sdcont_rea1(i_equa)
        v_sdcont_rea1(i_equa) = v_disp_cumu(i_equa)   - v_sdcont_rea2(i_equa)
    end do
!
! - Find maximas
!
    call cnomax(sdcont_rea1, nb_cmp_disp, list_cmp_disp, rea1_maxi, rea1_node)
    call cnomax(sdcont_rea2, nb_cmp_disp, list_cmp_disp, rea2_maxi, rea2_node) 
!
! - Update maximum
!
    geom_maxi = ds_contact%geom_maxi
    if (geom_maxi .lt. 0.d0) then
        geom_maxi = rea2_maxi
        geom_mini = r8prem()
    else
        geom_maxi = max(geom_maxi, rea2_maxi)
        geom_mini = 1.d-6*geom_maxi
    endif
    ds_contact%geom_maxi = geom_maxi
!
! - Compute criterion
!
    if (rea2_maxi .le. geom_mini) then
        if (rea2_maxi .eq. 0.d0) then
            loop_geom_vale = 10.0d0*geom_epsi_maxi
            l_first_reac   = .true.
        else
            loop_geom_vale = 1.d-1*geom_epsi_maxi
        endif
    else
        loop_geom_vale = rea1_maxi/rea2_maxi
    endif
    
!
! - Get name of node
!
    if (rea2_node .eq. 0) then
        loop_geom_node = ' '
    else
        call jenuno(jexnum(mesh//'.NOMNOE', rea2_node), loop_geom_node)
    endif
!
! - For REAC_GEOM = 'MANU'
!
    if (l_geom_manu) then
        if (loop_geom_count .eq. nb_iter_geom) then
            call mmbouc(ds_contact, 'Geom', 'Set_Convergence')
            if (loop_geom_vale .ge. geom_epsi_maxi) then
                if (.not.l_first_reac) then
                    loop_geom_alarm = .true.
                else
                    loop_geom_alarm = .false.
                endif
            endif
        else
            call mmbouc(ds_contact, 'Geom', 'Set_Divergence')
        endif
    endif
!
! - For REAC_GEOM = 'SANS'
!
    if (l_geom_sans) then
        if (loop_geom_vale .ge. geom_epsi_maxi) then
            if (.not.l_first_reac) then
                loop_geom_alarm = .true.
            else
                loop_geom_alarm = .false.
            endif
        endif
        call mmbouc(ds_contact, 'Geom', 'Set_Convergence')
    endif
!
! - For REAC_GEOM = 'AUTO'
!    
    if (l_geom_auto) then
        if (loop_geom_vale .lt. geom_epsi_maxi) then
            call mmbouc(ds_contact, 'Geom', 'Set_Convergence')
        else
            call mmbouc(ds_contact, 'Geom', 'Set_Divergence')
            if (loop_geom_count .eq. iter_geom_maxi) then
                call cfverl(ds_contact)
                call mmbouc(ds_contact, 'Geom', 'Set_Error')
            endif  
        endif
    endif
!
! - Alarm for 5% tolerance exceeded
!
    if (loop_geom_alarm) then
        call utmess('A', 'CONTACT3_96')
    endif
!
! - New pairing
!
    ds_contact%l_pair = .true._1
!
! - Save values
!
    call mmbouc(ds_contact, 'Geom', 'Set_Locus', loop_locus_ = loop_geom_node)
    call mmbouc(ds_contact, 'Geom', 'Set_Vale' , loop_vale_  = loop_geom_vale)
!
end subroutine

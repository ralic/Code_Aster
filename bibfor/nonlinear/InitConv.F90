subroutine InitConv(ds_conv, list_func_acti, sdcont_defi)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisr.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/getvr8.h"
#include "asterfort/GetResi.h"
#include "asterfort/SetResi.h"
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
    type(NL_DS_Conv), intent(inout) :: ds_conv
    integer, optional, intent(in) :: list_func_acti(*)
    character(len=24), optional, intent(in) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Initializations for convergence management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_conv          : datastructure for convergence management
! In  list_func_acti   : list of active functionnalities
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real (kind=8) :: resi_glob_rela, resi_frot, resi_geom
    integer :: iret
    aster_logical :: l_newt_frot, l_newt_geom, l_resi_user, l_rela, l_maxi, l_refe, l_comp
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Initializations for convergence management'
    endif
!
! - No information from user: RESI_GLOB_RELA with 1E-6
!
    call GetResi(ds_conv, type = 'RESI_GLOB_RELA' , user_para_ = resi_glob_rela,&
                 l_resi_test_ = l_rela)
    call GetResi(ds_conv, type = 'RESI_GLOB_MAXI' , l_resi_test_ = l_maxi)
    call GetResi(ds_conv, type = 'RESI_REFE_RELA' , l_resi_test_ = l_refe)
    call GetResi(ds_conv, type = 'RESI_COMP_RELA' , l_resi_test_ = l_comp)
    l_resi_user = l_rela .or. l_maxi .or. l_refe .or. l_comp
    if (.not.l_resi_user) then
        call SetResi(ds_conv   , type_ = 'RESI_GLOB_RELA', &
                     user_para_ = 1.d-6, l_resi_test_ = .true._1)
    endif
!
! - Relaxation of convergence criterion: alarm !
!
    if (l_rela .and. resi_glob_rela .gt. 1.0001d-4) then
        call utmess('A', 'MECANONLINE5_21')
    endif
!
! - ARRET=NON: alarm ! 
!
    if (.not.ds_conv%l_stop) then
        call utmess('A', 'MECANONLINE5_37')
    endif
!
! - No NEWTON/PAS_MINI_ELAS parameter => ITER_GLOB_ELAS is useless
!
    call getvr8('NEWTON', 'PAS_MINI_ELAS', iocc=1, nbret=iret)
    if ((iret.eq.0) .and. (ds_conv%l_iter_elas)) then
        call utmess('A', 'MECANONLINE5_38')
    endif
!
! - No NEWTON/PAS_MINI_ELAS parameter => using ITER_GLOB_MAXI instead of ITER_GLOB_ELAS
!
    if (iret.eq.0) then
        ds_conv%iter_glob_elas = ds_conv%iter_glob_maxi
    endif
!
! - Set contact residuals (not for SIMU_POINT_MAT)
!
    if (present(list_func_acti)) then
!
! ----- Active functionnalites
!
        l_newt_frot = isfonc(list_func_acti,'FROT_NEWTON')
        l_newt_geom = isfonc(list_func_acti,'GEOM_NEWTON')
!
! ----- Activation of contact residuals for generalized Newton
!
        if (l_newt_frot) then
            resi_frot = cfdisr(sdcont_defi, 'RESI_FROT')
            call SetResi(ds_conv   , type_ = 'RESI_FROT', user_para_ = resi_frot,&
                         l_resi_test_ = .true._1)
        endif
        if (l_newt_geom) then
            resi_geom = cfdisr(sdcont_defi, 'RESI_GEOM')
            call SetResi(ds_conv   , type_ = 'RESI_GEOM', user_para_ = resi_geom,&
                         l_resi_test_ = .true._1)
        endif
    endif
!
end subroutine

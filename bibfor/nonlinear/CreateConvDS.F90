subroutine CreateConvDS(ds_conv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8nnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/SetResi.h"
#include "asterfort/SetResiRefe.h"
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
    type(NL_DS_Conv), intent(out) :: ds_conv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Create convergence management datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_conv          : datastructure for convergence management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_resi, nb_refe
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create convergence management datastructure'
    endif
!
! - Checks
!
    nb_resi = 6
    ds_conv%nb_resi = nb_resi
    ASSERT(nb_resi.le.ds_conv%nb_resi_maxi)
    nb_refe = 11
    ds_conv%nb_refe = nb_refe
    ASSERT(nb_refe.le.ds_conv%nb_refe_maxi)
!
! - Set type of residuals
!
    ds_conv%list_resi(1)%type = 'RESI_GLOB_RELA'
    ds_conv%list_resi(2)%type = 'RESI_GLOB_MAXI'
    ds_conv%list_resi(3)%type = 'RESI_REFE_RELA'
    ds_conv%list_resi(4)%type = 'RESI_COMP_RELA'
    ds_conv%list_resi(5)%type = 'RESI_FROT'
    ds_conv%list_resi(6)%type = 'RESI_GEOM'
!
! - Set name of rows in convergence table (for values)
!
    ds_conv%list_resi(1)%row_name = 'RESI_RELA'
    ds_conv%list_resi(2)%row_name = 'RESI_MAXI'
    ds_conv%list_resi(3)%row_name = 'RESI_REFE'
    ds_conv%list_resi(4)%row_name = 'RESI_COMP'
    ds_conv%list_resi(5)%row_name = 'FROT_NEWT'
    ds_conv%list_resi(6)%row_name = 'GEOM_NEWT'
!
! - Set name of rows in convergence table (for locus)
!
    ds_conv%list_resi(1)%row_name_locus = 'RELA_NOEU'
    ds_conv%list_resi(2)%row_name_locus = 'MAXI_NOEU'
    ds_conv%list_resi(3)%row_name_locus = 'REFE_NOEU'
    ds_conv%list_resi(4)%row_name_locus = 'COMP_NOEU'
    ds_conv%list_resi(5)%row_name_locus = 'FROT_NOEU'
    ds_conv%list_resi(6)%row_name_locus = 'GEOM_NOEU'
!
! - Set event for divergence
!
    ds_conv%list_resi(1)%event_type = 'DIVE_RELA'
    ds_conv%list_resi(2)%event_type = 'DIVE_MAXI'
    ds_conv%list_resi(3)%event_type = 'DIVE_REFE'
    ds_conv%list_resi(4)%event_type = 'DIVE_COMP'
    ds_conv%list_resi(5)%event_type = 'DIVE_FROT'
    ds_conv%list_resi(6)%event_type = 'DIVE_GEOM'
!
! - Initializations for all residuals
!
    call SetResi(ds_conv   , &
                 vale_calc_   = r8vide(), locus_calc_ = ' ', user_para_ = r8vide(),&
                 l_conv_ = .false._1, l_resi_test_ = .false._1)
!
! - Set name of reference residuals
!
    ds_conv%list_refe(1)%type  = 'SIGM_REFE'
    ds_conv%list_refe(2)%type  = 'EPSI_REFE'
    ds_conv%list_refe(3)%type  = 'FLUX_THER_REFE'
    ds_conv%list_refe(4)%type  = 'FLUX_HYD1_REFE'
    ds_conv%list_refe(5)%type  = 'FLUX_HYD2_REFE'
    ds_conv%list_refe(6)%type  = 'VARI_REFE'
    ds_conv%list_refe(7)%type  = 'EFFORT_REFE'
    ds_conv%list_refe(8)%type  = 'MOMENT_REFE'
    ds_conv%list_refe(9)%type  = 'DEPL_REFE'
    ds_conv%list_refe(10)%type = 'LAGR_REFE'
    ds_conv%list_refe(11)%type = 'PI_REFE'
!
! - Set name of component for reference residuals
!
    ds_conv%list_refe(1)%cmp_name  = 'SIGM'
    ds_conv%list_refe(2)%cmp_name  = 'EPSI'
    ds_conv%list_refe(3)%cmp_name  = 'FTHERM'
    ds_conv%list_refe(4)%cmp_name  = 'FHYDR1'
    ds_conv%list_refe(5)%cmp_name  = 'FHYDR2'
    ds_conv%list_refe(6)%cmp_name  = 'VARI'
    ds_conv%list_refe(7)%cmp_name  = 'EFFORT'
    ds_conv%list_refe(8)%cmp_name  = 'MOMENT'
    ds_conv%list_refe(9)%cmp_name  = 'DEPL'
    ds_conv%list_refe(10)%cmp_name = 'LAG_GV'
    ds_conv%list_refe(11)%cmp_name = 'PI'
!
! - Initializations for all reference residuals
!
    call SetResiRefe(ds_conv, user_para_ = r8nnem(), l_refe_test_ = .false._1)
!
! - Other convergence parameters
!
    ds_conv%iter_glob_maxi   = 0
    ds_conv%iter_glob_elas   = 0
    ds_conv%l_stop           = .true._1
    ds_conv%l_iter_elas      = .false._1
!
! - Parameters for automatic swap of convergence criterias
!
    ds_conv%swap_trig        = 0.d0
!
end subroutine

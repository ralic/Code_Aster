subroutine cfconv(mesh      , ds_measure, sderro, hval_algo, ds_print,&
                  ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/cfcgeo.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmrvai.h"
#include "asterfort/mmbouc.h"
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
    character(len=24), intent(in) :: sderro
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Evaluate convergence
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sderro           : datastructure for errors during algorithm
! IO  ds_measure       : datastructure for measure and statistics management
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_print         : datastructure for printing parameters
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: loop_geom_node
    integer :: nb_cont_iter
    real(kind=8) :: loop_geom_vale
    aster_logical :: l_resi_conv
    aster_logical :: l_all_verif, l_eval_geom
!
! --------------------------------------------------------------------------------------------------
!
    l_eval_geom    = .false.
    loop_geom_node = ' '
    loop_geom_vale = r8vide()
    nb_cont_iter = 0
!
! - Get contact parameters
!
    l_all_verif = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    call nmrvai(ds_measure, 'Cont_Algo', phasis = 'N', input_count = nb_cont_iter)
!
! - Values in convergence table: not affected
!
    call nmimck(ds_print, 'BOUC_NOEU', ' ' , .false._1)
    call nmimcr(ds_print, 'BOUC_VALE', 0.d0, .false._1)
!
! - Residuals have converged ?
!
    call nmlecv(sderro, 'RESI', l_resi_conv)
!
! - Evaluate convvergence
!
    if (.not.l_all_verif) then
!
! ----- Evaluate geometry loop
!
        l_eval_geom = .false.
        if (l_resi_conv) then
            call cfcgeo(mesh, hval_algo, ds_contact)
            l_eval_geom = .true.
        endif
    endif
!
! - Set values in convergence table for contact geometry informations
!
    if (l_eval_geom) then
        call mmbouc(ds_contact, 'Geom', 'Get_Locus', loop_locus_ = loop_geom_node)
        call mmbouc(ds_contact, 'Geom', 'Get_Vale' , loop_vale_  = loop_geom_vale)
        call nmimck(ds_print, 'BOUC_NOEU', loop_geom_node, .true._1)
        call nmimcr(ds_print, 'BOUC_VALE', loop_geom_vale, .true._1)
    endif
    call nmimci(ds_print, 'CTCD_NBIT', nb_cont_iter, .true._1)
!
end subroutine

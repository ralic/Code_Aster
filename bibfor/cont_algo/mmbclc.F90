subroutine mmbclc(mesh     , model     , nume_dof  , iter_newt, nume_inst,&
                  sddisc   , sddyna    , ds_measure, hval_incr,&
                  hval_algo, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmchml.h"
#include "asterfort/mmligr.h"
#include "asterfort/mmstat.h"
#include "asterfort/mmctcg.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: iter_newt
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue methods - Applying generalized Newton method at Newton's iteration
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  iter_newt        : index of current Newton iteration
! In  nume_inst        : index of current time step
! In  sddisc           : datastructure for time discretization
! In  sddyna           : dynamic parameters datastructure
! IO  ds_measure       : datastructure for measure and statistics management
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_all_verif, l_newt_cont, l_newt_geom
    character(len=19) :: sdcont_depgeo, disp_curr
!
! --------------------------------------------------------------------------------------------------
!
!
! - Get fields' name
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
    sdcont_depgeo = ds_contact%sdcont_solv(1:14)//'.DEPG'
!
! - Get contact parameters
!
    l_all_verif = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    l_newt_cont = cfdisl(ds_contact%sdcont_defi,'CONT_NEWTON')
    l_newt_geom = cfdisl(ds_contact%sdcont_defi,'GEOM_NEWTON')
!
! - No computation
!
    if (l_all_verif) then
        call mmbouc(ds_contact, 'Cont', 'Set_Convergence')
    endif
!
! - For generalized Newton method
!
    if (.not. l_all_verif) then
!
! ----- New pairing
!
        if (l_newt_geom) then
            call copisd('CHAMP_GD', 'V', disp_curr, sdcont_depgeo)
            call mmctcg(mesh, ds_contact, nume_dof, ds_measure)
        endif
!
! ----- Start timer for preparation of contact
!
        if (l_newt_cont .or. l_newt_geom) then
            call nmtime(ds_measure, 'Launch', 'Cont_Prep')
        endif
!
! ----- Create contact elements
!
        if (l_newt_geom) then
            call mmligr(mesh, model, ds_contact)
        endif
!
! ----- Management of contact loop
!
        if (l_newt_cont .or. l_newt_geom) then
            call mmstat(mesh  , iter_newt, nume_inst, sddyna    , ds_measure,&
                        sddisc, hval_incr, hval_algo, ds_contact)
        endif
!
! ----- Update input field
!
        if (l_newt_cont .or. l_newt_geom) then
            call mmchml(ds_contact, sddisc, sddyna, nume_inst)
        endif
!
! ----- Stop timer for preparation of contact
!
        if (l_newt_cont .or. l_newt_geom) then
            call nmtime(ds_measure, 'Stop', 'Cont_Prep')
            call nmrinc(ds_measure, 'Cont_Prep ')
        endif
    endif
!
end subroutine

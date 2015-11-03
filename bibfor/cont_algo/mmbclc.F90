subroutine mmbclc(mesh     , model     , nume_dof      , iter_newt     , nume_inst,&
                  sddisc   , sddyna    , sdtime        , sdstat        , hval_incr,&
                  hval_algo, ds_contact, loop_cont_conv, loop_cont_node)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/mmchml.h"
#include "asterfort/mmligr.h"
#include "asterfort/mmstat.h"
#include "asterfort/mmctcg.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: iter_newt
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
    aster_logical, intent(out) :: loop_cont_conv
    integer, intent(out) :: loop_cont_node
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
! In  sdtime           : datastructure for timers management
! In  sdstat           : datastructure for statistics
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_contact       : datastructure for contact management
! Out loop_cont_conv   : .true. if contact loop converged
! Out loop_cont_node   : number of contact state changing
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_all_verif, l_newt_cont, l_newt_geom
    character(len=19) :: sdcont_depgeo, disp_curr
!
! --------------------------------------------------------------------------------------------------
!
    loop_cont_node = 0
    loop_cont_conv = .true.
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
! - For generalized NEwton method
!
    if (.not. l_all_verif) then
!
! ----- New pairing
!
        if (l_newt_geom) then
            call copisd('CHAMP_GD', 'V', disp_curr, sdcont_depgeo)
            call mmctcg(mesh, ds_contact, nume_dof, sdstat, sdtime)
        endif
!
! ----- Create contact elements
!
        if (l_newt_geom) then
            call nmtime(sdtime, 'INI', 'CTCC_PREP')
            call nmtime(sdtime, 'RUN', 'CTCC_PREP')
            call mmligr(mesh, model, ds_contact)
            call nmtime(sdtime, 'END', 'CTCC_PREP')
        endif
!
! ----- Management of contact loop
!
        if (l_newt_cont .or. l_newt_geom) then
            call nmtime(sdtime, 'INI', 'CTCC_CONT')
            call nmtime(sdtime, 'RUN', 'CTCC_CONT')
            call mmstat(mesh          , iter_newt, nume_inst, sddyna    , sdstat        ,&
                        sddisc        , hval_incr, hval_algo, ds_contact, loop_cont_node,&
                        loop_cont_conv)
            call nmtime(sdtime, 'END', 'CTCC_CONT')
            call nmrinc(sdstat, 'CTCC_CONT')
        endif
!
! ----- Update input field
!
        if (l_newt_cont .or. l_newt_geom) then
            call nmtime(sdtime, 'INI', 'CTCC_PREP')
            call nmtime(sdtime, 'RUN', 'CTCC_PREP')
            call mmchml(ds_contact, sddisc, sddyna, nume_inst)
            call nmtime(sdtime, 'END', 'CTCC_PREP')
            call nmrinc(sdstat, 'CTCC_PREP')
        endif
    endif
!
end subroutine

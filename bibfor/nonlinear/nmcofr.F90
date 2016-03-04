subroutine nmcofr(mesh      , disp_curr, disp_cumu_inst, disp_iter, solver        ,&
                  nume_dof  , matr_asse, iter_newt     , time_curr, resi_glob_rela,&
                  ds_measure, ds_contact    , ctccvg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfalgo.h"
#include "asterfort/cfgeom.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/jeveuo.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: disp_curr
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=19), intent(in) :: disp_iter
    character(len=19), intent(in) :: solver
    character(len=14), intent(in) :: nume_dof
    character(len=19), intent(in) :: matr_asse
    integer, intent(in) :: iter_newt
    real(kind=8), intent(in) :: time_curr
    real(kind=8), intent(in) :: resi_glob_rela
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_Contact), intent(inout) :: ds_contact 
    integer, intent(out) :: ctccvg
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Solve contact (pairing and algorithm)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  disp_curr        : current displacements
! In  disp_iter        : displacement iteration
! In  disp_cumu_inst   : displacement increment from beginning of current time
! In  solver           : datastructure for solver parameters
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  matr_asse        : matrix
! In  iter_newt        : index of current Newton iteration
! In  time_curr        : current time
! In  resi_glob_rela   : current value of RESI_GLOB_RELA
! IO  ds_measure       : datastructure for measure and statistics management
! IO  ds_contact       : datastructure for contact management
! Out ctccvg           : output code for contact algorithm
!                        -1 - No solving
!                         0 - OK
!                        +1 - Maximum contact iteration
!                        +2 - Singular contact matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> DEBUT DU TRAITEMENT DES CONDITIONS DE CONTACT'
    endif
!
! - Initializations
!
    ctccvg = -1
!
! - Pairing
!
    call cfgeom(iter_newt, mesh     , ds_measure, ds_contact,&
                disp_curr, time_curr)
!
! - Contact solving
!
    call nmtime(ds_measure, 'Init'  , 'Cont_Algo')
    call nmtime(ds_measure, 'Launch', 'Cont_Algo')
    call cfalgo(mesh          , ds_measure, resi_glob_rela, iter_newt,&
                solver        , nume_dof  , matr_asse     , disp_iter,&
                disp_cumu_inst, ds_contact, ctccvg        )
    call nmtime(ds_measure, 'Stop', 'Cont_Algo')
!
! - Pairing ended
!
    ds_contact%l_pair       = .false._1
    ds_contact%l_first_geom = .false._1
!
! - Yes for computation
!
    ASSERT(ctccvg.ge.0)
!
end subroutine

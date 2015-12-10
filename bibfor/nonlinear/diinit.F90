subroutine diinit(mesh_         , model_     , ds_inout, mate       , cara_elem,&
                  list_func_acti, sddyna     , ds_conv , ds_algopara, solver   ,&
                  ds_contact    , sddisc)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmcrar.h"
#include "asterfort/nmcrdd.h"
#include "asterfort/nmcrli.h"
#include "asterfort/nmcrpc.h"
#include "asterfort/nmcrsu.h"
#include "asterfort/ndxcfl.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: mesh_
    character(len=*), intent(in) :: model_
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: mate
    type(NL_DS_Conv), intent(in) :: ds_conv
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19), intent(in) :: solver
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: list_func_acti(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Datastructures
!
! Time discretization and storing datastructures
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  sddyna           : name of dynamic parameters
! In  ds_conv          : datastructure for convergence management
! In  ds_algo          : datastructure for algorithm management
! In  list_func_acti   : active functionnalities vector (see nmfonc)
! In  ds_contact       : datastructure for contact management
! In  solver           : name of solver parameters
! In  sddisc           : datastructure for time discretization
! In  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_expl, l_implex
    character(len=19) :: list_inst
    character(len=8) :: model, mesh, result
    real(kind=8) :: init_time
!
! --------------------------------------------------------------------------------------------------
!
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=list_inst)
    model = model_
    mesh  = mesh_
!
! - Get parameters
!
    init_time   = ds_inout%init_time
    result      = ds_inout%result
!
! - Active functionnalities
!
    l_expl       = ndynlo(sddyna,'EXPLICITE')
    l_implex     = isfonc(list_func_acti,'IMPLEX')
!
! - Create time discretization datastructure
!
    call nmcrli(init_time, list_inst, sddisc)
!
! - Courant condition
!
    if (l_expl) then
        call ndxcfl(mate, cara_elem, sddyna, sddisc)
    endif
!
! - Create storing datastructure
!
    call nmcrar(result, sddisc, list_func_acti)
!
! - Automatic management of time stepping
!
    call nmcrsu(sddisc, list_inst , ds_conv, ds_algopara, l_implex,&
                solver, ds_contact)
!
! - Table for parameters
!
    call nmcrpc(result)
!
end subroutine

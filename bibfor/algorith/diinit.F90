subroutine diinit(mesh_         , model_     , result , mate       , cara_elem,&
                  list_func_acti, sddyna     , ds_conv, ds_algopara, inst_init,&
                  solver        , sdcont_defi, sddisc)
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
    real(kind=8), intent(in) :: inst_init
    type(NL_DS_Conv), intent(in) :: ds_conv
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=8), intent(in) :: result
    character(len=19), intent(in) :: solver
    character(len=24), intent(in) :: sdcont_defi
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
! In  result           : name of the results datastructure
! In  sddyna           : name of dynamic parameters
! In  ds_conv          : datastructure for convergence management
! In  ds_algopara      : datastructure for algorithm parameters
! In  list_func_acti   : active functionnalities vector (see nmfonc)
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  solver           : name of solver parameters
! In  sddisc           : datastructure for time discretization
! In  inst_init        : initial time if ETAT_INIT
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_expl, l_implex, l_cont_disc
    character(len=19) :: list_inst
    character(len=8) :: model, mesh
!
! --------------------------------------------------------------------------------------------------
!
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=list_inst)
    model = model_
    mesh  = mesh_
!
! - Active functionnalities
!
    l_expl       = ndynlo(sddyna,'EXPLICITE')
    l_implex     = isfonc(list_func_acti,'IMPLEX')
    l_cont_disc  = isfonc(list_func_acti,'CONT_DISCRET')
!
! - Create time discretization datastructure
!
    call nmcrli(inst_init, list_inst, sddisc)
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
    call nmcrsu(sddisc     , list_inst, ds_conv    , ds_algopara, l_implex,&
                l_cont_disc, solver   , sdcont_defi)
!
! - Table for parameters
!
    call nmcrpc(result)
!
end subroutine

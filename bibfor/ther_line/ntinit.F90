subroutine ntinit(model   , mate  , cara_elem, list_load, para    ,&
                  nume_dof, l_stat, l_evol   , sddisc   , ds_inout,&
                  mesh    , time)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dismoi.h"
#include "asterfort/ntcrch.h"
#include "asterfort/ntetcr.h"
#include "asterfort/ntdoet.h"
#include "asterfort/ntnoli.h"
#include "asterfort/ntnume.h"
#include "asterfort/tiinit.h"
#include "asterfort/utmess.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: list_load
    real(kind=8), intent(in) :: para(*)
    character(len=19), intent(in) :: sddisc
    type(NL_DS_InOut), intent(inout) :: ds_inout
    character(len=24), intent(out) :: nume_dof
    aster_logical, intent(out) :: l_stat
    aster_logical, intent(out) :: l_evol
    character(len=8), intent(out) :: mesh
    character(len=24), intent(out) :: time
!
! --------------------------------------------------------------------------------------------------
!
! THER_LINEAIRE - Algorithm
!
! Initializations
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  list_load        : name of datastructure for list of loads
! In  para             : parameters for time
!                            (1) THETA
!                            (2) DELTAT
! In  sddisc           : datastructure for time discretization
! IO  ds_inout         : datastructure for input/output management
! Out nume_dof         : name of numbering object (NUME_DDL)
! Out l_stat           : .true. is stationnary
! Out l_evol           : .true. if transient
! Out mesh             : name of mesh
! Out time             : name of field to save time parameters
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    l_stat = .false.
    result = ds_inout%result
    time   = result(1:8)//'.CHTPS'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! - Create numbering
!
    call ntnume(model, list_load, result, nume_dof)
!
! - Create unknowns
!
    call ntcrch(model, nume_dof)
!
! - Create input/output datastructure
!
    call ntetcr(nume_dof, ds_inout,&
                list_load_ = list_load)
!
! - Read initial state
!
    call ntdoet(model, nume_dof, l_stat, ds_inout)
!
! - Time discretization and storing datastructures
!
    call tiinit(ds_inout, sddisc, l_stat, l_evol)
!
! - Prepare storing
!
    call ntnoli(model, mate  , cara_elem, l_stat, l_evol,&
                para , sddisc, ds_inout)
!
end subroutine

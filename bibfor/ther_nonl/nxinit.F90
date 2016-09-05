subroutine nxinit(model , mate    , cara_elem , compor       , list_load,&
                  para  , nume_dof, l_stat    , l_evol       , l_rom    ,&
                  sddisc, ds_inout, vhydr     , sdobse       , mesh     ,&
                  sdcrit, time    , ds_algorom, l_line_search)
!
use NonLin_Datastructure_type
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/ntcrob.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/ntcrch.h"
#include "asterfort/ntcrcv.h"
#include "asterfort/ntetcr.h"
#include "asterfort/ntdoet.h"
#include "asterfort/nxnoli.h"
#include "asterfort/ntnume.h"
#include "asterfort/tiinit.h"
#include "asterfort/utmess.h"
#include "asterfort/romAlgoNLInit.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: list_load
    real(kind=8), intent(in) :: para(*)
    character(len=24), intent(out) :: nume_dof
    aster_logical, intent(out) :: l_stat
    aster_logical, intent(out) :: l_evol
    aster_logical, intent(out) :: l_rom
    character(len=19), intent(in) :: sddisc
    type(NL_DS_InOut), intent(inout) :: ds_inout
    character(len=24), intent(in) :: vhydr
    character(len=19), intent(out) :: sdobse
    character(len=8), intent(out) :: mesh
    character(len=19), intent(in) :: sdcrit
    character(len=24), intent(out) :: time
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
    aster_logical, intent(in) :: l_line_search
!
! --------------------------------------------------------------------------------------------------
!
! THER_NON_LINE - Algorithm
!
! Initializations
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  compor           : name of comportment definition (field)
! In  list_load        : name of datastructure for list of loads
! In  para             : parameters for time
!                            (1) THETA
!                            (2) DELTAT
! In  vhydr            : field for hydration
! Out sdobse           : datastructure for observation parameters
! In  sddisc           : datastructure for time discretization
! In  sdcrit           : name of datastructure to save convergence parameters
! IO  ds_inout         : datastructure for input/output management
! Out nume_dof         : name of numbering object (NUME_DDL)
! Out l_stat           : .true. is stationnary
! Out l_evol           : .true. if transient
! Out mesh             : name of mesh
! Out time             : name of field to save time parameters
! IO  ds_algorom       : datastructure for ROM parameters
! In  l_line_search    : .true. if line search
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    character(len=24) :: hydr_init
!
! --------------------------------------------------------------------------------------------------
!
    l_stat = .false._1
    l_evol = .false._1
    l_rom  = ds_algorom%l_rom
    result = ds_inout%result
    time   = result(1:8)//'.CHTPS'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! - Create numbering
!
    call ntnume(model, list_load, result, nume_dof)
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call ntcrcv(sdcrit)
!
! - Create unknowns
!
    call ntcrch(model, nume_dof, vhydr, hydr_init)
!
! - Create input/output datastructure
!
    call ntetcr(nume_dof , ds_inout,&
                list_load, compor  , vhydr, hydr_init)
!
! - Read initial state
!
    call ntdoet(model, nume_dof, l_stat, ds_inout)
!
! - Initialization for reduced method
!
    if (l_rom) then
        call romAlgoNLInit('THER'       , mesh, nume_dof, result, ds_algorom,&
                           l_line_search)
    endif
!
! - Time discretization and storing datastructures
!
    call tiinit(ds_inout, sddisc, l_stat, l_evol)
!
! - Create observation datastructure
!
    call ntcrob(mesh  , model, result, sddisc, ds_inout,&
                sdobse)
!
! - Prepare storing
!
    call nxnoli(model, mate  , cara_elem, l_stat  , l_evol    ,&
                para , sddisc, sdcrit   , ds_inout, ds_algorom)
!
end subroutine

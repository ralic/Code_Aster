subroutine nmnume(model , result, list_load, l_cont, sdcont_defi ,&
                  compor, solver, nume_ddl , sdnume, sd_iden_relaz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/nmprof.h"
#include "asterfort/nuendo.h"
#include "asterfort/nunuco.h"
#include "asterfort/nurota.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(out) :: nume_ddl
    character(len=8), intent(in) :: result
    character(len=19), intent(in) :: list_load
    character(len=19), intent(in) :: solver
    character(len=19), intent(in) :: sdnume
    aster_logical, intent(in) :: l_cont
    character(len=*), optional, intent(in) :: sd_iden_relaz
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initializations
!
! Create information about numbering
!
! --------------------------------------------------------------------------------------------------
!
! Out nume_ddl       : name of numbering object (NUME_DDL)
! In  compor         : name of <CARTE> COMPOR
! In  solver         : name of solver datastructure
! In  result         : name of result datastructure (EVOL_NOLI)
! In  model          : name of model datastructure
! In  sdcont_defi    : name of contact definition datsatructure (from DEFI_CONTACT)
! In  list_load      : list of loads
! In  l_cont         : .true. if contact
! In  sd_iden_rela   : name of object for identity relations between dof
! In  sdnume         : name of dof positions datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdnuro, sdnuen, sdnuco
    aster_logical :: l_ctcc
!
! --------------------------------------------------------------------------------------------------
!

!
! - Create numbering 
!
    call nmprof(model        , result, list_load, solver, nume_ddl,&
                sd_iden_relaz)
!
! - Get position of large rotation dof
!
    sdnuro = sdnume(1:19)//'.NDRO'
    call nurota(model, nume_ddl, compor, sdnuro)
!
! - Get position of damaged dof 
!
    sdnuen = sdnume(1:19)//'.ENDO'
    call nuendo(model, nume_ddl, sdnuen)
!
! - Get position of contact dof 
!
    sdnuco = sdnume(1:19)//'.NUCO'
    if (l_cont) then
        l_ctcc = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
        if (l_ctcc) then
            call nunuco(nume_ddl, sdnuco)
        endif
    endif  
!
end subroutine

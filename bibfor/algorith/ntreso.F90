subroutine ntreso(model , mate  , cara_elem, list_load, nume_dof,&
                  solver, l_stat, time     , tpsthe   , reasrg  ,&
                  reasms, cn2mbr, matass   , maprec   , cndiri  ,&
                  cncine, mediri, compor)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/ntacmv.h"
#include "asterfort/nxreso.h"
#include "asterfort/preres.h"
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
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: solver
    aster_logical, intent(in) :: l_stat
    character(len=24), intent(in) :: time
    real(kind=8), intent(in) :: tpsthe(6)
    aster_logical, intent(in) :: reasrg
    aster_logical, intent(in) :: reasms
    character(len=24), intent(in) :: cn2mbr
    character(len=24), intent(in) :: matass
    character(len=19), intent(in) :: maprec
    character(len=24), intent(in) :: cndiri
    character(len=24), intent(out) :: cncine
    character(len=24), intent(in) :: mediri
    character(len=24), intent(in) :: compor
!
! --------------------------------------------------------------------------------------------------
!
! THER_LINEAIRE - Algorithm
!
! Solve system
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  list_load        : name of datastructure for list of loads
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: chsol, varc_curr
    character(len=24) :: dry_prev, dry_curr, vhydr, vtemp
    integer :: ierr, ibid
!
! --------------------------------------------------------------------------------------------------
!
    chsol     = '&&NTRESO_SOLUTION'
    varc_curr = '&&NTRESO.CHVARC'
    vhydr     = ' '
    vtemp     = '&&NXLECTVAR_____'
    dry_prev  = ' '
    dry_curr  = ' '
!
! - Construct second member
!
    call ntacmv(model , mate  , cara_elem, list_load, nume_dof,&
                l_stat, time  , tpsthe   , reasrg   , reasms  ,&
                vtemp , vhydr , varc_curr, dry_prev , dry_curr,&
                cn2mbr, matass, cndiri   , cncine   , mediri  ,&
                compor)
!
! - Factor
!
    if (reasrg .or. reasms) then
        call preres(solver, 'V', ierr, maprec, matass,&
                    ibid, -9999)
    endif
!
! - Solve linear system
!
    call nxreso(matass, maprec, solver, cncine, cn2mbr,&
                chsol)
!
! - Save solution
!
    call copisd('CHAMP_GD', 'V', chsol, vtemp)
    call detrsd('CHAMP_GD', chsol)
!
end subroutine

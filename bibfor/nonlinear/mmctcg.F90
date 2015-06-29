subroutine mmctcg(mesh  , sdcont_defi, sdcont_solv, nume_dof, sdstat,&
                  sdtime)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmappa.h"
#include "asterfort/mreacg.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sdcont_defi 
    character(len=24), intent(in) :: sdcont_solv
    character(len=24), intent(in) :: nume_dof
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Geometric loop: geometric actualisation and pairing 
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  sdtime           : datastructure for timers
! In  sdstat           : datastructure for statistics
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Geometric actualisation and pairing'
    endif
!
! - Geometric loop: new geometric iteration for statistics
!
    call nmrinc(sdstat, 'CONT_GEOM')
!
! - Geometric loop: begin timer
!
    call nmtime(sdtime, 'INI', 'CONT_GEOM')
    call nmtime(sdtime, 'RUN', 'CONT_GEOM')
!
! - Geometric actualisation
!
    call mreacg(mesh, sdcont_solv)
!
! - Pairing
!
    call mmappa(mesh, nume_dof, sdcont_defi, sdcont_solv)
!
! - Geometric loop: end timer
!
    call nmtime(sdtime, 'END', 'CONT_GEOM')
!
end subroutine

subroutine xmctcg(model, mesh, ds_contact, sdstat, sdtime)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/xappar.h"
#include "asterfort/xreacg.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM method - Geometric loop: geometric actualisation and pairing 
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
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
    call xreacg(model, ds_contact)
!
! - Pairing
!
    call xappar(mesh, model, ds_contact)
!
! - Geometric loop: end timer
!
    call nmtime(sdtime, 'END', 'CONT_GEOM')
!
end subroutine

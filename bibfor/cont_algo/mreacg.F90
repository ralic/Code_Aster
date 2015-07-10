subroutine mreacg(mesh, sdcont_solv, field_update_)
!
implicit none
!
#include "asterfort/infdbg.h"
#include "asterfort/mmfield_prep.h"
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
    character(len=24), intent(in) :: sdcont_solv
    character(len=*), optional, intent(in) :: field_update_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Geometry update
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_solv      : name of contact solving datastructure
! In  field_update     : displacement field to use for update. If not present, using DEPGEO
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: depgeo, oldgeo
    character(len=19) :: newgeo, field_update
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... Geometry update'
    endif
!
! - Name of objects
!
    oldgeo = mesh(1:8)//'.COORDO'
    depgeo = sdcont_solv(1:14)//'.DEPG'
    newgeo = sdcont_solv(1:14)//'.NEWG'
    if (present(field_update_)) then
        field_update = field_update_
    else
        field_update = depgeo(1:19)
    endif
!
! - Update
!
    call mmfield_prep(oldgeo, newgeo,&
                      l_update_ = .true._1, field_update_ = field_update, alpha_ = 1.d0)
!
end subroutine

subroutine exipat(mesh, iret)
!
implicit none
!
#include "asterfort/jeexin.h"
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    character(len=8), intent(in) :: mesh
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Check if exist PATCH in mesh
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! Out iret             : 1 if PATCH exists in mesh 0 otherwise
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ipatch
!
! --------------------------------------------------------------------------------------------------
!
    iret = 0
    call jeexin(mesh//'.PATCH', ipatch)
    if (ipatch .ne. 0) then
        iret=1
    endif
!
end subroutine

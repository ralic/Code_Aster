!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine hujini(mod, nmat, mater, intg, deps,&
                      nr, yd, nvi, vind, sigd,&
                      sigf, bnews, mtrac, dy, indi,&
                      iret)
        integer :: nvi
        integer :: nmat
        character(len=8) :: mod
        real(kind=8) :: mater(nmat, 2)
        integer :: intg
        real(kind=8) :: deps(6)
        integer :: nr
        real(kind=8) :: yd(18)
        real(kind=8) :: vind(nvi)
        real(kind=8) :: sigd(6)
        real(kind=8) :: sigf(6)
        aster_logical :: bnews(3)
        aster_logical :: mtrac
        real(kind=8) :: dy(18)
        integer :: indi(7)
        integer :: iret
    end subroutine hujini
end interface

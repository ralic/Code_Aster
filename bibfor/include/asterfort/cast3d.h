!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine cast3d(proj, gamma, dh, def, nno,&
                      kpg, nub, nu, dsidep, calbn,&
                      bn, jac, matuu)
        integer :: proj
        real(kind=8) :: gamma(4, 8)
        real(kind=8) :: dh(4, 24)
        real(kind=8) :: def(6, 3, 8)
        integer :: nno
        integer :: kpg
        real(kind=8) :: nub
        real(kind=8) :: nu
        real(kind=8) :: dsidep(6, 6)
        aster_logical :: calbn
        real(kind=8) :: bn(6, 3, 8)
        real(kind=8) :: jac
        real(kind=8) :: matuu(*)
    end subroutine cast3d
end interface

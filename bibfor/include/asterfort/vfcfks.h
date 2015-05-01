!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine vfcfks(cont, tange, maxfa, nface, uk,&
                      dukp1, dukp2, ufa, dufa1, dufa2,&
                      c, pesa, rho, drho1, drho2,&
                      xk, xfa, maxdim, ndim, fks,&
                      dfks1, dfks2)
        integer :: ndim
        integer :: maxdim
        integer :: nface
        integer :: maxfa
        aster_logical :: cont
        aster_logical :: tange
        real(kind=8) :: uk
        real(kind=8) :: dukp1
        real(kind=8) :: dukp2
        real(kind=8) :: ufa(1:nface)
        real(kind=8) :: dufa1(1:nface)
        real(kind=8) :: dufa2(1:nface)
        real(kind=8) :: c(1:maxfa, 1:nface)
        real(kind=8) :: pesa(ndim)
        real(kind=8) :: rho
        real(kind=8) :: drho1
        real(kind=8) :: drho2
        real(kind=8) :: xk(ndim)
        real(kind=8) :: xfa(1:maxdim, 1:nface)
        real(kind=8) :: fks(nface)
        real(kind=8) :: dfks1(1+maxfa, nface)
        real(kind=8) :: dfks2(1+maxfa, nface)
    end subroutine vfcfks
end interface

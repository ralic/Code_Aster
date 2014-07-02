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
    subroutine nmgeom(ndim, nno, axi, grand, geom,&
                      kpg, ipoids, ivf, idfde, depl,&
                      ldfdi, poids, dfdi, f, eps,&
                      r)
        integer :: nno
        integer :: ndim
        aster_logical :: axi
        aster_logical :: grand
        real(kind=8) :: geom(ndim, nno)
        integer :: kpg
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        real(kind=8) :: depl(ndim, nno)
        aster_logical :: ldfdi
        real(kind=8) :: poids
        real(kind=8) :: dfdi(nno, ndim)
        real(kind=8) :: f(3, 3)
        real(kind=8) :: eps(6)
        real(kind=8) :: r
    end subroutine nmgeom
end interface

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
    subroutine chauxi(ndim, mu, ka, r, t,&
                      invp, lcour, courb, du1dm, du2dm,&
                      du3dm, u1l, u2l, u3l)
        integer :: ndim
        real(kind=8) :: mu
        real(kind=8) :: ka
        real(kind=8) :: r
        real(kind=8) :: t
        real(kind=8) :: invp(3, 3)
        aster_logical :: lcour
        real(kind=8) :: courb(3, 3, 3)
        real(kind=8) :: du1dm(3, 3)
        real(kind=8) :: du2dm(3, 3)
        real(kind=8) :: du3dm(3, 3)
        real(kind=8) :: u1l(3)
        real(kind=8) :: u2l(3)
        real(kind=8) :: u3l(3)
    end subroutine chauxi
end interface

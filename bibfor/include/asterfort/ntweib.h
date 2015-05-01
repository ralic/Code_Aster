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
    subroutine ntweib(nrupt, cals, sk, sigw, nur,&
                      nt, nbres, x1, x2, xacc,&
                      rtsafe, impr, ifm, indtp, nbtp)
        integer :: nrupt
        aster_logical :: cals
        real(kind=8) :: sk(*)
        real(kind=8) :: sigw(*)
        integer :: nur(*)
        integer :: nt(*)
        integer :: nbres
        real(kind=8) :: x1
        real(kind=8) :: x2
        real(kind=8) :: xacc
        real(kind=8) :: rtsafe
        aster_logical :: impr
        integer :: ifm
        integer :: indtp(*)
        integer :: nbtp
    end subroutine ntweib
end interface

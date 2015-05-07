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
    subroutine xmmab1(ndim, jnne, ndeple, nnc, jnnm,&
                      hpg, ffc, ffe,&
                      ffm, jacobi, lambda, coefcr,&
                      dvitet, coeffr, jeu,&
                      coeffp, coefff, lpenaf, tau1, tau2,&
                      rese, mproj, norm, nsinge,&
                      nsingm, rre, rrm, nvit, nconta,&
                      jddle, jddlm, nfhe, nfhm, heavn, mmat)
        integer :: ndim
        integer :: jnne(3)
        integer :: ndeple
        integer :: nnc
        integer :: jnnm(3)
        real(kind=8) :: hpg
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffe(20)
        real(kind=8) :: ffm(20)
        real(kind=8) :: jacobi
        real(kind=8) :: lambda
        real(kind=8) :: coefcr
        real(kind=8) :: dvitet(3)
        real(kind=8) :: coeffr
        real(kind=8) :: jeu
        real(kind=8) :: coeffp
        real(kind=8) :: coefff
        aster_logical :: lpenaf
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: rese(3)
        real(kind=8) :: mproj(3, 3)
        real(kind=8) :: norm(3)
        integer :: nsinge
        integer :: nsingm
        real(kind=8) :: rre
        real(kind=8) :: rrm
        integer :: nvit
        integer :: nconta
        integer :: jddle(2)
        integer :: jddlm(2)
        integer :: nfhe
        integer :: nfhm
        integer :: heavn(*)
        real(kind=8) :: mmat(336, 336)
    end subroutine xmmab1
end interface

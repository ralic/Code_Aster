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
    subroutine xmvef1(ndim, jnne, jnnm, ndeple, nnc,&
                      hpg, ffc, ffe,&
                      ffm, jacobi, dlagrc, dlagrf,&
                      coeffr, lpenaf, coefff, tau1,&
                      tau2, rese, mproj, coefcr,&
                      jeu, nsinge, nsingm, rre,&
                      rrm, nvit, nconta, jddle, jddlm,&
                      nfhe, nfhm, lmulti, heavn, heavfa,&
                      vtmp)
        integer :: ndim
        integer :: jnne(3)
        integer :: jnnm(3)
        integer :: ndeple
        integer :: nnc
        real(kind=8) :: hpg
        real(kind=8) :: ffc(9)
        real(kind=8) :: ffe(20)
        real(kind=8) :: ffm(20)
        real(kind=8) :: jacobi
        real(kind=8) :: dlagrc
        real(kind=8) :: dlagrf(2)
        real(kind=8) :: coeffr
        aster_logical :: lpenaf
        real(kind=8) :: coefff
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: rese(3)
        real(kind=8) :: mproj(3, 3)
        real(kind=8) :: coefcr
        real(kind=8) :: jeu
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
        integer :: heavfa(*)
        aster_logical :: lmulti
        real(kind=8) :: vtmp(336)
    end subroutine xmvef1
end interface

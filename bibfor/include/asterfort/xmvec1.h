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
    subroutine xmvec1(ndim, jnne, ndeple, nnc, jnnm,&
                      hpg, nfaes, ffc, ffe, ffm,&
                      jacobi, dlagrc, jpcai, cface, coefcr,&
                      coefcp, lpenac, jeu, norm, typmai,&
                      nsinge, nsingm, rre, rrm, nconta,&
                      jddle, jddlm, nfhe, nfhm, lmulti,&
                      heavno, heavfa, vtmp)
        integer :: ndim
        integer :: jnne(3)
        integer :: ndeple
        integer :: nnc
        integer :: jnnm(3)
        real(kind=8) :: hpg
        integer :: nfaes
        real(kind=8) :: ffc(9)
        real(kind=8) :: ffe(20)
        real(kind=8) :: ffm(20)
        real(kind=8) :: jacobi
        real(kind=8) :: dlagrc
        integer :: jpcai
        integer :: cface(3, 5)
        real(kind=8) :: coefcr
        real(kind=8) :: coefcp
        aster_logical :: lpenac
        real(kind=8) :: jeu
        real(kind=8) :: norm(3)
        character(len=8) :: typmai
        integer :: nsinge
        integer :: nsingm
        real(kind=8) :: rre
        real(kind=8) :: rrm
        integer :: nconta
        integer :: jddle(2)
        integer :: jddlm(2)
        integer :: nfhe
        integer :: nfhm
        aster_logical :: lmulti
        integer :: heavno(8)
        integer :: heavfa(*)
        real(kind=8) :: vtmp(336)
    end subroutine xmvec1
end interface

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
interface
    subroutine xmvec0(ndim, jnne, nnc, nfaes, dlagrc,&
                      hpg, ffc, jacobi, cface, jpcai,&
                      coefcr, coefcp, lpenac, typmai, jddle,&
                      nconta, nfhe, lmulti, heavno, vtmp)
        integer :: ndim
        integer :: jnne(3)
        integer :: nnc
        integer :: nfaes
        real(kind=8) :: dlagrc
        real(kind=8) :: hpg
        real(kind=8) :: ffc(9)
        real(kind=8) :: jacobi
        integer :: cface(3, 5)
        integer :: jpcai
        real(kind=8) :: coefcr
        real(kind=8) :: coefcp
        logical :: lpenac
        character(len=8) :: typmai
        integer :: jddle(2)
        integer :: nconta
        integer :: nfhe
        logical :: lmulti
        integer :: heavno(8)
        real(kind=8) :: vtmp(336)
    end subroutine xmvec0
end interface

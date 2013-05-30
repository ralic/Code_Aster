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
    subroutine xtedd2(ndim, jnne, ndeple, jnnm, nddl,&
                      option, lesclx, lmaitx, lcontx, stano,&
                      lact, jddle, jddlm, nfhe, nfhm,&
                      lmulti, heavno, mmat, vtmp)
        integer :: ndim
        integer :: jnne(3)
        integer :: ndeple
        integer :: jnnm(3)
        integer :: nddl
        character(len=16) :: option
        logical :: lesclx
        logical :: lmaitx
        logical :: lcontx
        integer :: stano(*)
        integer :: lact(8)
        integer :: jddle(2)
        integer :: jddlm(2)
        integer :: nfhe
        integer :: nfhm
        logical :: lmulti
        integer :: heavno(8)
        real(kind=8) :: mmat(336, 336)
        real(kind=8) :: vtmp(336)
    end subroutine xtedd2
end interface

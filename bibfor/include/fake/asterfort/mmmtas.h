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
    subroutine mmmtas(nbdm, ndim, nnl, nne, nnm,&
                      nbcps, matrcc, matree, matrmm, matrem,&
                      matrme, matrce, matrcm, matrmc, matrec,&
                      matrff, matrfe, matrfm, matrmf, matref,&
                      mmat)
        integer :: nbdm
        integer :: ndim
        integer :: nnl
        integer :: nne
        integer :: nnm
        integer :: nbcps
        real(kind=8) :: matrcc(9, 9)
        real(kind=8) :: matree(27, 27)
        real(kind=8) :: matrmm(27, 27)
        real(kind=8) :: matrem(27, 27)
        real(kind=8) :: matrme(27, 27)
        real(kind=8) :: matrce(9, 27)
        real(kind=8) :: matrcm(9, 27)
        real(kind=8) :: matrmc(27, 9)
        real(kind=8) :: matrec(27, 9)
        real(kind=8) :: matrff(18, 18)
        real(kind=8) :: matrfe(18, 27)
        real(kind=8) :: matrfm(18, 27)
        real(kind=8) :: matrmf(27, 18)
        real(kind=8) :: matref(27, 18)
        real(kind=8) :: mmat(81, 81)
    end subroutine mmmtas
end interface

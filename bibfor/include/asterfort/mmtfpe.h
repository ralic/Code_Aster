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
interface
    subroutine mmtfpe(phasep,iresof,ndim  ,nne   ,nnm   , &
                  nnl   ,nbcps ,wpg   ,jacobi, ffl  , &
                  ffe   ,ffm    ,norm  ,tau1  , &
                  tau2  ,mprojn,mprojt,rese  ,nrese , &
                  lambda,coefff,coefaf,coefac, &
                  dlagrf,djeut ,matree,matrmm, &
                  matrem,matrme,matrec,matrmc,matref, &
                  matrmf)
        character(len=9) :: phasep
        integer :: iresof
        integer :: ndim
        integer :: nne
        integer :: nnm
        integer :: nnl
        integer :: nbcps
        real(kind=8) :: wpg
        real(kind=8) :: jacobi
        real(kind=8) :: ffl(9)
        real(kind=8) :: ffe(9)
        real(kind=8) :: ffm(9)
        real(kind=8) :: norm(3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: mprojn(3, 3)
        real(kind=8) :: mprojt(3, 3)
        real(kind=8) :: rese(3)
        real(kind=8) :: nrese
        real(kind=8) :: lambda
        real(kind=8) :: jeu
        real(kind=8) :: coefff
        real(kind=8) :: coefaf
        real(kind=8) :: coefac
        real(kind=8) :: dlagrf(2)
        real(kind=8) :: djeut(3)
        real(kind=8) :: matree(27, 27)
        real(kind=8) :: matrmm(27, 27)
        real(kind=8) :: matrem(27, 27)
        real(kind=8) :: matrme(27, 27)
        real(kind=8) :: matrec(27, 9)
        real(kind=8) :: matrmc(27, 9)
        real(kind=8) :: matref(27, 18)
        real(kind=8) :: matrmf(27, 18)
    end subroutine mmtfpe
end interface

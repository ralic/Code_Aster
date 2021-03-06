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
    subroutine cfpcdi(resoco, neq, nbliai, tole, epsipc,&
                      mu, apcoef, apddl, appoin, inliac,&
                      matass, solveu, premax, ssgrad, ssgrpr)
        character(len=24) :: resoco
        integer :: neq
        integer :: nbliai
        real(kind=8) :: tole
        real(kind=8) :: epsipc
        real(kind=8) :: mu(*)
        real(kind=8) :: apcoef(*)
        integer :: apddl(*)
        integer :: appoin(*)
        integer :: inliac(*)
        character(len=19) :: matass
        character(len=19) :: solveu
        integer :: premax
        real(kind=8) :: ssgrad(*)
        real(kind=8) :: ssgrpr(*)
    end subroutine cfpcdi
end interface

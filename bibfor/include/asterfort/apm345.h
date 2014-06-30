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
    subroutine apm345(nbtetc, typcon, rayonc, centrc, nk,&
                      k24rc, pivot2, ltest, typcha, lraide,&
                      lmasse, ldynam, solveu, lamor, lc,&
                      impr, ifapm)
        integer :: nbtetc
        character(len=8) :: typcon
        real(kind=8) :: rayonc
        complex(kind=8) :: centrc
        integer :: nk
        character(len=24) :: k24rc
        integer :: pivot2
        logical(kind=1) :: ltest
        character(len=8) :: typcha
        integer :: lraide
        integer :: lmasse
        integer :: ldynam
        character(len=19) :: solveu
        integer :: lamor
        logical(kind=1) :: lc
        character(len=3) :: impr
        integer :: ifapm
    end subroutine apm345
end interface

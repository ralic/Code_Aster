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
    subroutine inithm(imate, yachai, yamec, phi0, em,&
                      cs, tbiot, t, epsv, depsv,&
                      epsvm, angmas, aniso, mdal, dalal,&
                      alphfi, cbiot, unsks, alpha0, ndim,&
                      phenom)
        integer :: imate
        logical :: yachai
        integer :: yamec
        real(kind=8) :: phi0
        real(kind=8) :: em
        real(kind=8) :: cs
        real(kind=8) :: tbiot(6)
        real(kind=8) :: t
        real(kind=8) :: epsv
        real(kind=8) :: depsv
        real(kind=8) :: epsvm
        real(kind=8) :: angmas(3)
        integer :: aniso
        real(kind=8) :: mdal(6)
        real(kind=8) :: dalal
        real(kind=8) :: alphfi
        real(kind=8) :: cbiot
        real(kind=8) :: unsks
        real(kind=8) :: alpha0
        integer :: ndim
        character(len=16) :: phenom
    end subroutine inithm
end interface 

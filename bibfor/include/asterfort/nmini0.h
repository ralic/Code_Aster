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
    subroutine nmini0(zpmet, zpcri, zconv, zpcon, znmeth,&
                      fonact, parmet, parcri, conv, parcon,&
                      method, eta, numins, matass, zmeelm,&
                      zmeass, zveelm, zveass, zsolal, zvalin,&
                      sdimpr)
        integer :: znmeth
        integer :: zpcon
        integer :: zconv
        integer :: zpcri
        integer :: zpmet
        integer :: fonact(*)
        real(kind=8) :: parmet(zpmet)
        real(kind=8) :: parcri(zpcri)
        real(kind=8) :: conv(zconv)
        real(kind=8) :: parcon(zpcon)
        character(len=16) :: method(znmeth)
        real(kind=8) :: eta
        integer :: numins
        character(len=19) :: matass
        integer :: zmeelm
        integer :: zmeass
        integer :: zveelm
        integer :: zveass
        integer :: zsolal
        integer :: zvalin
        character(len=24) :: sdimpr
    end subroutine nmini0
end interface

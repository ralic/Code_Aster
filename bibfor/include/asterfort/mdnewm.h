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
    subroutine mdnewm(nbpas, dt, nbmode, pulsat, pulsa2,&
                      masgen, riggen, rgygen, lamor, amogen,&
                      gyogen, foncv, fonca, typbas, basemo,&
                      tinit, iparch, depsto, vitsto, accsto,&
                      iorsto, temsto, nomres, nbexci, idescf,&
                      nomfon, coefm, liad, inumor, passto)
        integer :: nbpas
        real(kind=8) :: dt
        integer :: nbmode
        real(kind=8) :: pulsat(*)
        real(kind=8) :: pulsa2(*)
        real(kind=8) :: masgen(*)
        real(kind=8) :: riggen(*)
        real(kind=8) :: rgygen(*)
        logical(kind=1) :: lamor
        real(kind=8) :: amogen(*)
        real(kind=8) :: gyogen(*)
        character(len=8) :: foncv
        character(len=8) :: fonca
        character(len=16) :: typbas
        character(len=8) :: basemo
        real(kind=8) :: tinit
        integer :: iparch(*)
        real(kind=8) :: depsto(*)
        real(kind=8) :: vitsto(*)
        real(kind=8) :: accsto(*)
        integer :: iorsto(*)
        real(kind=8) :: temsto(*)
        character(len=8) :: nomres
        integer :: nbexci
        integer :: idescf(*)
        character(len=8) :: nomfon(*)
        real(kind=8) :: coefm(*)
        integer :: liad(*)
        integer :: inumor(*)
        real(kind=8) :: passto(*)
    end subroutine mdnewm
end interface

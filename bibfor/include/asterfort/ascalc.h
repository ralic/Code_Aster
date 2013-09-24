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
    subroutine ascalc(resu, masse, mome, psmo, stat,&
                      nbmode, neq, nordr, knomsy, nbopt,&
                      ndir, monoap, muapde, nbsup, nsupp,&
                      typcmo, temps, comdir, typcdi, tronc,&
                      amort, spectr, asspec, nomsup, reasup,&
                      depsup, tcosup, corfre, f1gup, f2gup)
        integer :: nbsup
        character(len=*) :: resu
        character(len=*) :: masse
        character(len=*) :: mome
        character(len=*) :: psmo
        character(len=*) :: stat
        integer :: nbmode
        integer :: neq
        integer :: nordr(*)
        character(len=*) :: knomsy(*)
        integer :: nbopt
        integer :: ndir(*)
        logical :: monoap
        logical :: muapde
        integer :: nsupp(*)
        character(len=*) :: typcmo
        real(kind=8) :: temps
        logical :: comdir
        character(len=*) :: typcdi
        logical :: tronc
        real(kind=8) :: amort(*)
        real(kind=8) :: spectr(*)
        real(kind=8) :: asspec(*)
        character(len=*) :: nomsup(*)
        real(kind=8) :: reasup(*)
        real(kind=8) :: depsup(*)
        integer :: tcosup(*)
        logical :: corfre
        real(kind=8) :: f1gup
        real(kind=8) :: f2gup
    end subroutine ascalc
end interface

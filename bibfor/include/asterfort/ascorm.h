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
    subroutine ascorm(monoap, typcmo, nbsup, nsupp, neq,&
                      nbmode, repmo1, repmo2, amort, modal,&
                      id, temps, recmor, recmop, tabs,&
                      nomsy, vecmod, reasup, spectr, corfre,&
                      muapde, tcosup, nintra, nbdis, f1gup,&
                      f2gup)
        integer :: nbmode
        integer :: neq
        integer :: nbsup
        logical :: monoap
        character(len=*) :: typcmo
        integer :: nsupp(*)
        real(kind=8) :: repmo1(nbsup, neq, *)
        real(kind=8) :: repmo2(nbsup, neq, *)
        real(kind=8) :: amort(*)
        real(kind=8) :: modal(nbmode, *)
        integer :: id
        real(kind=8) :: temps
        real(kind=8) :: recmor(nbsup, neq, *)
        real(kind=8) :: recmop(nbsup, neq, *)
        real(kind=8) :: tabs(nbsup, *)
        character(len=16) :: nomsy
        real(kind=8) :: vecmod(neq, *)
        real(kind=8) :: reasup(nbsup, nbmode, *)
        real(kind=8) :: spectr(*)
        logical :: corfre
        logical :: muapde
        integer :: tcosup(nbsup, *)
        integer :: nintra
        integer :: nbdis(nbsup)
        real(kind=8) :: f1gup
        real(kind=8) :: f2gup
    end subroutine ascorm
end interface

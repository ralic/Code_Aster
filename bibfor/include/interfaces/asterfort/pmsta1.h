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
    subroutine pmsta1(sigm, sigp, deps, vim, vip,&
                      nbvari, nbvita, iforta, nbpar, nompar,&
                      vr, igrad, typpar, nomvi, sddisc,&
                      liccvg, itemax, conver, actite)
        real(kind=8) :: sigm(6)
        real(kind=8) :: sigp(6)
        real(kind=8) :: deps(9)
        real(kind=8) :: vim(*)
        real(kind=8) :: vip(*)
        integer :: nbvari
        integer :: nbvita
        integer :: iforta
        integer :: nbpar
        character(len=16) :: nompar(*)
        real(kind=8) :: vr(*)
        integer :: igrad
        character(len=8) :: typpar(*)
        character(len=8) :: nomvi(*)
        character(len=19) :: sddisc
        integer :: liccvg(5)
        logical :: itemax
        logical :: conver
        integer :: actite
    end subroutine pmsta1
end interface

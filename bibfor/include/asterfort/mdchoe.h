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
    subroutine mdchoe(np1, np2, np3, nbm, nbmcd,&
                      nbnl, typch, nbseg, phii, choc,&
                      alpha, beta, gamma, orig, rc,&
                      theta, cmod, kmod, vitg, depg,&
                      vitg0, depg0, old, oldia, fmod,&
                      fmres, ftmp, mtmp1, mtmp6, testc,&
                      itforn, inewto, toln)
        integer :: np3
        integer :: np2
        integer :: np1
        integer :: nbm
        integer :: nbmcd
        integer :: nbnl
        integer :: typch(*)
        integer :: nbseg(*)
        real(kind=8) :: phii(np2, np1, *)
        real(kind=8) :: choc(6, *)
        real(kind=8) :: alpha(2, *)
        real(kind=8) :: beta(2, *)
        real(kind=8) :: gamma(2, *)
        real(kind=8) :: orig(6, *)
        real(kind=8) :: rc(np3, *)
        real(kind=8) :: theta(np3, *)
        real(kind=8) :: cmod(np1, *)
        real(kind=8) :: kmod(np1, *)
        real(kind=8) :: vitg(*)
        real(kind=8) :: depg(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: depg0(*)
        real(kind=8) :: old(9, *)
        integer :: oldia(*)
        real(kind=8) :: fmod(*)
        real(kind=8) :: fmres(*)
        real(kind=8) :: ftmp(*)
        real(kind=8) :: mtmp1(np1, *)
        real(kind=8) :: mtmp6(3, *)
        integer :: testc
        integer :: itforn(*)
        integer :: inewto
        real(kind=8) :: toln
    end subroutine mdchoe
end interface

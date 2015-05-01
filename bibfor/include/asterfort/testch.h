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
    subroutine testch(np1, np2, np3, nbmcd, nbnl,&
                      toln, tolc, tolv, typch, nbseg,&
                      phii, alpha, beta, gamma, orig,&
                      rc, theta, tconf1, depg, nbch,&
                      nbchex, iconf, ftest, iconfb, tconf2)
        integer :: np3
        integer :: np2
        integer :: np1
        integer :: nbmcd
        integer :: nbnl
        real(kind=8) :: toln
        real(kind=8) :: tolc
        real(kind=8) :: tolv
        integer :: typch(*)
        integer :: nbseg(*)
        real(kind=8) :: phii(np2, np1, 3)
        real(kind=8) :: alpha(2, *)
        real(kind=8) :: beta(2, *)
        real(kind=8) :: gamma(2, *)
        real(kind=8) :: orig(6, *)
        real(kind=8) :: rc(np3, *)
        real(kind=8) :: theta(np3, *)
        real(kind=8) :: tconf1(4, *)
        real(kind=8) :: depg(*)
        integer :: nbch
        integer :: nbchex
        integer :: iconf
        real(kind=8) :: ftest
        integer :: iconfb(*)
        real(kind=8) :: tconf2(4, *)
    end subroutine testch
end interface

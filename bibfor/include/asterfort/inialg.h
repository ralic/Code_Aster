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
    subroutine inialg(nbm, np2, np3, np4, nbmc,&
                      nbnl, npf, npfmax, npfts, depg,&
                      vitg, depg0, vitg0, accg0, amor00,&
                      puls00, fexttr, fext, text, fextts,&
                      texts, typch, nbseg, phii, alpha,&
                      beta, gamma, orig, rc, theta,&
                      iconfb, tconf1, ftest0)
        integer :: np4
        integer :: np3
        integer :: np2
        integer :: nbm
        integer :: nbmc
        integer :: nbnl
        integer :: npf
        integer :: npfmax
        integer :: npfts
        real(kind=8) :: depg(*)
        real(kind=8) :: vitg(*)
        real(kind=8) :: depg0(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: accg0(*)
        real(kind=8) :: amor00(*)
        real(kind=8) :: puls00(*)
        real(kind=8) :: fexttr(*)
        real(kind=8) :: fext(np4, *)
        real(kind=8) :: text(*)
        real(kind=8) :: fextts(np4, *)
        real(kind=8) :: texts(*)
        integer :: typch(*)
        integer :: nbseg(*)
        real(kind=8) :: phii(np2, nbm, *)
        real(kind=8) :: alpha(2, *)
        real(kind=8) :: beta(2, *)
        real(kind=8) :: gamma(2, *)
        real(kind=8) :: orig(6, *)
        real(kind=8) :: rc(np3, *)
        real(kind=8) :: theta(np3, *)
        integer :: iconfb(*)
        real(kind=8) :: tconf1(4, *)
        real(kind=8) :: ftest0
    end subroutine inialg
end interface

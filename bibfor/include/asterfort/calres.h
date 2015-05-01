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
    subroutine calres(np3, ic, typch, nbseg, choc,&
                      rc, theta, vloc, xloc, vloc0,&
                      xloc0, excloc, tetaj, jacobc, jacobk,&
                      floc, flres, old, oldia, iforn,&
                      toln)
        integer :: np3
        integer :: ic
        integer :: typch(*)
        integer :: nbseg(*)
        real(kind=8) :: choc(6, *)
        real(kind=8) :: rc(np3, *)
        real(kind=8) :: theta(np3, *)
        real(kind=8) :: vloc(*)
        real(kind=8) :: xloc(*)
        real(kind=8) :: vloc0(*)
        real(kind=8) :: xloc0(*)
        real(kind=8) :: excloc(*)
        real(kind=8) :: tetaj
        real(kind=8) :: jacobc(3, *)
        real(kind=8) :: jacobk(3, *)
        real(kind=8) :: floc(*)
        real(kind=8) :: flres(*)
        real(kind=8) :: old(9, *)
        integer :: oldia(*)
        integer :: iforn
        real(kind=8) :: toln
    end subroutine calres
end interface

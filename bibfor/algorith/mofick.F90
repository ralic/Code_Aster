subroutine mofick(fa, fav, cont, tange, maxfa,&
                  nface, nfacev, nfacem, fluxk, flux1k,&
                  flux2k, fluxl, flux1l, flux2l, moyfl,&
                  moyfl1, moyfl2)
!
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
!
    logical(kind=1) :: cont, tange
    integer :: maxfa
    integer :: nface, nfacev, nfacem
    integer :: jfa, jfav, fa, fav
    real(kind=8) :: fluxk(nface), fluxl(nfacev)
    real(kind=8) :: flux1k(1:maxfa+1, nface), flux2k(1:maxfa+1, nface)
    real(kind=8) :: flux1l(1:maxfa+1, nfacev), flux2l(1:maxfa+1, nfacev)
    real(kind=8) :: moyfl(nfacem)
    real(kind=8) :: moyfl1(1:maxfa, 1:maxfa+1, 0:1)
    real(kind=8) :: moyfl2(1:maxfa, 1:maxfa+1, 0:1)
!
    if (cont) then
        moyfl(fa) = 0.5d0 * ( fluxk(fa) - fluxl(fav) )
    endif
    if (tange) then
        moyfl1(fa,1,0) = 0.5d0 * flux1k(1,fa)
        moyfl2(fa,1,0) = 0.5d0 * flux2k(1,fa)
!
        moyfl1(fa,1,1) = -0.5d0 * flux1l(1,fav)
        moyfl2(fa,1,1) = -0.5d0 * flux2l(1,fav)
!
        do 1 jfa = 1, nface
            moyfl1(fa,1+jfa,0) = 0.5d0 * flux1k(1+jfa,fa)
            moyfl2(fa,1+jfa,0) = 0.5d0 * flux2k(1+jfa,fa)
 1      continue
!
        do 2 jfav = 1, nfacev
            moyfl1(fa,1+jfav,1) = -0.5d0 * flux1l(1+jfav,fav)
            moyfl2(fa,1+jfav,1) = -0.5d0 * flux2l(1+jfav,fav)
 2      continue
    endif
end subroutine

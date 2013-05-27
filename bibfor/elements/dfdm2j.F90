subroutine dfdm2j(nno, ipg, idfde, coor, jac)
    implicit none
    include 'jeveux.h'
    integer :: nno, ipg, idfde
    real(kind=8) :: coor(1), jac
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DU JACOBIEN (AVEC SIGNE)
!               POUR LES ELEMENTS 2D
!
!    - ARGUMENTS:
!        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
!                     DFRDE,DFRDK   -->  DERIVEES FONCTIONS DE FORME
!                     COOR          -->  COORDONNEES DES NOEUDS
!
!        RESULTATS:   JAC           <--  JACOBIEN AU POINT DE GAUSS
! ......................................................................
!
    integer :: i, ii, k
    real(kind=8) :: de, dk, dxde, dxdk, dyde, dydk
!
!
    dxde = 0.d0
    dxdk = 0.d0
    dyde = 0.d0
    dydk = 0.d0
    do 100 i = 1, nno
        k = 2*nno*(ipg-1)
        ii = 2*(i-1)
        de = zr(idfde-1+k+ii+1)
        dk = zr(idfde-1+k+ii+2)
        dxde = dxde + coor(2*i-1)*de
        dxdk = dxdk + coor(2*i-1)*dk
        dyde = dyde + coor(2*i )*de
        dydk = dydk + coor(2*i )*dk
100  end do
!
    jac = dxde*dydk - dxdk*dyde
!
end subroutine

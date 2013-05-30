subroutine invjac(nno, ipg, ipoids, idfde, coor,&
                  invja, jac)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/matini.h'
    include 'asterfort/matinv.h'
    integer :: ipg, ipoids, idfde, nno
    real(kind=8) :: coor(1), jac
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE
!                          POUR LES ELEMENTS 3D
!
!    - ARGUMENTS:
!        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
!                     POIDS         -->  POIDS DU POINT DE GAUSS
!              DFDRDE,DFRDN,DFRDK   -->  DERIVEES FONCTIONS DE FORME
!                     COOR          -->  COORDONNEES DES NOEUDS
!
!      RESULTAT :   INVJA         <--  INVERSE DE LA MATRICE JACOBIENNE
!                   JAC           <--  JACOBIEN
! ......................................................................
!
    integer :: i, j, ii, k
    real(kind=8) :: poids, g(3, 3)
    real(kind=8) :: de, dn, dk, invja(3, 3)
!
    poids = zr(ipoids+ipg-1)
!
    call matini(3, 3, 0.d0, g)
!
    do 100 i = 1, nno
        k = 3*nno*(ipg-1)
        ii = 3*(i-1)
        de = zr(idfde-1+k+ii+1)
        dn = zr(idfde-1+k+ii+2)
        dk = zr(idfde-1+k+ii+3)
        do 101 j = 1, 3
            g(1,j) = g(1,j) + coor(ii+j) * de
            g(2,j) = g(2,j) + coor(ii+j) * dn
            g(3,j) = g(3,j) + coor(ii+j) * dk
101      continue
100  end do
!
    call matinv('S', 3, g, invja, jac)
!
    jac = abs(jac)*poids
!
end subroutine

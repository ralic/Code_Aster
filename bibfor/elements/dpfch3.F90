subroutine dpfch3(nno, nnf, poids, dfrdef, dfrdnf,&
                  dfrdkf, coor, dfrdeg, dfrdng, dfrdkg,&
                  dfdx, dfdy, dfdz, jac)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!      REAL*8 (A-H,O-Z)
    include 'asterfort/matini.h'
    include 'asterfort/u2mesg.h'
    integer :: nno, nnf
    real(kind=8) :: poids, dfrdeg(1), dfrdng(1), dfrdkg(1), coor(1)
    real(kind=8) :: dfrdef(1), dfrdnf(1), dfrdkf(1)
    real(kind=8) :: dfdx(1), dfdy(1), dfdz(1), jac
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES DERIVEES DES FONCTIONS DE FORME
!               PAR RAPPORT A UN ELEMENT COURANT EN UN POINT DE GAUSS
!               POUR LES ELEMENTS 3D NON ISOPARAMETRIQUES
!                                    ====================
!    - ARGUMENTS:
!        DONNEES:
!          NNO           -->  NOMBRE DE NOEUDS
!          NNF           -->  NOMBRE DE FONCTIONS DE FORME
!          POIDS         -->  POIDS DU POINT DE GAUSS
!   DFRDEG,DFDNG,DFRDKG  -->  DERIVEES FONCTIONS DE FORME (GEOMETRIE)
!   DFRDEF,DFDNF,DFRDKF  -->  DERIVEES FONCTIONS DE FORME (VARIABLES)
!          COOR          -->  COORDONNEES DES NOEUDS
!
!        RESULTATS:
!          DFDX          <--  DERIVEES DES F. DE F. / X
!          DFDY          <--  DERIVEES DES F. DE F. / Y
!          DFDZ          <--  DERIVEES DES F. DE F. / Z
!          JAC           <--  JACOBIEN AU POINT DE GAUSS
! ......................................................................
!
    integer :: i, j, ii
    real(kind=8) :: g(3, 3), j11, j12, j13, j21, j22, j23, j31, j32, j33, de, dn
    real(kind=8) :: dk
    real(kind=8) :: valr
!
!     --- INITIALISATION DE LA MATRICE JACOBIENNE A ZERO
!
    call matini(3, 3, 0.d0, g)
!
!
!     --- CALCUL DE LA MATRICE JACOBIENNE (TRANSFORMATION GEOMETRIQUE)
!
    do 100 i = 1, nno
        ii = 3*(i-1)
        de = dfrdeg(i)
        dn = dfrdng(i)
        dk = dfrdkg(i)
        do 110 j = 1, 3
            g(1,j) = g(1,j) + coor(ii+j) * de
            g(2,j) = g(2,j) + coor(ii+j) * dn
            g(3,j) = g(3,j) + coor(ii+j) * dk
110      continue
100  end do
!
!     --- CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE
!
    j11 = g(2,2) * g(3,3) - g(2,3) * g(3,2)
    j21 = g(3,1) * g(2,3) - g(2,1) * g(3,3)
    j31 = g(2,1) * g(3,2) - g(3,1) * g(2,2)
    j12 = g(1,3) * g(3,2) - g(1,2) * g(3,3)
    j22 = g(1,1) * g(3,3) - g(1,3) * g(3,1)
    j32 = g(1,2) * g(3,1) - g(3,2) * g(1,1)
    j13 = g(1,2) * g(2,3) - g(1,3) * g(2,2)
    j23 = g(2,1) * g(1,3) - g(2,3) * g(1,1)
    j33 = g(1,1) * g(2,2) - g(1,2) * g(2,1)
!
!     --- DETERMINANT DE LA MATRICE JACOBIENNE
!
    jac = g(1,1) * j11 + g(1,2) * j21 + g(1,3) * j31
    if (jac .le. 0.0d0) then
        valr = jac
        call u2mesg('A', 'ELEMENTS5_30', 0, ' ', 0,&
                    0, 1, valr)
    endif
!
!     --- CALCUL DES DERIVEES EN ESPACE DES FONCTIONS DE FORME
!         DES VARIABLES
!
    do 200 i = 1, nnf
        dfdx(i) = (j11*dfrdef(i)+j12*dfrdnf(i)+j13*dfrdkf(i))/jac
        dfdy(i) = (j21*dfrdef(i)+j22*dfrdnf(i)+j23*dfrdkf(i))/jac
        dfdz(i) = (j31*dfrdef(i)+j32*dfrdnf(i)+j33*dfrdkf(i))/jac
200  end do
!
!
    jac = abs(jac) * poids
!
end subroutine

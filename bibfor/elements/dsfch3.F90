subroutine dsfch3(nno, nnf, poids, dpdef, dpdnf,&
                  dpdkf, dsdeef, dsdnnf, dsdkkf, dsdenf,&
                  dsdekf, dsdnkf, coor, dpdeg, dpdng,&
                  dpdkg, dsdeeg, dsdnng, dsdkkg, dsdeng,&
                  dsdekg, dsdnkg, dsdxxf, dsdyyf, dsdzzf,&
                  dsdxyf, dsdyzf, dsdxzf, jac)
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
! aslint: disable=W1504
    implicit none
!      REAL*8 (A-H,O-Z)
#include "asterfort/matini.h"
#include "asterfort/utmess.h"
    real(kind=8) :: valr
    integer :: nno, nnf
    real(kind=8) :: poids, dpdeg(1), dpdng(1), dpdkg(1), dsdeeg(1), dsdnng(1)
    real(kind=8) :: dsdkkg(1), dsdeng(1), dsdnkg(1), dsdekg(1), coor(1)
    real(kind=8) :: dpdef(1), dpdnf(1), dpdkf(1), dsdeef(1), dsdnnf(1)
    real(kind=8) :: dsdkkf(1), dsdenf(1), dsdnkf(1), dsdekf(1)
    real(kind=8) :: dsdxxf(1), dsdyyf(1), dsdzzf(1), dsdxyf(1), dsdxzf(1)
    real(kind=8) :: dsdyzf(1), jac
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES DERIVEES DES FONCTIONS DE FORME
!               PAR RAPPORT A UN ELEMENT COURANT EN UN POINT DE GAUSS
!               POUR LES ELEMENTS 3D NON ISOPARAMETRIQUES
!                                    ====================
!    - ARGUMENTS:
!        DONNEES:
!          NNO          -->  NOMBRE DE NOEUDS
!          NNF          -->  NOMBRE DE FONCTIONS DE FORME
!          POIDS        -->  POIDS DU POINT DE GAUSS
!    DPDEG,DFDNG,DPDKG  -->  DERIVEES 1ERE FONCTIONS DE FORME (GEOMETR
!    DPDEF,DFDNF,DPDKF  -->  DERIVEES 1ERE FONCTIONS DE FORME (VARIABL
!    DSDEEG,...,DSDNKG  -->  DERIVEES 1ERE FONCTIONS DE FORME (GEOMETR
!    DSDEEF,...,DSDNKF  -->  DERIVEES 1ERE FONCTIONS DE FORME (VARIABL
!    COOR               -->  COORDONNEES DES NOEUDS
!
!        RESULTATS:
!          DSDXXF        <--  DERIVEES 2EME DES F. DE F. / XX
!          DSDYYF        <--  DERIVEES 2EME DES F. DE F. / YY
!          DSDZZF        <--  DERIVEES 2EME DES F. DE F. / ZZ
!          DSDXYF        <--  DERIVEES 2EME DES F. DE F. / XY
!          DSDYZF        <--  DERIVEES 2EME DES F. DE F. / YZ
!          DSDXZF        <--  DERIVEES 2EME DES F. DE F. / XZ
!          JAC           <--  JACOBIEN AU POINT DE GAUSS
! ......................................................................
!
    integer :: i, j, k, ii
    real(kind=8) :: g(3, 3), de, dk, dn, jac2iv, j11, j12, j13, j21, j22, j23
    real(kind=8) :: j31, j32, j33, t2(6, 6), t1(6, 3), cj(6, 3), c2(6, 3)
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
        de = dpdeg(i)
        dn = dpdng(i)
        dk = dpdkg(i)
        do 110 j = 1, 3
            g(1,j) = g(1,j) + coor(ii+j) * de
            g(2,j) = g(2,j) + coor(ii+j) * dn
            g(3,j) = g(3,j) + coor(ii+j) * dk
110      continue
100  end do
!
!     --- CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE
!                  (AU DETERMINANT PRES)
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
        call utmess('A', 'ELEMENTS5_30', sr=valr)
    endif
!
!     --- CALCUL DA LA MATRICE T1
!
    jac2iv = 1.d0 / (jac * jac)
!
    t2(1,1) = j11 * j11 * jac2iv
    t2(1,2) = j12 * j12 * jac2iv
    t2(1,3) = j13 * j13 * jac2iv
    t2(2,1) = j21 * j21 * jac2iv
    t2(2,2) = j22 * j22 * jac2iv
    t2(2,3) = j23 * j23 * jac2iv
    t2(3,1) = j31 * j31 * jac2iv
    t2(3,2) = j32 * j32 * jac2iv
    t2(3,3) = j33 * j33 * jac2iv
!
    t2(1,4) = 2.d0 * j11 * j12 * jac2iv
    t2(1,5) = 2.d0 * j12 * j13 * jac2iv
    t2(1,6) = 2.d0 * j13 * j11 * jac2iv
    t2(2,4) = 2.d0 * j21 * j22 * jac2iv
    t2(2,5) = 2.d0 * j22 * j23 * jac2iv
    t2(2,6) = 2.d0 * j23 * j21 * jac2iv
    t2(3,4) = 2.d0 * j31 * j32 * jac2iv
    t2(3,5) = 2.d0 * j32 * j33 * jac2iv
    t2(3,6) = 2.d0 * j33 * j31 * jac2iv
!
    t2(4,1) = j11 * j21 * jac2iv
    t2(4,2) = j12 * j22 * jac2iv
    t2(4,3) = j13 * j23 * jac2iv
    t2(5,1) = j21 * j31 * jac2iv
    t2(5,2) = j22 * j32 * jac2iv
    t2(5,3) = j23 * j33 * jac2iv
    t2(6,1) = j31 * j11 * jac2iv
    t2(6,2) = j32 * j12 * jac2iv
    t2(6,3) = j33 * j13 * jac2iv
!
    t2(4,4) = (j11 * j22 + j12 * j21) * jac2iv
    t2(4,5) = (j12 * j23 + j13 * j22) * jac2iv
    t2(4,6) = (j11 * j23 + j13 * j21) * jac2iv
    t2(5,4) = (j21 * j32 + j22 * j31) * jac2iv
    t2(5,5) = (j22 * j33 + j23 * j32) * jac2iv
    t2(5,6) = (j21 * j33 + j23 * j31) * jac2iv
    t2(6,4) = (j31 * j12 + j32 * j11) * jac2iv
    t2(6,5) = (j32 * j13 + j33 * j12) * jac2iv
    t2(6,6) = (j31 * j13 + j33 * j11) * jac2iv
!
!     --- CALCUL DE LA MATRICE C2
!
    call matini(6, 3, 0.d0, c2)
!
    do 400 i = 1, nno
        ii = 3 * (i-1)
        c2(1,1) = c2(1,1) + coor(ii+1) * dsdeeg(i)
        c2(1,2) = c2(1,2) + coor(ii+2) * dsdeeg(i)
        c2(1,3) = c2(1,3) + coor(ii+3) * dsdeeg(i)
        c2(2,1) = c2(2,1) + coor(ii+1) * dsdnng(i)
        c2(2,2) = c2(2,2) + coor(ii+2) * dsdnng(i)
        c2(2,3) = c2(2,3) + coor(ii+3) * dsdnng(i)
        c2(3,1) = c2(3,1) + coor(ii+1) * dsdkkg(i)
        c2(3,2) = c2(3,2) + coor(ii+2) * dsdkkg(i)
        c2(3,3) = c2(3,3) + coor(ii+3) * dsdkkg(i)
        c2(4,1) = c2(4,1) + coor(ii+1) * dsdeng(i)
        c2(4,2) = c2(4,2) + coor(ii+2) * dsdeng(i)
        c2(4,3) = c2(4,3) + coor(ii+3) * dsdeng(i)
        c2(5,1) = c2(5,1) + coor(ii+1) * dsdnkg(i)
        c2(5,2) = c2(5,2) + coor(ii+2) * dsdnkg(i)
        c2(5,3) = c2(5,3) + coor(ii+3) * dsdnkg(i)
        c2(6,1) = c2(6,1) + coor(ii+1) * dsdekg(i)
        c2(6,2) = c2(6,2) + coor(ii+2) * dsdekg(i)
        c2(6,3) = c2(6,3) + coor(ii+3) * dsdekg(i)
400  end do
!
!     --- CALCUL DE LA MATRICE T1
!
    do 500 i = 1, 6
        cj(i,1) = c2(i,1) * j11 + c2(i,2) * j21 + c2(i,3) * j31
        cj(i,2) = c2(i,1) * j12 + c2(i,2) * j22 + c2(i,3) * j32
        cj(i,3) = c2(i,1) * j13 + c2(i,2) * j23 + c2(i,3) * j33
500  end do
!
    do 510 i = 1, 6
        do 510 j = 1, 3
            cj(i,j) = cj(i,j) / jac
510      continue
!
    do 520 i = 1, 6
        do 530 j = 1, 3
            t1(i,j) = 0.d0
            do 540 k = 1, 6
                t1(i,j) = t1(i,j) + t2(i,k) * cj(k,j)
540          continue
            t1(i,j) = - t1(i,j)
530      continue
520  end do
!
!     --- CALCUL DES DERIVEES EN ESPACE DES FONCTIONS DE FORME
!         DES VARIABLES
!
    do 600 i = 1, nnf
!
        dsdxxf(i) = t1(1,1)*dpdef(i)+t1(1,2)*dpdnf(i)+t1(1,3)*dpdkf(i) + t2(1,1)*dsdeef(i)+t2(1,2&
                    &)*dsdnnf(i)+t2(1,3)*dsdkkf(i) + t2(1,4)*dsdenf(i)+t2(1,5)*dsdnkf(i)+t2(1,6)*&
                    &dsdekf(i)
!
        dsdyyf(i) = t1(2,1)*dpdef(i)+t1(2,2)*dpdnf(i)+t1(2,3)*dpdkf(i) + t2(2,1)*dsdeef(i)+t2(2,2&
                    &)*dsdnnf(i)+t2(2,3)*dsdkkf(i) + t2(2,4)*dsdenf(i)+t2(2,5)*dsdnkf(i)+t2(2,6)*&
                    &dsdekf(i)
!
        dsdzzf(i) = t1(3,1)*dpdef(i)+t1(3,2)*dpdnf(i)+t1(3,3)*dpdkf(i) + t2(3,1)*dsdeef(i)+t2(3,2&
                    &)*dsdnnf(i)+t2(3,3)*dsdkkf(i) + t2(3,4)*dsdenf(i)+t2(3,5)*dsdnkf(i)+t2(3,6)*&
                    &dsdekf(i)
!
        dsdxyf(i) = t1(4,1)*dpdef(i)+t1(4,2)*dpdnf(i)+t1(4,3)*dpdkf(i) + t2(4,1)*dsdeef(i)+t2(4,2&
                    &)*dsdnnf(i)+t2(4,3)*dsdkkf(i) + t2(4,4)*dsdenf(i)+t2(4,5)*dsdnkf(i)+t2(4,6)*&
                    &dsdekf(i)
!
        dsdyzf(i) = t1(5,1)*dpdef(i)+t1(5,2)*dpdnf(i)+t1(5,3)*dpdkf(i) + t2(5,1)*dsdeef(i)+t2(5,2&
                    &)*dsdnnf(i)+t2(5,3)*dsdkkf(i) + t2(5,4)*dsdenf(i)+t2(5,5)*dsdnkf(i)+t2(5,6)*&
                    &dsdekf(i)
!
        dsdxzf(i) = t1(6,1)*dpdef(i)+t1(6,2)*dpdnf(i)+t1(6,3)*dpdkf(i) + t2(6,1)*dsdeef(i)+t2(6,2&
                    &)*dsdnnf(i)+t2(6,3)*dsdkkf(i) + t2(6,4)*dsdenf(i)+t2(6,5)*dsdnkf(i)+t2(6,6)*&
                    &dsdekf(i)
!
600  end do
!
    jac = abs(jac) * poids
!
end subroutine

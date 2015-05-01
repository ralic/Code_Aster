subroutine xmmco3(ino, ndim, dsidep, pla, p,&
                  ffc, jac, nnol, raug, mmat)
    implicit none
#include "jeveux.h"
#include "asterfort/matini.h"
#include "asterfort/promat.h"
#include "asterfort/transp.h"
    integer :: ino, ndim, pla(27)
    integer :: nnol
    real(kind=8) :: mmat(216, 216), dsidep(6, 6)
    real(kind=8) :: jac, ffc(8), raug
    real(kind=8) :: p(3, 3)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES MATRICES DE COHESION:
! --- PARTIE DEPENDANTE DE LA LOI D INTERFACE POUR CZM_LIN_MIX
!
! ----------------------------------------------------------------------
!
! IN  INO    : NUMERO LOCAL DU NOEUD
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  DSIDEP : DERIVEE DE LA CONTRAINTE COHESIVE PAR RAPPORT
!              AU MULTIPLICATEUR AUGMENTE, EN BASE LOCALE
! IN  PLA    : PLACE DES LAGRANGES DANS LA NUMEROTATION
! IN  P      : MATRICE DE CHANGEMENT DE BASE
! IN  FFC    : FONCTIONS DE FORME DE CONTACT
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  NNOL   : NOMBRE DE NOEUDS PORTANT DES INCONNUES DE CONTACT
! IN  RAUG   : PARAMETRE D AUGMENTATION
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
!
!
    integer :: i, j, k, l, pli
    real(kind=8) :: dside2(3, 3), ptr(3, 3), temp(3, 3), au(3, 3)
!
! ----------------------------------------------------------------------
!
!     INITIALISATION
! on reecrit tout
! matrices A et AT
    call matini(3, 3, 0.d0, au)
    call matini(3, 3, 0.d0, dside2)
    call matini(3, 3, 0.d0, temp)
    call matini(3, 3, 0.d0, ptr)
! ensuite, on prend exemple sur XMMCO2 pour l'opération
! changement de base
    do 10 i = 1, ndim
        do 11 j = 1, ndim
            dside2(i,j) = dsidep(i,j)
11      continue
10  continue
!
! MATRICE TANGENTE EN BASE FIXE [P]T [DSIDEP] [P]
!
! ne sert plus dans la formulation avec directions
    call transp(p, 3, ndim, ndim, ptr,&
                3)
    call promat(ptr, 3, ndim, ndim, dside2,&
                3, ndim, ndim, temp)
    call promat(temp, 3, ndim, ndim, p,&
                3, ndim, ndim, au)
! on peut alors remplir la matrice C : w*/lambda et sa transposee
! on vire la boucle sur I
! elle est déjà à l'extérieur
! avec la formulation qui inclut les directions
! on prend direct la matrice locale
    pli=pla(ino)
    do 12 l = 1, ndim
        do 13 k = 1, ndim
            mmat(pli-1+k,pli-1+ndim+l) = mmat(pli-1+k, pli-1+ndim+l)+ &
                ffc(ino)*dside2(k,l)*jac
            mmat(pli-1+ndim+l,pli-1+k) = mmat(pli-1+ndim+l,pli-1+k)+ &
                ffc(ino)*dside2(k,l)*jac
13      continue
12  continue
! on remplit la matrice w*/w
    pli=pla(ino)
    do 14 l = 1, ndim
        do 15 k = 1, ndim
            mmat(pli-1+ndim+k,pli-1+ndim+l) = mmat(pli-1+ndim+k, pli-1+ndim+l)+ &
                raug*ffc(ino)*dside2(k,l)*jac
15      continue
14  continue
! on remplit la matrice lambda*/lambda :
! attention a ne pas oublier lambda*lambda*/raug
! sur la diagonale uniquement!
    pli=pla(ino)
    do 1 l = 1, ndim
        do 2 k = 1, ndim
            mmat(pli-1+k,pli-1+l) = mmat(pli-1+k, pli-1+l)+ &
                ffc(ino)*dside2(k,l)*jac/raug
2      continue
       mmat(pli-1+l,pli-1+l) = mmat(pli-1+l, pli-1+l)- &
           ffc(ino)*jac/raug
1  continue
!
end subroutine

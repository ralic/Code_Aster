subroutine vpgskp(nbeq, nconv, vect, alpha, lmatb,&
                  typeps, vaux, ddlexc, delta)
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     SUBROUTINE ASTER ORTHONORMALISANT LES NCONV VECTEUR VECT(:,J)
! VIA L'ALGORITHME DE TYPE GRAM-SCHMIDT ITERATIF (IGS) SUIVANT LA
! VERSION DE KAHAN-PARLETT.
!---------------------------------------------------------------------
! L'ORTHONORMALISATION EST REALISEE AU SENS DU PRODUIT SCALAIRE:
!                             (MATRICE (B) DOIT ETRE SYMETRIQUE + ...)
!   SI TYPEPS=0 -> L2         (QUELCONQUE -> PRODUIT SCALAIRE EUCLIDIEN)
!   SI TYPEPS=1 -> LMATB*SIGN (INDEFINIE -> PSEUDO-PRODUIT SCALAIRE)
!   SI TYPEPS=2 -> LMATB  (SEMI DEFINIE POSITIVE -> B-PRODUIT SCALAIRE)
!   -------------------------------------------------------------------
!     PARTANT DE LA MATRICE DE VECTEUR (V1,V2..VI..VN)
!     ON CHERCHE A OBTENIR LA MATRICE ORTHOGONALE (Q1,Q2..QN)
!
!     (0) NORMALISATION DE V1: Q1 <- V1/||V1||
!     (I) BOUCLE I = 2..N
!     (IJ)  BOUCLE J = 1..I-1
!             CALCUL DE (VJ,VI)
!             CALCUL DE ||VI||
!             CALCUL DE VI+ <- VI - (VJ,VI)VJ
!             CALCUL DE ||VI+||
!             SI ||VI+|| >= ALPHA * ||VI|| ALORS (TEST 1)
!               QI <- VI+
!             SINON
!               CALCUL DE (VJ,VI+)
!               CALCUL DE VI++ <- VI+ - (VJ,VI+)VJ
!               CALCUL DE ||VI++||
!               SI ||VI++|| >= ALPHA * ||VI+|| ALORS (TEST 2)
!                 QI <- VI++
!               SINON
!                 QI <- 0
!               FIN TEST 2
!             FIN DE TEST 1
!           FIN DE BOUCLE J
!         FIN DE BOUCLE I
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!
!       MRMULT -> (SUBROUTINE ASTER)
!         PRODUIT MATRICE-VECTEUR.
!       R8MIEM -> (FONCTION ASTER)
!         LA VALEUR MINIMUM EVITANT L'OVERFLOW LORSQU'ON L'INVERSE.
!
!     FONCTIONS INTRINSEQUES:
!
!       MAX, SIGN, ABS, SQRT.
!   --------------------------------------------------------------------
!     PARAMETRES D'APPELS:
!
! IN  NBEQ   : IS : DIMENSION DES VECTEURS.
! IN  NCONV  : IS : NOMBRE DE VALEURS PROPRES CONVERGEES.
! IN  ALPHA  : R8 : PARAMETRE DE L'ALGORITHME DE KAHAN-PARLETT.
! IN  LMATB  : IS : DESCRIPTEUR MATRICE DE PRODUIT SCALAIRE.
! IN  TYPEPS : IS : TYPE DE PRODUIT SCALAIRE.
! IN  DDLEXC : IS : DDLEXC(1..NBEQ) VECTEUR POSITION DES DDLS BLOQUES.
!
! IN/OUT VECT  : R8 : VECT(1..NBEQ,1..NCONV) MATRICE DES VECTEURS A
!                     ORTHONORMALISER (IN),
!                     MATRICE ORTHOGONALE RESULTAT (OUT).
! IN/OUT VAUX  : R8 : VAUX(1..NBEQ) VECTEUR DE TRAVAIL.
! IN/OUT DELTA : R8 : DELTA(1..NCONV) VECTEUR DE STOCKAGE DES SIGN(PS).
!
! ASTER INFORMATION
! 11/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE DLAMCH PAR R8MIEM,
!            REMPLACEMENT DE DABS, DSQRT, DSIGN.
!--------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterc/r8miem.h"
#include "asterfort/mrmult.h"
    integer :: nbeq, nconv, lmatb, typeps, ddlexc(nbeq)
    real(kind=8) :: vect(nbeq, nconv), alpha, vaux(nbeq), delta(nconv)
!
!----------------------------------------------------------------
! DECLARATION VARIABLES LOCALES
!
    integer :: i, j, k, step
    real(kind=8) :: eps, raux, rauold, delta1
!----------------------------------------------------------------
! INITIALISATION DU PLUS PETIT REEL*8 EVITANT L'OVERFLOW
    eps = r8miem()**(2.0d+0 / 3.0d+0)
!
! NORMALISATION DU PREMIER VECTEUR PROPRE
    if (typeps .ne. 0) then
        call mrmult('ZERO', lmatb, vect(1, 1), vaux, 1,&
                    .false._1)
    else
        do 5 i = 1, nbeq
            vaux(i) = vect(i,1)
 5      continue
    endif
    raux = 0.d0
    do 10 i = 1, nbeq
        raux = raux + vect(i,1) * vaux(i) * ddlexc(i)
10  end do
    if (typeps .eq. 1) then
        delta(1) = sign(1.d0,raux)
    else
        delta(1) = 1.d0
    endif
    raux = delta(1) / max(eps,sqrt(abs(raux)))
    do 15 i = 1, nbeq
        vect(i,1) = vect(i,1) * raux * ddlexc(i)
15  end do
!
! BOUCLE 1 SUR LES VECTEURS PROPRES
    do 70 i = 2, nconv
!
! BOUCLE 2 SUR LES VECTEURS PROPRES
        do 65 j = 1, i-1
!
! CALCUL (VJ,VI) ET ||VI|| (STEP 1)/ (VJ,VI+) (STEP 2 SI NECESSAIRE)
            step = 0
20          continue
            step = step + 1
!
            if (typeps .ne. 0) then
                call mrmult('ZERO', lmatb, vect(1, i), vaux, 1,&
                            .false._1)
            else
                do 25 k = 1, nbeq
                    vaux(k) = vect(k,i)
25              continue
            endif
            raux = 0.d0
            do 30 k = 1, nbeq
                raux = raux + vect(k,j) * vaux(k) * ddlexc(k)
30          continue
            if (step .eq. 1) then
                rauold = 0.d0
                do 35 k = 1, nbeq
                    rauold = rauold + vect(k,i) * vaux(k) * ddlexc(k)
35              continue
            endif
!
! CALCUL VI+ <- VI - (VJ,VI)VJ (STEP 1)
! CALCUL VI++ <- VI+ - (VJ,VI+)VJ (STEP 2)
            delta1 = raux * delta(j)
            do 40 k = 1, nbeq
                vect(k,i) = (vect(k,i) - delta1 * vect(k,j)) * ddlexc( k)
40          continue
!
! CALCUL DE ||VI+|| (STEP 1) ET ||VI++|| (STEP 2)
            if (typeps .ne. 0) then
                call mrmult('ZERO', lmatb, vect(1, i), vaux, 1,&
                            .false._1)
            else if (typeps.eq.0) then
                do 45 k = 1, nbeq
                    vaux(k) = vect(k,i)
45              continue
            endif
            raux = 0.d0
            do 50 k = 1, nbeq
                raux = raux + vect(k,i) * vaux(k) * ddlexc(k)
50          continue
!
! PREMIER TEST
            if ((sqrt(abs(raux)).gt.(alpha*sqrt(abs(rauold)) -eps)) .and. (step.le.2)) then
                goto 60
            else if (step.eq.1) then
                rauold = raux
                goto 20
            else if (step.eq.2) then
                do 55 k = 1, nbeq
                    vect(k,i) = 0.d0
55              continue
            endif
!
60          continue
            if (typeps .eq. 1) then
                delta(i) = sign(1.d0,raux)
            else
                delta(i) = 1.d0
            endif
            raux = delta(i)/max(eps,sqrt(abs(raux)))
            do 65 k = 1, nbeq
                vect(k,i) = vect(k,i) * ddlexc(k) * raux
65          continue
!
70  continue
!
! FIN ROUTINE VPGSKP
end subroutine

subroutine possvd(nm, m, n, w, matu,&
                  u, matv, v, eps, rg,&
                  rv1)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
! DESCRIPTION :   POST-TRAITEMENTS AU CALCUL DE LA DECOMPOSITION AUX
! -----------     VALEURS SINGULIERES
!                                      T
!                             A = U S V
!
!                 REALISE PAR LA ROUTINE CALSVD
!                 A EST UNE MATRICE REELLE RECTANGULAIRE (M,N)
!
! IN     : NM   : INTEGER , SCALAIRE
!                 PREMIERE DIMENSION DES TABLEAUX A, U ET V, DECLAREE
!                 DANS L'APPELANT, NM >= MAX(M,N)
! IN     : M    : INTEGER , SCALAIRE
!                 NOMBRE DE LIGNES DES MATRICES A ET U
! IN     : N    : INTEGER , SCALAIRE
!                 NOMBRE DE COLONNES DES MATRICES A ET U
!                  = ORDRE DE LA MATRICE V
! IN/OUT : W    : REAL*8 , VECTEUR DE DIMENSION N
!                 CONTIENT LES N VALEURS SINGULIERES DE A
!                 EN SORTIE LES VALEURS SINGULIERES SONT REORDONNEES
!                 PAR MODULE DECROISSANT
! IN     : MATU : LOGICAL , SCALAIRE
!                 SI MATU = .TRUE. LA MATRICE U A ETE CALCULEE
! IN/OUT : U    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
!                 SI MATU = .TRUE. LE TABLEAU U CONTIENT LA MATRICE U
!                 (MATRICE (M,N) A COLONNES ORTHOGONALES)
!                 EN SORTIE LES COLONNES SONT REORDONNEES CONFORMEMENT
!                 AUX VALEURS SINGULIERES
! IN     : MATV : LOGICAL , SCALAIRE
!                 SI MATV = .TRUE. LA MATRICE V A ETE CALCULEE
! IN/OUT : V    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
!                 SI MATV = .TRUE. LE TABLEAU V CONTIENT LA MATRICE V
!                 (MATRICE CARREE D'ORDRE N ORTHOGONALE)
!                 EN SORTIE LES COLONNES SONT REORDONNEES CONFORMEMENT
!                 AUX VALEURS SINGULIERES
! IN     : EPS  : REAL*8 , SCALAIRE
!                 CRITERE DE PRECISION
! OUT    : RG   : INTEGER , SCALAIRE
!                 RANG DE LA MATRICE A
! IN/OUT : RV1  : REAL*8 , VECTEUR DE DIMENSION N
!                 VECTEUR DE TRAVAIL
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "blas/dswap.h"
    integer :: nm, m, n, rg
    real(kind=8) :: w(n), u(nm, n), v(nm, n), eps, rv1(n)
    logical(kind=1) :: matu, matv
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j, jmax, rgmax
    real(kind=8) :: wmax
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ON REORDONNE LES VALEURS SINGULIERES PAR MODULE DECROISSANT
!     SIMULTANEMENT ON EFFECTUE LES PERMUTATIONS ADEQUATES DES COLONNES
!     DES MATRICES U ET V SI ELLES ONT ETE CALCULEES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (n .gt. 1) then
        do 10 j = 1, n-1
            jmax = j
            wmax = w(j)
            do 20 i = j+1, n
                if (w(i) .gt. wmax) then
                    jmax = i
                    wmax = w(i)
                endif
20          continue
            if (jmax .ne. j) then
                w(jmax) = w(j)
                w(j) = wmax
                if (matu) call dswap(m, u(1, j), 1, u(1, jmax), 1)
                if (matv) call dswap(n, v(1, j), 1, v(1, jmax), 1)
            endif
10      continue
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   DETERMINATION DU RANG DE LA MATRICE A
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (w(1) .eq. 0.0d0) then
        rg = 0
    else
        rgmax = min(m,n)
        if (rgmax .gt. 1) then
            call dcopy(rgmax, w(1), 1, rv1(1), 1)
            call dscal(rgmax, 1.0d0/rv1(1), rv1(1), 1)
            do 30 j = 2, rgmax
                if (rv1(j) .lt. eps) goto 40
30          continue
40          continue
            rg = j - 1
        else
            rg = 1
        endif
    endif
!
! --- FIN DE POSSVD.
end subroutine

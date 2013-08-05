subroutine diatri(n, d, e, vector, evec,&
                  ldevec)
    implicit none
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/r8rotg.h"
#include "asterfort/r8sqrt.h"
#include "blas/dcopy.h"
#include "blas/drot.h"
#include "blas/dscal.h"
#include "blas/dswap.h"
#include "blas/idamax.h"
    integer :: n, ldevec
    real(kind=8) :: d(*), e(*), evec(ldevec, *)
    logical :: vector
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!   CALCUL DES VALEURS PROPRES ET VECTEURS PROPRES (OPTION => "VECTOR")
!      SUITE A LA TRANSFORMATION DE HOUSEHOLDER ( SUB. TRIDIA).
!-----------------------------------------------------------------------
! IN  : N    : DIMENSION DES MATRICES.
! I/O : D    : VECTEUR DE REELS DE LONGUEUR N.
!         IN : CONTIENT LA DIAGONALE DE LA MATRICE.
!        OUT : CONTIENT LES VALEURS PROPRES DANS L'ORDRE CROISSANT.
!     : E    : VECTEUR DE REELS DE LONGUEUR N.
!         IN : CONTIENT LES ELEMENTS DE LA DIAGONALE, E(1) ARBITRAIRE
!        OUT : VALEURS QUELCONQUES.
! IN  :VECTOR: VARIABLE LOGIQUE  .TRUE. SI LES VECTEURS PROPRES SONT
!              CALCULEES.
! I/O : EVEC : MATRICE REELLE D'ORDRE N.
!         IN : MATRICE TRANSFORMEE UTILISEE POUR TRANFORMER LA MATRICE
!              INITIALE EN UNE MATRICE TRIDIAGONALE.SI LES VECTEURS
!              PROPRES SONT CALCULES, ELLE CONTIENT LA MATRICE IDENTITE.
!        OUT : LE VECTEUR PROPRE ASSOCIE A EVAL(J) (J-TH VALEUR PROPRE
!              EST STOCKE DANS LA J-TH CLONNE.
! IN  :LDEVEC: DIMENSION EXACTE DE EVEC.
!-----------------------------------------------------------------------
    integer :: i, iter, j, k, l, m
    real(kind=8) :: b, c, f, g, p, r, s, scale, tiny, tol
!
    if (n .eq. 1) goto 9000
!
    call dcopy(n-1, e(2), 1, e(1), 1)
    e(n) = 0.0d0
!
    tiny = 100.0d0*r8miem()
    tol = r8prem()
    iter = 0
    do 60 l = 1, n
!    --- RECHERCHE DE LA PLUS PETITE VALEUR DE LA DIAGONALE SUPERIEURE.
10      continue
        do 20 m = l, n
            if (m .eq. n) goto 30
            if (abs(e(m)) .le. max(tol*(abs(d(m))+abs(d(m+1))), tiny)) goto 30
20      continue
!
30      continue
        p = d(l)
        if (m .eq. l) goto 60
        if (iter .eq. 30*n) then
!C            WRITE(6,*)  'THE ITERATION FOR THE EIGENVALUES DID '//
!C     &                  'NOT CONVERGE.'
            ASSERT(.false.)
        endif
        iter = iter + 1
!       --- VALEUR DE SHIFT ---
        g = (d(l+1)-p)/(2.0d0*e(l))
        r = r8sqrt(g,1.0d0)
        g = d(m) - p + e(l)/(g+sign(r,g))
        s = 1.0d0
        c = 1.0d0
        p = 0.0d0
!
        do 40 i = m - 1, l, -1
            f = s*e(i)
            b = c*e(i)
            call r8rotg(g, f, c, s)
            e(i+1) = g
            if (g .eq. 0.0d0) goto 50
            g = d(i+1) - p
            r = (d(i)-g)*s + 2.0d0*c*b
            p = s*r
            d(i+1) = g + p
            g = c*r - b
!
            if (vector) call drot(n, evec(1, i+1), 1, evec(1, i), 1,&
                                  c, s)
!
40      continue
!
        d(l) = d(l) - p
        e(l) = g
        e(m) = 0.0d0
        goto 10
!
50      continue
        d(i+1) = d(i+1) - p
        e(m) = 0.0d0
        goto 10
60  end do
!    --- POSITION DES VALEURS ET VECTERUS PROPRES ---
    do 90 i = 1, n - 1
        k = i
        p = d(i)
!
        do 70 j = i + 1, n
            if (d(j) .lt. p) then
                k = j
                p = d(j)
            endif
70      continue
!
        if (k .ne. i) then
            d(k) = d(i)
            d(i) = p
            if (vector) call dswap(n, evec(1, i), 1, evec(1, k), 1)
        endif
!
90  end do
!          --- NORMALISATION DES VECTEURS PROPRES ---
    if (vector) then
        do 100 j = 1, n
            i = idamax(n,evec(1,j),1)
            scale = evec(i,j)
            call dscal(n, 1.0d0/scale, evec(1, j), 1)
100      continue
    endif
!
9000  continue
end subroutine

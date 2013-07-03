subroutine vpzbal(mat, neq, mxeq, d, k,&
                  l)
    implicit none
#include "asterc/r8baem.h"
    integer :: neq, mxeq, k, l
    real(kind=8) :: mat(mxeq, 1), d(1)
!     ------------------------------------------------------------------
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
!     REDUCTION DE LA NORME DE LA MATRICE PAR LA TRANSFORMATION DE
!     SIMILITUDE STOCKEE DANS "D"
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE 320
!     ------------------------------------------------------------------
    integer :: l1, k1, j, i, ll, noconv
    real(kind=8) :: b, b2, r, c, f, g, s
!     ------------------------------------------------------------------
!     --- RECUPERATION DE LA BASE DE NUMEROTATION DE LA MACHINE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    b = r8baem()
    b2 = b * b
!
!     ---RECHERCHE DES VALEURS PROPRES ISOLEES (ON LES METS A LA FIN)---
!     --- CAS DES LIGNES ---
    l1 = 1
    k1 = neq
 5  continue
    do 30 j = k1, 1, -1
        r = -abs(mat(j,j))
        do 10 i = 1, k1
            r = r + abs(mat(j,i))
10      continue
        if (r .eq. 0.d0) then
            d(k1) = j
            if (j .ne. k1) then
                do 15 i = 1, k1
                    f = mat(i,j)
                    mat(i,j) = mat(i,k1)
                    mat(i,k1) = f
15              continue
                do 20 i = l1, neq
                    f = mat(j,i)
                    mat(j,i) = mat(k1,i)
                    mat(k1,i) = f
20              continue
            endif
            k1 = k1-1
            goto 5
        endif
30  end do
!
!     ---RECHERCHE DES VALEURS PROPRES ISOLEES (ON LES METS A GAUCHE)---
!     --- CAS DES COLONNES -
35  continue
    ll = l1
    do 60 j = ll, k1
        c = -abs(mat(j,j))
        do 40 i = l1, k1
            c = c + abs(mat(i,j))
40      continue
        if (c .eq. 0.d0) then
            d(l1) = j
            if (j .ne. l1) then
                do 45 i = 1, k1
                    f = mat(i,j)
                    mat(i,j) = mat(i,l1)
                    mat(i,l1) = f
45              continue
                do 50 i = l1, neq
                    f = mat(j,i)
                    mat(j,i) = mat(l1,i)
                    mat(l1,i) = f
50              continue
            endif
            l1 = l1+1
            goto 35
        endif
60  end do
!
!     EQUILIBRER LA SOUS-MATRICE DE LA LIGNES L1 A K1
    k = l1
    l = k1
    do 70 i = l1, k1
        d(i) = 1.d0
70  end do
75  continue
    noconv = 0
    do 115 i = l1, k1
        c = -abs(mat(i,i))
        r = c
        do 80 j = l1, k1
            c = c + abs(mat(j,i))
            r = r + abs(mat(i,j))
80      continue
        g = r/b
        f = 1.d0
        s = c+r
85      continue
        if (c .lt. g) then
            f = f*b
            c = c*b2
            goto 85
        endif
        g = r*b
95      continue
        if (c .ge. g) then
            f = f/b
            c = c/b2
            goto 95
        endif
!
!        --- EQUILIBRAGE ---
        if ((c+r)/f .lt. 0.95d0*s) then
            g = 1.d0/f
            d(i) = d(i)*f
            noconv = 1
            do 105 j = l1, neq
                mat(i,j) = mat(i,j)*g
105          continue
            do 110 j = 1, k1
                mat(j,i) = mat(j,i)*f
110          continue
        endif
115  end do
    if (noconv .eq. 1) goto 75
end subroutine

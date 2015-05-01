subroutine rms(imatr, vect1, long1, vect2, long2,&
               nbpts, nfcod, df, nfonc)
    implicit none
#include "jeveux.h"
    integer :: imatr, long1, long2, nbpts, nfcod
    real(kind=8) :: vect1(long1), vect2(long2)
!     ------------------------------------------------------------------
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
!     CALCUL DES INTEGRALES DES AUTOSPECTRES ET INTERSPECTRES
!     ------------------------------------------------------------------
!     IN  : VECT1 : VECTEURS DES VALEURS CUMULEES DES AUTOSPECTRES ET
!                   DES INTERSPECTRES EN FONCTION DU NOMBRE DE TIRAGES
!     OUT : VECT2 : VALEURS DES INTEGRALES
!           IMATR : NOMBRE DE TIRAGES REALISES
!
!-----------------------------------------------------------------------
    integer :: i, i1, ii, j, j1, jj, k
    integer :: kb, kf, kfonc, kk, lauto, lautor, lint1
    integer :: lint2, lintr, nbpts2, nfonc, nmatr
    real(kind=8) :: df, var1, var2, varij1, varij2, varmod
!-----------------------------------------------------------------------
    nmatr = long2 / nfcod
    kb = 0
    nbpts2 = nbpts/2
    do 10 kf = 1, nfonc
        var1= 0.d0
        var2= 0.d0
        kb = kb + kf
        lauto = (kb-1)*nbpts
        do 20 kk = 1, nbpts2
            var1= var1+(vect1(lauto+kk)/dble(imatr))*df
            var2= var2+(vect1(lauto+nbpts2+kk)/dble(imatr))*df
20      continue
        lautor = imatr+(kb-1)*nmatr
        vect2(lautor) = var1 + var2
10  end do
    kfonc = 1
    do 30 j = 1, nfonc
        do 40 i = 1, j
            if (i .eq. j) then
            else
                varij1 = 0.d0
                varij2 = 0.d0
                do 50 k = 1, nbpts2
                    lint1 = (kfonc-1)*nbpts + k
                    lint2 = lint1 + nbpts2
                    varij1 = varij1 + (vect1(lint1)/dble(imatr))*df
                    varij2 = varij2 + (vect1(lint2)/dble(imatr))*df
50              continue
                ii = 0
                jj = 0
                do 60 i1 = 1, i
                    ii = ii + i1
60              continue
                do 70 j1 = 1, j
                    jj = jj + j1
70              continue
                lintr = imatr + ( kfonc-1) * nmatr
                varmod = (sqrt(varij1**2+varij2**2))
                vect2(lintr) = varmod
            endif
            kfonc = kfonc + 1
40      continue
30  end do
end subroutine

subroutine mltfmj(nb, n, p, front, frn,&
                  adper, trav, c)
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
use superv_module
    implicit none
! aslint: disable=C1513
#include "blas/dgemm.h"
    integer :: n, p, adper(*)
    real(kind=8) :: front(*), frn(*)
    integer :: nb, decal, add, ind, nmb, i, j, kb, ia, ib, restm
    character(len=1) :: tra, trb
    integer :: i1, j1, k, m, it, numpro
    real(kind=8) :: s, trav(p, nb, *)
    real(kind=8) :: c(nb, nb, *), alpha, beta
    tra='N'
    trb='N'
    alpha=-1.d0
    beta=0.d0
    m=n-p
    nmb=m/nb
    restm = m -(nb*nmb)
    decal = adper(p+1) - 1
!
    !$OMP PARALLEL DO DEFAULT(PRIVATE) &
    !$OMP SHARED(N,M,P,NMB,NB,RESTM,FRONT,ADPER,DECAL,FRN,TRAV,C) &
    !$OMP SHARED(TRA,TRB,ALPHA,BETA) &
    !$OMP SCHEDULE(STATIC,1)
    do 1000 kb = 1, nmb
        numpro = asthread_getnum() + 1
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
        k = nb*(kb-1) + 1 +p
        do 100 i = 1, p
            s = front(adper(i))
            add= n*(i-1) + k
            do 50 j = 1, nb
                trav(i,j,numpro) = front(add)*s
                add = add + 1
50          continue
100      continue
!     BLOC DIAGONAL
!
!     SOUS LE BLOC DIAGONAL
!     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
!
        do 500 ib = kb, nmb
            ia = k + nb*(ib-kb)
            it=1
            call dgemm(tra, trb, nb, nb, p,&
                       alpha, front(ia), n, trav(it, 1, numpro), p,&
                       beta, c(1, 1, numpro), nb)
!     RECOPIE
!
!
            do 501 i = 1, nb
                i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                if (ib .eq. kb) then
                    j1= i
                    ind = adper(k + i1) - decal
                else
                    j1=1
                    ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                endif
                do 502 j = j1, nb
                    frn(ind) = frn(ind) +c(j,i,numpro)
                    ind = ind +1
502              continue
501          continue
500      continue
        if (restm .gt. 0) then
            ib = nmb + 1
            ia = k + nb*(ib-kb)
            it=1
            call dgemm(tra, trb, restm, nb, p,&
                       alpha, front(ia), n, trav(it, 1, numpro), p,&
                       beta, c(1, 1, numpro), nb)
!
!     RECOPIE
!
!
            do 801 i = 1, nb
                i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                j1=1
                ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                do 802 j = j1, restm
                    frn(ind) = frn(ind) +c(j,i,numpro)
                    ind = ind +1
802              continue
801          continue
        endif
1000  end do
    !$OMP END PARALLEL DO
    if (restm .gt. 0) then
        kb = 1+nmb
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
        k = nb*(kb-1) + 1 +p
        do 101 i = 1, p
            s = front(adper(i))
            add= n*(i-1) + k
            do 51 j = 1, restm
                trav(i,j,1) = front(add)*s
                add = add + 1
51          continue
101      continue
!     BLOC DIAGONAL
!
        ib = kb
        ia = k + nb*(ib-kb)
        it=1
        call dgemm(tra, trb, restm, restm, p,&
                   alpha, front(ia), n, trav(it, 1, 1), p,&
                   beta, c(1, 1, 1), nb)
!     RECOPIE
!
!
        do 902 i = 1, restm
            i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
            j1= i
            ind = adper(k + i1) - decal
            do 901 j = j1, restm
                frn(ind) = frn(ind) +c(j,i,1)
                ind = ind +1
901          continue
902      continue
!
    endif
end subroutine

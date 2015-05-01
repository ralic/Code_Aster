subroutine mlnflj(nb, n, ll, m, it,&
                  p, frontl, frontu, frnl, frnu,&
                  adper, travl, travu, cl, cu)
! person_in_charge: olivier.boiteau at edf.fr
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
    implicit none
#include "asterc/mlnbpr.h"
#include "asterc/mlnump.h"
#include "blas/dgemm.h"
    integer :: n, p, adper(*)
    real(kind=8) :: frontl(*), frnl(*), frontu(*), frnu(*)
    integer :: nb, decal, add, ind, nmb, i, j, kb, ia, ib, nlb, ll
    character(len=1) :: tra, trb
    integer :: m, k, i1, it, j1, restm, restl, nbl
    integer :: nproc, numpro
    real(kind=8) :: travl(p, nb, *), travu(p, nb, *)
    real(kind=8) :: cl(nb, nb, *), cu(nb, nb, *), alpha, beta
    tra='N'
    trb='N'
    alpha=-1.d0
    beta=0.d0
    nbl = p-it+1
    nmb=m/nb
    nlb = ll/nb
    restm = m -(nb*nmb)
    restl = ll-(nb*nlb)
    decal = adper(p+1) -1
    nproc = mlnbpr()
    numpro = 1
    if (nmb .ge. nproc) then
        !$OMP PARALLEL DO DEFAULT(PRIVATE) &
    !$OMP SHARED(N,M,P,NMB,NBL,NLB,NB,RESTM,RESTL,CL,CU,TRA,TRB,ALPHA,BETA) &
    !$OMP SHARED(FRONTL,FRONTU,ADPER,DECAL,FRNL,FRNU,TRAVL,TRAVU,IT) &
    !$OMP SCHEDULE(STATIC,1)
        do 1000 kb = 1, nmb
            numpro=mlnump()
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
            k = nb*(kb-1) + 1 +p
            do 100 i = it, p
                add= n*(i-1) + k
                do 50 j = 1, nb
                    travl(i,j,numpro) = frontl(add)
                    travu(i,j,numpro) = frontu(add)
                    add = add + 1
50              continue
100          continue
!     BLOC DIAGONAL
!
!     SOUS LE BLOC DIAGONAL
!     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
!
!
            do 500 ib = kb, nlb
                ia = n*(it-1) + k + nb*(ib-kb)
                call dgemm(tra, trb, nb, nb, nbl,&
                           alpha, frontl(ia), n, travu(it, 1, numpro), p,&
                           beta, cl(1, 1, numpro), nb)
                call dgemm(tra, trb, nb, nb, nbl,&
                           alpha, frontu(ia), n, travl(it, 1, numpro), p,&
                           beta, cu(1, 1, numpro), nb)
!     RECOPIE
!
!
                do 35 i = 1, nb
                    i1=i-1
!              IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                    if (ib .eq. kb) then
                        j1= i
                        ind = adper(k + i1) - decal
                    else
                        j1=1
                        ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                    endif
                    do 34 j = j1, nb
                        frnl(ind) = frnl(ind) +cl(j,i,numpro)
                        frnu(ind) = frnu(ind) +cu(j,i,numpro)
                        ind = ind +1
34                  continue
35              continue
500          continue
            if (restl .gt. 0) then
                ib = nlb + 1
                ia = n*(it-1) +k + nb*(ib-kb)
                call dgemm(tra, trb, restl, nb, nbl,&
                           alpha, frontl(ia), n, travu(it, 1, numpro), p,&
                           beta, cl(1, 1, numpro), nb)
                call dgemm(tra, trb, restl, nb, nbl,&
                           alpha, frontu(ia), n, travl(it, 1, numpro), p,&
                           beta, cu(1, 1, numpro), nb)
!           RECOPIE
!
!
                do 45 i = 1, nb
                    i1=i-1
                    j1=1
                    ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                    do 44 j = j1, restl
                        frnl(ind) = frnl(ind) +cl(j,i,numpro)
                        frnu(ind) = frnu(ind) +cu(j,i,numpro)
                        ind = ind +1
44                  continue
45              continue
            endif
1000      end do
    else
        do 2000 kb = 1, nmb
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
            k = nb*(kb-1) + 1 +p
            do 2100 i = it, p
                add= n*(i-1) + k
                do 250 j = 1, nb
                    travl(i,j,1) = frontl(add)
                    travu(i,j,1) = frontu(add)
                    add = add + 1
250              continue
2100          continue
!     BLOC DIAGONAL
!
!     SOUS LE BLOC DIAGONAL
!     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
!
!
            do 2500 ib = kb, nlb
                ia = n*(it-1) + k + nb*(ib-kb)
                call dgemm(tra, trb, nb, nb, nbl,&
                           alpha, frontl(ia), n, travu(it, 1, 1), p,&
                           beta, cl(1, 1, 1), nb)
                call dgemm(tra, trb, nb, nb, nbl,&
                           alpha, frontu(ia), n, travl(it, 1, 1), p,&
                           beta, cu(1, 1, 1), nb)
!     RECOPIE
!
!
                do 235 i = 1, nb
                    i1=i-1
                    if (ib .eq. kb) then
                        j1= i
                        ind = adper(k + i1) - decal
                    else
                        j1=1
                        ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                    endif
                    do 234 j = j1, nb
                        frnl(ind) = frnl(ind) +cl(j,i,1)
                        frnu(ind) = frnu(ind) +cu(j,i,1)
                        ind = ind +1
234                  continue
235              continue
2500          continue
            if (restl .gt. 0) then
                ib = nlb + 1
                ia = n*(it-1) +k + nb*(ib-kb)
                call dgemm(tra, trb, restl, nb, nbl,&
                           alpha, frontl(ia), n, travu(it, 1, 1), p,&
                           beta, cl(1, 1, 1), nb)
                call dgemm(tra, trb, restl, nb, nbl,&
                           alpha, frontu(ia), n, travl(it, 1, 1), p,&
                           beta, cu(1, 1, 1), nb)
!           RECOPIE
!
!
                do 245 i = 1, nb
                    i1=i-1
!              IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                    j1=1
                    ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                    do 244 j = j1, restl
                        frnl(ind) = frnl(ind) +cl(j,i,1)
                        frnu(ind) = frnu(ind) +cu(j,i,1)
                        ind = ind +1
244                  continue
245              continue
            endif
2000      end do
    endif
    if (restm .gt. 0) then
        kb = 1+nmb
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
        k = nb*(kb-1) + 1 +p
        do 101 i = it, p
            add= n*(i-1) + k
            do 51 j = 1, restm
                travl(i,j,1) = frontl(add)
                travu(i,j,1) = frontu(add)
                add = add + 1
51          continue
101      continue
!     BLOC DIAGONAL
!
!     SOUS LE BLOC DIAGONAL
!     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
!
        do 600 ib = kb, nlb
            ia = n*(it-1 ) + k + nb*(ib-kb)
            call dgemm(tra, trb, nb, restm, nbl,&
                       alpha, frontl(ia), n, travu(it, 1, 1), p,&
                       beta, cl(1, 1, 1), nb)
            call dgemm(tra, trb, nb, restm, nbl,&
                       alpha, frontu(ia), n, travl(it, 1, 1), p,&
                       beta, cu(1, 1, 1), nb)
!     RECOPIE
!
!
            do 55 i = 1, restm
                i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                if (ib .eq. kb) then
                    j1= i
                    ind = adper(k + i1) - decal
                else
                    j1=1
                    ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                endif
                do 54 j = j1, nb
                    frnl(ind) = frnl(ind) +cl(j,i,1)
                    frnu(ind) = frnu(ind) +cu(j,i,1)
                    ind = ind +1
54              continue
55          continue
600      continue
        if (restl .gt. 0) then
            ib = nlb + 1
            ia = n*(it-1) + k + nb*(ib-kb)
            call dgemm(tra, trb, restl, restm, nbl,&
                       alpha, frontl(ia), n, travu(it, 1, 1), p,&
                       beta, cl(1, 1, 1), nb)
            call dgemm(tra, trb, restl, restm, nbl,&
                       alpha, frontu(ia), n, travl(it, 1, 1), p,&
                       beta, cu(1, 1, 1), nb)
!     RECOPIE
!
!
            do 65 i = 1, restm
                i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                j1=1
                ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                do 64 j = j1, restl
                    frnl(ind) = frnl(ind) +cl(j,i,1)
                    frnu(ind) = frnu(ind) +cu(j,i,1)
                    ind = ind +1
64              continue
65          continue
        endif
    endif
end subroutine

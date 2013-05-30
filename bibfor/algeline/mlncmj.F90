subroutine mlncmj(nb, n, p, frontl, frontu,&
                  frnl, frnu, adper, t1, t2,&
                  cl, cu)
! person_in_charge: olivier.boiteau at edf.fr
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
!
!     VERSION AVEC APPEL A DGEMV POUR LES PRODUITS MATRICE-VECTEUR
!     AU DELA D' UN CERTAIN SEUIL
!     DGEMV EST APPEL A T1ERS LA FONCTION C DGEMW POUR CAR DGEMV
!     NECESSITE  DES ARGUMENTS ENTIER INTEGER*4 REFUSES PAR ASTER
!
    implicit none
    include 'asterc/mlnump.h'
    include 'blas/zgemm.h'
    integer :: n, p, adper(*), restm, decal
    complex(kind=8) :: frontl(*), frontu(*), frnl(*), frnu(*)
    integer :: nmb
    character(len=1) :: tra, trb
    integer :: i1, j1, k, m, it, nb, numprc
    complex(kind=8) :: t1(p, nb, *), t2(p, nb, *), alpha, beta
    complex(kind=8) :: cl(nb, nb, *), cu(nb, nb, *)
    integer :: i, kb, j, ib, ia, ind, add
    m=n-p
    nmb=m/nb
    restm = m -(nb*nmb)
    decal = adper(p+1) - 1
    tra='N'
    trb='N'
    alpha=dcmplx(-1.d0,0.d0)
    beta =dcmplx( 0.d0,0.d0)
!
    !$OMP PARALLEL DO DEFAULT(PRIVATE) &
    !$OMP SHARED(N,M,P,NMB,NB,RESTM,FRONTL,FRONTU,ADPER,DECAL,FRNL,FRNU) &
    !$OMP SHARED(T1,T2,CL,CU,TRA,TRB,ALPHA,BETA) &
    !$OMP SCHEDULE(STATIC,1)
    do 1000 kb = 1, nmb
        numprc=mlnump()
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
        k = nb*(kb-1) + 1 +p
        do 100 i = 1, p
            add= n*(i-1) + k
            do 50 j = 1, nb
                t1(i,j,numprc) = frontu(add)
                t2(i,j,numprc) = frontl(add)
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
            call zgemm(tra, trb, nb, nb, p,&
                       alpha, frontl(ia), n, t1(it, 1, numprc), p,&
                       beta, cl(1, 1, numprc), nb)
            call zgemm(tra, trb, nb, nb, p,&
                       alpha, frontu(ia), n, t2(it, 1, numprc), p,&
                       beta, cu(1, 1, numprc), nb)
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
                    frnl(ind) = frnl(ind) +cl(j,i,numprc)
                    frnu(ind) = frnu(ind) +cu(j,i,numprc)
                    ind = ind +1
502              continue
501          continue
500      continue
!
        if (restm .gt. 0) then
            ib = nmb + 1
            ia = k + nb*(ib-kb)
            it=1
            call zgemm(tra, trb, restm, nb, p,&
                       alpha, frontl(ia), n, t1(it, 1, numprc), p,&
                       beta, cl(1, 1, numprc), nb)
            call zgemm(tra, trb, restm, nb, p,&
                       alpha, frontu(ia), n, t2(it, 1, numprc), p,&
                       beta, cu(1, 1, numprc), nb)
!     RECOPIE
!
!
            do 801 i = 1, nb
                i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
                j1=1
                ind = adper(k + i1) - decal + nb*(ib-kb) - i1
                do 802 j = j1, restm
                    frnl(ind) = frnl(ind) +cl(j,i,numprc)
                    frnu(ind) = frnu(ind) +cu(j,i,numprc)
                    ind = ind +1
802              continue
801          continue
!
!
        endif
1000  end do
    !$OMP END PARALLEL DO
    numprc=1
    if (restm .gt. 0) then
        kb = 1+nmb
!     K : INDICE DE COLONNE DANS LA MATRICE FRONTLALE (ABSOLU DE 1 A N)
        k = nb*(kb-1) + 1 +p
        do 101 i = 1, p
            add= n*(i-1) + k
            do 51 j = 1, restm
                t1(i,j,1) = frontu(add)
                t2(i,j,1) = frontl(add)
                add = add + 1
51          continue
101      continue
!     BLOC DIAGONAL
!
        ib = kb
        ia = k + nb*(ib-kb)
        it=1
        call zgemm(tra, trb, restm, restm, p,&
                   alpha, frontl(ia), n, t1(it, 1, 1), p,&
                   beta, cl(1, 1, numprc), nb)
        call zgemm(tra, trb, restm, restm, p,&
                   alpha, frontu(ia), n, t2(it, 1, 1), p,&
                   beta, cu(1, 1, numprc), nb)
!     RECOPIE
!
!
        do 902 i = 1, restm
            i1=i-1
!     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
            j1= i
            ind = adper(k + i1) - decal
!
            do 901 j = j1, restm
!
                frnl(ind) = frnl(ind) +cl(j,i,numprc)
                frnu(ind) = frnu(ind) +cu(j,i,numprc)
                ind = ind +1
901          continue
902      continue
!
    endif
end subroutine

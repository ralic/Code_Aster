subroutine mlnflm(nb, n, p, frontl, frontu,&
                  adper, tu, tl, ad, eps,&
                  ier, cl, cu)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/mlnfld.h'
    include 'asterfort/mlnflj.h'
    include 'blas/dgemv.h'
    integer :: nb, n, p
    integer :: adper(*), ad(*), ier
    real(kind=8) :: frontl(*), frontu(*), tu(*), eps, tl(*), cl(nb, nb, *)
    real(kind=8) :: cu(nb, nb, *)
    real(kind=8) :: alpha, beta
    integer :: i, kb, adk, adki, decal, l
    integer :: m, ll, k, ind, ia, j, restp, npb
    integer :: incx, incy
    character(len=1) :: tra
    npb=p/nb
    restp = p -(nb*npb)
    ll = n
    tra='N'
    alpha=-1.d0
    beta=1.d0
    incx=1
    incy=1
!
    do 1000 kb = 1, npb
!     K : INDICE (DANS LA MATRICE FRONTALE ( DE 1 A P)),
!     DE LA PREMIERE COLONNE DU BLOC
        k = nb*(kb-1) + 1
        adk=adper(k)
!     BLOC DIAGONAL
        call mlnfld(nb, frontl(adk), frontu(adk), adper, tu,&
                    tl, ad, eps, ier)
        if (ier .gt. 0) goto 9999
!
!     NORMALISATION DES BLOCS SOUS LE BLOC DIAGONAL
!
        ll = ll -nb
        ia = adk + nb
        do 55 i = 1, nb
            ind = ia +n*(i-1)
            if (i .gt. 1) then
                do 51 l = 1, i-1
                    tu(l) = frontu(n*(k+l-2)+k+i-1)
                    tl(l) = frontl(n*(k+l-2)+k+i-1)
51              continue
            endif
            call dgemv(tra, ll, i-1, alpha, frontl(ia),&
                       n, tu, incx, beta, frontl(ind),&
                       incy)
            call dgemv(tra, ll, i-1, alpha, frontu(ia),&
                       n, tl, incx, beta, frontu(ind),&
                       incy)
            adki = adper(k+i-1)
!        LA PARTIE INFERIEURE  SEULE EST DIVISEE PAR LE TERME DIAGONAL,
!        PAS LA PARTIE SUPERIEURE
            do 53 j = 1, ll
                frontl(ind) = frontl(ind)/frontl(adki)
                ind = ind +1
53          continue
55      continue
!
        decal = kb*nb
        ll = n- decal
        m = p -decal
        ind =adper(k+nb)
        call mlnflj(nb, n, ll, m, k,&
                    decal, frontl, frontu, frontl(ind), frontu(ind),&
                    adper, tu, tl, cl, cu)
1000  end do
!     COLONNES RESTANTES
    if (restp .gt. 0) then
!     K : INDICE (DANS LA MATRICE FRONTALE ( DE 1 A P)),
!     DE LA PREMIERE COLONNE DU BLOC
        kb = npb + 1
        k = nb*npb + 1
        adk=adper(k)
!     BLOC DIAGONAL
        call mlnfld(restp, frontl(adk), frontu(adk), adper, tu,&
                    tl, ad, eps, ier)
        if (ier .gt. 0) goto 9999
!
!     NORMALISATION DES BLOCS SOUS LE BLOC DIAGONAL
!
        ll = n-p
        ia = adk +restp
        do 65 i = 1, restp
            ind = ia +n*(i-1)
            if (i .gt. 1) then
                do 59 l = 1, i-1
                    tu(l) = frontu(n*(k+l-2)+k+i-1)
                    tl(l) = frontl(n*(k+l-2)+k+i-1)
59              continue
            endif
            call dgemv(tra, ll, i-1, alpha, frontl(ia),&
                       n, tu, incx, beta, frontl(ind),&
                       incy)
            call dgemv(tra, ll, i-1, alpha, frontu(ia),&
                       n, tl, incx, beta, frontu(ind),&
                       incy)
            adki = adper(k+i-1)
!              SEUL FRONTL EST DIVISE PAR LE TERME DIAGONAL
            do 63 j = 1, ll
                frontl(ind) = frontl(ind)/frontl(adki)
                ind = ind +1
63          continue
65      continue
!
    endif
9999  continue
    if (ier .gt. 0) ier = ier + nb*(kb-1)
end subroutine

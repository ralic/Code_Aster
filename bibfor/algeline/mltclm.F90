subroutine mltclm(nb, n, p, front, adper,&
                  t1, ad, eps, ier, c)
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
#include "asterfort/mltcld.h"
#include "asterfort/mltclj.h"
#include "blas/zgemv.h"
    integer :: n, p, adper(*), ad(*), ier, nb
    real(kind=8) :: eps
    complex(kind=8) :: front(*), t1(*), c(nb, nb, *), alpha, beta
    integer :: i, kb, adk, adki, decal, l
    integer :: m, ll, k, ind, ia, j, restp, npb
    integer :: incx, incy
    character(len=1) :: tra
    npb=p/nb
    restp = p -(nb*npb)
    ll = n
    tra='N'
    alpha= dcmplx(-1.d0,0.d0)
    beta = dcmplx( 1.d0,0.d0)
    incx = 1
    incy = 1
!
    do 1000 kb = 1, npb
!     K : INDICE (DANS LA MATRICE FRONTALE ( DE 1 A P)),
!     DE LA PREMIERE COLONNE DU BLOC
        k = nb*(kb-1) + 1
        adk=adper(k)
!     BLOC DIAGONAL
        call mltcld(nb, front(adk), adper, t1, ad,&
                    eps, ier)
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
                    t1(l) = front(adper(k+l-1))*front(n*(k+l-2)+k+i-1)
51              continue
            endif
            call zgemv(tra, ll, i-1, alpha, front(ia),&
                       n, t1, incx, beta, front(ind),&
                       incy)
            adki = adper(k+i-1)
            do 53 j = 1, ll
                front(ind) = front(ind)/front(adki)
                ind = ind +1
53          continue
55      continue
!
        decal = kb*nb
        ll = n- decal
        m = p -decal
        ind =adper(k+nb)
        call mltclj(nb, n, ll, m, k,&
                    decal, front, front(ind), adper, t1,&
                    c)
1000  end do
!     COLONNES RESTANTES
    if (restp .gt. 0) then
!     K : INDICE (DANS LA MATRICE FRONTALE ( DE 1 A P)),
!     DE LA PREMIERE COLONNE DU BLOC
        kb = npb + 1
        k = nb*npb + 1
        adk=adper(k)
!     BLOC DIAGONAL
        call mltcld(restp, front(adk), adper, t1, ad,&
                    eps, ier)
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
                    t1(l) = front(adper(k+l-1))*front(n*(k+l-2)+k+i-1)
59              continue
            endif
            call zgemv(tra, ll, i-1, alpha, front(ia),&
                       n, t1, incx, beta, front(ind),&
                       incy)
            adki = adper(k+i-1)
            do 63 j = 1, ll
                front(ind) = front(ind)/front(adki)
                ind = ind +1
63          continue
65      continue
!
    endif
9999  continue
    if (ier .gt. 0) ier = ier + nb*(kb-1)
end subroutine

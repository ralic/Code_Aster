subroutine mltdrb(nbloc, ncbloc, decal, seq, nbsn,&
                  nbnd, supnd, adress, global, lgsn,&
                  factol, factou, x, temp, invp,&
                  perm, ad, trav, typsym, nbsm,&
                  s)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1304,W1504
    implicit none
    include 'jeveux.h'
!
    include 'asterc/llbloc.h'
    include 'asterc/mlnbpr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mlfmlt.h'
    include 'asterfort/mlfmul.h'
    integer :: nbsn, nbnd, nbloc, ncbloc(nbnd), decal(nbsn)
    integer(kind=4) :: global(*)
    integer :: seq(nbsn), supnd(nbsn+1), lgsn(nbsn)
    integer :: adress(nbsn+1), invp(nbnd), perm(nbnd), ad(nbnd)
    integer :: typsym, nbsm
    real(kind=8) :: temp(nbnd), x(nbnd, nbsm), trav(nbnd, nbsm), s(nbsm)
    character(len=24) :: factol, factou, factor
    integer :: ib, nc, isnd, long, l, i, ndj, p
!
    integer :: deb1
    integer :: sni, k, j, deb, ifac, ism
    integer :: seuil, tranch, nproc, larg
    integer :: opta, optb, nb
    nb=llbloc()
    call jemarq()
    optb=1
    nproc=mlnbpr()
    tranch = (nbsm + nproc - 1) /nproc
    seuil = nproc - mod(tranch*nproc-nbsm,nproc)
    do 130 ism = 1, nbsm
        do 110 j = 1, nbnd
            temp(invp(j)) = x(j,ism)
110      continue
        do 120 j = 1, nbnd
            x(j,ism) = temp(j)
120      continue
130  end do
!
!     DESCENTE  L * Y = B
    isnd = 0
    do 180 ib = 1, nbloc
        call jeveuo(jexnum(factol, ib), 'L', ifac)
        do 170 nc = 1, ncbloc(ib)
            isnd = isnd + 1
            sni = seq(isnd)
            long = adress(sni+1) - adress(sni)
            l = lgsn(sni)
            do 135 ism = 1, nbsm
                k = 1
                do 125 i = adress(sni), adress(sni+1) - 1
                    trav(k,ism) = x(global(i),ism)
                    k = k + 1
125              continue
135          continue
            ad(1) = decal(sni)
            ndj = supnd(sni) - 1
            do 150 j = 1, l - 1
                ndj = ndj + 1
!     CALCUL DU BLOC  DIAGONAL
                temp(ndj) = zr(ifac-1+ad(j))
                do 145 ism = 1, nbsm
                    k = 1
                    do 140 i = j + 1, l
                        trav(i,ism) = trav(i,ism) - zr(ifac-1+ad(j)+k) *trav(j,ism)
                        k = k + 1
140                  continue
145              continue
                ad(j+1) = ad(j) + long + 1
                ad(j) = ad(j) + l - j + 1
150          continue
            ndj = ndj + 1
!     RANGEMENT DU TERME DIAGONAL
            temp(ndj) = zr(ifac-1+ ad(l))
            ad(l) = ad(l) + 1
            if (long .gt. l) then
                p = l
                opta=1
                do 152 ism = 1, nproc
                    if (ism .gt. seuil) then
                        larg=tranch - 1
                        deb = seuil*tranch + (ism -seuil-1)*larg+ 1
                    else
                        deb = (ism-1)*tranch + 1
                        larg=tranch
                    endif
! APPEL AU PRODUIT PAR BLOCS
                    call mlfmul(trav(p+1, deb), zr(ifac+ad(1)-1), trav(1, deb), nbnd, long,&
                                p, larg, opta, optb, nb)
152              continue
            endif
            do 153 ism = 1, nbsm
                k = 1
                do 160 i = adress(sni), adress(sni+1) - 1
                    x(global(i),ism) = trav(k,ism)
                    k = k + 1
160              continue
153          continue
170      continue
        call jelibe(jexnum(factol, ib))
180  end do
!
    if (typsym .ne. 0) then
        factor = factol
        deb1=1
    else
        factor = factou
!     ON DIVISE PAR LE TERME DIAGONAL DANS LA REMONTEE EN NON-SYMETRIQUE
        deb1 = nbnd +1
    endif
!=======================================================================
!     D * Z = Y
    do 194 ism = 1, nbsm
        do 190 j = deb1, nbnd
            x(j,ism) = x(j,ism)/temp(j)
190      continue
194  end do
!=======================================================================
!     REMONTEE  U * X = Z
    isnd = nbsn + 1
    opta=0
    do 260 ib = nbloc, 1, -1
        call jeveuo(jexnum(factor, ib), 'L', ifac)
        do 250 nc = 1, ncbloc(ib)
            isnd = isnd - 1
            sni = seq(isnd)
            l = lgsn(sni)
            long = adress(sni+1) - adress(sni)
            deb = adress(sni) + lgsn(sni)
            do 205 ism = 1, nbsm
                k = 1
                do 200 i = adress(sni), adress(sni+1) - 1
                    trav(k,ism) = x(global(i),ism)
                    k = k + 1
200              continue
205          continue
            p=l
            larg=nbsm
            deb=1
            if (long .gt. p) then
! APPEL AU PRODUIT PAR BLOCS
                call mlfmlt(trav(1, deb), zr(ifac-1 + decal(sni) + p), trav(p+1, deb), nbnd,&
                            long, p, larg, opta, optb,&
                            nb)
            endif
!     PARTIE DIAGONALE
            ad(1)=decal(sni)
            do 300 j = 1, l-1
                ad(j+1) = ad(j)+ long+1
300          continue
            do 350 j = l, 1, -1
!
                do 345 ism = 1, nbsm
                    k = 1
                    s(ism) =0.d0
                    do 340 i = j + 1, l
                        s(ism) =s(ism) + zr(ifac-1+ad(j)+k) *trav(i,&
                        ism)
                        k = k + 1
340                  continue
                    trav(j,ism) = trav(j,ism) - s(ism)
                    if (typsym .eq. 0) then
                        trav(j,ism)= trav(j,ism)/zr(ifac-1+ad(j))
                    endif
345              continue
350          continue
            do 221 ism = 1, nbsm
                k = 1
                do 240 i = adress(sni), adress(sni+1) - 1
                    x(global(i),ism) = trav(k,ism)
                    k = k + 1
240              continue
221          continue
250      continue
        call jelibe(jexnum(factor, ib))
260  end do
!     ON RANGE DANS SM  LA SOLUTION DANS LA NUMEROTATION INITIALE
    do 265 ism = 1, nbsm
        do 270 j = 1, nbnd
!
            temp(perm(j)) = x(j,ism)
270      continue
!
        do 275 j = 1, nbnd
            x(j,ism) = temp(j)
275      continue
265  end do
    call jedema()
end subroutine

subroutine mltpas(nbnd, nbsn, supnd, xadj, adjncy,&
                  anc, nouv, seq, global, adress,&
                  nblign, lgsn, nbloc, ncbloc, lgbloc,&
                  diag, col, lmat, place)
! ----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!
! aslint: disable=W1304
    implicit none
    integer :: nbnd, nbsn, nbloc, ncbloc(*), lgbloc(*)
    integer :: supnd(nbsn+1), diag(0:nbnd), seq(nbsn)
    integer :: col(*)
    integer :: xadj(nbnd+1), adjncy(*), lmat
    integer :: anc(nbnd), nouv(nbnd)
    integer(kind=4) :: global(*)
    integer :: adress(nbsn+1)
    integer :: nblign(nbsn), lgsn(nbsn)
!
!=========================================================
!     CALCUL DES ADRESSES DANS LA FACTORISEE DES TERMES INITIAUX
!     VERSION ASTER AVEC MATRICE INITIALE COMPACTE PAR LIGNES
!     ET DDL DE LAGRANGE
!     DANS CETTE VERSION LES ADRESSES DES TERMES INITIAUX
!     SONT RANGEES DANS COL, QUI NE SERT PLUS.
!     AUPARAVANT ON UTILISAIT UN TABLEAU ADINIT
!==========================================================
    integer :: place(nbnd)
    integer :: i, j, ndj, sni, andi, andj, code, haut
    integer :: ndi, lfac, depart, ad, isn, longb, ib, ic
    isn = 0
    longb = 0
    lmat=diag(nbnd)
    do 290 ib = 1, nbloc
        lfac = longb
        do 280 ic = 1, ncbloc(ib)
            isn = isn + 1
            sni = seq(isn)
            do 120 i = adress(sni), adress(sni+1) - 1
                place(global(i)) = i - adress(sni) + 1
120          continue
            haut = nblign(sni)
            do 270 i = 0, lgsn(sni) - 1
                ndi = supnd(sni) + i
                andi = anc(ndi)
                depart = diag(andi-1) + 1
                col(diag(andi))=lfac + i*haut+i+1 +nbnd
                do 170 j = xadj(andi), xadj(andi+1) - 1
                    andj = adjncy(j)
                    ndj = nouv(andj)
                    if (ndj .ge. ndi) then
                        if (andj .le. andi) then
                            do 130 ad = depart, diag(andi)
                                if (col(ad) .eq. andj) goto 140
130                          continue
                            goto 170
140                          continue
                            code = -1
                            depart = ad
                        else
                            do 150 ad = diag(andj-1) + 1, diag(andj)
                                if (col(ad) .eq. andi) goto 160
150                          continue
                            goto 170
160                          continue
                            code = 1
                        endif
!     CHANGTDGEMV                  ADINIT(AD) = LFAC + PLACE(NDJ) - I
!     ADINIT(AD) = LFAC + I*HAUT + PLACE(NDJ)
                        col(ad)= lfac + i*haut + place(ndj) +nbnd
!
                        if (code .lt. 0) then
                            col(ad) = -col(ad)
                        endif
                    endif
170              continue
!
270          continue
            lfac = lfac + haut*lgsn(sni)
280      continue
        longb = lgbloc(ib) + longb
290  end do
    do 300 i = 1, lmat
        if (col(i) .gt. nbnd) then
            col(i)=col(i)-nbnd
        else if (col(i).lt.(-nbnd)) then
            col(i) = col(i) +nbnd
        else
        endif
300  end do
end subroutine

subroutine mltpos(nbsn, parent, fils, frere, pile,&
                  lfront, seq, flag, estim, u,&
                  w, tab, liste)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_6
    implicit none
    include 'asterfort/blimax.h'
    include 'asterfort/tri.h'
    integer :: nbsn, parent(*), fils(*), frere(*), pile(*), lfront(*)
    integer :: seq(*), estim
    integer :: flag(*)
    integer :: u(nbsn), w(nbsn), tab(nbsn), liste(nbsn)
!
    integer :: init, filsi, nd, iq, md, m, i, k, sni, itemp, lp, q1, q2, sn
!-----------------------------------------------------------------------
!     CALCUL DES TABLEAUX U ET W (VOIR NOTES RESP. DE  ASHCRAFT ET YANG)
!
    do 110 sni = 1, nbsn
        u(sni) = (lfront(sni)* (lfront(sni)+1))/2
110  end do
    do 150 sni = 1, nbsn
        q1 = u(sni)
        sn = fils(sni)
        if (sn .eq. 0) then
            w(sni) = q1
        else
            m = 1
            sn = fils(sni)
            liste(1) = sn
            tab(1) = u(sn)
            sn = frere(sn)
!          DO WHILE (SN.NE.0)
120          continue
            if (sn .ne. 0) then
                m = m + 1
                liste(m) = sn
                tab(m) = tab(m-1) + u(m)
                sn = frere(sn)
                goto 120
! FIN DO WHILE
            endif
            do 130 k = 1, m
                tab(k) = tab(k) + w(liste(k))
130          continue
            q2 = tab(blimax(m,tab,1))
            do 140 i = 1, m
                q1 = q1 + u(liste(i))
140          continue
            w(sni) = max(q1,q2)
        endif
150  end do
!-----------------------------------------------------------------------
!      MODIFICATION DE FILS ET FRERE POUR MINIMISER LA PILE
    do 180 sni = 1, nbsn
        sn = fils(sni)
        if (sn .ne. 0) then
            m = 1
            liste(m) = sn
            tab(m) = w(liste(m)) - u(liste(m))
            sn = frere(sn)
!          DO WHILE (SN.NE.0)
160          continue
            if (sn .ne. 0) then
                m = m + 1
                liste(m) = sn
                tab(m) = w(liste(m)) - u(liste(m))
                sn = frere(sn)
                goto 160
! FIN DO WHILE
            endif
            call tri(tab, liste, 1, m)
            fils(sni) = liste(m)
            sn = fils(sni)
            k = m - 1
!          DO WHILE (K.GE.1)
170          continue
            if (k .ge. 1) then
                frere(sn) = liste(k)
                sn = liste(k)
                k = k - 1
                goto 170
! FIN DO WHILE
            endif
            frere(liste(1)) = 0
        endif
180  end do
!-----------------------------------------------------------------------
!      CALCUL DE LA SEQUENCE D'EXECUTION
!
    do 190 i = 1, nbsn
        flag(i) = 0
190  end do
    iq = 0
    do 240 init = 1, nbsn
        if (parent(init) .eq. 0) then
            lp = 0
            filsi = init
!          DO WHILE (FILSI.NE.0)
200          continue
            if (filsi .ne. 0) then
!             ND = FILSI
                lp = lp + 1
                pile(lp) = filsi
                filsi = fils(filsi)
                goto 200
! FIN DO WHILE
            endif
!          DO WHILE (LP.GT.0)
210          continue
            if (lp .gt. 0) then
220              continue
                nd = pile(lp)
                md = fils(nd)
!            DO WHILE (MD.NE.0)
230              continue
                if (md .ne. 0) then
                    if (flag(md) .eq. 0) then
                        if (fils(md) .eq. 0) then
                            iq = iq + 1
                            seq(iq) = md
                            flag(md) = 1
                        else
                            lp = lp + 1
                            pile(lp) = md
                            goto 220
                        endif
                    endif
                    md = frere(md)
                    goto 230
! FIN DO WHILE
                endif
                iq = iq + 1
                seq(iq) = nd
                flag(nd) = 1
                lp = lp - 1
                goto 210
! FIN DO WHILE
            endif
        endif
240  end do
!      ESTIMATION DE LA PILE
    estim = 1
    itemp = 1
    do 250 i = 1, nbsn
        sni = seq(i)
        m = lfront(sni)
        if (fils(sni) .eq. 0) then
            pile(sni) = itemp
            itemp = itemp + (m* (m+1))/2
            estim = max(estim,itemp-1)
        else
            itemp = itemp + (m* (m+1))/2
            estim = max(estim,itemp-1)
            pile(sni) = pile(fils(sni))
            itemp = pile(fils(sni)) + (m* (m+1))/2
        endif
250  end do
end subroutine

subroutine rfbefl(base)
    implicit none
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/foattr.h'
    include 'asterfort/foimpr.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/ordonn.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: base
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
!
!     OPERATEUR "RECU_FONCTION"  MOT CLE "BASE_ELAS_FLUI"
!     ------------------------------------------------------------------
    integer :: ifm, niv
    character(len=4) :: interp(2)
    character(len=8) :: k8b, basefl, paray, ttordr
    character(len=16) :: nomcmd, typcon
    character(len=19) :: nomfon
    character(len=24) :: vite, numeo, numo, freq
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ibid, id, imod, ind, ind1, ind2
    integer :: inumeo, j, lfon, lfreq, lnumo, lpro, lvar
    integer :: lvite, min, n1, n2, n3, n4, n5
    integer :: nbm, nbno, nbv, npv, nummod
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typcon, nomcmd)
    basefl = base
    nbv = 0
    interp(1) = 'NON '
    interp(2) = 'NON '
!
!     --- RECUPERATION DES ENTREES ---
!
    call getvtx(' ', 'PARA_Y', 0, iarg, 1,&
                paray, n2)
    call getvtx(' ', 'TOUT_ORDRE', 0, iarg, 1,&
                ttordr, n3)
    call getvis(' ', 'NUME_MODE', 0, iarg, 1,&
                nummod, n4)
    call getvtx(' ', 'INTERPOL', 0, iarg, 2,&
                interp, n5)
    if (n5 .eq. 1) interp(2) = interp(1)
!
!     --- REMPLISSAGE DU .PROL ---
!
    call assert(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCTION'
    zk24(lpro+1) = interp(1)//interp(2)
    zk24(lpro+2) = 'VITE_FLU'
    zk24(lpro+3) = paray(1:4)
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon
!
!     --- RECUPERATION DES OJB ---
    vite = basefl//'           .VITE'
    call jelira(vite, 'LONUTI', npv, k8b)
    call jeveuo(vite, 'L', lvite)
    freq = basefl//'           .FREQ'
    call jeveuo(freq, 'L', lfreq)
    numo = basefl//'           .NUMO'
    call jelira(numo, 'LONUTI', nbm, k8b)
    call jeveuo(numo, 'L', lnumo)
!
!   --- VERIFICATION DE LA VALIDITE DES NUMEROS D'ORDRE DES VITESSES -
!
    if (ttordr .ne. 'OUI') then
        numeo = '&&RFBEFL.NUME_ORDRE'
        call getvis(' ', 'NUME_ORDRE', 0, iarg, 0,&
                    ibid, nbno)
        nbno = -nbno
        call wkvect(numeo, 'V V I', nbno, inumeo)
        call getvis(' ', 'NUME_ORDRE', 0, iarg, nbno,&
                    zi(inumeo), n1)
        min = zi(inumeo)
        do 10 i = 1, nbno
            id = min - zi(inumeo + i - 1)
            if (id .gt. 0) min = zi(inumeo + i - 1)
10      continue
        if (min .gt. npv) then
            call u2mess('F', 'UTILITAI4_9')
        endif
    endif
!
!     --- DETERMINATION DU NUMERO D'ORDRE DU MODE VOULU ---
!
    do 20 imod = 1, nbm
        id = nummod - zi(lnumo + imod - 1)
        if (id .eq. 0) goto 30
20  end do
    call u2mess('F', 'UTILITAI4_10')
30  continue
!
!     --- CAS 1 : REMPLISSAGE POUR TOUS LES NUMEROS D'ORDRE ---
!
    if (ttordr .eq. 'OUI') then
        call wkvect(nomfon//'.VALE', 'G V R', 2*npv, lvar)
        lfon = lvar + npv
        do 40 i = 1, npv
            zr(lvar + i - 1) = zr(lvite + i - 1)
            if (paray(1:4) .eq. 'FREQ') then
                ind = 2*nbm*(i - 1) + 2*(imod - 1)
                zr(lfon + i - 1) = zr(lfreq + ind)
            else
                ind = 2*nbm*(i - 1) + 2*(imod - 1) + 1
                zr(lfon + i - 1) = zr(lfreq + ind)
            endif
40      continue
    else
!
!     --- CAS 2 : REMPLISSAGE POUR UNE LISTE DE NUMEROS D'ORDRE ---
!
!---------2.1 ON ORDONNE LA LISTE DES NUMEROS D'ORDRE
        if (nbno .gt. 1) then
            do 41 i = 1, nbno
                ind = i
                min = zi(inumeo + i - 1)
                do 42 j = i+1, nbno
                    id = min - zi(inumeo + j - 1)
                    if (id .gt. 0) then
                        ind = j
                        min = zi(inumeo + j - 1)
                    endif
42              continue
                zi(inumeo + ind - 1) = zi(inumeo + i - 1)
                zi(inumeo + i - 1) = min
41          continue
        endif
!
!---------2.2 DETERMINATION DU NOMBRE DE NUMEROS D'ORDRE VALIDES
        if (nbno .gt. 1) then
            do 43 i = 1, nbno
                if (zi(inumeo + i - 1) .gt. npv) goto 44
                nbv = nbv + 1
43          continue
44          continue
        else
            nbv = 1
        endif
!
!---------2.3 REMPLISSAGE
        call wkvect(nomfon//'.VALE', 'G V R', 2*nbv, lvar)
        lfon = lvar + nbv
        do 45 i = 1, nbv
            ind1 = zi(inumeo + i - 1)
            zr(lvar + i - 1) = zr(lvite + ind1 - 1)
            if (paray(1:4) .eq. 'FREQ') then
                ind2 = 2*nbm*(ind1 - 1) + 2*(imod - 1)
                zr(lfon + i - 1) = zr(lfreq + ind2)
            else
                ind2 = 2*nbm*(ind1 - 1) + 2*(imod - 1) + 1
                zr(lfon + i - 1) = zr(lfreq + ind2)
            endif
45      continue
        call jedetr(numeo)
!
    endif
!
    call foattr(' ', 1, nomfon)
!
!     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine

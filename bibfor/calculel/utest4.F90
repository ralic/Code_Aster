subroutine utest4(chamgd, typtes, typres, nbref, tbtxt,&
                  refi, refr, refc, epsi, lign1,&
                  lign2, crit, ific, nbcmp, nocmp,&
                  llab, ssigne)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/celces.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utites.h'
    include 'asterfort/wkvect.h'
    integer :: nbref, refi(nbref), ific, nbcmp
    real(kind=8) :: refr(nbref), epsi
    character(len=8) :: typtes, nocmp(*)
    character(len=16) :: tbtxt(2)
    character(len=*) :: chamgd, typres, crit, ssigne
    character(len=200) :: lign1, lign2
    complex(kind=8) :: refc(nbref)
    logical :: llab
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
! IN  : CHAMGD : NOM DU CHAM_GD
! IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
! IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
! IN  : REFR   : VALEUR REELLE ATTENDUE
! IN  : REFC   : VALEUR COMPLEXE ATTENDUE
! IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
! IN  : EPSI   : PRECISION ESPEREE
! IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
! IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
! IN  : NODDL  : NOM DU DDL A TRAITER
! IN  : LLAB   : FLAG D IMPRESSION DES LABELS
! IN/OUT  : LIGN1  : PREMIERE LIGNE D'IMPRESSION DU RESULTAT
! IN/OUT  : LIGN2  : DEUXIEME LIGNE D'IMPRESSION DU RESULTAT
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, ibid, neq, i, j, k, iret1, valii, icmp
    integer :: ncmp, vnocmp, jcsd, jcsc, jcsv, jcsl, jcmp, ind
    integer :: nl1, nl11, nl2, nl22
    real(kind=8) :: valr, valrr
    complex(kind=8) :: valc
    character(len=1) :: typrez
    character(len=24) :: valk(3)
    character(len=4) :: type
    character(len=8) :: tych, noddl
    character(len=19) :: cham19, cnsinr
!     ------------------------------------------------------------------
!
    call jemarq()
!
    cham19 = chamgd
    typrez = typres(1:1)
!
    call wkvect('&&UTEST4_CMP', 'V V I', nbcmp, jcmp)
!
!     -- LE CHAMP EXISTE-T-IL ?
!     =========================
    call dismoi('C', 'TYPE_CHAMP', cham19, 'CHAMP', ibid,&
                tych, iret1)
!
    call assert(nbcmp.eq.1)
!
    if (tych(1:4) .eq. 'NOEU') then
!         -------------------
        cnsinr = '&&UTEST4.CNSINR'
        call cnocns(cham19, 'V', cnsinr)
        call jeveuo(cnsinr//'.CNSV', 'L', jcsv)
        call jeveuo(cnsinr//'.CNSC', 'L', jcsc)
        call jeveuo(cnsinr//'.CNSL', 'L', jcsl)
        call jeveuo(cnsinr//'.CNSD', 'L', jcsd)
        ncmp = zi(jcsd-1+2)
        do 10 i = 1, nbcmp
            noddl = nocmp(i)
            do 12 j = 1, ncmp
                if (zk8(jcsc-1+j) .eq. noddl) then
                    zi(jcmp-1+i) = j
                    goto 10
                endif
12          continue
            call u2mesk('F', 'CALCULEL6_88', 1, noddl)
10      continue
        call jelira(cnsinr//'.CNSV', 'TYPE', ibid, type)
        call jelira(cnsinr//'.CNSV', 'LONMAX', neq, type)
        neq = neq / ncmp
        if (type(1:1) .ne. typrez) then
            write(ific,*) 'NOOK '
            valk(1) = cham19
            valk(2) = type
            valk(3) = typrez
            call u2mesk('A', 'CALCULEL5_13', 3, valk)
            goto 9999
        endif
!
    else if (tych(1:2).eq.'EL') then
!              -----------------
        cnsinr = '&&UTEST4.CNSINR'
        call celces(cham19, 'V', cnsinr)
        call jeveuo(cnsinr//'.CESV', 'L', jcsv)
        call jeveuo(cnsinr//'.CESC', 'L', jcsc)
        call jeveuo(cnsinr//'.CESL', 'L', jcsl)
        call jeveuo(cnsinr//'.CESD', 'L', jcsd)
        ncmp = zi(jcsd-1+2)
        do 20 i = 1, nbcmp
            noddl = nocmp(i)
            do 22 j = 1, ncmp
                if (zk8(jcsc-1+j) .eq. noddl) then
                    zi(jcmp-1+i) = j
                    goto 20
                endif
22          continue
            call u2mesk('F', 'CALCULEL6_88', 1, noddl)
20      continue
        call jelira(cnsinr//'.CESV', 'TYPE', ibid, type)
        call jelira(cnsinr//'.CESV', 'LONMAX', neq, type)
        neq = neq / ncmp
        if (type(1:1) .ne. typrez) then
            write(ific,*) 'NOOK '
            valk(1) = cham19
            valk(2) = type
            valk(3) = typrez
            call u2mesk('A', 'CALCULEL5_13', 3, valk)
            goto 9999
        endif
    else
        write(ific,*) 'NOOK '
        call u2mesk('A', 'CALCULEL5_14', 1, cham19)
    endif
!
    nl1 = lxlgut(lign1)
    lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
    lign1(nl1+17:nl1+17)='.'
!
    if (type .eq. 'I') then
        if (typtes .eq. 'SOMM_ABS') then
            vali = 0
            do 102 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 100 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        vali = vali + abs( zi(jcsv-1+ind) )
                    endif
100              continue
102          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'SOMM') then
            vali = 0
            do 112 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 110 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        vali = vali + zi(jcsv-1+ind)
                    endif
110              continue
112          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'MAX') then
            do 122 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 120 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = zi(jcsv-1+ind)
                        goto 124
                    endif
120              continue
124              continue
                do 126 k = j+1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = max( valii , zi(jcsv-1+ind) )
                    endif
126              continue
                if (i .eq. 1) then
                    vali=valii
                    icmp=1
                else
                    if (valii .gt. vali) then
                        vali=valii
                        icmp=i
                    endif
                endif
122          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'MIN') then
            do 132 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 130 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = zi(jcsv-1+ind)
                        goto 134
                    endif
130              continue
134              continue
                do 136 k = j+1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = min( valii , zi(jcsv-1+ind) )
                        icmp=vnocmp
                    endif
136              continue
                if (i .eq. 1) then
                    vali=valii
                    icmp=1
                else
                    if (valii .lt. vali) then
                        vali=valii
                        icmp=i
                    endif
                endif
132          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
        else
            write(ific,*) 'NOOK '
            call u2mess('A', 'CALCULEL5_12')
            goto 9999
        endif
!
    else if (type .eq. 'R') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do 202 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 200 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valr = valr + abs( zr(jcsv-1+ind) )
                    endif
200              continue
202          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else if (typtes .eq. 'SOMM') then
            valr = 0.d0
            do 212 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 210 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valr = valr + zr(jcsv-1+ind)
                    endif
210              continue
212          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else if (typtes .eq. 'MAX') then
            do 222 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 220 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valrr = zr(jcsv-1+ind)
                        goto 224
                    endif
220              continue
224              continue
                do 226 k = j+1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valrr = max( valrr , zr(jcsv-1+ind) )
                    endif
226              continue
                if (i .eq. 1) then
                    valr=valrr
                    icmp=1
                else
                    if (valrr .gt. valr) then
                        valr=valrr
                        icmp=i
                    endif
                endif
222          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'MIN') then
            do 232 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 230 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valrr = zr(jcsv-1+ind)
                        goto 234
                    endif
230              continue
234              continue
                do 236 k = j+1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valrr = min( valrr , zr(jcsv-1+ind) )
                    endif
236              continue
                if (i .eq. 1) then
                    valr=valrr
                    icmp=1
                else
                    if (valrr .lt. valr) then
                        valr=valrr
                        icmp=i
                    endif
                endif
232          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
        else
            write(ific,*) 'NOOK '
            call u2mess('A', 'CALCULEL5_12')
            goto 9999
        endif
!
    else if (type .eq. 'C') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do 302 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 300 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valr = valr + abs( zc(jcsv-1+ind) )
                    endif
300              continue
302          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else if (typtes .eq. 'SOMM') then
            valc = dcmplx(0.d0,0.d0)
            do 312 i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do 310 j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valc = valc + zc(jcsv-1+ind)
                    endif
310              continue
312          continue
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else
            write(ific,*) 'NOOK '
            call u2mess('A', 'CALCULEL5_12')
            goto 9999
        endif
    endif
!
    nl1 = lxlgut(lign1)
    nl11 = lxlgut(lign1(1:nl1-1))
    nl2 = lxlgut(lign2)
    nl22 = lxlgut(lign2(1:nl2-1))
!
    if (llab) then
        if (nl11 .lt. 80) then
            write (ific,*) lign1(1:nl11)
        else if (nl11.lt.160) then
            write (ific,1160) lign1(1:80),lign1(81:nl11)
        else
            write (ific,1200) lign1(1:80),lign1(81:160),lign1(161:&
            nl11)
        endif
!
        if (nl22 .lt. 80) then
            write (ific,*) lign2(1:nl22)
        else if (nl22.lt.160) then
            write (ific,1160) lign2(1:80),lign2(81:nl22)
        else
            write (ific,1200) lign2(1:80),lign2(81:160),lign2(161:&
            nl22)
        endif
    endif
!
    call utites(tbtxt(1), tbtxt(2), typres, nbref, refi,&
                refr, refc, vali, valr, valc,&
                epsi, crit, ific, llab, ssigne)
!
    call detrsd('CHAM_NO_S', cnsinr)
9999  continue
    call jedetr('&&UTEST4_CMP')
!
    1160 format(1x,a80,a)
    1200 format(1x,2(a80),a)
!
    call jedema()
end subroutine

subroutine trresu(ific, nocc)
! aslint: disable=W1501
    implicit   none
    include 'jeveux.h'
    include 'asterc/getvc8.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/trprec.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utcmp1.h'
    include 'asterfort/utest1.h'
    include 'asterfort/utest2.h'
    include 'asterfort/utest3.h'
    include 'asterfort/utest4.h'
    include 'asterfort/utestr.h'
    include 'asterfort/utites.h'
    include 'asterfort/utnono.h'
    include 'asterfort/wkvect.h'
    integer :: ific, nocc
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     COMMANDE:  TEST_RESU
!                MOT CLE FACTEUR "RESU"
! ----------------------------------------------------------------------
!
!
    integer :: vali, ibid, ie, iocc, iret, ivari, jlue, jordr, n1, n2, n3, n4
    integer :: nbordr, numord, nupo, nbcmp, jcmp
    integer :: n1r, n2r, n3r, irefrr, irefir, irefcr
    integer :: nusp, irefr, irefi, irefc, nref, nl1, nl2, nl11, nl22
    real(kind=8) :: valr, epsi, epsir, prec, r8b
    complex(kind=8) :: valc, c16b
    character(len=1) :: typres
    character(len=3) :: ssigne
    character(len=4) :: typch, testok, chpt
    character(len=8) :: crit, crit2, nomail, noddl, nomma
    character(len=8) :: noresu, typtes, nomgd
    character(len=8) :: leresu
    character(len=11) :: motcle
    character(len=16) :: nopara, k16b, tbtxt(2), tbref(2)
    character(len=19) :: cham19, knum
    character(len=33) :: nonoeu
    character(len=24) :: travr, travi, travc, travrr, travir, travcr, nogrno
    character(len=33) :: titres, valk(3)
    character(len=200) :: lign1, lign2
    integer :: iarg
    logical :: lref
!     NONOEU= NOM_NOEUD (K8) SUIVI EVENTUELLEMENT DU NOM DU GROUP_NO
!             A PARTIR DUQUEL ON TROUVE LE NOM DU NOEUD.
!     ------------------------------------------------------------------
    call jemarq()
!
    motcle = 'RESULTAT'
    travr = '&&TRRESU_TRAVR'
    travi = '&&TRRESU_TRAVI'
    travc = '&&TRRESU_TRAVC'
    travrr = '&&TRRESU_TRAVR_R'
    travir = '&&TRRESU_TRAVI_R'
    travcr = '&&TRRESU_TRAVC_R'
    irefi=1
    irefr=1
    irefc=1
    irefir=1
    irefcr=1
    irefrr=1
    do 70 iocc = 1, nocc
        noddl = ' '
        testok = 'NOOK'
!
        call getvtx('RESU', 'NOM_CMP', iocc, iarg, 1,&
                    noddl, n1)
        call getvid('RESU', 'RESULTAT', iocc, iarg, 1,&
                    noresu, n1)
!
        call trprec('RESU', iocc, epsi, crit, prec,&
                    crit2)
!
        call getvtx('RESU', 'VALE_ABS', iocc, iarg, 1,&
                    ssigne, n1)
!
        call getvr8('RESU', 'VALE_CALC', iocc, iarg, 0,&
                    r8b, n1)
        call getvis('RESU', 'VALE_CALC_I', iocc, iarg, 0,&
                    ibid, n2)
        call getvc8('RESU', 'VALE_CALC_C', iocc, iarg, 0,&
                    c16b, n3)
        if (n1 .ne. 0) then
            nref=-n1
            typres = 'R'
            call jedetr(travr)
            call wkvect(travr, 'V V R', nref, irefr)
            call getvr8('RESU', 'VALE_CALC', iocc, iarg, nref,&
                        zr(irefr), iret)
        else if (n2 .ne. 0) then
            nref=-n2
            typres = 'I'
            call jedetr(travi)
            call wkvect(travi, 'V V I', nref, irefi)
            call getvis('RESU', 'VALE_CALC_I', iocc, iarg, nref,&
                        zi( irefi), iret)
        else if (n3 .ne. 0) then
            nref=-n3
            typres = 'C'
            call jedetr(travc)
            call wkvect(travc, 'V V C', nref, irefc)
            call getvc8('RESU', 'VALE_CALC_C', iocc, iarg, nref,&
                        zc( irefc), iret)
        endif
! ----------------------------------------------------------------------
        lref=.false.
        call getvr8('RESU', 'PRECISION', iocc, iarg, 1,&
                    epsir, iret)
        if (iret .ne. 0) then
            lref=.true.
            call getvr8('RESU', 'VALE_REFE', iocc, iarg, 0,&
                        r8b, n1r)
            call getvis('RESU', 'VALE_REFE_I', iocc, iarg, 0,&
                        ibid, n2r)
            call getvc8('RESU', 'VALE_REFE_C', iocc, iarg, 0,&
                        c16b, n3r)
            if (n1r .ne. 0) then
                call assert((n1r.eq.n1))
                nref=-n1r
                call jedetr(travrr)
                call wkvect(travrr, 'V V R', nref, irefrr)
                call getvr8('RESU', 'VALE_REFE', iocc, iarg, nref,&
                            zr(irefrr), iret)
            else if (n2r.ne.0) then
                call assert((n2r.eq.n2))
                nref=-n2r
                call jedetr(travir)
                call wkvect(travir, 'V V I', nref, irefir)
                call getvis('RESU', 'VALE_REFE_I', iocc, iarg, nref,&
                            zi(irefir), iret)
            else if (n3r.ne.0) then
                call assert((n3r.eq.n3))
                nref=-n3r
                call jedetr(travcr)
                call wkvect(travcr, 'V V C', nref, irefcr)
                call getvc8('RESU', 'VALE_REFE_C', iocc, iarg, nref,&
                            zc(irefcr), iret)
            endif
        endif
! ----------------------------------------------------------------------
!
        lign1 = ' '
        lign2 = ' '
        leresu = noresu
        titres = ' '
!
        knum = '&&TRRESU.NUME_ORDRE'
        call rsutnu(leresu, 'RESU', iocc, knum, nbordr,&
                    prec, crit2, iret)
        if (iret .ne. 0) call u2mess('F', 'CALCULEL6_94')
!
        call jeveuo(knum, 'L', jordr)
        numord = zi(jordr)
!
        lign1(1:21)='---- '//motcle(1:8)
        lign1(22:22)='.'
        lign2(1:21)='     '//noresu
        lign2(22:22)='.'
        nl1 = lxlgut(lign1)
        nl2 = lxlgut(lign2)
        lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_ORDRE'
        call codent(numord, 'G', chpt)
        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//chpt
        lign1(nl1+17:nl1+17)='.'
        lign2(nl2+17:nl2+17)='.'
!
        call getvtx('RESU', 'PARA', iocc, iarg, 1,&
                    nopara, n1)
!
        if (n1 .ne. 0) then
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' PARA'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nopara
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
!
!
            call utest3('RESU', iocc, tbtxt)
!
            call rsadpa(leresu, 'L', 1, nopara, numord,&
                        1, jlue, k16b)
            if (k16b(1:1) .ne. typres) then
                call u2mess('F', 'CALCULEL6_95')
            else if (typres.eq.'R') then
                valr = zr(jlue)
            else if (typres.eq.'I') then
                vali = zi(jlue)
            else if (typres.eq.'C') then
                valc = zc(jlue)
            endif
!
            nl1 = lxlgut(lign1)
            nl11 = lxlgut(lign1(1:nl1-1))
            nl2 = lxlgut(lign2)
            nl22 = lxlgut(lign2(1:nl2-1))
            if (nl11 .lt. 80) then
                write (ific,*) lign1(1:nl11)
            else if (nl11.lt.160) then
                write (ific,1160) lign1(1:80), lign1(81:nl11)
            else
                write (ific,1200) lign1(1:80), lign1(81:160), lign1(&
                161:nl11)
            endif
            if (nl22 .lt. 80) then
                write (ific,*) lign2(1:nl22)
            else if (nl22.lt.160) then
                write (ific,1160) lign2(1:80), lign2(81:nl22)
            else
                write (ific,1200) lign2(1:80), lign2(81:160), lign2(&
                161:nl22)
            endif
!
            if (lref) then
                tbref(1)=tbtxt(1)
                tbref(2)=tbtxt(2)
                tbtxt(1)='NON_REGRESSION'
            endif
            call utites(tbtxt(1), tbtxt(2), typres, nref, zi(irefi),&
                        zr(irefr), zc(irefc), vali, valr, valc,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) then
                call utites(tbref(1), tbref(2), typres, nref, zi(irefir),&
                            zr(irefrr), zc(irefcr), vali, valr, valc,&
                            epsir, crit, ific, .false., ssigne)
            endif
        endif
!
        call getvtx('RESU', 'NOM_CHAM', iocc, iarg, 1,&
                    nopara, n1)
!
        if (n1 .ne. 0) then
!
            call rsexch('F', leresu, nopara, numord, cham19,&
                        iret)
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CHAM'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nopara
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
!
            call utest3('RESU', iocc, tbtxt)
!
            call getvtx('RESU', 'TYPE_TEST', iocc, iarg, 1,&
                        typtes, n1)
!
            if (n1 .ne. 0) then
!
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' TYPE_TEST'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//typtes
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
!
!
!
                call getvtx('RESU', 'NOM_CMP', iocc, iarg, 0,&
                            noddl, n4)
!
                if (n4 .eq. 0) then
                    nl1 = lxlgut(lign1)
                    nl11 = lxlgut(lign1(1:nl1-1))
                    nl2 = lxlgut(lign2)
                    nl22 = lxlgut(lign2(1:nl2-1))
                    if (nl11 .lt. 80) then
                        write (ific,*) lign1(1:nl11)
                    else if (nl11.lt.160) then
                        write (ific,1160) lign1(1:80),lign1(81:nl11)
                    else
                        write (ific,1200) lign1(1:80),lign1(81:160),&
                        lign1(161:nl11)
                    endif
                    if (nl22 .lt. 80) then
                        write (ific,*) lign2(1:nl22)
                    else if (nl22.lt.160) then
                        write (ific,1160) lign2(1:80),lign2(81:nl22)
                    else
                        write (ific,1200) lign2(1:80),lign2(81:160),&
                        lign2(161:nl22)
                    endif
!
                    if (lref) then
                        tbref(1)=tbtxt(1)
                        tbref(2)=tbtxt(2)
                        tbtxt(1)='NON_REGRESSION'
                    endif
                    call utest1(cham19, typtes, typres, nref, tbtxt,&
                                zi( irefi), zr(irefr), zc(irefc), epsi, crit,&
                                ific, .true., ssigne)
                    if (lref) then
                        call utest1(cham19, typtes, typres, nref, tbref,&
                                    zi(irefir), zr(irefrr), zc(irefcr), epsi, crit,&
                                    ific, .false., ssigne)
                    endif
                else
                    nbcmp = -n4
                    call wkvect('&&TRRESU.NOM_CMP', 'V V K8', nbcmp, jcmp)
                    call getvtx('RESU', 'NOM_CMP', iocc, iarg, nbcmp,&
                                zk8(jcmp), n4)
                    if (lref) then
                        tbref(1)=tbtxt(1)
                        tbref(2)=tbtxt(2)
                        tbtxt(1)='NON_REGRESSION'
                    endif
                    call utest4(cham19, typtes, typres, nref, tbtxt,&
                                zi( irefi), zr(irefr), zc(irefc), epsi, lign1,&
                                lign2, crit, ific, nbcmp, zk8(jcmp),&
                                .true., ssigne)
                    if (lref) then
                        call utest4(cham19, typtes, typres, nref, tbref,&
                                    zi(irefir), zr(irefrr), zc(irefcr), epsir, lign1,&
                                    lign2, crit, ific, nbcmp, zk8(jcmp),&
                                    .false., ssigne)
                    endif
                    call jedetr('&&TRRESU.NOM_CMP')
                endif
!
            else
                call getvtx('RESU', 'NOM_CMP', iocc, iarg, 1,&
                            noddl, n1)
!
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//noddl
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
!
!
                nonoeu = ' '
                call dismoi('F', 'NOM_MAILLA', cham19, 'CHAMP', ibid,&
                            nomma, ie)
                call getvem(nomma, 'NOEUD', 'RESU', 'NOEUD', iocc,&
                            iarg, 1, nonoeu(1:8), n1)
                if (n1 .ne. 0) then
                    nl1 = lxlgut(lign1)
                    nl2 = lxlgut(lign2)
                    lign1(1:nl1+16)=lign1(1:nl1-1)//' NOEUD'
                    lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nonoeu(1:8)
                    lign1(nl1+17:nl1+17)='.'
                    lign2(nl2+17:nl2+17)='.'
                endif
!
                call getvem(nomma, 'GROUP_NO', 'RESU', 'GROUP_NO', iocc,&
                            iarg, 1, nogrno, n2)
                if (n2 .ne. 0) then
                    nl1 = lxlgut(lign1)
                    nl2 = lxlgut(lign2)
                    lign1(1:nl1+16)=lign1(1:nl1-1)//' GROUP_NO'
                    lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nogrno
                    lign1(nl1+17:nl1+17)='.'
                    lign2(nl2+17:nl2+17)='.'
                endif
!
                if (n1 .ne. 0) then
!              RIEN A FAIRE.
                else if (n2.ne.0) then
                    call utnono('A', nomma, 'NOEUD', nogrno, nonoeu(1:8),&
                                iret)
                    if (iret .ne. 0) then
                        write (ific,*) testok
                        goto 50
                    endif
                    nonoeu(10:33) = nogrno
                endif
                call dismoi('F', 'TYPE_CHAMP', cham19, 'CHAMP', ibid,&
                            typch, ie)
                call dismoi('F', 'NOM_MAILLA', cham19, 'CHAMP', ibid,&
                            nomma, ie)
                call dismoi('F', 'NOM_GD', cham19, 'CHAMP', ibid,&
                            nomgd, ie)
                call utcmp1(nomgd, 'RESU', iocc, noddl, ivari)
                call getvis('RESU', 'SOUS_POINT', iocc, iarg, 1,&
                            nusp, n2)
                if (n2 .eq. 0) nusp = 0
                nupo=0
                call getvis('RESU', 'POINT', iocc, iarg, 1,&
                            nupo, n2)
                if (typch .eq. 'NOEU') then
                    if (n2 .ne. 0) then
                        valk(1) = noresu
                        valk(2) = nopara
                        valk(3) = titres
                        call u2mesg('F', 'CALCULEL6_97', 3, valk, 1,&
                                    numord, 0, 0.d0)
                    endif
!
                    nl1 = lxlgut(lign1)
                    nl11 = lxlgut(lign1(1:nl1-1))
                    nl2 = lxlgut(lign2)
                    nl22 = lxlgut(lign2(1:nl2-1))
!
                    if (nl11 .lt. 80) then
                        write (ific,*) lign1(1:nl11)
                    else if (nl11.lt.160) then
                        write (ific,1160) lign1(1:80), lign1(81:nl11)
                    else
                        write (ific,1200) lign1(1:80), lign1(81:160),&
                        lign1(161:nl11)
                    endif
                    if (nl22 .lt. 80) then
                        write (ific,*) lign2(1:nl22)
                    else if (nl22.lt.160) then
                        write (ific,1160) lign2(1:80), lign2(81:nl22)
                    else
                        write (ific,1200) lign2(1:80), lign2(81:160),&
                        lign2(161:nl22)
                    endif
!
                    if (lref) then
                        tbref(1)=tbtxt(1)
                        tbref(2)=tbtxt(2)
                        tbtxt(1)='NON_REGRESSION'
                    endif
                    call utestr(cham19, nonoeu, noddl, nref, tbtxt,&
                                zi( irefi), zr(irefr), zc(irefc), typres, epsi,&
                                crit, ific, .true., ssigne)
                    if (lref) then
                        call utestr(cham19, nonoeu, noddl, nref, tbref,&
                                    zi(irefir), zr(irefrr), zc(irefcr), typres, epsir,&
                                    crit, ific, .false., ssigne)
                    endif
                else if (typch(1:2).eq.'EL') then
                    call getvem(nomma, 'MAILLE', 'RESU', 'MAILLE', iocc,&
                                iarg, 1, nomail, n1)
                    if (n1 .eq. 0) then
                        call u2mess('F', 'CALCULEL5_8')
                    endif
!
                    nl1 = lxlgut(lign1)
                    nl2 = lxlgut(lign2)
                    lign1(1:nl1+16)=lign1(1:nl1-1)//' MAILLE'
                    lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nomail
                    lign1(nl1+17:nl1+17)='.'
                    lign2(nl2+17:nl2+17)='.'
!
                    nl1 = lxlgut(lign1)
                    nl2 = lxlgut(lign2)
                    lign1(1:nl1+16)=lign1(1:nl1-1)//' POINT'
                    call codent(nupo, 'G', chpt)
                    lign2(1:nl2+16)=lign2(1:nl2-1)//' '//chpt
                    lign1(nl1+17:nl1+17)='.'
                    lign2(nl2+17:nl2+17)='.'
!
                    if (nusp .ne. 0) then
                        nl1 = lxlgut(lign1)
                        nl2 = lxlgut(lign2)
                        lign1(1:nl1+16)=lign1(1:nl1-1)//' SOUS_POINT'
                        call codent(nusp, 'G', chpt)
                        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//chpt
                        lign1(nl1+17:nl1+17)='.'
                        lign2(nl2+17:nl2+17)='.'
                    endif
!
!
                    nl1 = lxlgut(lign1)
                    nl11 = lxlgut(lign1(1:nl1-1))
                    nl2 = lxlgut(lign2)
                    nl22 = lxlgut(lign2(1:nl2-1))
                    if (nl11 .lt. 80) then
                        write (ific,*) lign1(1:nl11)
                    else if (nl11.lt.160) then
                        write (ific,1160) lign1(1:80), lign1(81:nl11)
                    else
                        write (ific,1200) lign1(1:80), lign1(81:160),&
                        lign1(161:nl11)
                    endif
                    if (nl22 .lt. 80) then
                        write (ific,*) lign2(1:nl22)
                    else if (nl22.lt.160) then
                        write (ific,1160) lign2(1:80), lign2(81:nl22)
                    else
                        write (ific,1200) lign2(1:80), lign2(81:160),&
                        lign2(161:nl22)
                    endif
!
                    if (lref) then
                        tbref(1)=tbtxt(1)
                        tbref(2)=tbtxt(2)
                        tbtxt(1)='NON_REGRESSION'
                    endif
                    call utest2(cham19, nomail, nonoeu, nupo, nusp,&
                                ivari, noddl, nref, tbtxt, zi(irefi),&
                                zr(irefr), zc(irefc), typres, epsi, crit,&
                                ific, .true., ssigne)
                    if (lref) then
                        call utest2(cham19, nomail, nonoeu, nupo, nusp,&
                                    ivari, noddl, nref, tbref, zi(irefir),&
                                    zr(irefrr), zc(irefcr), typres, epsir, crit,&
                                    ific, .false., ssigne)
                    endif
                endif
            endif
        endif
50      continue
        call jedetr(knum)
        write (ific,*)' '
70  end do
!
    1160 format(1x,a80,a)
    1200 format(1x,2(a80),a)
!
    call jedetr(travr)
    call jedetr(travc)
    call jedetr(travi)
!
    call jedema()
end subroutine

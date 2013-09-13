subroutine trgene(ific, nocc)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/codree.h"
#include "asterfort/extrac.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/trprec.h"
#include "asterfort/utest3.h"
#include "asterfort/utites.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zxtrac.h"
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
!     COMMANDE:  TEST_RESU      MOT CLE FACTEUR "GENE"
! ----------------------------------------------------------------------
    character(len=6) :: nompro
    parameter (nompro='TRGENE')
    integer :: vali, iocc, iret, jlue, jordr, jdesc, jrefe, n1, n2, n3
    integer :: nbordr, numord, ncmp, nbinst, im, jinst, jcham, nbmode, jvecg
    integer :: jnume, jdeeq, istru, i, irefr, irefi, irefc, nref, nl1, nl2
    integer :: jfreq, nbfreq
    integer :: n1r, n2r, n3r, irefrr, irefir, irefcr
    real(kind=8) :: valr, epsi, epsir, prec, temps, freq
    complex(kind=8) :: valc
    character(len=1) :: typres
    character(len=3) :: ssigne
    character(len=4) :: ch4
    character(len=8) :: crit, crit2, interp, mode
    character(len=11) :: motcle
    character(len=14) :: nugene
    character(len=16) :: nopara, nsym, k16b, tysd, ch16, tbtxt(2), tbref(2)
    character(len=19) :: cham19, knum, resu19
    character(len=24) :: travr, travi, travc, travrr, travir, travcr
    character(len=200) :: lign1, lign2
    logical :: lref
!     ------------------------------------------------------------------
    call jemarq()
    motcle = 'RESU_GENE'
    travr = '&&'//nompro//'_TRAVR          '
    travi = '&&'//nompro//'_TRAVI          '
    travc = '&&'//nompro//'_TRAVC          '
    travrr = '&&'//nompro//'_TRAVR_R        '
    travir = '&&'//nompro//'_TRAVI_R        '
    travcr = '&&'//nompro//'_TRAVC_R        '
    irefi=1
    irefr=1
    irefc=1
    irefir=1
    irefcr=1
    irefrr=1
!
    do 100 iocc = 1, nocc
        lign1 = ' '
        lign2 = ' '
!
        call trprec('GENE', iocc, epsi, crit, prec,&
                    crit2)
!
        call getvtx('GENE', 'VALE_ABS', iocc=iocc, scal=ssigne, nbret=n1)
!
        call getvr8('GENE', 'VALE_CALC', iocc=iocc, nbval=0, nbret=n1)
        call getvis('GENE', 'VALE_CALC_I', iocc=iocc, nbval=0, nbret=n2)
        call getvc8('GENE', 'VALE_CALC_C', iocc=iocc, nbval=0, nbret=n3)
        if (n1 .ne. 0) then
            nref=-n1
            typres = 'R'
            call jedetr(travr)
            call wkvect(travr, 'V V R', nref, irefr)
            call getvr8('GENE', 'VALE_CALC', iocc=iocc, nbval=nref, vect=zr(irefr),&
                        nbret=iret)
        else if (n2 .ne. 0) then
            nref=-n2
            typres = 'I'
            call jedetr(travi)
            call wkvect(travi, 'V V I', nref, irefi)
            call getvis('GENE', 'VALE_CALC_I', iocc=iocc, nbval=nref, vect=zi( irefi),&
                        nbret=iret)
        else if (n3 .ne. 0) then
            nref=-n3
            typres = 'C'
            call jedetr(travc)
            call wkvect(travc, 'V V C', nref, irefc)
            call getvc8('GENE', 'VALE_CALC_C', iocc=iocc, nbval=nref, vect=zc( irefc),&
                        nbret=iret)
        endif
! ----------------------------------------------------------------------
        lref=.false.
        call getvr8('GENE', 'PRECISION', iocc=iocc, scal=epsir, nbret=iret)
        if (iret .ne. 0) then
            lref=.true.
            call getvr8('GENE', 'VALE_REFE', iocc=iocc, nbval=0, nbret=n1r)
            call getvis('GENE', 'VALE_REFE_I', iocc=iocc, nbval=0, nbret=n2r)
            call getvc8('GENE', 'VALE_REFE_C', iocc=iocc, nbval=0, nbret=n3r)
            if (n1r .ne. 0) then
                ASSERT((n1r.eq.n1))
                nref=-n1r
                call jedetr(travrr)
                call wkvect(travrr, 'V V R', nref, irefrr)
                call getvr8('GENE', 'VALE_REFE', iocc=iocc, nbval=nref, vect=zr(irefrr),&
                            nbret=iret)
            else if (n2r.ne.0) then
                ASSERT((n2r.eq.n2))
                nref=-n2r
                call jedetr(travir)
                call wkvect(travir, 'V V I', nref, irefir)
                call getvis('GENE', 'VALE_REFE_I', iocc=iocc, nbval=nref, vect=zi(irefir),&
                            nbret=iret)
            else if (n3r.ne.0) then
                ASSERT((n3r.eq.n3))
                nref=-n3r
                call jedetr(travcr)
                call wkvect(travcr, 'V V C', nref, irefcr)
                call getvc8('GENE', 'VALE_REFE_C', iocc=iocc, nbval=nref, vect=zc(irefcr),&
                            nbret=iret)
            endif
        endif
! ----------------------------------------------------------------------
!
        call getvid('GENE', 'RESU_GENE', iocc=iocc, scal=resu19, nbret=n1)
        call gettco(resu19, tysd)
! ----------------------------------------------------------------------
        if (tysd .eq. 'VECT_ASSE_GENE') then
            call getvis('GENE', 'NUME_CMP_GENE', iocc=iocc, scal=ncmp, nbret=n1)
            call jeveuo(resu19//'.VALE', 'L', jlue)
            call jelira(resu19//'.VALE', 'TYPE', cval=k16b)
!
            call jeveuo(resu19//'.REFE', 'L', jrefe)
            mode = zk24(jrefe)(1:8)
            if (mode .eq. '        ') then
                nugene = zk24(jrefe+1)(1:14)
                call jeveuo(nugene//'.NUME.DEEQ', 'L', jdeeq)
                call jeveuo(nugene//'.NUME.NEQU', 'L', jnume)
                nbmode = zi(jnume)
                im = 0
                do 110 i = 1, nbmode
                    istru = zi(jdeeq+2*(i-1)+2-1)
                    if (istru .lt. 0) goto 110
                    im = im + 1
                    if (im .eq. ncmp) goto 114
110              continue
                call utmess('F', 'CALCULEL6_98')
114              continue
                im = i
            else
                im = ncmp
            endif
!
            if (k16b(1:1) .ne. typres) then
                call utmess('F', 'CALCULEL6_95')
            else if (typres.eq.'R') then
                valr = zr(jlue+im-1)
            else if (typres.eq.'I') then
                vali = zi(jlue+im-1)
            else if (typres.eq.'C') then
                valc = zc(jlue+im-1)
            endif
!
            lign1(1:21)='---- '//motcle(1:9)
            lign1(22:22)='.'
            lign2(1:21)='     '//resu19(1:8)
            lign2(22:22)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_ORDRE'
            ch4=' '
            call codent(numord, 'G', ch4)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_CMP_GENE'
            ch4=' '
            call codent(ncmp, 'G', ch4)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            if (nl1 .lt. 80) then
                write (ific,*) lign1(1:nl1)
            else if (nl1.lt.160) then
                write (ific,1160) lign1(1:80), lign1(81:nl1)
            else
                write (ific,1200) lign1(1:80), lign1(81:160), lign1(&
                161:nl1)
            endif
            if (nl2 .lt. 80) then
                write (ific,*) lign2(1:nl2)
            else if (nl2.lt.160) then
                write (ific,1160) lign2(1:80), lign2(81:nl2)
            else
                write (ific,1200) lign2(1:80), lign2(81:160), lign2(&
                161:nl2)
            endif
!
            call utest3('GENE', iocc, tbtxt)
!
            if (lref) then
                tbref(1)=tbtxt(1)
                tbref(2)=tbtxt(2)
                tbtxt(1)='NON_REGRESSION'
            endif
            call utites(tbtxt(1), tbtxt(2), typres, nref, zi(irefi),&
                        zr( irefr), zc(irefc), vali, valr, valc,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) then
                call utites(tbref(1), tbref(2), typres, nref, zi(irefir),&
                            zr(irefrr), zc(irefcr), vali, valr, valc,&
                            epsir, crit, ific, .false., ssigne)
            endif
!
        else if (tysd .eq. 'MODE_GENE') then
!
            knum = '&&TRGENE.NUME_ORDRE'
            call rsutnu(resu19, 'GENE', iocc, knum, nbordr,&
                        prec, crit2, iret)
            if (iret .ne. 0) then
                call utmess('F', 'CALCULEL6_99', sk=resu19)
            endif
!
            call jeveuo(knum, 'L', jordr)
            numord = zi(jordr)
!
            call getvtx('GENE', 'PARA', iocc=iocc, scal=nopara, nbret=n1)
            if (n1 .ne. 0) then
                call rsadpa(resu19, 'L', 1, nopara, numord,&
                            1, jlue, k16b)
                if (k16b(1:1) .ne. typres) then
                    call utmess('F', 'CALCULEL6_95')
                else if (typres.eq.'R') then
                    valr = zr(jlue)
                else if (typres.eq.'I') then
                    vali = zi(jlue)
                else if (typres.eq.'C') then
                    valc = zc(jlue)
                endif
!
                lign1(1:21)='---- '//motcle(1:9)
                lign1(22:22)='.'
                lign2(1:21)='     '//resu19(1:8)
                lign2(22:22)='.'
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_ORDRE'
                ch4=' '
                call codent(numord, 'G', ch4)
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' PARA'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nopara(1:16)
!
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                if (nl1 .lt. 80) then
                    write (ific,*) lign1(1:nl1)
                else if (nl1.lt.160) then
                    write (ific,1160) lign1(1:80), lign1(81:nl1)
                else
                    write (ific,1200) lign1(1:80), lign1(81:160),&
                    lign1(161:nl1)
                endif
                if (nl2 .lt. 80) then
                    write (ific,*) lign2(1:nl2)
                else if (nl2.lt.160) then
                    write (ific,1160) lign2(1:80), lign2(81:nl2)
                else
                    write (ific,1200) lign2(1:80), lign2(81:160),&
                    lign2(161:nl2)
                endif
!
                call utest3('GENE', iocc, tbtxt)
!
                if (lref) then
                    tbref(1)=tbtxt(1)
                    tbref(2)=tbtxt(2)
                    tbtxt(1)='NON_REGRESSION'
                endif
                call utites(tbtxt(1), tbtxt(2), typres, nref, zi(irefi),&
                            zr(irefr), zc(irefc), vali, valr, valc,&
                            epsi, crit, ific, .true., ssigne)
                if (lref) call utites(tbref(1), tbref(2), typres, nref, zi(irefir),&
                                      zr(irefrr), zc(irefcr), vali, valr, valc,&
                                      epsir, crit, ific, .false., ssigne)
                call jedetr(knum)
                goto 100
            endif
!
            call getvtx('GENE', 'NOM_CHAM', iocc=iocc, scal=nsym, nbret=n1)
            call getvis('GENE', 'NUME_CMP_GENE', iocc=iocc, scal=ncmp, nbret=n1)
            call rsexch('F', resu19, nsym, numord, cham19,&
                        iret)
            call jeveuo(cham19//'.VALE_CALC', 'L', jlue)
            call jelira(cham19//'.VALE_CALC', 'TYPE', cval=k16b)
!
            call jeveuo(cham19//'.REFE', 'L', jrefe)
            mode = zk24(jrefe)(1:8)
            if (mode .eq. '        ') then
                nugene = zk24(jrefe+1)(1:14)
                call jeveuo(nugene//'.NUME.DEEQ', 'L', jdeeq)
                call jeveuo(nugene//'.NUME.NEQU', 'L', jnume)
                nbmode = zi(jnume)
                im = 0
                do 120 i = 1, nbmode
                    istru = zi(jdeeq+2*(i-1)+2-1)
                    if (istru .lt. 0) goto 120
                    im = im + 1
                    if (im .eq. ncmp) goto 124
120              continue
                call utmess('F', 'CALCULEL6_98')
                goto 100
124              continue
                im = i
            else
                im = ncmp
            endif
!
            if (k16b(1:1) .ne. typres) then
                call utmess('F', 'CALCULEL6_95')
            else if (typres.eq.'R') then
                valr = zr(jlue+im-1)
            else if (typres.eq.'I') then
                vali = zi(jlue+im-1)
            else if (typres.eq.'C') then
                valc = zc(jlue+im-1)
            endif
!
            lign1(1:21)='---- '//motcle(1:9)
            lign1(22:22)='.'
            lign2(1:21)='     '//resu19(1:8)
            lign2(22:22)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_ORDRE'
            ch4=' '
            call codent(numord, 'G', ch4)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CHAM'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nsym
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_CMP_GENE'
            ch4=' '
            call codent(ncmp, 'G', ch4)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            if (nl1 .lt. 80) then
                write (ific,*) lign1(1:nl1)
            else if (nl1.lt.160) then
                write (ific,1160) lign1(1:80), lign1(81:nl1)
            else
                write (ific,1200) lign1(1:80), lign1(81:160), lign1(&
                161:nl1)
            endif
            if (nl2 .lt. 80) then
                write (ific,*) lign2(1:nl2)
            else if (nl2.lt.160) then
                write (ific,1160) lign2(1:80), lign2(81:nl2)
            else
                write (ific,1200) lign2(1:80), lign2(81:160), lign2(&
                161:nl2)
            endif
!
            call utest3('GENE', iocc, tbtxt)
!
            if (lref) then
                tbref(1)=tbtxt(1)
                tbref(2)=tbtxt(2)
                tbtxt(1)='NON_REGRESSION'
            endif
            call utites(tbtxt(1), tbtxt(2), typres, nref, zi(irefi),&
                        zr( irefr), zc(irefc), vali, valr, valc,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) call utites(tbref(1), tbref(2), typres, nref, zi(irefir),&
                                  zr(irefrr), zc(irefcr), vali, valr, valc,&
                                  epsir, crit, ific, .false., ssigne)
            call jedetr(knum)
!
        else if (tysd .eq. 'HARM_GENE') then
            call getvtx('GENE', 'NOM_CHAM', iocc=iocc, scal=nsym, nbret=n1)
            call getvis('GENE', 'NUME_CMP_GENE', iocc=iocc, scal=ncmp, nbret=n1)
!
            interp = 'NON'
            call jeveuo(resu19//'.DISC', 'L', jfreq)
            call jelira(resu19//'.DISC', 'LONMAX', nbfreq)
!
            call getvr8('GENE', 'FREQ', iocc=iocc, scal=freq, nbret=n1)
            if (n1 .eq. 0) then
                call getvis('GENE', 'NUME_ORDRE', iocc=iocc, scal=numord, nbret=n1)
                freq = zr(jfreq+numord-1)
            endif
!
            call jeexin(resu19//'.'//nsym(1:4), iret)
            if (iret .eq. 0) then
                call utmess('F', 'CALCULEL6_99', sk=resu19)
            endif
            call jeveuo(resu19//'.'//nsym(1:4), 'L', jcham)
            call jeveuo(resu19//'.DESC', 'L', jdesc)
            nbmode = zi(jdesc+2-1)
            call wkvect('&&TRGENE.CHAMP', 'V V C', nbmode, jvecg)
            call zxtrac(interp, prec, crit2, nbfreq, zr(jfreq),&
                        freq, zc(jcham), nbmode, zc(jvecg), iret)
            if (iret .ne. 0) then
                call utmess('F', 'CALCULEL6_2', sk=resu19)
            endif
            valc = zc(jvecg+ncmp-1)
!
            lign1(1:21)='---- '//motcle(1:9)
            lign1(22:22)='.'
            lign2(1:21)='     '//resu19(1:8)
            lign2(22:22)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' FREQ'
            ch16=' '
            call codree(freq, 'E', ch16)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch16
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CHAM'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nsym(1:4)
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_CMP_GENE'
            ch4=' '
            call codent(ncmp, 'G', ch4)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            if (nl1 .lt. 80) then
                write (ific,*) lign1(1:nl1)
            else if (nl1.lt.160) then
                write (ific,1160) lign1(1:80), lign1(81:nl1)
            else
                write (ific,1200) lign1(1:80), lign1(81:160), lign1(&
                161:nl1)
            endif
            if (nl2 .lt. 80) then
                write (ific,*) lign2(1:nl2)
            else if (nl2.lt.160) then
                write (ific,1160) lign2(1:80), lign2(81:nl2)
            else
                write (ific,1200) lign2(1:80), lign2(81:160), lign2(&
                161:nl2)
            endif
!
            call utest3('GENE', iocc, tbtxt)
            if (lref) then
                tbref(1)=tbtxt(1)
                tbref(2)=tbtxt(2)
                tbtxt(1)='NON_REGRESSION'
            endif
            call utites(tbtxt(1), tbtxt(2), 'C', nref, zi(irefi),&
                        zr(irefr), zc(irefc), vali, valr, valc,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) then
                call utites(tbref(1), tbref(2), 'C', nref, zi(irefir),&
                            zr(irefrr), zc(irefcr), vali, valr, valc,&
                            epsir, crit, ific, .false., ssigne)
            endif
            call jedetr('&&TRGENE.CHAMP')
!
        else if (tysd .eq. 'TRAN_GENE') then
            call getvtx('GENE', 'NOM_CHAM', iocc=iocc, scal=nsym, nbret=n1)
            call getvis('GENE', 'NUME_CMP_GENE', iocc=iocc, scal=ncmp, nbret=n1)
!
            interp = 'NON'
            call jeveuo(resu19//'.DISC', 'L', jinst)
            call jelira(resu19//'.DISC', 'LONMAX', nbinst)
!
            call getvr8('GENE', 'INST', iocc=iocc, scal=temps, nbret=n1)
            if (n1 .eq. 0) then
                call getvis('GENE', 'NUME_ORDRE', iocc=iocc, scal=numord, nbret=n1)
                temps = zr(jinst+numord-1)
            endif
!
            call jeexin(resu19//'.'//nsym(1:4), iret)
            if (iret .eq. 0) then
                call utmess('F', 'CALCULEL6_99', sk=resu19)
            endif
            call jeveuo(resu19//'.'//nsym(1:4), 'L', jcham)
            call jeveuo(resu19//'.DESC', 'L', jdesc)
            nbmode = zi(jdesc+2-1)
            call wkvect('&&TRGENE.CHAMP', 'V V R', nbmode, jvecg)
            call extrac(interp, prec, crit2, nbinst, zr(jinst),&
                        temps, zr(jcham), nbmode, zr(jvecg), iret)
            if (iret .ne. 0) then
                call utmess('F', 'CALCULEL6_2', sk=resu19)
            endif
            valr = zr(jvecg+ncmp-1)
!
            lign1(1:21)='---- '//motcle(1:9)
            lign1(22:22)='.'
            lign2(1:21)='     '//resu19(1:8)
            lign2(22:22)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' INST'
            ch16=' '
            call codree(temps, 'E', ch16)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch16
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CHAM'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nsym(1:4)
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NUME_CMP_GENE'
            ch4=' '
            call codent(ncmp, 'G', ch4)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ch4
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            if (nl1 .lt. 80) then
                write (ific,*) lign1(1:nl1)
            else if (nl1.lt.160) then
                write (ific,1160) lign1(1:80), lign1(81:nl1)
            else
                write (ific,1200) lign1(1:80), lign1(81:160), lign1(&
                161:nl1)
            endif
            if (nl2 .lt. 80) then
                write (ific,*) lign2(1:nl2)
            else if (nl2.lt.160) then
                write (ific,1160) lign2(1:80), lign2(81:nl2)
            else
                write (ific,1200) lign2(1:80), lign2(81:160), lign2(&
                161:nl2)
            endif
!
            call utest3('GENE', iocc, tbtxt)
            if (lref) then
                tbref(1)=tbtxt(1)
                tbref(2)=tbtxt(2)
                tbtxt(1)='NON_REGRESSION'
            endif
            call utites(tbtxt(1), tbtxt(2), 'R', nref, zi(irefi),&
                        zr(irefr), zc(irefc), vali, valr, valc,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) then
                call utites(tbref(1), tbref(2), 'R', nref, zi(irefir),&
                            zr(irefrr), zc(irefcr), vali, valr, valc,&
                            epsir, crit, ific, .false., ssigne)
            endif
            call jedetr('&&TRGENE.CHAMP')
        endif
        write (ific,*)' '
100  end do
    1160 format(1x,a80,a)
    1200 format(1x,2(a80),a)
    call jedetr(travr)
    call jedetr(travc)
    call jedetr(travi)
    call jedema()
end subroutine

subroutine trchel(ific, nocc)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utcmp1.h"
#include "asterfort/utest1.h"
#include "asterfort/utest2.h"
#include "asterfort/utest3.h"
#include "asterfort/utest4.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
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
!                MOT CLE FACTEUR "CHAM_ELEM"
! ----------------------------------------------------------------------
!
    character(len=6) :: nompro
    parameter (nompro='TRCHEL')
!
    integer :: iocc, iret, nbcmp, jcmp, n1, n2, n3, n4, ivari, nupo, nusp
    integer :: irefr, irefi, irefc, nref, nl1, nl2, nl11, nl22, n1r, n2r, n3r
    integer :: irefrr, irefir, irefcr
    real(kind=8) :: epsi, epsir
    character(len=1) :: typres
    character(len=3) :: ssigne
    character(len=4) :: testok, chpt
    character(len=8) :: crit, noddl, nomma, typtes, nomail, nomgd
    character(len=11) :: motcle
    character(len=19) :: cham19
    character(len=16) :: tbtxt(2), tbref(2)
    character(len=33) :: nonoeu
    character(len=24) :: travr, travi, travc, travrr, travir, travcr, nogrno
    character(len=200) :: lign1, lign2
    integer :: iarg
    logical :: lref
!     ------------------------------------------------------------------
    call jemarq()
!
    motcle = 'CHAM_ELEM'
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
        testok = 'NOOK'
        nonoeu = ' '
        noddl = ' '
        call getvid('CHAM_ELEM', 'CHAM_GD', iocc=iocc, scal=cham19, nbret=n1)
        lign1(1:21)='---- '//motcle(1:9)
        lign1(22:22)='.'
        lign2(1:21)='     '//cham19(1:8)
        lign2(22:22)='.'
        call utest3('CHAM_ELEM', iocc, tbtxt)
!
        call getvtx('CHAM_ELEM', 'NOM_CMP', iocc=iocc, scal=noddl, nbret=n1)
        if (n1 .ne. 0) then
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//noddl
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
        endif
!
        call getvtx('CHAM_ELEM', 'VALE_ABS', iocc=iocc, scal=ssigne, nbret=n1)
        if (ssigne .eq. 'OUI') then
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' VALE_ABS'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ssigne
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
        endif
!
        call getvr8('CHAM_ELEM', 'TOLE_MACHINE', iocc=iocc, scal=epsi, nbret=n1)
        call getvtx('CHAM_ELEM', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
!
        call getvr8('CHAM_ELEM', 'VALE_CALC', iocc=iocc, nbval=0, nbret=n1)
        call getvis('CHAM_ELEM', 'VALE_CALC_I', iocc=iocc, nbval=0, nbret=n2)
        call getvc8('CHAM_ELEM', 'VALE_CALC_C', iocc=iocc, nbval=0, nbret=n3)
!
        if (n1 .ne. 0) then
            nref=-n1
            typres = 'R'
            call jedetr(travr)
            call wkvect(travr, 'V V R', nref, irefr)
            call getvr8('CHAM_ELEM', 'VALE_CALC', iocc=iocc, nbval=nref, vect=zr( irefr),&
                        nbret=iret)
        else if (n2 .ne. 0) then
            nref=-n2
            typres = 'I'
            call jedetr(travi)
            call wkvect(travi, 'V V I', nref, irefi)
            call getvis('CHAM_ELEM', 'VALE_CALC_I', iocc=iocc, nbval=nref, vect=zi(irefi),&
                        nbret=iret)
        else if (n3 .ne. 0) then
            nref=-n3
            typres = 'C'
            call jedetr(travc)
            call wkvect(travc, 'V V C', nref, irefc)
            call getvc8('CHAM_ELEM', 'VALE_CALC_C', iocc=iocc, nbval=nref, vect=zc(irefc),&
                        nbret=iret)
        endif
!
! ----------------------------------------------------------------------
        lref=.false.
        call getvr8('CHAM_ELEM', 'PRECISION', iocc=iocc, scal=epsir, nbret=iret)
        if (iret .ne. 0) then
            lref=.true.
            call getvr8('CHAM_ELEM', 'VALE_REFE', iocc=iocc, nbval=0, nbret=n1r)
            call getvis('CHAM_ELEM', 'VALE_REFE_I', iocc=iocc, nbval=0, nbret=n2r)
            call getvc8('CHAM_ELEM', 'VALE_REFE_C', iocc=iocc, nbval=0, nbret=n3r)
            if (n1r .ne. 0) then
                ASSERT((n1r.eq.n1))
                nref=-n1r
                call jedetr(travrr)
                call wkvect(travrr, 'V V R', nref, irefrr)
                call getvr8('CHAM_ELEM', 'VALE_REFE', iocc=iocc, nbval=nref, vect=zr(irefrr),&
                            nbret=iret)
            else if (n2r.ne.0) then
                ASSERT((n2r.eq.n2))
                nref=-n2r
                call jedetr(travir)
                call wkvect(travir, 'V V I', nref, irefir)
                call getvis('CHAM_ELEM', 'VALE_REFE_I', iocc=iocc, nbval=nref, vect=zi(irefir),&
                            nbret=iret)
            else if (n3r.ne.0) then
                ASSERT((n3r.eq.n3))
                nref=-n3r
                call jedetr(travcr)
                call wkvect(travcr, 'V V C', nref, irefcr)
                call getvc8('CHAM_ELEM', 'VALE_REFE_C', iocc=iocc, nbval=nref, vect=zc(irefcr),&
                            nbret=iret)
            endif
        endif
! ----------------------------------------------------------------------
        call getvtx('CHAM_ELEM', 'TYPE_TEST', iocc=iocc, scal=typtes, nbret=n1)
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
            call getvtx('CHAM_ELEM', 'NOM_CMP', iocc=iocc, nbval=0, nbret=n4)
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
                if (lref) then
                    tbref(1)=tbtxt(1)
                    tbref(2)=tbtxt(2)
                    tbtxt(1)='NON_REGRESSION'
                endif
                call utest1(cham19, typtes, typres, nref, tbtxt,&
                            zi(irefi), zr(irefr), zc(irefc), epsi, crit,&
                            ific, .true., ssigne)
                if (lref) then
                    call utest1(cham19, typtes, typres, nref, tbref,&
                                zi(irefir), zr(irefrr), zc(irefcr), epsir, crit,&
                                ific, .false., ssigne)
                endif
            else
                nbcmp = -n4
                call wkvect('&&OP0023.NOM_CMP', 'V V K8', nbcmp, jcmp)
                call getvtx('CHAM_ELEM', 'NOM_CMP', iocc=iocc, nbval=nbcmp, vect=zk8(jcmp),&
                            nbret=n4)
                if (lref) then
                    tbref(1)=tbtxt(1)
                    tbref(2)=tbtxt(2)
                    tbtxt(1)='NON_REGRESSION'
                endif
                call utest4(cham19, typtes, typres, nref, tbtxt,&
                            zi(irefi), zr(irefr), zc(irefc), epsi, lign1,&
                            lign2, crit, ific, nbcmp, zk8(jcmp),&
                            .true., ssigne)
                if (lref) then
                    call utest4(cham19, typtes, typres, nref, tbref,&
                                zi(irefir), zr(irefrr), zc(irefcr), epsir, lign1,&
                                lign2, crit, ific, nbcmp, zk8(jcmp),&
                                .false., ssigne)
                endif
                call jedetr('&&OP0023.NOM_CMP')
            endif
!
! ----------------------------------------------------------------------
        else
!
            call getvtx('CHAM_ELEM', 'NOM_CMP', iocc=iocc, scal=noddl, nbret=n1)
            call dismoi('NOM_MAILLA', cham19, 'CHAMP', repk=nomma)
            call getvem(nomma, 'MAILLE', 'CHAM_ELEM', 'MAILLE', iocc,&
                        iarg, 1, nomail, n1)
            if (n1 .ne. 0) then
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' MAILLE'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nomail
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
            endif
!
            call getvem(nomma, 'NOEUD', 'CHAM_ELEM', 'NOEUD', iocc,&
                        iarg, 1, nonoeu(1:8), n3)
            if (n3 .ne. 0) then
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' NOEUD'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nonoeu(1:8)
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
            endif
!
            call getvem(nomma, 'GROUP_NO', 'CHAM_ELEM', 'GROUP_NO', iocc,&
                        iarg, 1, nogrno, n4)
            if (n4 .ne. 0) then
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' GROUP_NO'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nogrno
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
            endif
!
            if (n3 .eq. 1) then
!             RIEN A FAIRE.
            else if (n4.eq.1) then
                call utnono('A', nomma, 'NOEUD', nogrno, nonoeu(1:8),&
                            iret)
                if (iret .ne. 0) then
                    write (ific,*) testok
                    goto 100
                endif
                nonoeu(10:33) = nogrno
            endif
!
            call dismoi('NOM_GD', cham19, 'CHAMP', repk=nomgd)
            call utcmp1(nomgd, 'CHAM_ELEM', iocc, noddl, ivari)
            call getvis('CHAM_ELEM', 'SOUS_POINT', iocc=iocc, scal=nusp, nbret=n2)
            if (n2 .eq. 0) nusp = 0
            call getvis('CHAM_ELEM', 'POINT', iocc=iocc, scal=nupo, nbret=n2)
            if (n2 .eq. 0) nupo = 0
!
            if (n2 .ne. 0) then
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' POINT'
                call codent(nupo, 'G', chpt)
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//chpt
                lign1(nl1+17:nl1+17)='.'
                lign2(nl2+17:nl2+17)='.'
            endif
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
            nl1 = lxlgut(lign1)
            nl1 = lxlgut(lign1(1:nl1-1))
            nl2 = lxlgut(lign2)
            nl2 = lxlgut(lign2(1:nl2-1))
            write (ific,'(A)') lign1(1:nl1)
            write (ific,*) lign2(1:nl2)
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
            write (ific,*)' '
        endif
! ----------------------------------------------------------------------
100 end do
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

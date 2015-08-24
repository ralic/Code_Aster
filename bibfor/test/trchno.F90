subroutine trchno(ific, nocc)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
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
#include "asterfort/tresu_champ_all.h"
#include "asterfort/tresu_champ_cmp.h"
#include "asterfort/tresu_champ_no.h"
#include "asterfort/tresu_ordgrd.h"
#include "asterfort/tresu_read_refe.h"
#include "asterfort/tresu_tole.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer, intent(in) :: ific
    integer, intent(in) :: nocc
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     COMMANDE:  TEST_RESU
!                MOT CLE FACTEUR "CHAM_NO"
! ----------------------------------------------------------------------
!
    character(len=6) :: nompro
    parameter (nompro='TRCHNO')
!
    integer :: iocc, iret, nbcmp, n1, n2, n3, n4
    integer :: n1r, n2r, n3r, irefrr, irefir, irefcr
    integer :: irefr, irefi, irefc, nref, nl1, nl2, nl11, nl22
    real(kind=8) :: epsi, epsir
    character(len=1) :: typres
    character(len=3) :: ssigne
    character(len=8) :: crit, noddl, nomma, typtes
    character(len=11) :: motcle
    character(len=19) :: cham19
    character(len=16) :: tbtxt(2), tbref(2)
    character(len=33) :: nonoeu
    character(len=24) :: travr, travi, travc, travrr, travir, travcr, nogrno
    character(len=200) :: lign1, lign2
    integer :: iarg
    aster_logical :: lref
    character(len=8), pointer :: nom_cmp(:) => null()
    aster_logical :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
    call jemarq()
!
    motcle = 'CHAM_NO'
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
    irefrr=1
    irefcr=1
!
    do iocc = 1, nocc
        lign1 = ' '
        lign2 = ' '
        nonoeu = ' '
        noddl = ' '
        call getvid('CHAM_NO', 'CHAM_GD', iocc=iocc, scal=cham19, nbret=n1)
        lign1(1:21)='---- '//motcle(1:8)
        lign1(22:22)='.'
        lign2(1:21)='     '//cham19(1:8)
        lign2(22:22)='.'
!
        call tresu_read_refe('CHAM_NO', iocc, tbtxt)
!
        call getvtx('CHAM_NO', 'NOM_CMP', iocc=iocc, scal=noddl, nbret=n1)
        if (n1 .ne. 0) then
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//noddl
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
        endif
        call getvtx('CHAM_NO', 'VALE_ABS', iocc=iocc, scal=ssigne, nbret=n1)
        if (ssigne .eq. 'OUI') then
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            lign1(1:nl1+16)=lign1(1:nl1-1)//' VALE_ABS'
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '//ssigne
            lign1(nl1+17:nl1+17)='.'
            lign2(nl2+17:nl2+17)='.'
        endif
        call tresu_tole(epsi, mcf='CHAM_NO', iocc=iocc)
        call getvtx('CHAM_NO', 'CRITERE', iocc=iocc, scal=crit, nbret=iret)
!
        call getvr8('CHAM_NO', 'VALE_CALC', iocc=iocc, nbval=0, nbret=n1)
        call getvis('CHAM_NO', 'VALE_CALC_I', iocc=iocc, nbval=0, nbret=n2)
        call getvc8('CHAM_NO', 'VALE_CALC_C', iocc=iocc, nbval=0, nbret=n3)
!
        skip = .false.
        ordgrd = 1.d0
        if (n1 .ne. 0) then
            nref=-n1
            typres = 'R'
            call jedetr(travr)
            call wkvect(travr, 'V V R', nref, irefr)
            call getvr8('CHAM_NO', 'VALE_CALC', iocc=iocc, nbval=nref, vect=zr(irefr),&
                        nbret=iret)
            call tresu_ordgrd(zr(irefr), skip, ordgrd, mcf='CHAM_NO', iocc=iocc)
        else if (n2 .ne. 0) then
            nref=-n2
            typres = 'I'
            call jedetr(travi)
            call wkvect(travi, 'V V I', nref, irefi)
            call getvis('CHAM_NO', 'VALE_CALC_I', iocc=iocc, nbval=nref, vect=zi( irefi),&
                        nbret=iret)
        else if (n3 .ne. 0) then
            nref=-n3
            typres = 'C'
            call jedetr(travc)
            call wkvect(travc, 'V V C', nref, irefc)
            call getvc8('CHAM_NO', 'VALE_CALC_C', iocc=iocc, nbval=nref, vect=zc( irefc),&
                        nbret=iret)
        endif
! ----------------------------------------------------------------------
        lref=.false.
        call getvr8('CHAM_NO', 'PRECISION', iocc=iocc, scal=epsir, nbret=iret)
        if (iret .ne. 0) then
            lref=.true.
            call getvr8('CHAM_NO', 'VALE_REFE', iocc=iocc, nbval=0, nbret=n1r)
            call getvis('CHAM_NO', 'VALE_REFE_I', iocc=iocc, nbval=0, nbret=n2r)
            call getvc8('CHAM_NO', 'VALE_REFE_C', iocc=iocc, nbval=0, nbret=n3r)
            if (n1r .ne. 0) then
                ASSERT((n1r.eq.n1))
                nref=-n1r
                call jedetr(travrr)
                call wkvect(travrr, 'V V R', nref, irefrr)
                call getvr8('CHAM_NO', 'VALE_REFE', iocc=iocc, nbval=nref, vect=zr(irefrr),&
                            nbret=iret)
            else if (n2r.ne.0) then
                ASSERT((n2r.eq.n2))
                nref=-n2r
                call jedetr(travir)
                call wkvect(travir, 'V V I', nref, irefir)
                call getvis('CHAM_NO', 'VALE_REFE_I', iocc=iocc, nbval=nref, vect=zi(irefir),&
                            nbret=iret)
            else if (n3r.ne.0) then
                ASSERT((n3r.eq.n3))
                nref=-n3r
                call jedetr(travcr)
                call wkvect(travcr, 'V V C', nref, irefcr)
                call getvc8('CHAM_NO', 'VALE_REFE_C', iocc=iocc, nbval=nref, vect=zc(irefcr),&
                            nbret=iret)
            endif
        endif
        if (skip .and. .not. lref) then
            call utmess('A', 'TEST0_11')
        endif
        if (skip .and. .not. lref) then
            call utmess('A', 'TEST0_11')
        endif
! ----------------------------------------------------------------------
!
        call getvtx('CHAM_NO', 'TYPE_TEST', iocc=iocc, scal=typtes, nbret=n1)
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
            call getvtx('CHAM_NO', 'NOM_CMP', iocc=iocc, nbval=0, nbret=n4)
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
                call tresu_champ_all(cham19, typtes, typres, nref, tbtxt,&
                                     zi(irefi), zr(irefr), zc(irefc), epsi, crit,&
                                     ific, .true._1, ssigne, ignore=skip, compare=ordgrd)
                if (lref) then
                    call tresu_champ_all(cham19, typtes, typres, nref, tbref,&
                                         zi(irefir), zr(irefrr), zc(irefcr), epsir, crit,&
                                         ific, .false._1, ssigne)
                endif
!
            else
                nbcmp = -n4
                AS_ALLOCATE(vk8=nom_cmp, size=nbcmp)
                call getvtx('CHAM_NO', 'NOM_CMP', iocc=iocc, nbval=nbcmp, vect=nom_cmp,&
                            nbret=n4)
                if (lref) then
                    tbref(1)=tbtxt(1)
                    tbref(2)=tbtxt(2)
                    tbtxt(1)='NON_REGRESSION'
                endif
                call tresu_champ_cmp(cham19, typtes, typres, nref, tbtxt,&
                                     zi(irefi), zr(irefr), zc(irefc), epsi, lign1,&
                                     lign2, crit, ific, nbcmp, nom_cmp,&
                                     .true._1, ssigne, ignore=skip, compare=ordgrd)
                if (lref) then
                    call tresu_champ_cmp(cham19, typtes, typres, nref, tbref,&
                                         zi( irefir), zr(irefrr), zc(irefcr), epsir, lign1,&
                                         lign2, crit, ific, nbcmp, nom_cmp,&
                                         .false._1, ssigne)
                endif
                AS_DEALLOCATE(vk8=nom_cmp)
            endif
!
        else
!
            call getvtx('CHAM_NO', 'NOM_CMP', iocc=iocc, scal=noddl, nbret=n1)
            call dismoi('NOM_MAILLA', cham19, 'CHAMP', repk=nomma)
            call getvem(nomma, 'NOEUD', 'CHAM_NO', 'NOEUD', iocc,&
                        iarg, 1, nonoeu(1:8), n1)
            if (n1 .ne. 0) then
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' NOEUD'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nonoeu(1:8)
            endif
!
            call getvem(nomma, 'GROUP_NO', 'CHAM_NO', 'GROUP_NO', iocc,&
                        iarg, 1, nogrno, n2)
            if (n2 .ne. 0) then
                nl1 = lxlgut(lign1)
                nl2 = lxlgut(lign2)
                lign1(1:nl1+16)=lign1(1:nl1-1)//' GROUP_NO'
                lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nogrno
            endif
!
            nl1 = lxlgut(lign1)
            nl2 = lxlgut(lign2)
            write (ific,*) lign1(1:nl1)
            write (ific,*) lign2(1:nl2)
!
!
            if (n1 .eq. 1) then
!            RIEN A FAIRE.
            else
                call utnono('F', nomma, 'NOEUD', nogrno, nonoeu(1:8),&
                            iret)
                nonoeu(10:33) = nogrno
            endif
!
            if (lref) then
                tbref(1)=tbtxt(1)
                tbref(2)=tbtxt(2)
                tbtxt(1)='NON_REGRESSION'
            endif
            call tresu_champ_no(cham19, nonoeu, noddl, nref, tbtxt,&
                                zi(irefi), zr( irefr), zc(irefc), typres, epsi,&
                                crit, ific, .true._1, ssigne, ignore=skip,&
                                compare=ordgrd)
            if (lref) then
                call tresu_champ_no(cham19, nonoeu, noddl, nref, tbref,&
                                    zi(irefir), zr(irefrr), zc(irefcr), typres, epsir,&
                                    crit, ific, .false._1, ssigne)
            endif
        endif
        write (ific,*)' '
    end do
!
    1160 format(1x,a80,a)
    1200 format(1x,2(a80),a)
!
    call jedetr(travr)
    call jedetr(travc)
    call jedetr(travi)
!
!
    call jedema()
end subroutine

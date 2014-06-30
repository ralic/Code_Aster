subroutine op0177()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE:  TEST_TABLE
!
! ----------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
! 0.2. ==> COMMUNS
! 0.3. ==> VARIABLES LOCALES
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tbimfi.h"
#include "asterfort/tbliva.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/tresu_ordgrd.h"
#include "asterfort/tresu_print_all.h"
#include "asterfort/tresu_tabl.h"
#include "asterfort/tresu_read_refe.h"
#include "asterfort/tresu_str.h"
#include "asterfort/tresu_tole.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=6) :: nompro
    parameter (nompro='OP0177')
!
    integer :: ibid, n1, n2, n3, nk, iret, ific, nparfi
    integer :: vali, irefr, irefi, irefc, irefk, nref
    integer :: n1r, n2r, n3r, nkr, irefrr, irefir, irefcr, irefkr
    integer :: nl1, nl2, nl11, nl22
!
    real(kind=8) :: r8b, valr, epsi, epsir
!
    complex(kind=8) :: cbid, valc
!
    character(len=1) :: typr
    character(len=3) :: ssigne
    character(len=8) :: k8b, crit, ctype, typtes, latabl
    character(len=8) :: motcle
    character(len=16) :: nomfi, tbtxt(2), tbref(2)
    character(len=19) :: newtab, newta1
    character(len=24) :: para
    character(len=24) :: travr, travi, travc, travrr, travir, travcr, travk, travkr
    character(len=80) :: valk
    character(len=200) :: lign1, lign2
    logical(kind=1) :: lref
    logical(kind=1) :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
    ibid=0
    cbid=(0.d0,0.d0)
    r8b=0.d0
    call infmaj()
!
    travr = '&&'//nompro//'_TRAVR          '
    travi = '&&'//nompro//'_TRAVI          '
    travc = '&&'//nompro//'_TRAVC          '
    travk = '&&'//nompro//'_TRAVK          '
    travrr = '&&'//nompro//'_TRAVR_R        '
    travir = '&&'//nompro//'_TRAVI_R        '
    travcr = '&&'//nompro//'_TRAVC_R        '
    travkr = '&&'//nompro//'_TRAVK_R        '
    motcle = 'TABLE'
!
    nomfi = ' '
    ific = iunifi('RESULTAT')
    if (.not. ulexis( ific )) then
        call ulopen(ific, ' ', nomfi, 'NEW', 'O')
    endif
    write(ific,100)
!
    call getvid(' ', 'TABLE', scal=latabl, nbret=n1)
!
    call getfac('FILTRE', nparfi)
!
    call getvtx(' ', 'VALE_ABS', scal=ssigne, nbret=n1)
    call tresu_tole(epsi)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
!
    call getvr8(' ', 'VALE_CALC', nbval=0, nbret=n1)
    call getvis(' ', 'VALE_CALC_I', nbval=0, nbret=n2)
    call getvc8(' ', 'VALE_CALC_C', nbval=0, nbret=n3)
    call getvtx(' ', 'VALE_CALC_K', nbval=0, nbret=nk)
!
    irefr = 0
    irefi = 0
    irefc = 0
    irefk = 0
    skip = .false.
    ordgrd = 1.d0
    if (n1 .ne. 0) then
        nref=-n1
        typr = 'R'
        call jedetr(travr)
        call wkvect(travr, 'V V R', nref, irefr)
        call getvr8(' ', 'VALE_CALC', nbval=nref, vect=zr(irefr))
        call tresu_ordgrd(zr(irefr), skip, ordgrd)
    else if (n2 .ne. 0) then
        nref=-n2
        typr = 'I'
        call jedetr(travi)
        call wkvect(travi, 'V V I', nref, irefi)
        call getvis(' ', 'VALE_CALC_I', nbval=nref, vect=zi(irefi))
    else if (n3 .ne. 0) then
        nref=-n3
        typr = 'C'
        call jedetr(travc)
        call wkvect(travc, 'V V C', nref, irefc)
        call getvc8(' ', 'VALE_CALC_C', nbval=nref, vect=zc(irefc))
    else
        ASSERT(nk .ne. 0)
        nref = -nk
        typr = 'K'
        call jedetr(travk)
        call wkvect(travk, 'V V K80', nref, irefk)
        call getvtx(' ', 'VALE_CALC_K', nbval=nref, vect=zk80(irefk))
    endif
! ----------------------------------------------------------------------
    lref=.false.
    call getvr8(' ', 'PRECISION', scal=epsir, nbret=iret)
    if (iret .ne. 0) then
        lref=.true.
        call getvr8(' ', 'VALE_REFE', nbval=0, nbret=n1r)
        call getvis(' ', 'VALE_REFE_I', nbval=0, nbret=n2r)
        call getvc8(' ', 'VALE_REFE_C', nbval=0, nbret=n3r)
        call getvtx(' ', 'VALE_REFE_K', nbval=0, nbret=nkr)
!
        irefrr = 0
        irefir = 0
        irefcr = 0
        irefkr = 0
        if (n1r .ne. 0) then
            ASSERT((n1r.eq.n1))
            nref=-n1r
            call jedetr(travrr)
            call wkvect(travrr, 'V V R', nref, irefrr)
            call getvr8(' ', 'VALE_REFE', nbval=nref, vect=zr(irefrr))
        else if (n2r.ne.0) then
            ASSERT((n2r.eq.n2))
            nref=-n2r
            call jedetr(travir)
            call wkvect(travir, 'V V I', nref, irefir)
            call getvis(' ', 'VALE_REFE_I', nbval=nref, vect=zi(irefir))
        else if (n3r.ne.0) then
            ASSERT((n3r.eq.n3))
            nref=-n3r
            call jedetr(travcr)
            call wkvect(travcr, 'V V C', nref, irefcr)
            call getvc8(' ', 'VALE_REFE_C', nbval=nref, vect=zc(irefcr))
        else
            ASSERT(nkr .ne. 0)
            ASSERT(nkr .eq. nk)
            nref = -nkr
            call jedetr(travkr)
            call wkvect(travkr, 'V V K80', nref, irefkr)
            call getvtx(' ', 'VALE_CALC_K', nbval=nref, vect=zk80(irefkr))
        endif
    endif
    if (skip .and. .not. lref) then
        call utmess('A', 'TEST0_11')
    endif
! ----------------------------------------------------------------------
    call getvtx(' ', 'NOM_PARA', scal=para)
    call getvtx(' ', 'TYPE_TEST', scal=typtes, nbret=n1)
!
    lign1 = ' '
    lign2 = ' '
!
!   Traitement du mot clé "FILTRE" ---
    newtab = latabl
!
    lign1(1:21)='---- '//motcle
    lign1(22:22)='.'
    lign2(1:21)='     '//latabl
    lign2(22:22)='.'
    nl1 = lxlgut(lign1)
    nl2 = lxlgut(lign2)
    lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_PARA'
    lign2(1:nl2+16)=lign2(1:nl2-1)//' '//para(1:16)
    lign1(nl1+17:nl1+17)='.'
    lign2(nl2+17:nl2+17)='.'
!
    if (nparfi .ne. 0) then
        newta1 = '&&'//nompro//'.FILTRE '
        call tbimfi(nparfi, newtab, newta1, iret)
        if (iret .ne. 0) then
            call utmess('F', 'CALCULEL6_7')
        endif
        newtab = newta1
    endif
!   ------------------------------------------------------------------
!
    call tresu_read_refe(' ', 1, tbtxt)
    if (lref) then
        tbref(1)=tbtxt(1)
        tbref(2)=tbtxt(2)
        tbtxt(1)='NON_REGRESSION'
    endif
!
    if (n1 .ne. 0) then
!   cas de TYPE_TEST
!
        nl1 = lxlgut(lign1)
        nl2 = lxlgut(lign2)
        lign1(1:nl1+16)=lign1(1:nl1-1)//' TYPE_TEST'
        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//typtes
        lign1(nl1+17:nl1+17)='.'
        lign2(nl2+17:nl2+17)='.'
!
        nl1 = lxlgut(lign1)
        nl11 = lxlgut(lign1(1:nl1-1))
        nl2 = lxlgut(lign2)
        nl22 = lxlgut(lign2(1:nl2-1))
        if (nl11 .lt. 80) then
            write (ific,*) lign1(1:nl11)
        else if (nl11.lt.160) then
            write (ific,116) lign1(1:80), lign1(81:nl11)
        else
            write (ific,120) lign1(1:80), lign1(81:160), lign1(161:&
            nl11)
        endif
        if (nl22 .lt. 80) then
            write (ific,*) lign2(1:nl22)
        else if (nl22.lt.160) then
            write (ific,116) lign2(1:80), lign2(81:nl22)
        else
            write (ific,120) lign2(1:80), lign2(81:160), lign2(161:&
            nl22)
        endif
!
        call tresu_tabl(newtab, para, typtes, typr, tbtxt,&
                    zi(irefi), zr(irefr), zc(irefc), epsi, crit,&
                    ific, .true._1, ssigne, ignore=skip, compare=ordgrd)
        if (lref) then
            call tresu_tabl(newtab, para, typtes, typr, tbref,&
                        zi( irefir), zr(irefrr), zc(irefcr), epsir, crit,&
                        ific, .false._1, ssigne)
        endif
    else
!
        call tbliva(newtab, 0, k8b, [ibid], [r8b],&
                    [cbid], k8b, k8b, [r8b], para,&
                    ctype, vali, valr, valc, valk,&
                    iret)
!
!
        nl1 = lxlgut(lign1)
        nl11 = lxlgut(lign1(1:nl1-1))
        nl2 = lxlgut(lign2)
        nl22 = lxlgut(lign2(1:nl2-1))
        if (nl11 .lt. 80) then
            write (ific,*) lign1(1:nl11)
        else if (nl11.lt.160) then
            write (ific,116) lign1(1:80), lign1(81:nl11)
        else
            write (ific,120) lign1(1:80), lign1(81:160), lign1(161:nl11)
        endif
        if (nl22 .lt. 80) then
            write (ific,*) lign2(1:nl22)
        else if (nl22.lt.160) then
            write (ific,116) lign2(1:80), lign2(81:nl22)
        else
            write (ific,120) lign2(1:80), lign2(81:160), lign2(161:nl22)
        endif
!
        if (iret .eq. 0) then
        else if (iret .eq. 1) then
            call utmess('F', 'CALCULEL6_3')
        else if (iret .eq. 2) then
            call utmess('F', 'CALCULEL6_4')
        else if (iret .eq. 3) then
            call utmess('F', 'CALCULEL6_5')
        else
            call utmess('F', 'CALCULEL6_6')
        endif
        if (ctype(1:1) .ne. typr) then
            call utmess('F', 'CALCULEL6_8', sk=ctype)
        endif
!
        if (nk .ne. 0) then
!       cas des chaînes de caractères
            call tresu_str(tbtxt, zk80(irefk), valk, ific, .true._1)
            if (lref) then
                call tresu_str(tbtxt, zk80(irefkr), valk, ific, .false._1)
            endif
        else
!       cas des réels, entiers, complexes
            call tresu_print_all(tbtxt(1), tbtxt(2), .true._1, typr, nref, &
                        crit, epsi, ssigne, zr(irefr), valr, &
                        zi(irefi), vali, zc(irefc), valc, ignore=skip, &
                        compare=ordgrd)
            if (lref) then
                call tresu_print_all(tbref(1), tbref(2), .false._1, typr, nref, &
                            crit, epsir, ssigne, zr(irefrr), valr, &
                            zi(irefir), vali, zc(irefcr), valc)
            endif
        endif
    endif
!
    if (nparfi .ne. 0) then
        call detrsd('TABLE', newta1)
    endif
    write (ific,*) ' '
!
100 format(/,80('-'))
116 format(1x,a80,a)
120 format(1x,2(a80),a)
!
    call jedema()
!
end subroutine

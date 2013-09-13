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
#include "asterfort/u2mess.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utest0.h"
#include "asterfort/utest3.h"
#include "asterfort/utites.h"
#include "asterfort/wkvect.h"
    character(len=6) :: nompro
    parameter (nompro='OP0177')
!
    integer :: ibid, n1, n2, n3, iret, ific, nparfi
    integer :: vali, irefr, irefi, irefc, nref
    integer :: n1r, n2r, n3r, irefrr, irefir, irefcr
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
    character(len=24) :: travr, travi, travc, travrr, travir, travcr
    character(len=80) :: valk
    character(len=200) :: lign1, lign2
    logical :: lref
!     ------------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
    call infmaj()
!
    travr = '&&'//nompro//'_TRAVR          '
    travi = '&&'//nompro//'_TRAVI          '
    travc = '&&'//nompro//'_TRAVC          '
    travrr = '&&'//nompro//'_TRAVR_R        '
    travir = '&&'//nompro//'_TRAVI_R        '
    travcr = '&&'//nompro//'_TRAVC_R        '
    motcle = 'TABLE'
!
    nomfi = ' '
    ific = iunifi('RESULTAT')
    if (.not. ulexis( ific )) then
        call ulopen(ific, ' ', nomfi, 'NEW', 'O')
    endif
    write(ific,1000)
!
    call getvid(' ', 'TABLE', scal=latabl, nbret=n1)
!
    call getfac('FILTRE', nparfi)
!
    call getvtx(' ', 'VALE_ABS', scal=ssigne, nbret=n1)
    call getvr8(' ', 'TOLE_MACHINE', scal=epsi, nbret=n1)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
!
    call getvr8(' ', 'VALE_CALC', nbval=0, nbret=n1)
    call getvis(' ', 'VALE_CALC_I', nbval=0, nbret=n2)
    call getvc8(' ', 'VALE_CALC_C', nbval=0, nbret=n3)
!
    irefr=1
    irefi=1
    irefc=1
    if (n1 .ne. 0) then
        nref=-n1
        typr = 'R'
        call jedetr(travr)
        call wkvect(travr, 'V V R', nref, irefr)
        call getvr8(' ', 'VALE_CALC', nbval=nref, vect=zr(irefr), nbret=iret)
    else if (n2 .ne. 0) then
        nref=-n2
        typr = 'I'
        call jedetr(travi)
        call wkvect(travi, 'V V I', nref, irefi)
        call getvis(' ', 'VALE_CALC_I', nbval=nref, vect=zi(irefi), nbret=iret)
    else if (n3 .ne. 0) then
        nref=-n3
        typr = 'C'
        call jedetr(travc)
        call wkvect(travc, 'V V C', nref, irefc)
        call getvc8(' ', 'VALE_CALC_C', nbval=nref, vect=zc(irefc), nbret=iret)
    endif
! ----------------------------------------------------------------------
    lref=.false.
    call getvr8(' ', 'PRECISION', scal=epsir, nbret=iret)
    if (iret .ne. 0) then
        lref=.true.
        call getvr8(' ', 'VALE_REFE', nbval=0, nbret=n1r)
        call getvis(' ', 'VALE_REFE_I', nbval=0, nbret=n2r)
        call getvc8(' ', 'VALE_REFE_C', nbval=0, nbret=n3r)
!
        irefrr=1
        irefir=1
        irefcr=1
        if (n1r .ne. 0) then
            ASSERT((n1r.eq.n1))
            nref=-n1r
            call jedetr(travrr)
            call wkvect(travrr, 'V V R', nref, irefrr)
            call getvr8(' ', 'VALE_REFE', nbval=nref, vect=zr(irefrr), nbret=iret)
        else if (n2r.ne.0) then
            ASSERT((n2r.eq.n2))
            nref=-n2r
            call jedetr(travir)
            call wkvect(travir, 'V V I', nref, irefir)
            call getvis(' ', 'VALE_REFE_I', nbval=nref, vect=zi(irefir), nbret=iret)
        else if (n3r.ne.0) then
            ASSERT((n3r.eq.n3))
            nref=-n3r
            call jedetr(travcr)
            call wkvect(travcr, 'V V C', nref, irefcr)
            call getvc8(' ', 'VALE_REFE_C', nbval=nref, vect=zc(irefcr), nbret=iret)
        endif
    endif
! ----------------------------------------------------------------------
!
!
    call getvtx(' ', 'NOM_PARA', scal=para, nbret=n1)
!
    call getvtx(' ', 'TYPE_TEST', scal=typtes, nbret=n1)
!
    lign1 = ' '
    lign2 = ' '
!
!
!     ------------------------------------------------------------------
!
!                 --- TRAITEMENT DU MOT CLE "FILTRE" ---
!
!     ------------------------------------------------------------------
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
!
    if (nparfi .ne. 0) then
        newta1 = '&&'//nompro//'.FILTRE '
        call tbimfi(nparfi, newtab, newta1, iret)
        if (iret .ne. 0) call u2mess('F', 'CALCULEL6_7')
        newtab = newta1
    endif
!     ------------------------------------------------------------------
!
    call utest3(' ', 1, tbtxt)
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
        nl1 = lxlgut(lign1)
        nl11 = lxlgut(lign1(1:nl1-1))
        nl2 = lxlgut(lign2)
        nl22 = lxlgut(lign2(1:nl2-1))
        if (nl11 .lt. 80) then
            write (ific,*) lign1(1:nl11)
        else if (nl11.lt.160) then
            write (ific,1160) lign1(1:80), lign1(81:nl11)
        else
            write (ific,1200) lign1(1:80), lign1(81:160), lign1(161:&
            nl11)
        endif
        if (nl22 .lt. 80) then
            write (ific,*) lign2(1:nl22)
        else if (nl22.lt.160) then
            write (ific,1160) lign2(1:80), lign2(81:nl22)
        else
            write (ific,1200) lign2(1:80), lign2(81:160), lign2(161:&
            nl22)
        endif
!
        if (lref) then
            tbref(1)=tbtxt(1)
            tbref(2)=tbtxt(2)
            tbtxt(1)='NON_REGRESSION'
        endif
        call utest0(newtab, para, typtes, typr, tbtxt,&
                    zi(irefi), zr(irefr), zc(irefc), epsi, crit,&
                    ific, .true., ssigne)
        if (lref) then
            call utest0(newtab, para, typtes, typr, tbref,&
                        zi( irefir), zr(irefrr), zc(irefcr), epsir, crit,&
                        ific, .false., ssigne)
        endif
        goto 9999
    endif
!
    call tbliva(newtab, 0, k8b, ibid, r8b,&
                cbid, k8b, k8b, r8b, para,&
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
        write (ific,1160) lign1(1:80), lign1(81:nl11)
    else
        write (ific,1200) lign1(1:80), lign1(81:160), lign1(161:nl11)
    endif
    if (nl22 .lt. 80) then
        write (ific,*) lign2(1:nl22)
    else if (nl22.lt.160) then
        write (ific,1160) lign2(1:80), lign2(81:nl22)
    else
        write (ific,1200) lign2(1:80), lign2(81:160), lign2(161:nl22)
    endif
!
!
    if (iret .eq. 0) then
    else if (iret .eq. 1) then
        call u2mess('F', 'CALCULEL6_3')
    else if (iret .eq. 2) then
        call u2mess('F', 'CALCULEL6_4')
    else if (iret .eq. 3) then
        call u2mess('F', 'CALCULEL6_5')
    else
        call u2mess('F', 'CALCULEL6_6')
    endif
    if (ctype(1:1) .ne. typr) call u2mess('F', 'CALCULEL6_8')
!
    if (lref) then
        tbref(1)=tbtxt(1)
        tbref(2)=tbtxt(2)
        tbtxt(1)='NON_REGRESSION'
    endif
    call utites(tbtxt(1), tbtxt(2), typr, nref, zi(irefi),&
                zr(irefr), zc(irefc), vali, valr, valc,&
                epsi, crit, ific, .true., ssigne)
    if (lref) then
        call utites(tbref(1), tbref(2), typr, nref, zi(irefir),&
                    zr(irefrr), zc(irefcr), vali, valr, valc,&
                    epsir, crit, ific, .false., ssigne)
    endif
!
9999  continue
    if (nparfi .ne. 0) call detrsd('TABLE', newta1)
    write (ific,*)' '
!
!
!
    1000 format(/,80('-'))
    1160 format(1x,a80,a)
    1200 format(1x,2(a80),a)
!
    call jedema()
!
end subroutine

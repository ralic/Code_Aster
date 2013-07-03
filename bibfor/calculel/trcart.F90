subroutine trcart(ific, nocc)
    implicit   none
#include "jeveux.h"
#include "asterc/getvc8.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utcmp1.h"
#include "asterfort/utest3.h"
#include "asterfort/utest5.h"
    integer :: ific, nocc
! person_in_charge: jacques.pellet at edf.fr
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
!                MOT CLE FACTEUR "CARTE"
! ----------------------------------------------------------------------
!
!
    integer :: iocc, ibid, iret
    integer :: n1, n2, n3, n1r, n2r, n3r, ivari
    integer :: nl1, nl2, vali, valir
    real(kind=8) :: epsi, epsir, valr, valrr
    complex(kind=8) :: valc, valcr
    character(len=1) :: typres
    character(len=8) :: crit, noddl, nomma, nomail, nomgd
    character(len=11) :: motcle
    character(len=19) :: cham19
    character(len=16) :: tbtxt(2), tbref(2)
    character(len=200) :: lign1, lign2
    integer :: iarg
    logical :: lref
!     ------------------------------------------------------------------
    call jemarq()
!
    motcle = 'CARTE'
!
    do 100 iocc = 1, nocc
        lign1 = ' '
        lign2 = ' '
        noddl = ' '
        call getvid('CARTE', 'CHAM_GD', iocc, iarg, 1,&
                    cham19, n1)
        lign1(1:21)='---- '//motcle(1:9)
        lign1(22:22)='.'
        lign2(1:21)='     '//cham19(1:8)
        lign2(22:22)='.'
        call utest3('CARTE', iocc, tbtxt)
!
        call getvtx('CARTE', 'NOM_CMP', iocc, iarg, 1,&
                    noddl, n1)
        call assert(n1.eq.1)
        nl1 = lxlgut(lign1)
        nl2 = lxlgut(lign2)
        lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//noddl
        lign1(nl1+17:nl1+17)='.'
        lign2(nl2+17:nl2+17)='.'
!
!
        call getvr8('CARTE', 'TOLE_MACHINE', iocc, iarg, 1,&
                    epsi, n1)
        call getvtx('CARTE', 'CRITERE', iocc, iarg, 1,&
                    crit, n1)
!
        call getvr8('CARTE', 'VALE_CALC', iocc, iarg, 1,&
                    valr, n1)
        call getvis('CARTE', 'VALE_CALC_I', iocc, iarg, 1,&
                    vali, n2)
        call getvc8('CARTE', 'VALE_CALC_C', iocc, iarg, 1,&
                    valc, n3)
!
        if (n1 .eq. 1) then
            typres = 'R'
        else if (n2 .eq. 1) then
            typres = 'I'
        else
            call assert(n3.eq.1)
            typres = 'C'
        endif
! ----------------------------------------------------------------------
        lref=.false.
        call getvr8('CARTE', 'PRECISION', iocc, iarg, 1,&
                    epsir, iret)
        if (iret .ne. 0) then
            lref=.true.
            call getvr8('CARTE', 'VALE_REFE', iocc, iarg, 1,&
                        valrr, n1r)
            call getvis('CARTE', 'VALE_REFE_I', iocc, iarg, 1,&
                        valir, n2r)
            call getvc8('CARTE', 'VALE_REFE_C', iocc, iarg, 1,&
                        valcr, n3r)
            call assert(n1r.eq.n1 .and. n2r.eq.n2 .and. n3r.eq.n3)
        endif
! ----------------------------------------------------------------------
!
!
        call getvtx('CARTE', 'NOM_CMP', iocc, iarg, 1,&
                    noddl, n1)
        call dismoi('F', 'NOM_MAILLA', cham19, 'CHAMP', ibid,&
                    nomma, iret)
        call getvem(nomma, 'MAILLE', 'CARTE', 'MAILLE', iocc,&
                    iarg, 1, nomail, n1)
        call assert(n1.eq.1)
        nl1 = lxlgut(lign1)
        nl2 = lxlgut(lign2)
        lign1(1:nl1+16)=lign1(1:nl1-1)//' MAILLE'
        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nomail
        lign1(nl1+17:nl1+17)='.'
        lign2(nl2+17:nl2+17)='.'
!
!
        call dismoi('F', 'NOM_GD', cham19, 'CHAMP', ibid,&
                    nomgd, iret)
        call utcmp1(nomgd, 'CARTE', iocc, noddl, ivari)
!
        nl1 = lxlgut(lign1)
        nl1 = lxlgut(lign1(1:nl1-1))
        nl2 = lxlgut(lign2)
        nl2 = lxlgut(lign2(1:nl2-1))
        write (ific,*) lign1(1:nl1)
        write (ific,*) lign2(1:nl2)
!
        if (lref) then
            tbref(1)=tbtxt(1)
            tbref(2)=tbtxt(2)
            tbtxt(1)='NON_REGRESSION'
        endif
        call utest5(cham19, nomail, noddl, tbtxt, vali,&
                    valr, valc, typres, epsi, crit,&
                    ific, .true.)
        if (lref) then
            call utest5(cham19, nomail, noddl, tbref, valir,&
                        valrr, valcr, typres, epsir, crit,&
                        ific, .false.)
        endif
        write (ific,*)' '
!
100  end do
!
!
    call jedema()
end subroutine

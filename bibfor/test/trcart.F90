subroutine trcart(ific, nocc)
    implicit none
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
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utcmp1.h"
#include "asterfort/utmess.h"
#include "asterfort/tresu_carte.h"
#include "asterfort/tresu_ordgrd.h"
#include "asterfort/tresu_read_refe.h"
#include "asterfort/tresu_tole.h"
    integer, intent(in) :: ific
    integer, intent(in) :: nocc
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
    integer :: iocc, iret
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
    logical(kind=1) :: lref
    logical(kind=1) :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
    call jemarq()
!
    motcle = 'CARTE'
!
    do iocc = 1, nocc
        lign1 = ' '
        lign2 = ' '
        noddl = ' '
        call getvid('CARTE', 'CHAM_GD', iocc=iocc, scal=cham19, nbret=n1)
        lign1(1:21)='---- '//motcle(1:9)
        lign1(22:22)='.'
        lign2(1:21)='     '//cham19(1:8)
        lign2(22:22)='.'
        call tresu_read_refe('CARTE', iocc, tbtxt)
!
        call getvtx('CARTE', 'NOM_CMP', iocc=iocc, scal=noddl, nbret=n1)
        ASSERT(n1.eq.1)
        nl1 = lxlgut(lign1)
        nl2 = lxlgut(lign2)
        lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//noddl
        lign1(nl1+17:nl1+17)='.'
        lign2(nl2+17:nl2+17)='.'
!
!
        call tresu_tole(epsi, mcf='CARTE', iocc=iocc)
        call getvtx('CARTE', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
!
        call getvr8('CARTE', 'VALE_CALC', iocc=iocc, scal=valr, nbret=n1)
        call getvis('CARTE', 'VALE_CALC_I', iocc=iocc, scal=vali, nbret=n2)
        call getvc8('CARTE', 'VALE_CALC_C', iocc=iocc, scal=valc, nbret=n3)
!
        skip = .false.
        ordgrd = 1.d0
        if (n1 .eq. 1) then
            typres = 'R'
            call tresu_ordgrd(valr, skip, ordgrd, mcf='CARTE', iocc=iocc)
        else if (n2 .eq. 1) then
            typres = 'I'
        else
            ASSERT(n3.eq.1)
            typres = 'C'
        endif
! ----------------------------------------------------------------------
        lref=.false.
        call getvr8('CARTE', 'PRECISION', iocc=iocc, scal=epsir, nbret=iret)
        if (iret .ne. 0) then
            lref=.true.
            call getvr8('CARTE', 'VALE_REFE', iocc=iocc, scal=valrr, nbret=n1r)
            call getvis('CARTE', 'VALE_REFE_I', iocc=iocc, scal=valir, nbret=n2r)
            call getvc8('CARTE', 'VALE_REFE_C', iocc=iocc, scal=valcr, nbret=n3r)
            ASSERT(n1r.eq.n1 .and. n2r.eq.n2 .and. n3r.eq.n3)
        endif
        if (skip .and. .not. lref) then
            call utmess('A', 'TEST0_11')
        endif
! ----------------------------------------------------------------------
!
!
        call getvtx('CARTE', 'NOM_CMP', iocc=iocc, scal=noddl, nbret=n1)
        call dismoi('NOM_MAILLA', cham19, 'CHAMP', repk=nomma)
        call getvem(nomma, 'MAILLE', 'CARTE', 'MAILLE', iocc,&
                    iarg, 1, nomail, n1)
        ASSERT(n1.eq.1)
        nl1 = lxlgut(lign1)
        nl2 = lxlgut(lign2)
        lign1(1:nl1+16)=lign1(1:nl1-1)//' MAILLE'
        lign2(1:nl2+16)=lign2(1:nl2-1)//' '//nomail
        lign1(nl1+17:nl1+17)='.'
        lign2(nl2+17:nl2+17)='.'
!
!
        call dismoi('NOM_GD', cham19, 'CHAMP', repk=nomgd)
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
        call tresu_carte(cham19, nomail, noddl, tbtxt, vali,&
                    valr, valc, typres, epsi, crit,&
                    ific, .true._1, ignore=skip, compare=ordgrd)
        if (lref) then
            call tresu_carte(cham19, nomail, noddl, tbref, valir,&
                        valrr, valcr, typres, epsir, crit,&
                        ific, .false._1)
        endif
        write (ific,*)' '
!
    end do
!
!
    call jedema()
end subroutine

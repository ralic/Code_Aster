subroutine afva01(typsd, nomsd, nomsym, lautr)
    implicit   none
#include "asterfort/assert.h"
#include "asterfort/cmpcha.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
    character(len=16) :: typsd, nomsd, nomsym
    logical :: lautr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
! BUT : DIRE SI DANS LA SD NOMSD DE TYPE TYPSD=CHAMP/EVOL+NOMSYM
!       ON TROUVE DES COMPOSANTES AUTRES QUE 'TEMP' ET 'LAGR'
! ----------------------------------------------------------------------
#include "jeveux.h"
    integer :: ncmp, ncmpmx, k, j1, jordr, j, iret, nbordr(1), ibid
    character(len=19) :: ch19, kbid, res19
    character(len=24) :: corr1, corr2, nomcmp
    real(kind=8) :: r8b
    complex(kind=8) :: c16b
! ----------------------------------------------------------------------
!
    call jemarq()
    nomcmp='&&AFVAUT.NOMCMP'
    corr1='&&AFVAUT.CORR1'
    corr2='&&AFVAUT.CORR2'
!
!
    if (typsd .eq. 'CHAMP') then
!     -----------------------------
        ch19=nomsd
        call cmpcha(ch19, nomcmp, corr1, corr2, ncmp,&
                    ncmpmx)
        call jeveuo(nomcmp, 'L', j1)
        do 1, k=1,ncmp
        if (zk8(j1-1+k) .ne. 'TEMP' .and. zk8(j1-1+k) .ne. 'LAGR') goto 7
 1      continue
        goto 8
!
!
    else if (typsd.eq.'EVOL') then
!     -----------------------------
        res19=nomsd
        call rsorac(res19, 'LONUTI', 0, r8b, kbid,&
                    c16b, r8b, kbid, nbordr, 1,&
                    ibid)
        call wkvect('&&AFVA01.NUME_ORDRE', 'V V I', nbordr(1), jordr)
        call rsorac(res19, 'TOUT_ORDRE', 0, r8b, kbid,&
                    c16b, r8b, kbid, zi(jordr), nbordr(1),&
                    ibid)
!
        do 20 j = 1, nbordr(1)
            call rsexch('F', res19, nomsym, zi(jordr-1+j), ch19,&
                        iret)
            if (iret .ne. 0) goto 20
!
            call cmpcha(ch19, nomcmp, corr1, corr2, ncmp,&
                        ncmpmx)
            call jeveuo(nomcmp, 'L', j1)
            do 2, k=1,ncmp
            if (zk8(j1-1+k) .ne. 'TEMP' .and. zk8(j1-1+k) .ne. 'LAGR') goto 7
 2          continue
            call jedetr(nomcmp)
            call jedetr(corr1)
            call jedetr(corr2)
20      continue
        call jedetr('&&AFVA01.NUME_ORDRE')
        lautr=.false.
        goto 8
!
!
    else
        write(6,*) typsd,nomsd,nomsym
        ASSERT(.false.)
    endif
!
 7  continue
    lautr=.true.
    goto 8
!
!
 8  continue
!
    call jedetr(nomcmp)
    call jedetr(corr1)
    call jedetr(corr2)
!
    call jedema()
end subroutine

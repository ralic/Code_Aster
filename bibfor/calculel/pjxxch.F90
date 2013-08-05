subroutine pjxxch(correz, ch1z, ch2z, tychv, prfchz,&
                  prol0, ligrez, base, iret)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pjefch.h"
#include "asterfort/pjngch.h"
    character(len=*) :: correz, ch1z, ch2z, prfchz, ligrez
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
! person_in_charge: jacques.pellet at edf.fr
!-------------------------------------------------------------------
!     BUT : PROJETER UN CHAMP "CH1" SUIVANT "CORRES"
!           POUR CREER "CH2" SUR LA BASE "BASE"
!-------------------------------------------------------------------
!  IRET (OUT)  : = 0    : OK
!                = 1    : PB : ON N' A PAS PU PROJETER LE CHAMP
!                = 10   : ON NE SAIT PAS ENCORE FAIRE
!-------------------------------------------------------------------
    character(len=19) :: ch1, ch2, prfchn, ligrel
    character(len=16) :: corres, method
    character(len=1) :: base
!     TYCHV = NOEU OU ' ', SI TYCHV = NOEU CH2Z SERA UN CHAM_NO
    character(len=4) :: tychv
    character(len=*) :: prol0
    integer :: iret, jxxk1
!
!
    call jemarq()
    corres=correz
    ch1=ch1z
    ch2=ch2z
    prfchn=prfchz
    ligrel=ligrez
!
!
!     -- GLUTE MODIFICATION STRUCTURALE : (SI CORRES=' ')
    if (corres .ne. ' ') then
        call jeveuo(corres//'.PJXX_K1', 'L', jxxk1)
        method=zk24(jxxk1-1+3)
    else
        method='COLLOCATION'
    endif
!
!
    if (method .eq. 'COLLOCATION') then
        ASSERT(tychv.eq.' ' .or. tychv.eq.'NOEU')
        call pjefch(corres, ch1, ch2, tychv, prfchn,&
                    prol0, ligrel, base, iret)
!
    else if (method(1:10).eq.'NUAGE_DEG_') then
        call pjngch(ch1, ch2, corres, base)
        iret=0
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine

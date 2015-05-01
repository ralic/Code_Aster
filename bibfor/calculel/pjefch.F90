subroutine pjefch(corres, ch1, ch2, tychv, prfchn,&
                  prol0, ligrel, base, iret)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
!-------------------------------------------------------------------
!     BUT : PROJETER UN CHAMP "CH1" SUIVANT "CORRES"
!           POUR CREER "CH2" SUR LA BASE "BASE"
!-------------------------------------------------------------------
!  IRET (OUT)  : = 0    : OK
!                = 1    : PB : ON N' A PAS PU PROJETER LE CHAMP
!                = 10   : ON NE SAIT PAS ENCORE FAIRE
!-------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescns.h"
#include "asterfort/cesprj.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnsprj.h"
#include "asterfort/cnsprm.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
    character(len=19) :: ch1, ch2, ch0s, ch1s, ch2s, ch3s, prfchn, ligrel
    character(len=16) :: option, corres
    character(len=4) :: tych, tychv
    character(len=1) :: base
    character(len=*) :: prol0
    integer :: iret, ibid,  nncp
    character(len=24), pointer :: celk(:) => null()
!
!
    ch0s = '&&PJEFCH'//'.CH0S'
    ch1s = '&&PJEFCH'//'.CH1S'
    ch2s = '&&PJEFCH'//'.CH2S'
    ch3s = '&&PJEFCH'//'.CH3S'
    iret = 0
!
    call dismoi('TYPE_CHAMP', ch1, 'CHAMP', repk=tych)
!
!
!     1 : TRANSFORMATION DE CH1 EN CHAMP SIMPLE : CH1S
!     -------------------------------------------------
    if (tych .eq. 'NOEU') then
        call cnocns(ch1, 'V', ch1s)
!
    else if ((tych.eq.'ELEM') .or. (tych.eq.'ELNO')) then
        if (corres .eq. ' ') then
!         -- CAS MODIFICATION STRUCTURALE :
            tych = 'NOEU'
            call celces(ch1, 'V', ch0s)
            call cescns(ch0s, ' ', 'V', ch1s, ' ',&
                        ibid)
            call detrsd('CHAM_ELEM_S', ch0s)
        else
            call celces(ch1, 'V', ch1s)
        endif
!
    else
!       -- ON NE SAIT PAS ENCORE TRAITER LES CART ET ELGA:
        iret = 10
        goto 10
!
    endif
!
!
!     2 : PROJECTION DU CHAMP SIMPLE : CH1S -> CH2S
!     ----------------------------------------------
    if (corres .eq. ' ') then
!       -- CAS MODIFICATION STRUCTURALE : PROJECTION SUR MAILLAGE MESURE
        call cnsprm(ch1s, 'V', ch2s, iret)
!
    else
        if (tych .eq. 'NOEU') then
            call cnsprj(ch1s, corres, 'V', ch2s, iret)
!
        else if ((tych.eq.'ELEM') .or. (tych.eq.'ELNO')) then
            call cesprj(ch1s, corres, 'V', ch2s, iret)
        endif
    endif
    if (iret .gt. 0) goto 10
!
!
!     3 : TRANSFORMATION DE CH2S EN CHAMP : CH2
!     ----------------------------------------------
    if (tychv .eq. 'NOEU' .and. tych(1:2) .eq. 'EL') then
        call cescns(ch2s, ' ', 'V', ch3s, 'F',&
                    iret)
        ASSERT(iret.eq.0)
        call cnscno(ch3s, prfchn, prol0, base, ch2,&
                    'A', iret)
        ASSERT(iret.eq.0)
        call detrsd('CHAM_NO_S', ch3s)
!
    else if (tych.eq.'NOEU') then
        call cnscno(ch2s, prfchn, prol0, base, ch2,&
                    'A', iret)
!
    else if ((tych.eq.'ELEM') .or. (tych.eq.'ELNO')) then
        call jeveuo(ch1//'.CELK', 'L', vk24=celk)
        option = celk(2)
        if (ligrel .eq. ' ') then
            call utmess('F', 'CALCULEL4_73')
        endif
        call cescel(ch2s, ligrel, option, ' ', prol0,&
                    nncp, base, ch2, 'A', iret)
    endif
!
!
!
 10 continue
    call detrsd('CHAM_NO_S', ch1s)
    call detrsd('CHAM_NO_S', ch2s)
    call detrsd('CHAM_ELEM_S', ch1s)
    call detrsd('CHAM_ELEM_S', ch2s)
end subroutine

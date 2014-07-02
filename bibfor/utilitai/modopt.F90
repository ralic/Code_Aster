subroutine modopt(resuco, modele, lesopt, nbopt)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/exithm.h"
#include "asterfort/exixfe.h"
#include "asterfort/indk16.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rschex.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbopt
    character(len=8) :: resuco, modele
    character(len=24) :: lesopt
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
!    CETTE ROUTINE AJOUTE DES OPTIONS DE CALCUL SI CELLES-CI
!    ONT ETE OMISES :
!
!    SI ERZ1_ELEM ALORS SIEF_ELGA PUIS SIZ1_NOEU
!    SI SIZ1_NOEU ALORS SIEF_ELGA
!    SI ERZ2_ELEM ALORS SIEF_ELGA PUIS SIZ2_NOEU
!    SI SIZ2_NOEU ALORS SIEF_ELGA
!    SI ERME_ELEM ALORS SIGM_ELNO ou SIEF_ELNO
!    SI ERTH_ELEM ALORS FLUX_ELNO
!
!    SI XXXX_NOEU ALORS XXXX_ELNO
!    SI XXXX_ELNO ALORS XXXX_ELEM
!
! ----------------------------------------------------------------------
    integer :: j, jopt, jopt2
    integer :: i, iret, irxfem, nbopt2
    integer :: ierz1, ierz2, inoz1, inoz2, ierth, ierto, iertno
    integer :: ierme, iermo, iermno, iqirel, iqiren
    character(len=16) :: tysd
    character(len=24) :: lesop2
    aster_logical :: yathm, perman
!
! DEB ------------------------------------------------------------------
!
    call jemarq()
!
    call gettco(resuco, tysd)
    call jeveuo(lesopt, 'L', jopt)
!
    ierz1 = indk16( zk16(jopt), 'ERZ1_ELEM', 1, nbopt )
!
    ierz2 = indk16( zk16(jopt), 'ERZ2_ELEM', 1, nbopt )
!
    inoz1 = indk16( zk16(jopt), 'SIZ1_NOEU', 1, nbopt )
!
    inoz2 = indk16( zk16(jopt), 'SIZ2_NOEU', 1, nbopt )
!
    ierth = indk16( zk16(jopt), 'ERTH_ELEM', 1, nbopt )
!
    ierto = indk16( zk16(jopt), 'ERTH_ELNO', 1, nbopt )
!
    iertno = indk16( zk16(jopt), 'ERTH_NOEU', 1, nbopt )
!
    ierme = indk16( zk16(jopt), 'ERME_ELEM', 1, nbopt )
!
    iermo = indk16( zk16(jopt), 'ERME_ELNO', 1, nbopt )
!
    iermno = indk16( zk16(jopt), 'ERME_NOEU', 1, nbopt )
!
    iqirel = indk16( zk16(jopt), 'QIRE_ELNO', 1, nbopt )
!
    iqiren = indk16( zk16(jopt), 'QIRE_NOEU', 1, nbopt )
!
    if (ierz1+ierz2+inoz1+inoz2+ierth+ierto+iertno+ierme+iermo+iermno+iqirel+iqiren .eq. 0) then
        goto 999
    endif
!
    lesop2 = '&&OP0058.NEW_OPTION'
    call jedupo(lesopt, 'V', lesop2, .false._1)
    call jedetr(lesopt)
    call wkvect(lesopt, 'V V K16', nbopt+20, jopt)
    call jeveuo(lesop2, 'L', jopt2)
!
    nbopt2 = 1
!
!     EST-CE DU XFEM ?
    call exixfe(modele, irxfem)
    if (irxfem .ne. 0) then
        call rschex(resuco, 'SISE_ELNO', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'SISE_ELNO'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
!
!     CHAMPS ERME_XXXX
!
    if ((ierme.ne.0) .or. (iermo.ne.0) .or. (iermno.ne.0)) then
!        EST-CE DE LA THM ?
        call exithm(modele, yathm, perman)
        if (yathm) then
            call rschex(resuco, 'SIEF_ELNO', iret)
            if (iret .eq. 0) then
                zk16(jopt+nbopt2-1) = 'SIEF_ELNO'
                nbopt2 = nbopt2 + 1
            endif
        else
            call rschex(resuco, 'SIGM_ELNO', iret)
            if (iret .eq. 0) then
                zk16(jopt+nbopt2-1) = 'SIGM_ELNO'
                nbopt2 = nbopt2 + 1
            endif
        endif
    endif
!
    if ((iermo.ne.0) .or. (iermno.ne.0)) then
        call rschex(resuco, 'ERME_ELEM', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'ERME_ELEM'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
    if (iermno .ne. 0) then
        call rschex(resuco, 'ERME_ELNO', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'ERME_ELNO'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
!
!     CHAMPS ERZX_ELEM ET SIZX_NOEU
!
    if (( ierz1 .ne. 0 ) .or. ( ierz2 .ne. 0 ) .or. ( inoz1 .ne. 0 ) .or. ( inoz2 .ne. 0 )) then
        if (tysd .eq. 'EVOL_ELAS') then
            call rschex(resuco, 'SIEF_ELGA', iret)
            if (iret .eq. 0) then
                call utmess('A', 'UTILITAI2_52')
                zk16(jopt+nbopt2-1) = 'SIEF_ELGA'
                nbopt2 = nbopt2 + 1
            endif
        endif
    endif
!
    if (ierz1 .ne. 0) then
        call rschex(resuco, 'SIZ1_NOEU', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'SIZ1_NOEU'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
    if (ierz2 .ne. 0) then
        call rschex(resuco, 'SIZ2_NOEU', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'SIZ2_NOEU'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
!
!     CHAMPS ERTH_XXXX
!
    if ((ierth.ne.0) .or. (ierto.ne.0) .or. (iertno.ne.0)) then
        call rschex(resuco, 'FLUX_ELNO', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'FLUX_ELNO'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
    if ((ierto.ne.0) .or. (iertno.ne.0)) then
        call rschex(resuco, 'ERTH_ELEM', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'ERTH_ELEM'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
    if (iertno .ne. 0) then
        call rschex(resuco, 'ERTH_ELNO', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'ERTH_ELNO'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
!
!     CHAMPS QIRE_XXXX
!
    if ((iqirel.ne.0) .or. (iqiren.ne.0)) then
        call rschex(resuco, 'QIRE_ELEM', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'QIRE_ELEM'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
    if (iqiren .ne. 0) then
        call rschex(resuco, 'QIRE_ELNO', iret)
        if (iret .eq. 0) then
            zk16(jopt+nbopt2-1) = 'QIRE_ELNO'
            nbopt2 = nbopt2 + 1
        endif
    endif
!
    nbopt2 = nbopt2 - 1
!
    do i = 1, nbopt
        do j = 1, nbopt2
            if (zk16(jopt2+i-1) .eq. zk16(jopt+j-1)) goto 15
        enddo
        nbopt2 = nbopt2 + 1
        zk16(jopt+nbopt2-1) = zk16(jopt2+i-1)
 15     continue
    end do
!
    nbopt = nbopt2
    call jedetr(lesop2)
!
999 continue
    call jedema()
end subroutine

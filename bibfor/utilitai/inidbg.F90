subroutine inidbg()
    implicit none
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
!
!  BUT: INITIALISE LE COMMON CZDBG
!       POUR FILTRER L'IMPRESSION DES MESSAGES EN NIVEAU 2
!
!     COMMON /CZDBG/CZCONT,CZMECA,CZPILO
!
!     CZCONT='CONTACT' SI ON SOUHAITE IMPRIMER LES MESSAGES DU CONTACT
!     CZMECA='MECA_NON_LINE' POUR LES MESSAGES DEDIES A 'MECA_NON_LINE'
!     CZPILO='PILOTE' POUR LES MESSAGES DEDIES AU PILOTAGE
!     CZAPPA='APPARIEMENT' POUR LES MESSAGES DEDIES A L'APPARIEMENT
!     CZFACT='FACTOR' POUR LES MESSAGES DEDIES A LA FACTORISATION
!
!-----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: ifm, niv, n, i, jdbg
    character(len=16) :: czcont, czmeca, czpilo, czfact, czappa
    common /czdbg/czcont,czmeca,czpilo,czfact,czappa
!
    call jemarq()
!
    czcont=' '
    czmeca=' '
    czpilo=' '
    czfact=' '
    czappa=' '
!
    call infniv(ifm, niv)
    if (niv .ne. 2) goto 9999
!
    call getvtx(' ', 'INFO_DBG', nbval=0, nbret=n)
    if (n .ne. 0) then
!        -- ON IMPRIME UNIQUEMENT CE QUI EST DEMANDE --
        n=-n
        call wkvect('&&INIDBG', 'V V K16', n, jdbg)
        call getvtx(' ', 'INFO_DBG', nbval=n, vect=zk16(jdbg))
        do 10 i = 1, n
            if (zk16(jdbg+i-1)(1:7) .eq. 'CONTACT') then
                czcont='CONTACT'
                else if(zk16(jdbg+i-1).eq.'MECA_NON_LINE' .or. zk16(jdbg+&
            i-1).eq.'MECANONLINE')then
                czmeca='MECA_NON_LINE'
            else if (zk16(jdbg+i-1).eq.'PILOTAGE') then
                czpilo='PILOTAGE'
            else if (zk16(jdbg+i-1).eq.'APPARIEMENT') then
                czappa='APPARIEMENT'
            else if (zk16(jdbg+i-1).eq.'FACTORISATION') then
                czfact='FACTORISATION'
            endif
10      continue
    else
!        -- ON IMPRIME TOUT SANS RESTRICTION --
        czcont='CONTACT'
        czmeca='MECA_NON_LINE'
        czpilo='PILOTE'
        czfact='FACTORISATION'
        czappa='APPARIEMENT'
    endif
!
    call jedetr('&&INIDBG')
!
9999  continue
!
    call jedema()
!
end subroutine

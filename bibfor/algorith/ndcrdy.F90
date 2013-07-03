subroutine ndcrdy(result, sddyna)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: result
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNEES)
!
! CREATION SDDYNA (DYNAMIQUE)
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DU CONCEPT RESULTAT
! IN  SDSYNA : SD DYNAMIQUE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: tsch, psch, losd, nosd, tfor, cfsc
    integer :: jtsch, jpsch, jlosd, jnosd, jtfor, jcfsc
    character(len=24) :: tcha, ncha, veol, vaol
    integer :: jtcha, jncha, jveol, jvaol
    character(len=24) :: vecent, vecabs
    integer :: jvecen, jvecab
    integer :: ifm, niv
    character(len=16) :: k16bid, nomcmd
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- OPERATEUR APPELANT (STATIQUE OU DYNAMIQUE)
!
    call getres(k8bid, k16bid, nomcmd)
!
! --- INITIALISATIONS
!
    sddyna = result(1:8)//'.SDDYNA'
!
! --- OBJET PRINCIPAL
!
    tsch = sddyna(1:15)//'.TYPE_SCH'
    call wkvect(tsch, 'V V K16', 9, jtsch)
!
    if (nomcmd(1:4) .eq. 'STAT') then
        zk16(jtsch+1-1) = 'STATIQUE'
        goto 999
    else
        zk16(jtsch+1-1) = 'DYNAMIQUE'
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... CREATION SD DYNAMIQUE'
        endif
    endif
!
! --- DEFINITION ET CREATION DES AUTRES OBJETS DE LA SD SDDYNA
!
    psch = sddyna(1:15)//'.PARA_SCH'
    losd = sddyna(1:15)//'.INFO_SD'
    nosd = sddyna(1:15)//'.NOM_SD'
    tfor = sddyna(1:15)//'.TYPE_FOR'
    cfsc = sddyna(1:15)//'.COEF_SCH'
    tcha = sddyna(1:15)//'.TYPE_CHA'
    ncha = sddyna(1:15)//'.NBRE_CHA'
    veol = sddyna(1:15)//'.VEEL_OLD'
    vaol = sddyna(1:15)//'.VEAS_OLD'
    vecent = sddyna(1:15)//'.VECENT'
    vecabs = sddyna(1:15)//'.VECABS'
!
    call wkvect(psch, 'V V R', 7, jpsch)
    call wkvect(losd, 'V V L', 16, jlosd)
    call wkvect(nosd, 'V V K24', 5, jnosd)
    call wkvect(tfor, 'V V I', 2, jtfor)
    call wkvect(cfsc, 'V V R', 24, jcfsc)
    call wkvect(tcha, 'V V K24', 4, jtcha)
    call wkvect(ncha, 'V V I', 5, jncha)
    call wkvect(veol, 'V V K24', 15, jveol)
    call wkvect(vaol, 'V V K24', 15, jvaol)
    call wkvect(vecent, 'V V K24', 3, jvecen)
    call wkvect(vecabs, 'V V K24', 3, jvecab)
!
999  continue
!
    call jedema()
!
end subroutine

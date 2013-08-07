subroutine rsmena(resu)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jelstc.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
    character(len=*) :: resu
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
! BUT : DETRUIRE LES OBJETS JEVEUX INUTILISES DANS UNE SD_RESULTAT
!     ------------------------------------------------------------------
!
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: n1, n2, k, jlist, nbcon, ibid, nbordr, jordr, jcoche
    integer :: i, nbnosy, jtach, j, iret, ierd, i1, jrs24
    character(len=8) :: kbid, tych, res8
    character(len=16) :: nomsym
    character(len=19) :: res19, noco19
    character(len=24) :: cham, noobj
    real(kind=8) :: r8b
    complex(kind=8) :: c16b
!     ------------------------------------------------------------------
    call jemarq()
    res19=resu
    res8=resu
!
!--------------------------------------------------------
!     IL FAUT DERUIRE :
!         * LES PROF_CHNO INUTILISES
!         * LES EXCIT INUTILISES
!         * LES LIGREL INUTILISES
!--------------------------------------------------------
!
!     0. CREATION DE 1 OBJET :
!     * LOBJ : LISTE DES OBJETS EXISTANT DANS LA SD_RESULTAT
    call jelstc('G', res8, 1, 0, kbid,&
                n1)
    call wkvect('&&RSMENA.LOBJ', 'V V K24', -n1, jlist)
    call jelstc('G', res8, 1, -n1, zk24(jlist),&
                n2)
!
!
!     1. LES PROF_CHNO :
!     ------------------
!
!     1.1 LISTE DES PROF_CHNO EXISTANTS
    call jecreo('&&RSMENA.DICO', 'V N K24')
    call jeecra('&&RSMENA.DICO', 'NOMMAX', n2)
    nbcon=0
    do 10,k=1,n2
    noobj=zk24(jlist-1+k)
    if (noobj(20:24) .eq. '.DEEQ') then
        nbcon=nbcon+1
        noco19=noobj(1:19)
        call jecroc(jexnom('&&RSMENA.DICO', noco19))
    endif
    10 end do
    if (nbcon .eq. 0) goto 50
!
!     1.2 ON "COCHE" LES  PROF_CHNO REFERENCES :
    call jelira(res19//'.DESC', 'NOMMAX', nbnosy)
    call rsorac(res19, 'LONUTI', ibid, r8b, kbid,&
                c16b, r8b, kbid, nbordr, 1,&
                ibid)
    call wkvect('&&RSMENA.NUME_ORDRE', 'V V I', nbordr, jordr)
    call rsorac(res19, 'TOUT_ORDRE', ibid, r8b, kbid,&
                c16b, r8b, kbid, zi(jordr), nbordr,&
                ibid)
    call wkvect('&&RSMENA.COCHE', 'V V I', nbcon, jcoche)
!
    do 30 i = 1, nbnosy
        call jenuno(jexnum(res19//'.DESC', i), nomsym)
        call jeveuo(jexnum(res19//'.TACH', i), 'E', jtach)
        do 20 j = 1, nbordr
            if (zk24(jtach+j-1)(1:1) .eq. ' ') goto 20
            call rsexch('F', res19, nomsym, zi(jordr-1+j), cham,&
                        iret)
            call dismoi('F', 'TYPE_CHAMP', cham, 'CHAMP', ibid,&
                        tych, ierd)
            if (tych .ne. 'NOEU') goto 30
            call dismoi('F', 'PROF_CHNO', cham, 'CHAMP', ibid,&
                        noco19, ierd)
            call jenonu(jexnom('&&RSMENA.DICO', noco19), i1)
            ASSERT(i1.gt.0 .and. i1.le.nbcon)
            zi(jcoche-1+i1)=1
20      continue
30  end do
!
!     1.2 ON DETRUIT LES  PROF_CHNO NON REFERENCES :
    do 40,k=1,nbcon
    if (zi(jcoche-1+k) .eq. 0) then
        call jenuno(jexnum('&&RSMENA.DICO', k), noco19)
        call detrsd('PROF_CHNO', noco19)
    endif
    40 end do
50  continue
!
!
!
!     2. LES LISTE_CHARGE (EXCIT) :
!     ------------------------------
    call jedetr('&&RSMENA.DICO')
    call jedetr('&&RSMENA.COCHE')
!
!     2.1 LISTE DES LISTE_CHARGE EXISTANTS
    call jecreo('&&RSMENA.DICO', 'V N K24')
    call jeecra('&&RSMENA.DICO', 'NOMMAX', n2)
    nbcon=0
    do 60,k=1,n2
    noobj=zk24(jlist-1+k)
    if (noobj(20:24) .eq. '.LCHA') then
        nbcon=nbcon+1
        noco19=noobj(1:19)
        call jecroc(jexnom('&&RSMENA.DICO', noco19))
    endif
    60 end do
    if (nbcon .eq. 0) goto 90
!
!     2.2 ON "COCHE" LES  LISTE_CHARGE REFERENCES :
    call jelira(res19//'.RS24', 'LONMAX', n1)
    call jeveuo(res19//'.RS24', 'L', jrs24)
    call wkvect('&&RSMENA.COCHE', 'V V I', nbcon, jcoche)
!
    do 70 i = 1, n1
        if (zk24(jrs24+i-1)(14:19) .ne. '.EXCIT') goto 70
        noco19=zk24(jrs24+i-1)(1:19)
        call jenonu(jexnom('&&RSMENA.DICO', noco19), i1)
        ASSERT(i1.gt.0 .and. i1.le.nbcon)
        zi(jcoche-1+i1)=1
70  end do
!
!     2.3 ON DETRUIT LES  LISTE_CHARGE NON REFERENCES :
    do 80,k=1,nbcon
    if (zi(jcoche-1+k) .eq. 0) then
        call jenuno(jexnum('&&RSMENA.DICO', k), noco19)
        call detrsd('LISTE_CHARGES', noco19)
    endif
    80 end do
90  continue
!
!
!
!     3. LES LIGREL REFERENCES PAR LES CHAM_ELEM
!     ------------------------------
    call jedetr('&&RSMENA.DICO')
    call jedetr('&&RSMENA.COCHE')
!
!     3.1 LISTE DES LIGREL EXISTANTS
    call jecreo('&&RSMENA.DICO', 'V N K24')
    call jeecra('&&RSMENA.DICO', 'NOMMAX', n2)
    nbcon=0
    do 100,k=1,n2
    noobj=zk24(jlist-1+k)
    if (noobj(20:24) .eq. '.LIEL') then
        nbcon=nbcon+1
        noco19=noobj(1:19)
        call jecroc(jexnom('&&RSMENA.DICO', noco19))
    endif
    100 end do
    if (nbcon .eq. 0) goto 140
!
!     3.2 ON "COCHE" LES LIGREL REFERENCES :
    call wkvect('&&RSMENA.COCHE', 'V V I', nbcon, jcoche)
!
    do 120 i = 1, nbnosy
        call jenuno(jexnum(res19//'.DESC', i), nomsym)
        call jeveuo(jexnum(res19//'.TACH', i), 'E', jtach)
        do 110 j = 1, nbordr
            if (zk24(jtach+j-1)(1:1) .eq. ' ') goto 110
            call rsexch('F', res19, nomsym, zi(jordr-1+j), cham,&
                        iret)
            call dismoi('F', 'TYPE_CHAMP', cham, 'CHAMP', ibid,&
                        tych, ierd)
            if (tych(1:2) .ne. 'EL') goto 120
            call dismoi('F', 'NOM_LIGREL', cham, 'CHAMP', ibid,&
                        noco19, ierd)
            call jenonu(jexnom('&&RSMENA.DICO', noco19), i1)
            if (i1 .gt. 0 .and. i1 .le. nbcon) zi(jcoche-1+i1)=1
110      continue
120  end do
!
!     3.2 ON DETRUIT LES LIGREL NON REFERENCES :
    do 130,k=1,nbcon
    if (zi(jcoche-1+k) .eq. 0) then
        call jenuno(jexnum('&&RSMENA.DICO', k), noco19)
        call detrsd('LIGREL', noco19)
    endif
    130 end do
140  continue
!
!
!
!     -- MENAGE :
!     -----------
    call jedetr('&&RSMENA.LOBJ')
    call jedetr('&&RSMENA.DICO')
    call jedetr('&&RSMENA.NUME_ORDRE')
    call jedetr('&&RSMENA.COCHE')
!
    call jedema()
end subroutine

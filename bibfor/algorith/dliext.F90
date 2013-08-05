subroutine dliext()
    implicit none
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
!     ------------------------------------------------------------------
!
!     COMMANDE : DEFI_LIST_ENTI/OPERATION='NUME_ORDRE'
!
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
    character(len=8) :: resu, k8b, kbid
    character(len=16) :: nomcmd, concep, param
    character(len=19) :: sdresu, resu19
    character(len=24) :: knum
    real(kind=8) :: inter2(2), r8b, vpara
    complex(kind=8) :: c16b
    integer :: ibid, n1, n2, n3, nbordr, k, iord, k1, jordr, nbvale, jvale, iad
    integer :: jnbpa, jbint, jlpas
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
    knum = '&&DLIEXT.KNUM'
!
    call getres(resu, concep, nomcmd)
!
    call getvid(' ', 'RESULTAT', 0, iarg, 1,&
                sdresu, n1)
    resu19 = resu
    call getvtx(' ', 'PARAMETRE', 0, iarg, 1,&
                param, n2)
    call getvr8(' ', 'INTERV_R', 0, iarg, 2,&
                inter2, n3)
    ASSERT(n1+n2+n3.eq.4)
!
!
!
!
!     1) CALCUL DE LA LISTE DES NUMEROS D'ORDRE (KNUM) :
!     ----------------------------------------------------
!     -- ON PARCOURT TOUS LES NUME_ORDRE ET ON NE CONSERVE
!        QUE CEUX QUI SONT DANS L'INTERVALLE
!        ATTENTION : ON LIT ET ECRIT DANS KNUM
    call rsorac(sdresu, 'LONUTI', ibid, r8b, k8b,&
                c16b, r8b, k8b, nbordr, 1,&
                ibid)
    call wkvect(knum, 'V V I', nbordr, jordr)
    call rsorac(sdresu, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbordr,&
                ibid)
    k1 = 0
    do 10,k = 1,nbordr
    iord = zi(jordr-1+k)
    call rsadpa(sdresu, 'L', 1, param, iord,&
                0, iad, kbid)
    ASSERT(iad.ne.0)
    vpara = zr(iad)
    if (vpara .ge. inter2(1) .and. vpara .le. inter2(2)) then
        k1 = k1 + 1
        zi(jordr-1+k1) = iord
    endif
    10 end do
    nbordr = k1
    nbvale = nbordr
    ASSERT(nbordr.gt.0)
!
!
!     2) CREATION DE LA STRUCTURE DE DONNEES :
!     ----------------------------------------------------
    call wkvect(resu19//'.VALE', 'G V I', nbvale, jvale)
    call wkvect(resu19//'.BINT', 'G V I', nbvale, jbint)
    do 20,k = 1,nbvale
    zi(jvale-1+k) = zi(jordr-1+k)
    zi(jbint-1+k) = zi(jordr-1+k)
    20 end do
!
    if (nbvale .gt. 1) then
        call wkvect(resu19//'.NBPA', 'G V I', nbvale-1, jnbpa)
        call wkvect(resu19//'.LPAS', 'G V I', nbvale-1, jlpas)
        do 21,k = 1,nbvale-1
        zi(jnbpa-1+k) = 1
        zi(jlpas-1+k) = zi(jordr-1+k+1)-zi(jordr-1+k)
21      continue
    else
        call wkvect(resu19//'.NBPA', 'G V I', 1, jnbpa)
        call wkvect(resu19//'.LPAS', 'G V I', 1, jlpas)
    endif
!
!
!
    call jedema()
end subroutine

subroutine cgnoiv(iocc, nomaz, lisnoz, nbno)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iocc, nbno
    character(len=*) :: nomaz, lisnoz
! person_in_charge: jacques.pellet at edf.fr
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
!
!       CGNOIV -- TRAITEMENT DE L'OPTION INTERVALLE_VALE
!                 DU MOT FACTEUR CREA_GROUP_NO DE
!                 LA COMMANDE DEFI_GROUP
!
! -------------------------------------------------------
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS RETENUS
!  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
    integer :: n1, k, nbnot, ino, jtrav, ncmp
    integer :: jlisno, jcn2v, jcn2l, jcn2k
    character(len=3) :: tsca
    character(len=8) :: nocmp, noma, ma1, nomgd
    character(len=16) :: motfac
    character(len=19) :: cham19, cns1, cns2
    character(len=24) :: lisnoi, valk(5)
    real(kind=8) :: valr(2), vmin, vmax, v1
!     -----------------------------------------------------------------
!
    call jemarq()
!
    motfac='CREA_GROUP_NO'
    lisnoi=lisnoz
    noma=nomaz
    nbno=0
!
    call getvid(motfac, 'CHAM_GD', iocc=iocc, scal=cham19, nbret=n1)
    call getvtx(motfac, 'NOM_CMP', iocc=iocc, scal=nocmp, nbret=n1)
    call getvr8(motfac, 'VALE', iocc=iocc, nbval=2, vect=valr(1),&
                nbret=n1)
    ASSERT(n1.eq.2)
    vmin=valr(1)
    vmax=valr(2)
    if (vmin .gt. vmax) goto 30
!
    call dismoi('NOM_MAILLA', cham19, 'CHAMP', repk=ma1)
    if (noma .ne. ma1) then
        valk(1)=cham19
        valk(2)=noma
        call utmess('F', 'CALCULEL2_50', nk=2, valk=valk)
    endif
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnot)
!
    cns1='&&CGNOIV.CNS1'
    cns2='&&CGNOIV.CNS2'
    call cnocns(cham19, 'V', cns1)
    ncmp=1
    call cnsred(cns1, 0, [0], ncmp, nocmp,&
                'V', cns2)
!
    call jeveuo(cns2//'.CNSK', 'L', jcn2k)
    call jeveuo(cns2//'.CNSV', 'L', jcn2v)
    call jeveuo(cns2//'.CNSL', 'L', jcn2l)
    nomgd=zk8(jcn2k-1+2)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    ASSERT(tsca.eq.'R'.or.tsca.eq.'I')
!
    call wkvect('&&CGNOIV.LISNO', 'V V I', nbnot, jtrav)
    do ino = 1, nbnot
        if (.not.zl(jcn2l-1+(ino-1)*ncmp+1)) goto 10
        if (tsca .eq. 'R') then
            v1=zr(jcn2v-1+(ino-1)*ncmp+1)
        else if (tsca.eq.'I') then
            v1=zi(jcn2v-1+(ino-1)*ncmp+1)
        endif
        if (v1 .ge. vmin .and. v1 .le. vmax) then
            nbno=nbno+1
            zi(jtrav-1+nbno)=ino
        endif
 10     continue
    end do
!
!
!
! --- ALLOCATION DU VECTEUR DES NUMEROS DES MAILLES RETENUES
!     --------------------------------------------------------
    call wkvect(lisnoi, 'V V I', max(nbno, 1), jlisno)
    do k = 1, nbno
        zi(jlisno-1+k)=zi(jtrav-1+k)
    end do
!
!
! --- MENAGE :
    call jedetr('&&CGNOIV.LISNO')
    call detrsd('CHAM_NO_S', cns1)
    call detrsd('CHAM_NO_S', cns2)
!
 30 continue
    call jedema()
!
end subroutine

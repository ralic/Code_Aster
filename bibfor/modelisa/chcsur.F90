subroutine chcsur(chcine, cnsz, type, mo, nomgd)
    implicit none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=1) :: type
    character(len=8) :: nomgd
    character(len=*) :: chcine, cnsz, mo
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! OBJET : CREATION D"UNE CHARGE CINEMATIQUE.
!        1) LE .REFE DE LA CHARGE DOIT DEJA EXISTER
!        2) MISE A JOUR DE : .AFCI ET .AFCV
!-----------------------------------------------------------------------
! OUT  CHCINE  K*19    : NOM DE LA CHARGE CINEMATIQUE
! IN   CNS     K*19    : NOM D'UN CHAM_NO_S CONTENANT LES DEGRES IMPOSES
! IN   TYPE    K*1     : 'R','C' OU 'F' TYPE DE LA CHARGE
! IN   MO      K*      : NOM DU MODELE
! IN   NOMGD   K*      : NOM DE LA GRANDEUR
!-----------------------------------------------------------------------
!
    integer :: nbloc, ibloc, icmp, ncmp, ino, nbno, nbec, ier, ii
    integer :: jcnsd, jcnsv, jcnsl, jafci, jafcv, iaprnm, jcnsc, kcmp
    integer :: jcmp, ncmpmx, jcorr
    character(len=8) :: k8b, nomo
    character(len=19) :: chci, cns
    character(len=24) :: cafci, cafcv
!
    data cafci /'                   .AFCI'/
    data cafcv /'                   .AFCV'/
!
! --- DEBUT -----------------------------------------------------------
!
    call jemarq()
!
    nomo = mo
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nbec,&
                k8b, ier)
    call jeveuo(nomo//'.MODELE    .PRNM', 'L', iaprnm)
!
    chci = chcine
    cns = cnsz
    cafci(1:19) = chci
    cafcv(1:19) = chci
!
    call jeveuo(cns//'.CNSD', 'L', jcnsd)
    call jeveuo(cns//'.CNSC', 'L', jcnsc)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    nbno = zi(jcnsd)
    ncmp = zi(jcnsd+1)
!
!
!     -- ON CALCULE LA CORRESPONDANCE ENTRE LES CMPS DE CNS
!        ET CELLES DE LA GRANDEUR.
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx)
    call wkvect('&&CHCSUR.CORRES', 'V V I', ncmpmx, jcorr)
    do 10, kcmp=1,ncmpmx
    icmp = indik8(zk8(jcnsc),zk8(jcmp-1+kcmp),1,ncmp)
    zi(jcorr-1+kcmp)=icmp
    10 end do
!
!
!     -- CALCUL DE NBLOC :
    nbloc = 0
    do 100 icmp = 1, ncmp
        do 110 ino = 1, nbno
            if (zl(jcnsl+(ino-1)*ncmp+icmp-1)) nbloc = nbloc + 1
110      continue
100  end do
!
!
!     -- CREATION DE LA SD
    call wkvect(cafci, 'G V I', (3*nbloc+1), jafci)
    if (type .eq. 'R') then
        call wkvect(cafcv, 'G V R', max(nbloc, 1), jafcv)
    else if (type.eq.'C') then
        call wkvect(cafcv, 'G V C', max(nbloc, 1), jafcv)
    else if (type.eq.'F') then
        call wkvect(cafcv, 'G V K8', max(nbloc, 1), jafcv)
    endif
!
!
!     -- ON REMPLIT LES OBJETS .AFCI .AFCV
    ibloc = 0
    do 120 ino = 1, nbno
        ii = 0
        do 122 kcmp = 1, ncmpmx
            if (exisdg(zi(iaprnm-1+nbec*(ino-1)+1),kcmp)) then
                ii = ii + 1
                icmp=zi(jcorr-1+kcmp)
                if (icmp .eq. 0) goto 122
                if (zl(jcnsl+(ino-1)*ncmp+icmp-1)) then
                    ibloc = ibloc + 1
                    zi(jafci+3*(ibloc-1)+1) = ino
                    zi(jafci+3*(ibloc-1)+2) = ii
                    if (type .eq. 'R') then
                        zr(jafcv-1+ibloc) = zr(jcnsv+(ino-1)*ncmp+ icmp-1)
                    else if (type.eq.'C') then
                        zc(jafcv-1+ibloc) = zc(jcnsv+(ino-1)*ncmp+ icmp-1)
                    else if (type.eq.'F') then
                        zk8(jafcv-1+ibloc) = zk8(jcnsv+(ino-1)*ncmp+ icmp-1)
                    else
                        ASSERT(.false.)
                    endif
                endif
            endif
122      continue
120  end do
!
    if (ibloc .eq. 0) call u2mess('F', 'CALCULEL_9')
    zi(jafci) = ibloc
!
    call jedetr('&&CHCSUR.CORRES')
    call jedema()
end subroutine

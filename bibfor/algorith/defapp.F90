subroutine defapp(ma, geomi, alpha, depla, base,&
                  geomf)
    implicit none
#include "jeveux.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    real(kind=8) :: alpha
    character(len=1) :: base
    character(len=8) :: depla, k8b, ma
    character(len=19) :: geomi, geomf
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
!     ------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: iavalf, iavali, ibid, icmp, ino, nbno, ncmp, jno, ngst, nbno1
    integer :: nbno2, iem, kno, jgnap, jnoap, ngap, jgnst, nno, jnost, jcnsk
    integer :: jcnsd, jcnsc, jcnsv, jcnsl, noap, nost
    real(kind=8) :: rdepla
    character(len=1) :: bas2, tsca
    character(len=8) :: nomgd
    character(len=19) :: geomi2, geomf2, chamns
    character(len=24) :: karg
    integer :: iarg
!
! ----------------------------------------------------------------------
    call jemarq()
    geomi2 = geomi
    geomf2= geomf
    bas2 = base(1:1)
!
!     -- ON RECOPIE BESTIALEMENT LE CHAMP POUR CREER LE NOUVEAU:
    call copisd('CHAMP_GD', bas2, geomi2, geomf2)
!
! RECUPERATION DES GROUPES DE NOEUDS
!
    call getvem(ma, 'GROUP_NO', 'DEFORME', 'GROUP_NO_STRU', 1,&
                iarg, 0, k8b, ngst)
    call getvem(ma, 'GROUP_NO', 'DEFORME', 'GROUP_NO_APPUI', 1,&
                iarg, 0, k8b, ngap)
    if (ngst .ne. ngap) then
        call utmess('F', 'ALGORITH2_61')
    endif
!
! TRAITEMENT DU GROUP_NO_STRUC
!
    if (ngst .ne. 0) then
        ngst = -ngst
        nbno1 = 0
        call wkvect('&&DEFAPP.GNST', 'V V K24', ngst, jgnst)
        call getvem(ma, 'GROUP_NO', 'DEFORME', 'GROUP_NO_STRU', 1,&
                    iarg, ngst, zk24(jgnst), ibid)
! PREMIERE BOUCLE POUR COMPTER LE NOMBRE DE NOEUDS
!
        do iem = 1, ngst
            karg = zk24(jgnst-1+iem)
            call jelira(jexnom(ma//'.GROUPENO', karg), 'LONUTI', nno)
            nbno1 = nbno1 + nno
        end do
!
        call wkvect('&&DEFAPP.NOST', 'V V I', nbno1, jnost)
!
! DEUXIEME BOUCLE POUR RECUPERER LES NUMEROS DES NOEUDS
        nbno1 =0
        do iem = 1, ngst
            karg = zk24(jgnst-1+iem)
            call jelira(jexnom(ma//'.GROUPENO', karg), 'LONUTI', nno)
            call jeveuo(jexnom(ma//'.GROUPENO', karg), 'L', kno)
            do jno = 1, nno
                zi(jnost-1+nbno1+jno) = zi(kno+jno-1)
            end do
            nbno1 = nbno1 +nno
        end do
    endif
!
! TRAITEMENT DU GROUP_NO_APPUI
!
    if (ngap .ne. 0) then
        ngap = -ngap
        nbno2 = 0
        call wkvect('&&DEFAPP.GNAP', 'V V K24', ngst, jgnap)
        call getvem(ma, 'GROUP_NO', 'DEFORME', 'GROUP_NO_APPUI', 1,&
                    iarg, ngap, zk24(jgnap), ibid)
!
! PREMIERE BOUCLE POUR COMPTER LE NOMBRE DE NOEUDS
!
        do iem = 1, ngap
            karg = zk24(jgnap-1+iem)
            call jelira(jexnom(ma//'.GROUPENO', karg), 'LONUTI', nno)
            nbno2 = nbno2 + nno
        end do
!
        call wkvect('&&DEFAPP.NOAP', 'V V I', nbno2, jnoap)
!
! DEUXIEME BOUCLE POUR RECUPERER LES NUMEROS DES NOEUDS
        nbno2 = 0
        do iem = 1, ngap
            karg = zk24(jgnap-1+iem)
            call jelira(jexnom(ma//'.GROUPENO', karg), 'LONUTI', nno)
            call jeveuo(jexnom(ma//'.GROUPENO', karg), 'L', kno)
            do jno = 1, nno
                zi(jnoap-1+nbno2+jno)= zi(kno+jno-1)
            end do
            nbno2 = nbno2 +nno
        end do
    endif
    if (nbno1 .ne. nbno2) then
        call utmess('F', 'ALGORITH2_62')
    endif
!
    nbno = nbno1
    call jeveuo(geomi2//'.VALE', 'L', iavali)
    call jeveuo(geomf2//'.VALE', 'E', iavalf)
!
    chamns = '&&DEFAPP.CHAMNO'
    call cnocns(depla, 'V', chamns)
!
    call jeveuo(chamns//'.CNSK', 'L', jcnsk)
    call jeveuo(chamns//'.CNSD', 'L', jcnsd)
    call jeveuo(chamns//'.CNSC', 'L', jcnsc)
    call jeveuo(chamns//'.CNSV', 'L', jcnsv)
    call jeveuo(chamns//'.CNSL', 'L', jcnsl)
!
    ncmp = zi(jcnsd-1+2)
    nomgd = zk8(jcnsk-1+2)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    if (tsca .ne. 'R') then
        call utmess('F', 'ALGORITH2_63')
    endif
    do ino = 1, nbno
        noap = zi(jnoap-1+ino)
        nost = zi(jnost -1 +ino)
        do icmp = 1, ncmp
            if (zl(jcnsl-1+ (noap-1)*ncmp+icmp)) then
                rdepla = zr(jcnsv-1+(nost-1)*ncmp+icmp)
                zr(iavalf-1+3*(noap-1)+icmp)= zr(iavali-1+3*(noap-1)+&
                icmp)+ alpha * rdepla
            endif
        end do
    end do
    call cnscno(chamns, ' ', 'NON', 'V', depla,&
                'F', ibid)
!
! -- MENAGE
    call jedetr('&&DEFAPP.GNST')
    call jedetr('&&DEFAPP.NOST')
    call jedetr('&&DEFAPP.GNAP')
    call jedetr('&&DEFAPP.NOAP')
!
!
    call jedema()
end subroutine

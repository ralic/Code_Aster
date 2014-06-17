subroutine vrcin2(modele, chmat, carele, chvars)
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
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesvar.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: modele, chmat, carele
    character(len=19) :: chvars
! ======================================================================
!   BUT : ALLOUER LE CHAM_ELEM_S RESULTAT (CHVARS)
!         ET CREER UN OBJET CHMAT.CESVI QUI EST UN OBJET DE MEME
!         STRUCTURE QUE CHVARS.CESV
!
!   IN :
!     MODELE (K8)  IN/JXIN : SD MODELE
!     CHMAT  (K8)  IN/JXIN : SD CHAM_MATER
!     CARELE (K8)  IN/JXIN : SD CARA_ELEM (SOUS-POINTS)
!
!   OUT :
!     + CHVARS (K19) IN/JXOUT: SD CHAM_ELEM_S ELGA (VARI_R) A ALLOUER
!     + CREATION DE CHMAT//'.CESVI' V V I LONG=LONG(CHVARS//.CESL)
!        SI CHVARS.CESL(K)= .TRUE.
!           CHMAT.CESVI(K)= ICHS : RANG DANS CHMAT.LISTE_CH(:) DU CHAMP
!                                  A RECOPIER DANS CHVARS.CESV(K)
!
! ----------------------------------------------------------------------
!
!
    integer :: n1, iret, iad, ichs, nbchs, isp, ipt,  jcesvi
    integer :: k, k2, nbma, ncmp, icmp, jcesl2,  jcesd2
    integer :: jcesd, jcesl, ima, nbpt, nbsp, nbcvrc
    integer :: jdcld, jdcll
    character(len=16) :: tysd1, tysd2, nosd1, nosd2, nosy1, nosy2
    character(len=8) :: varc
    character(len=19) :: dceli, celmod, cart2, ces2, ligrmo
    character(len=24) :: valk(5)
    character(len=8), pointer :: cvrcvarc(:) => null()
    character(len=16), pointer :: liste_sd(:) => null()
    character(len=8), pointer :: cesk2(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    character(len=16), pointer :: cesv2(:) => null()
    integer, pointer :: dclv(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(chmat//'.CVRCVARC', 'L', vk8=cvrcvarc)
    call jelira(chmat//'.CVRCVARC', 'LONMAX', nbcvrc)
    ligrmo=modele//'.MODELE'
!
!     -- CALCUL DE JLISSD ET NBCHS :
    call jelira(chmat//'.LISTE_CH', 'LONMAX', nbchs)
    call jeveuo(chmat//'.LISTE_SD', 'L', vk16=liste_sd)
    call jelira(chmat//'.LISTE_SD', 'LONMAX', n1)
    ASSERT(n1.eq.nbchs*7)
!
!
!     1. ALLOCATION DE CHVARS ET DE CHMAT.CESVI:
!     ------------------------------------------
    dceli='&&VRCIN2.DCELI'
    celmod='&&VRCIN2.CELMOD'
    call cesvar(carele, ' ', ligrmo, dceli)
!
!     -- MODIFICATION DE DCELI : TOUTES LES MAILLES ONT
!        NBCVRC COMPOSANTES.
    call jeveuo(dceli//'.CESD', 'L', jdcld)
    call jeveuo(dceli//'.CESL', 'L', jdcll)
    call jeveuo(dceli//'.CESV', 'E', vi=dclv)
    nbma = zi(jdcld-1+1)
!
    do 170,ima = 1,nbma
    nbpt = zi(jdcld-1+5+4* (ima-1)+1)
    nbsp = max(1,zi(jdcld-1+5+4* (ima-1)+2))
    ASSERT(nbpt.eq.1)
    ASSERT(nbsp.eq.1)
    call cesexi('C', jdcld, jdcll, ima, 1,&
                1, 2, iad)
    if (iad .gt. 0) dclv(iad)=nbcvrc
    170 end do
!
    call alchml(ligrmo, 'INIT_VARC', 'PVARCPR', 'V', celmod,&
                iret, dceli)
    ASSERT(iret.eq.0)
    call detrsd('CHAMP', dceli)
    call celces(celmod, 'V', chvars)
    call detrsd('CHAMP', celmod)
!
    call jelira(chvars//'.CESV', 'LONMAX', n1)
    call wkvect(chmat//'.CESVI', 'V V I', n1, jcesvi)
!
    call jeveuo(chvars//'.CESK', 'L', vk8=cesk)
    call jeveuo(chvars//'.CESD', 'L', jcesd)
    call jeveuo(chvars//'.CESL', 'E', jcesl)
    call jelira(chvars//'.CESL', 'LONMAX', n1)
    do 777, k=1,n1
    zl(jcesl-1+k)=.false.
    777 end do
!
!
!
!     2. REMPLISSAGE DE CHMAT.CESVI :
!     ------------------------------------------
!
!     -- ON CHERCHE A BOUCLER SUR LES VARC.
!        POUR CELA ON BOUCLE SUR LES CVRC ET ON "SAUTE"
!        LES CVRC SUIVANTES (DE LA MEME VARC)
    varc=' '
    do 1, k=1,nbcvrc
    if (cvrcvarc(k) .eq. varc) goto 1
!
    varc=cvrcvarc(k)
    cart2 = chmat//'.'//varc//'.2'
    ces2='&&VRCIN2.CES2'
    call carces(cart2, 'ELEM', ' ', 'V', ces2,&
                'A', iret)
    ASSERT(iret.eq.0)
!
    call jeveuo(ces2//'.CESK', 'L', vk8=cesk2)
    call jeveuo(ces2//'.CESD', 'L', jcesd2)
    call jeveuo(ces2//'.CESV', 'L', vk16=cesv2)
    call jeveuo(ces2//'.CESL', 'L', jcesl2)
!
    if (cesk(1) .ne. cesk2(1)) then
        valk(1)=cesk(1)
        valk(2)=cesk2(1)
        call utmess('F', 'CALCULEL2_11', nk=2, valk=valk)
    endif
    nbma = zi(jcesd-1+1)
    ASSERT(nbma.eq.zi(jcesd2-1+1))
!
!       -- CALCUL DE NCMP (NOMBRE DE CVRC DANS VARC)
    ncmp=0
    do 69, k2=k,nbcvrc
    if (cvrcvarc(k2) .eq. varc) ncmp=ncmp+1
69  continue
!
    do 70,ima = 1,nbma
    nbpt = zi(jcesd-1+5+4* (ima-1)+1)
    nbsp = max(1,zi(jcesd-1+5+4* (ima-1)+2))
!
    call cesexi('C', jcesd2, jcesl2, ima, 1,&
                1, 1, iad)
    if (iad .le. 0) goto 70
!
!         -- CALCUL DE ICHS :
    tysd1=cesv2(iad+1)
    nosd1=cesv2(iad+2)
    nosy1=cesv2(iad+3)
    do 71, ichs=1,nbchs
    tysd2=liste_sd(7*(ichs-1)+1)(1:8)
    nosd2=liste_sd(7*(ichs-1)+2)(1:8)
    nosy2=liste_sd(7*(ichs-1)+3)
    if ((tysd1.eq.tysd2) .and. (nosd1.eq.nosd2) .and. ( nosy1.eq.nosy2)) goto 72
71  continue
    ASSERT(.false.)
72  continue
!
    do 60,ipt = 1,nbpt
    do 50,isp = 1,nbsp
    do 51,icmp = 1,ncmp
    call cesexi('C', jcesd, jcesl, ima, ipt,&
                isp, k-1+ icmp, iad)
!               LA FORMULE K-1+ICMP PEUT PARAITRE CURIEUSE MAIS
!               EN REALITE, K S'INCREMENTE PAR PAQUETS DE NCMP
!               (VOIR COMMENTAIRE EN DEBUT DE BOUCLE 1)
    if (iad .eq. 0) goto 51
    ASSERT(iad.lt.0)
    iad=-iad
    zl(jcesl-1+iad)=.true.
    zi(jcesvi-1+iad)=ichs
51  continue
50  continue
60  continue
70  continue
    call detrsd('CHAMP', ces2)
    1 end do
!
!
    call jedema()
end subroutine

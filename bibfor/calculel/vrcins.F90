subroutine vrcins(modelz, chmatz, carelz, inst, chvarc,&
                  codret)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/imprsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcin1.h"
#include "asterfort/vrcin2.h"
#include "asterfort/detrsd.h"
!
    character(len=2) :: codret
    character(len=19) :: chvarc
    character(len=*) :: chmatz, carelz, modelz
    real(kind=8) :: inst
! ======================================================================
!   BUT : FABRIQUER LE CHAMP DE VARIABLES DE COMMANDE CORRESPONDANT A
!         UN INSTANT DONNE.
!   ARGUMENTS :
!   MODELZ (K8)  IN/JXIN : SD MODELE
!   CHMATZ (K8)  IN/JXIN : SD CHAM_MATER
!   CARELZ (K8)  IN/JXIN : SD CARA_ELEM (SOUS-POINTS)
!   INST   (R)   IN      : VALEUR DE L'INSTANT
!   CHVARC (K19) IN/JXOUT: SD CHAM_ELEM/ELGA CONTENANT LES VARC
!   CODRET (K2)  OUT : POUR CHAQUE RESULTAT, 'OK' SI ON A TROUVE,
!                                            'NO' SINON
!
!
! ----------------------------------------------------------------------
!
!
    integer :: iret, ichs, nbchs,   jcesd,  jcesl
    integer ::   nbcmp, kcmp, kcvrc
    integer :: nbma, ima, nbpt, nbsp, ipt, isp, iad, iad1
    integer ::  jce1d, jce1l,   nncp, n1, k
    real(kind=8) :: valeur, rundef
    character(len=19) :: chvars, ligrmo, chs
    character(len=8) :: valk(4)
    logical(kind=1) :: avrc, dbg
    integer :: ibid, nbcvrc
    character(len=8) :: modele, chmat, carele, varc1, varc2, nocmp1, nocmp2
    character(len=8), pointer :: cvrcvarc(:) => null()
    character(len=24), pointer :: liste_ch(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    character(len=16), pointer :: liste_sd(:) => null()
    character(len=8), pointer :: cvrccmp(:) => null()
    integer, pointer :: cesvi(:) => null()
    real(kind=8), pointer :: ce1v(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    chmat=chmatz
    carele=carelz
    modele=modelz
    call detrsd('CHAM_ELEM', chvarc)
!
!
    call jeexin(chmat//'.CVRCVARC', iret)
!     AVRC : .TRUE. SI AFFE_MATERIAU/AFFE_VARC EST UTILISE
    avrc=(iret.gt.0)
    if (.not.avrc) goto 9999
!
!
!     1. INTERPOLATION EN TEMPS :
!        FABRICATION D'UNE LISTE DE CHAM_ELEM_S / ELGA
!        CONTENANT LES VRC A L'INSTANT INST
!        CALCUL DE  CHMAT.LISTE_CH(:) ET CHMAT.LISTE_SD(:)
!     -----------------------------------------------------
    call vrcin1(modele, chmat, carele, inst, codret)
!
!     1.1 SI IL N'Y A PAS VRAIMENT DE VARIABLES DE COMMANDE
!         (PAR EXEMPLE IL EXISTE TEMP/VALE_REF MAIS PAS DE TEMP
    call jeexin(chmat//'.LISTE_SD', iret)
    if (iret .eq. 0) goto 9999
!
!
!     2. ALLOCATION DU CHAMP_ELEM_S RESULTAT (CHVARS)
!        CALCUL DE CHMAT.CESVI
!        (CETTE ETAPE EST ECONOMISEE D'UN INSTANT A L'AUTRE)
!     -------------------------------------------------------------
    chvars=chmat//'.CHVARS'
    call jeexin(chmat//'.CESVI', iret)
    if (iret .eq. 0) call vrcin2(modele, chmat, carele, chvars)
!
!
!     3. CONCATENATION DES CHAMPS DE .LISTE_CH  DANS CHVARS :
!     -----------------------------------------------------
    call jeveuo(chmat//'.LISTE_CH', 'L', vk24=liste_ch)
    call jelira(chmat//'.LISTE_CH', 'LONMAX', nbchs)
    call jeveuo(chmat//'.LISTE_SD', 'L', vk16=liste_sd)
    call jeveuo(chmat//'.CVRCVARC', 'L', vk8=cvrcvarc)
    call jeveuo(chmat//'.CVRCCMP', 'L', vk8=cvrccmp)
    call jelira(chmat//'.CVRCCMP', 'LONMAX', nbcvrc)
!
    call jeveuo(chvars//'.CESD', 'L', jce1d)
    call jeveuo(chvars//'.CESL', 'E', jce1l)
    call jeveuo(chmat//'.CESVI', 'L', vi=cesvi)
!     -- IL FAUT REMETTRE CESV A NAN:
    rundef=r8nnem()
    call jeveuo(chvars//'.CESV', 'E', vr=ce1v)
    call jelira(chvars//'.CESV', 'LONMAX', n1)
    do 5, k=1,n1
    ce1v(k)=rundef
    5 end do
!
    do 1, ichs=1,nbchs
    chs=liste_ch(ichs)(1:19)
    varc1=liste_sd(7*(ichs-1)+4)(1:8)
    call jeveuo(chs//'.CESD', 'L', jcesd)
    call jeveuo(chs//'.CESL', 'L', jcesl)
    call jeveuo(chs//'.CESV', 'L', vr=cesv)
    call jeveuo(chs//'.CESC', 'L', vk8=cesc)
    call jelira(chs//'.CESC', 'LONMAX', nbcmp)
!
    do 2,kcmp=1,nbcmp
    nocmp1=cesc(kcmp)
!
!         -- CALCUL DE KCVRC :
    do 3,kcvrc=1,nbcvrc
    varc2=cvrcvarc(kcvrc)
    nocmp2=cvrccmp(kcvrc)
    if ((varc1.eq.varc2) .and. (nocmp1.eq.nocmp2)) goto 4
 3  continue
    goto 2
!
 4  continue
    ASSERT(kcvrc.ge.1 .and. kcvrc.le.nbcvrc)
!
!         -- BOUCLE SUR LES MAILLES :
    nbma = zi(jcesd-1+1)
    ASSERT(nbma.eq.zi(jce1d-1+1))
!
    do 70,ima = 1,nbma
    nbpt = zi(jcesd-1+5+4* (ima-1)+1)
    if (nbpt .eq. 0) goto 70
    ASSERT(nbpt.eq.zi(jce1d-1+5+4* (ima-1)+1))
    nbsp = max(1,zi(jcesd-1+5+4* (ima-1)+2))
    if (nbsp .ne. zi(jce1d-1+5+4* (ima-1)+2)) then
        valk(1) = nocmp1
        valk(2) = carele
        valk(3) = chmat
        call utmess('F', 'CALCULEL6_57', nk=3, valk=valk)
    endif
!
    call cesexi('C', jce1d, jce1l, ima, 1,&
                1, kcvrc, iad1)
    if (iad1 .eq. 0) then
!           -- L'ELEMENT FINI NE CONNAIT PAS LES VARIABLES DE COMMANDE
        goto 70
    endif
!
    if (iad1 .lt. 0) then
!           -- LA MAILLE PORTE UN ELEMENT FINI QUI SAURAIT UTILISER
!              LES VARIABLES DE COMMANDE MAIS ELLE N'EST PAS AFFECTEE.
!              ON ESPERE QUE LES ROUTINES TE00IJ ARRETERONT EN <F>
!              SI NECESSAIRE.
        goto 70
    endif
!
    do 60,ipt = 1,nbpt
    do 50,isp = 1,nbsp
    call cesexi('C', jcesd, jcesl, ima, ipt,&
                isp, kcmp, iad)
    if (iad .gt. 0) then
        call cesexi('C', jce1d, jce1l, ima, ipt,&
                    isp, kcvrc, iad1)
        ASSERT(iad1.gt.0)
        if (cesvi(iad1) .eq. ichs) then
            valeur=cesv(iad)
            zl(jce1l-1+iad1)=.true.
            ce1v(iad1)=valeur
        endif
    endif
50  continue
60  continue
70  continue
!
 2  continue
    1 end do
!
!
!     4. RECOPIE DU CHAMP SIMPLE DANS LE CHAMP CHVARC
!     -----------------------------------------------------
    ligrmo=modele//'.MODELE'
    call cescel(chvars, ligrmo, 'INIT_VARC', 'PVARCPR', 'NAN',&
                nncp, 'V', chvarc, 'F', ibid)
!
    dbg=.false.
    if (dbg) call imprsd('CHAMP', chvarc, 6, 'VRCINS/CHVARC')
!
9999  continue
    call jedema()
end subroutine

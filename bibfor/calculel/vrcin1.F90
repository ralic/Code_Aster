subroutine vrcin1(modele, chmat, carele, inst, codret)
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
#include "asterc/indik8.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cesces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesvar.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsces.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/fointe.h"
#include "asterfort/indk80.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/manopg.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=2) :: codret
    character(len=8) :: modele, chmat, carele
    real(kind=8) :: inst
! ======================================================================
!   BUT : FAIRE L'INTERPOLATION AU TEMPS INST DES DIFFERENTS CHAMPS
!         DE VARIABLES DE COMMANDE.
!         CES CHAMPS SONT PASSES AUX POINTS DE GAUSS MECANIQUE.
!         (INIT_VARC/PVARCPR)
!
!   IN :
!     MODELE (K8)  IN/JXIN : SD MODELE
!     CHMAT  (K8)  IN/JXIN : SD CHAM_MATER
!     CARELE  (K8)  IN/JXIN : SD CARA_ELEM
!     INST   (R)   IN      : VALEUR DE L'INSTANT
!
!   OUT :
!       - CREATION DE CHMAT//'.LISTE_CH' V V K24  LONG=NBCHS
!          .LISTE_CH(I) : IEME CHAM_ELEM_S / ELGA PARTICIPANT A
!                         LA CREATION DU CHAMP DE CVRC
!       - CREATION DE CHMAT//'.LISTE_SD' V V K16  LONG=7*NBCHS
!          .LISTE_SD(7*(I-1)+1) : /'EVOL' /'CHAMP' :
!               TYPE DE LA SD DONT EST ISSU .LISTE_CHS(I)
!          .LISTE_SD(7*(I-1)+2) : NOMSD
!               NOM DE L'EVOL (OU DU CHAMP) DONT EST ISSU .LISTE_CHS(I)
!          .LISTE_SD(7*(I-1)+3) : NOMSYM / ' '
!               SI 'EVOL' : NOM SYMBOLIQUE DU CHAMP DONT EST
!               ISSU .LISTE_CHS(I). SINON : ' '
!          .LISTE_SD(7*(I-1)+4) : VARC
!               VARC ASSOCIE A .LISTE_CHS(I).
!          .LISTE_SD(7*(I-1)+5) : PROLGA
!               (SI EVOL : TYPE DE PROLONGEMENT A GAUCHE)
!          .LISTE_SD(7*(I-1)+6) : PROLDR
!               (SI EVOL : TYPE DE PROLONGEMENT A DROITE)
!          .LISTE_SD(7*(I-1)+7) : FINST (OU ' ')
!               (SI EVOL : FONCTION DE TRANSFORMATION DU TEMPS)
!        CODRET (K2) : POUR CHAQUE RESULTAT, 'OK' SI ON A TROUVE,
!                                            'NO' SINON
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: instev
!
    integer :: n1, ibid, nbma, jcesd1, jcesl1, jcesv1, iad, lonk80, jcvvar
    integer :: itrou, nbk80, k, ima, jlk80, iret, nbchs, jlissd, ichs
    integer :: nbcvrc, jcvgd, jlisch, nval1, nval2
    character(len=8) :: varc, mailla, tysd, proldr, prolga, nomevo, finst
    character(len=8) :: ma2
    character(len=8) :: nomgd, nomgd2, tych, nomsd
    character(len=16) :: nomsym, optio1
    character(len=19) :: cart2, chs, cesmod, celmod, ligrmo, mnoga, dceli, ligr1
    character(len=19) :: ces1, cns1, nomch
    character(len=24) :: valk(3)
    character(len=80) :: k80, k80pre
    save nval1
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mailla)
    call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbma)
    ligrmo=modele//'.MODELE'
    call jelira(chmat//'.CVRCVARC', 'LONMAX', nbcvrc)
    call jeveuo(chmat//'.CVRCVARC', 'L', jcvvar)
    call jeveuo(chmat//'.CVRCGD', 'L', jcvgd)
!
!     INITIALISATION
!
    codret = 'OK'
!
!
!     1. CREATION DE CHMAT.LISTE_SD :
!     -------------------------------
    call jeexin(chmat//'.LISTE_SD', iret)
    if (iret .eq. 0) then
        varc=' '
        k80pre=' '
        nbk80=0
        lonk80=5
        call wkvect('&&VRCIN1.LK80', 'V V K80', lonk80, jlk80)
        do k = 1, nbcvrc
            if (zk8(jcvvar-1+k) .eq. varc) goto 1
            varc=zk8(jcvvar-1+k)
            cart2 = chmat//'.'//varc//'.2'
            ces1='&&VRCIN1.CES1'
            call carces(cart2, 'ELEM', ' ', 'V', ces1,&
                        'A', iret)
            ASSERT(iret.eq.0)
            call jeveuo(ces1//'.CESD', 'L', jcesd1)
            call jeveuo(ces1//'.CESL', 'L', jcesl1)
            call jeveuo(ces1//'.CESV', 'L', jcesv1)
            do ima = 1, nbma
                call cesexi('C', jcesd1, jcesl1, ima, 1,&
                            1, 1, iad)
                if (iad .le. 0) goto 2
                iad=iad-1
                tysd=zk16(jcesv1-1+iad+2)(1:8)
                nomsd =zk16(jcesv1-1+iad+3)(1:8)
                nomsym=zk16(jcesv1-1+iad+4)
                prolga=zk16(jcesv1-1+iad+5)
                proldr=zk16(jcesv1-1+iad+6)
                finst =zk16(jcesv1-1+iad+7)
                ASSERT((tysd.eq.'EVOL') .or. (tysd.eq.'CHAMP') .or. (tysd.eq.'VIDE'))
                if (tysd .eq. 'VIDE') goto 2
!
                k80=' '
                k80(1:8) =tysd
                k80(9:16) =nomsd
                k80(17:32)=nomsym
                k80(33:40)=varc
                k80(41:48)=prolga
                k80(49:56)=proldr
                k80(57:64)=finst
                if (k80 .eq. k80pre) goto 2
                k80pre=k80
                itrou=indk80(zk80(jlk80),k80,1,nbk80)
                if (itrou .gt. 0) goto 2
                nbk80=nbk80+1
                if (nbk80 .gt. lonk80) then
                    lonk80=2*lonk80
                    call juveca('&&VRCIN1.LK80', lonk80)
                    call jeveuo('&&VRCIN1.LK80', 'E', jlk80)
                endif
                zk80(jlk80-1+nbk80)=k80
  2             continue
            end do
            call detrsd('CHAM_ELEM_S', ces1)
  1         continue
        end do
!
        nbchs=nbk80
        if (nbchs .eq. 0) then
            call jedetr('&&VRCIN1.LK80')
            goto 999
        endif
        call wkvect(chmat//'.LISTE_SD', 'V V K16', 7*nbchs, jlissd)
        do ichs = 1, nbchs
            k80=zk80(jlk80-1+ichs)
            zk16(jlissd-1+7*(ichs-1)+1)=k80(1:8)
            zk16(jlissd-1+7*(ichs-1)+2)=k80(9:16)
            zk16(jlissd-1+7*(ichs-1)+3)=k80(17:32)
            zk16(jlissd-1+7*(ichs-1)+4)=k80(33:40)
            zk16(jlissd-1+7*(ichs-1)+5)=k80(41:48)
            zk16(jlissd-1+7*(ichs-1)+6)=k80(49:56)
            zk16(jlissd-1+7*(ichs-1)+7)=k80(57:64)
        end do
        call jedetr('&&VRCIN1.LK80')
    endif
!
!
!     2. CREATION DE CHMAT.LISTE_CH :
!     -------------------------------
    call jeveuo(chmat//'.LISTE_SD', 'L', jlissd)
    call jelira(chmat//'.LISTE_SD', 'LONMAX', n1)
    nbchs=n1/7
    ASSERT(n1.eq.7*nbchs)
    call jedetr(chmat//'.LISTE_CH')
    call wkvect(chmat//'.LISTE_CH', 'V V K24', nbchs, jlisch)
    chs=chmat//'.CHS000'
!
!     2.0.1  CREATION DE CESMOD :
!     ---------------------------
    cesmod=modele//'.VRC.CESMOD'
!     --  CESMOD N'EST PAS DETRUIT POUR GAGNER DU TEMPS
    call exisd('CHAM_ELEM_S', cesmod, iret)
    if (iret .eq. 0) then
        celmod='&&VRCIN1.CELMOD'
        dceli='&&VRCIN1.DCELI'
        call cesvar(carele, ' ', ligrmo, dceli)
        call alchml(ligrmo, 'INIT_VARC', 'PVARCPR', 'V', celmod,&
                    iret, dceli)
        ASSERT(iret.eq.0)
        call detrsd('CHAMP', dceli)
        call celces(celmod, 'V', cesmod)
        call jelira(celmod//'.CELV', 'LONMAX', nval1)
        call detrsd('CHAMP', celmod)
    endif
!
!
!     2.0.2  CREATION DE MNOGA :
!     ---------------------------
    mnoga = modele//'.VRC.MNOGA'
    call exisd('CHAM_ELEM_S', mnoga, iret)
    if (iret .eq. 0) call manopg(ligrmo, 'INIT_VARC', 'PVARCPR', mnoga)
!
!
    do ichs = 1, nbchs
        call codent(ichs, 'D0', chs(13:15))
        zk24(jlisch-1+ichs)=chs
        tysd=zk16(jlissd-1+7*(ichs-1)+1)(1:8)
        varc=zk16(jlissd-1+7*(ichs-1)+4)(1:8)
!
!         2.1 INTERPOLATION EN TEMPS => NOMCH
!         ------------------------------------
        if (tysd .eq. 'EVOL') then
!           -- SI TYSD='EVOL', ON INTERPOLE AU TEMPS INST
            nomevo=zk16(jlissd-1+7*(ichs-1)+2)(1:8)
            nomsym=zk16(jlissd-1+7*(ichs-1)+3)
            prolga=zk16(jlissd-1+7*(ichs-1)+5)
            proldr=zk16(jlissd-1+7*(ichs-1)+6)
            finst =zk16(jlissd-1+7*(ichs-1)+7)
            nomch='&&VRCIN1.NOMCH'
!
!           -- PRISE EN COMPTE DE L'EVENTUELLE TRANSFORMATION DU TEMPS
!              (AFFE_VARC/FONC_INST):
            if (finst .ne. ' ') then
                call fointe('F', finst, 1, ['INST'], [inst],&
                            instev, ibid)
            else
                instev=inst
            endif
            call rsinch(nomevo, nomsym, 'INST', instev, nomch,&
                        proldr, prolga, 2, 'V', iret)
            ASSERT(iret.le.12)
            if (iret .ge. 10) then
                codret = 'NO'
                goto 999
            endif
        else
            ASSERT(tysd.eq.'CHAMP')
!           -- SI TYSD='CHAMP', C'EST UN CHAMP INDEPENDANT DU TEMPS :
            nomch=zk16(jlissd-1+7*(ichs-1)+2)
        endif
        call dismoi('NOM_MAILLA', nomch, 'CHAMP', repk=ma2)
        if (ma2 .ne. mailla) then
            valk(1)=mailla
            valk(2)=ma2
            call utmess('F', 'CALCULEL4_13', nk=2, valk=valk)
        endif
!
!         2.2 PASSAGE AUX POINTS DE GAUSS => CHS
!         --------------------------------------
!         -- VERIFICATION DE NOMCH :
        itrou=indik8(zk8(jcvvar),varc,1,nbcvrc)
        ASSERT(itrou.gt.0)
        nomgd=zk8(jcvgd-1+itrou)
        call dismoi('NOM_GD', nomch, 'CHAMP', repk=nomgd2)
        if (nomgd .ne. nomgd2) then
            valk(1) = varc
            valk(2) = nomgd
            valk(3) = nomgd2
            call utmess('F', 'CALCULEL5_39', nk=3, valk=valk)
        endif
        call dismoi('TYPE_CHAMP', nomch, 'CHAMP', repk=tych)
!
!
        if (tych .eq. 'CART') then
            call carces(nomch, 'ELGA', cesmod, 'V', chs,&
                        'A', iret)
            ASSERT(iret.eq.0)
!
        else if (tych.eq.'NOEU') then
            cns1='&&VRCIN1.CNS1'
            call cnocns(nomch, 'V', cns1)
            call cnsces(cns1, 'ELGA', cesmod, mnoga, 'V',&
                        chs)
            call detrsd('CHAM_NO_S', cns1)
!
        else if ((tych.eq.'ELNO').or.(tych.eq.'ELEM')) then
            ces1='&&VRCIN1.CES1'
            call celces(nomch, 'V', ces1)
            call cesces(ces1, 'ELGA', cesmod, mnoga, ' ',&
                        'V', chs)
            call detrsd('CHAM_ELEM_S', ces1)
!
        else if (tych.eq.'ELGA') then
!            2.2.3
!            -- CAS OU LE CHAMP PROVIENT DE PROJ_CHAMP /
!               METHODE='SOUS_POINT'.
!            2.2.3.1 :
!               ON VERIFIE QUE LE CHAMP EST BIEN PREPARE :
            call dismoi('NOM_LIGREL', nomch, 'CHAM_ELEM', repk=ligr1)
            if (ligr1 .ne. ligrmo) then
                valk(1)=nomch
                valk(2)=ligr1
                valk(3)=ligrmo
                call utmess('F', 'CALCULEL4_25', nk=3, valk=valk)
            endif
!
            call dismoi('NOM_OPTION', nomch, 'CHAM_ELEM', repk=optio1)
            if (optio1 .ne. 'INI_SP_MATER') then
                valk(1)=nomch
                valk(2)=optio1
                call utmess('F', 'CALCULEL4_26', nk=2, valk=valk)
            endif
!
            call jelira(nomch//'.CELV', 'LONMAX', nval2)
            ASSERT(nval1.eq.nval2)
!
!            2.2.3.2 : SIMPLE RECOPIE
            call celces(nomch, 'V', chs)
!
        else
            ASSERT(.false.)
        endif
!
!
!
        if (tysd .eq. 'EVOL') call detrsd('CHAMP', nomch)
    end do
!
999 continue
    call jedema()
end subroutine

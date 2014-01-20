subroutine irsspt(cesz, unite, nbmat, nummai, nbcmp,&
                  nomcmp, lsup, linf, lmax, lmin,&
                  borinf, borsup)
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
!
#include "asterc/indik8.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: cesz, nomcmp(*)
    integer :: unite, nbmat, nummai(*), nbcmp
    real(kind=8) :: borinf, borsup
    logical :: lsup, linf, lmax, lmin
! ---------------------------------------------------------------------
! BUT: IMPRIMER LES VALEURS MIN/MAX DES COMPOSANTES D'UN CHAM_ELEM_S
!      A DES SOUS-POINTS
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CESZ   IN/JXIN  K19 : SD CHAM_ELEM_S A IMPRIMER
! UNITE  IN       I   : NUMERO DE L'UNITE LOGIQUE D'IMPRESSION
! NBMAT  IN       I   : /0 : ON IMPRIME TOUTES LES MAILLES
! NBMAT  IN       I   : >0 : ON N'IMPRIME QUE LES MAILLES DE NUMMAI(*)
!                            DE 1 A NBMAT
! NUMMAI IN      V(I) : NUMEROS DES MAILLES A IMPRIMER (SI NBMAT >0)
! NBCMP  IN       I   : NOMBRE DE COMPOSANTES A IMPRIMER
! NOMCMP IN       K8  : NOMS DES COMPOSANTES A IMPRIMER
! LSUP   IN       L   : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
! BORSUP IN       R8  : VALEUR DE LA BORNE SUPERIEURE
! LINF   IN       L   : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE INFERIEURE
! BORINF IN       R8  : VALEUR DE LA BORNE INFERIEURE
! LMAX   IN       L   : =.TRUE.  INDIQUE IMPRESSION VALEUR MAXIMALE
! LMIN   IN       L   : =.TRUE.  INDIQUE IMPRESSION VALEUR MINIMALE
! ---------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: jcesk, jcesd, jcesc, jcesv, jcesl, ncmpc, icmp, i
    integer :: ncmp,  nbmac, nbma, nbpt, nbsp, j, ipt, isp, iad
    integer :: ispmin, ispmax, ispmi2, ispma2, ispmi3, ispma3
    integer :: iptmin, iptmax, iptmi2, iptma2
    integer :: imamin, imamax, ima
    real(kind=8) :: vspmi3, vspma3, valr
    real(kind=8) :: vptmi2, vptma2, vmamin, vmamax
    character(len=8) ::  ma, noma
    character(len=19) :: ces
    logical :: lmamin, lmamax, lptmin, lptmax, lspmin, lspmax
    integer, pointer :: num_cmp_cham(:) => null()
    integer, pointer :: num_mail_cham(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!     ---------------
    ces = cesz
!
    call jeveuo(ces//'.CESK', 'L', jcesk)
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', jcesc)
    call jeveuo(ces//'.CESV', 'L', jcesv)
    call jeveuo(ces//'.CESL', 'L', jcesl)
    call jelira(ces//'.CESC', 'LONMAX', ncmpc)
    ma = zk8(jcesk-1+1)
    nbmac = zi(jcesd-1+1)
!
    AS_ALLOCATE(vi=num_cmp_cham, size=ncmpc)
    AS_ALLOCATE(vi=num_mail_cham, size=nbmac)
!
!
! --- ON RECUPERE LES COMPOSANTES AD-HOC:
!     ------------------------------------
!     SI L'UTILISATEUR A RENSEIGNE NOM_CMP
    if (nbcmp .ne. 0) then
        ncmp=0
        do 10 i = 1, nbcmp
            icmp=indik8(zk8(jcesc),nomcmp(i),1,ncmpc)
            if (icmp .ne. 0) then
                num_cmp_cham(ncmp+1)=icmp
                ncmp=ncmp+1
            endif
10      continue
    else
!       SINON TOUT_CMP='OUI'
        do 20 i = 1, ncmpc
            num_cmp_cham(i)=i
20      continue
        ncmp=ncmpc
    endif
!
! --- ON RECUPERE LES MAILLES AD-HOC:
!     -------------------------------
!     SI L'UTILISATEUR A RENSEIGNE MAILLE/GROUP_MA
    if (nbmat .ne. 0) then
        do 30 i = 1, nbmat
            num_mail_cham(i)=nummai(i)
30      continue
        nbma=nbmat
    else
!        SINON
        do 40 i = 1, nbmac
            num_mail_cham(i)=i
40      continue
        nbma=nbmac
    endif
!
!
! --- RECUPERATION DES VALEURS MIN/MAX
!     -------------------------------
!
!     BOUCLE SUR LES COMPOSANTES
    do 50 i = 1, ncmp
        icmp=num_cmp_cham(i)
!
!       LMAxxx : BOOLEEN INDIQUANT LE PREMIER PASSAGE
!       LORS DU DES MAILLES POUR STOCKER LES VALEURS
        lmamin=.true.
        lmamax=.true.
!
!       BOUCLE SUR LES MAILLES
        do 60 j = 1, nbma
            ima=num_mail_cham(j)
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
!
!         LPTxxx : BOOLEEN INDIQUANT LE PREMIER PASSAGE
!         LORS DU PARCOURT DES POINTS POUR STOCKER LES VALEURS MIN/MAX
            lptmin=.true.
            lptmax=.true.
!
!         BOUCLE SUR LES POINTS
            do 70 ipt = 1, nbpt
!
!           VSPMA3: VALEUR MAX SUR TOUS LES SOUS-POINTS
!           VSPMI3: VALEUR MIN SUR TOUS LES SOUS-POINTS
!           ISPMA3: NUMERO DU SOUS_POINT ASSOCIE A VSPMA3
!           ISPMI3: NUMERO DU SOUS_POINT ASSOCIE A VSPMI3
!
!           LSPxxx : BOOLEEN INDIQUANT LE PREMIER PASSAGE
!           LORS DU PARCOURT DES SOUS-POINTS POUR STOCKER LES VALEURS
                lspmin=.true.
                lspmax=.true.
!
!           BOUCLE SUR LES SOUS-POINTS:
                do 80 isp = 1, nbsp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, icmp, iad)
                    if (iad .gt. 0) then
!
                        valr=zr(jcesv+iad-1)
!
!                SI VALE_MAX
                        if (lmax) then
!                  SI BORNE_SUP
                            if (lsup) then
                                if ((valr-borsup) .gt. 0.d0) goto 80
                            endif
!                  SI BORNE_INF
                            if (linf) then
                                if ((valr-borinf) .lt. 0.d0) goto 80
                            endif
!                  PREMIER PASSAGE
                            if (lspmax) then
                                vspma3=valr
                                ispma3=isp
                                lspmax=.false.
                            else
                                if (valr .gt. vspma3) then
                                    vspma3=valr
                                    ispma3=isp
                                endif
                            endif
                        endif
!
!                SI VALE_MIN
                        if (lmin) then
!                  SI BORNE_SUP
                            if (lsup) then
                                if ((valr-borsup) .gt. 0.d0) goto 80
                            endif
!                  SI BORNE_INF
                            if (linf) then
                                if ((valr-borinf) .lt. 0.d0) goto 80
                            endif
!                  PREMIER PASSAGE
                            if (lspmin) then
                                vspmi3=valr
                                ispmi3=isp
                                lspmin=.false.
                            else
                                if (valr .lt. vspmi3) then
                                    vspmi3=valr
                                    ispmi3=isp
                                endif
                            endif
                        endif
!
                    endif
!
!           FIN BOUCLE SUR LES SOUS-POINTS
80              continue
!
!           VPTMA2: VALEUR MAX SUR TOUS LES POINTS
!           VPTMI2: VALEUR MIN SUR TOUS LES POINTS
!           IPTMA2: NUMERO DU POINT ASSOCIE A VPTMA2
!           IPTMI2: NUMERO DU POINT ASSOCIE A VPTMI2
!           ISPMA2: NUMERO DU SOUS_POINT ASSOCIE A IPTMA2
!           ISPMI2: NUMERO DU SOUS_POINT ASSOCIE A IPTMI2
!
!           SI VALE_MAX
                if (lmax .and. .not.lspmax) then
!             PREMIER PASSAGE
                    if (lptmax) then
                        iptma2=ipt
                        vptma2=vspma3
                        ispma2=ispma3
                        lptmax=.false.
                    else
!               ON REACTUALISE LA VALEUR MAX
                        if (vptma2 .lt. vspma3) then
                            vptma2=vspma3
                            iptma2=ipt
                            ispma2=ispma3
                        endif
                    endif
                endif
!
!           SI VALE_MIN
                if (lmin .and. .not.lspmin) then
!             PREMIER PASSAGE
                    if (lptmin) then
                        iptmi2=ipt
                        vptmi2=vspmi3
                        ispmi2=ispmi3
                        lptmin=.false.
                    else
!               ON REACTUALISE LA VALEUR MIN
                        if (vptmi2 .gt. vspmi3) then
                            vptmi2=vspmi3
                            iptmi2=ipt
                            ispmi2=ispmi3
                        endif
                    endif
                endif
!
!         FIN BOUCLE SUR LES POINTS
70          continue
!
!         VMAMAX: VALEUR MAX SUR TOUTES LES MAILLES
!         VMAMIN: VALEUR MIN SUR TOUTES LES MAILLES
!         IMAMAX: NUMERO DE LA MAILLE ASSOCIEE A VMAMAX
!         IMAMIN: NUMERO DE LA MAILLE ASSOCIEE A VMAMIN
!         IPTMAX: NUMERO DU POINT ASSOCIE A IMAMAX
!         IPTMIN: NUMERO DU POINT ASSOCIE A IMAMIN
!         ISPMAX: NUMERO DU SOUS_POINT ASSOCIE A IPTMAX
!         ISPMIN: NUMERO DU SOUS_POINT ASSOCIE A IPTMIN
!
!         SI VALE_MAX
            if (lmax .and. .not.lptmax) then
!           PREMIER PASSAGE
                if (lmamax) then
                    imamax=ima
                    vmamax=vptma2
                    iptmax=iptma2
                    ispmax=ispma2
                    lmamax=.false.
                else
!             ON REACTUALISE LA VALEUR MAX
                    if (vmamax .lt. vptma2) then
                        vmamax=vptma2
                        iptmax=iptma2
                        ispmax=ispma2
                        imamax=ima
                    endif
                endif
            endif
!
!         SI VALE_MIN
            if (lmin .and. .not.lptmin) then
!           PREMIER PASSAGE
                if (lmamin) then
                    imamin=ima
                    vmamin=vptmi2
                    iptmin=iptmi2
                    ispmin=ispmi2
                    lmamin=.false.
                else
!             ON REACTUALISE LA VALEUR MIN
                    if (vmamin .gt. vptmi2) then
                        vmamin=vptmi2
                        iptmin=iptmi2
                        ispmin=ispmi2
                        imamin=ima
                    endif
                endif
            endif
!
!     FIN BOUCLE SUR LES MAILLES
60      end do
!
!
!     IMPRESSIONS
!     -----------
!
        if (lmax .and. .not.lmamax) then
            call jenuno(jexnum(ma//'.NOMMAI', imamax), noma)
            write(unite,*)' '
            write(unite,2000)'LA VALEUR MAXIMALE DE ',zk8(jcesc+icmp-&
            1), 'EST: ',vmamax
            write(unite,2001)'OBTENUE DANS LA MAILLE ',noma,&
     &    'AU SOUS_POINT ',ispmax,' DU POINT ',iptmax
        endif
!
        if (lmin .and. .not.lmamin) then
            call jenuno(jexnum(ma//'.NOMMAI', imamin), noma)
            write(unite,*)' '
            write(unite,2000)'LA VALEUR MINIMALE DE ',zk8(jcesc+icmp-&
            1), 'EST: ',vmamin
            write(unite,2001)'OBTENUE DANS LA MAILLE ',noma,&
     &    'AU SOUS_POINT ',ispmin,' DU POINT ',iptmin
        endif
!
50  end do
!
    2000 format(3(a),e12.5)
    2001 format(3(a),i3,a,i3)
!
    AS_DEALLOCATE(vi=num_cmp_cham)
    AS_DEALLOCATE(vi=num_mail_cham)
!
    call jedema()
!
end subroutine

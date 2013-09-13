subroutine peecal(tych, resu, nomcha, lieu, nomlie,&
                  modele, ichagd, chpost, nbcmp, nomcmp,&
                  nomcp2, nuord, inst, iocc)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/chpond.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbcmp, nuord, iocc, ichagd
    character(len=8) :: nomcmp(nbcmp), nomcp2(nbcmp), modele, lieu
    character(len=19) :: chpost, resu
    character(len=24) :: nomcha
    character(len=*) :: nomlie
    character(len=4) :: tych
!    -------------------------------------------------------------------
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
!
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "INTEGRALE"
!     ROUTINE D'APPEL : PEEINT
!
!     BUT : REALISER LES CALCULS DE MOYENNE ET LES
!           STOCKER DANS LA TABLE
!
!     IN  RESU   : NOM DE LA TABLE
!     IN  NOMCHA : NOM SYMBOLIQUE DU CHAMP DU POST-TRAITEMENT
!                  OU NOM DU CHAM_GD
!     IN  TYCH   : TYPE DU CHAMP 'ELGA/ELEM/ELNO)
!     IN  LIEU   : LIEU DU POST-TRAITEMENT
!         (LIEU='TOUT'/'GROUP_MA'/'MAILLE')
!     IN  NOMLIE : NOM DU LIEU
!     IN  MODELE : NOM DU MODELE
!     IN  ICHAGD : INDIQUE SI ON CHAM_GD (ICHAGD=0) OU RESULTAT
!     IN  CHPOST  : NOM DU CHAMP DU POST-TRAITEMENT
!     IN  NBCMP   : NOMBRE DE COMPOSANTES
!     IN  NOMCMP  : NOM DES COMPOSANTES
!     IN  NOMCP2  : NOM DES COMPOSANTES A AFFICHER
!                   (DIFFERENT DE NOMCMP DANS CERTAINS CAS)
!     IN  NUORD   : NUMERO D'ORDRE
!     IN  INST    : INSTANT
!     IN  IOCC    : NUMERO DE L'OCCURENCE DE INTEGRALE
!     ------------------------------------------------------------------
!
    integer :: iret, nbma, nbmai, i, jcesv, jcesl, jcesd, jpoiv, jpoil, jpoid
    integer :: nucmp, jcesk, jcmpgd, ncmpm, ibid, iad, jintr, jintk, indma
    integer :: jmesma, ipt, nbsp, nbpt, icmp, ima, nbpara
    integer :: jpdsm, ico, ind1, ind2, ifm, niv
    real(kind=8) :: vol, val, inst, volpt
    complex(kind=8) :: cbid
    character(len=8) :: noma, k8b, typmcl(2), nomgd, nomva
    character(len=4) :: dejain
    character(len=16) :: motcle(2)
    character(len=19) :: ligrel, cesout, cespoi
    character(len=24) :: mesmai, valk(3)
    logical :: exist
!
    call jemarq()
    call infniv(ifm, niv)
!
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrel, iret)
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, iret)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8b, iret)
!
! --- TABLEAUX DE TRAVAIL:
!     - TABLEAU DES PARAMETRES INTE_XXXX : ZK16(JINTK)
!     - TABLEAU DES VALEURS DES MOYENNES : ZR(JINTR)
    if (ichagd .ne. 0) then
        call wkvect('&&PEECAL.INTE_R', 'V V R', 2*nbcmp+2, jintr)
        call wkvect('&&PEECAL.INTE_K', 'V V K16', 2*nbcmp+5, jintk)
        zk16(jintk) ='NOM_CHAM'
        zk16(jintk+1)='NUME_ORDRE'
        zk16(jintk+2)='INST'
        zk16(jintk+3)='VOL'
        zk16(jintk+4)=lieu
        zr(jintr)=inst
        valk(1)=nomcha
        valk(2)=nomlie
        ind1 = 5
        ind2 = 1
    else
        call wkvect('&&PEECAL.INTE_R', 'V V R', 2*nbcmp, jintr)
        call wkvect('&&PEECAL.INTE_K', 'V V K16', 2*nbcmp+3, jintk)
        zk16(jintk) ='CHAM_GD'
        zk16(jintk+1)='VOL'
        zk16(jintk+2)=lieu
        valk(1)=nomcha
        valk(2)=nomlie
        ind1 = 3
        ind2 = 0
    endif
!
! --- CREATION D'UN TABLEAU D'INDICES POUR REPERER
!     LES MAILLES DU POST TRAITEMENT
    call wkvect('&&PEECAL.IND.MAILLE', 'V V I', nbma, indma)
    if (lieu(1:4) .ne. 'TOUT') then
        mesmai = '&&PEECAL_NUM.MAILLE'
        motcle(1) = 'GROUP_MA'
        motcle(2) = 'MAILLE'
        typmcl(1) = 'GROUP_MA'
        typmcl(2) = 'MAILLE'
        call reliem(' ', noma, 'NU_MAILLE', 'INTEGRALE', iocc,&
                    2, motcle, typmcl, mesmai, nbmai)
        call jeveuo(mesmai, 'L', jmesma)
        do 5 i = 1, nbma
            zi(indma+i-1)=0
 5      continue
        do 10 i = 1, nbmai
            zi(indma+zi(jmesma+i-1)-1)=1
10      continue
    else
        do 15 i = 1, nbma
            zi(indma+i-1)=1
15      continue
    endif
!
!
! --- POUR LES CHAM_ELEM / ELEM : MOT CLE DEJA_INTEGRE:
    if (tych .eq. 'ELEM') then
        call getvtx('INTEGRALE', 'DEJA_INTEGRE', iocc=iocc, scal=dejain, nbret=iret)
        if (iret .eq. 0) call u2mesk('F', 'UTILITAI7_13', 1, valk)
    endif
!
!
! --- CALCULS DES CHAMPS SIMPLES:
!      CESOUT: CHAMP ELXX CORRESPONDANT AU CHAMP CHPOST (SIMPLE) PONDERE
!              PAR LE POIDS*JACOBIEN.
!      CESPOI: CHAMP ELXX CORRESPONDANT AU POIDS*JACOBIEN
    cesout='&&PEECAL.CESOUT'
    cespoi='&&PEECAL_POIDS_'//nomcha(1:4)
    call chpond(tych, dejain, chpost, cesout, cespoi,&
                modele)
    call jeveuo(cesout//'.CESV', 'L', jcesv)
    call jeveuo(cesout//'.CESL', 'L', jcesl)
    call jeveuo(cesout//'.CESD', 'L', jcesd)
    call jeveuo(cesout//'.CESK', 'L', jcesk)
    call jeveuo(cespoi//'.CESV', 'L', jpoiv)
    call jeveuo(cespoi//'.CESL', 'L', jpoil)
    call jeveuo(cespoi//'.CESD', 'L', jpoid)
    if (tych .ne. 'ELGA') call jeveuo(cespoi//'.PDSM', 'L', jpdsm)
!
!
! --- RECUPERATION DE LA LISTE DES CMPS DU CATALOGUE :
!     (POUR LA GRANDEUR VARI_* , IL FAUT CONSTITUER :(V1,V2,...,VN))
    nomgd = zk8(jcesk-1+2)
    call jelira(cesout//'.CESC', 'LONMAX', ncmpm)
    if (nomgd(1:5) .ne. 'VARI_') then
        call jeveuo(cesout//'.CESC', 'L', jcmpgd)
    else
        call wkvect('&&PEECAL.LIST_CMP', 'V V K8', ncmpm, jcmpgd)
        do 25 i = 1, ncmpm
            nomva = 'V'
            call codent(i, 'G', nomva(2:8))
            zk8(jcmpgd-1+i) = nomva
25      continue
    endif
!
!     - INFOS
    if (niv .gt. 1) then
        write(6,*) '<PEECAL> NOMBRE DE MAILLES A TRAITER : ',nbmai
        write(6,*) '<PEECAL> NOMBRE DE COMPOSANTES : ',ncmpm
    endif
!
!
! --- CALCUL DE L'INTEGRALE ET DE LA MOYENNE(=INTEGRALE/VOLUME):
    do 30 icmp = 1, nbcmp
        nucmp=indik8(zk8(jcmpgd),nomcmp(icmp),1,ncmpm)
        val=0.d0
        vol=0.d0
        ico=0
        do 35 ima = 1, nbma
            if (zi(indma+ima-1) .ne. 1) goto 35
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
            if (nbsp .gt. 1) call u2mess('F', 'UTILITAI8_60')
            do 40 ipt = 1, nbpt
                call cesexi('C', jcesd, jcesl, ima, ipt,&
                            1, nucmp, iad)
                ASSERT(iad.ge.0)
                if (iad .eq. 0) goto 35
!
                val=val+zr(jcesv-1+iad)
!
                if (tych .eq. 'ELGA') then
                    call cesexi('C', jpoid, jpoil, ima, ipt,&
                                1, 1, iad)
                    ASSERT(iad.gt.0)
                    volpt=zr(jpoiv-1+iad)
                else if (tych.eq.'ELEM') then
                    ASSERT(nbpt.eq.1)
                    volpt=zr(jpdsm-1+ima)
                else if (tych.eq.'ELNO') then
                    ASSERT(nbpt.ge.1)
                    volpt=zr(jpdsm-1+ima)/nbpt
                endif
                ico=ico+1
                vol=vol+volpt
40          continue
35      continue
        if (ico .eq. 0) then
            valk(3)=nomcmp(icmp)
            call u2mesk('F', 'UTILITAI7_12', 3, valk)
        endif
!
        if (icmp .eq. 1) zr(jintr+icmp+ind2-1)=vol
        zr(jintr+icmp+ind2)=val
        zk16(jintk+ind1+icmp-1)='INTE_'//nomcp2(icmp)
        zr(jintr+nbcmp+icmp+ind2)=val/vol
        zk16(jintk+ind1+nbcmp+icmp-1)='MOYE_'//nomcp2(icmp)
30  end do
!
!
! --- ON AJOUTE LES PARAMETRES MANQUANTS DANS LA TABLE:
    call tbexip(resu, lieu, exist, k8b)
    if (.not.exist) then
        call tbajpa(resu, 1, zk16(jintk+ind1-1), 'K16')
    endif
    do 45 icmp = 1, nbcmp*2
        call tbexip(resu, zk16(jintk+ind1+icmp-1), exist, k8b)
        if (.not.exist) then
            call tbajpa(resu, 1, zk16(jintk+ind1+icmp-1), 'R')
        endif
45  continue
!
! --- ON REMPLIT LA TABLE
    nbpara=ind1+nbcmp*2
    call tbajli(resu, nbpara, zk16(jintk), nuord, zr(jintr),&
                cbid, valk, 0)
    call detrsd('CHAM_ELEM_S', '&&PEECAL.CESOUT')
    call jedetr('&&PEECAL.INTE_R')
    call jedetr('&&PEECAL.INTE_K')
    call jedetr('&&PEECAL.IND.MAILLE')
    call jedetr('&&PEECAL.LIST_CMP')
!
    call jedema()
!
end subroutine

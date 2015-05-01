subroutine peecal(tych, resu, nomcha, lieu, nomlie,&
                  modele, ichagd, chpost, nbcmp, nomcmp,&
                  nomcp2, nuord, inst, iocc, ligrel, cespoi)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/chpond.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
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
#include "asterfort/utflmd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbcmp, nuord, iocc, ichagd
    character(len=8) :: nomcmp(nbcmp), nomcp2(nbcmp), modele, lieu
    character(len=19) :: chpost, resu, cespoi, ligrel
    character(len=24) :: nomcha
    character(len=*) :: nomlie
    character(len=4) :: tych
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: iret, nbma, nbmai, i, jcesl, jcesd, jpoil, jpoid
    integer :: nucmp, jcmpgd, ncmpm, iad, jintr, jintk
    integer :: ipt, nbsp, nbpt, icmp, ima, nbpara
    integer :: ico, ind1, ind2, ifm, niv
    real(kind=8) :: vol, val, inst, volpt
    complex(kind=8) :: cbid
    character(len=8) :: noma, k8b, nomgd, nomva
    character(len=4) :: dejain
    character(len=19) :: cesout
    character(len=24) :: valk(3)
    aster_logical :: exist
    real(kind=8), pointer :: pdsm(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    real(kind=8), pointer :: poiv(:) => null()
    integer, pointer :: repe(:) => null()
! -------------------------------------------------------------------------
    call jemarq()
    cbid=(0.d0,0.d0)
    call infniv(ifm, niv)
!
    k8b='        '
!
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)

    call jeveuo(ligrel//'.REPE', 'L', vi=repe)
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
! --- POUR LES CHAM_ELEM / ELEM : MOT CLE DEJA_INTEGRE:
    if (tych .eq. 'ELEM') then
        call getvtx('INTEGRALE', 'DEJA_INTEGRE', iocc=iocc, scal=dejain, nbret=iret)
        if (iret .eq. 0) then
            call utmess('F', 'UTILITAI7_13', sk=valk(1))
        endif
    endif
!
!
! --- CALCULS DES CHAMPS SIMPLES:
!      CESOUT: CHAMP ELXX CORRESPONDANT AU CHAMP CHPOST (SIMPLE) PONDERE
!              PAR LE POIDS*JACOBIEN.
!      CESPOI: CHAMP ELXX CORRESPONDANT AU POIDS*JACOBIEN
    cesout='&&PEECAL.CESOUT'
    call chpond(tych, dejain, chpost, cesout, cespoi, ligrel,k8b)
    call jeveuo(cesout//'.CESV', 'L', vr=cesv)
    call jeveuo(cesout//'.CESL', 'L', jcesl)
    call jeveuo(cesout//'.CESD', 'L', jcesd)
    call jeveuo(cesout//'.CESK', 'L', vk8=cesk)
    call jeveuo(cespoi//'.CESV', 'L', vr=poiv)
    call jeveuo(cespoi//'.CESL', 'L', jpoil)
    call jeveuo(cespoi//'.CESD', 'L', jpoid)
    if (tych .ne. 'ELGA') call jeveuo(cespoi//'.PDSM', 'L', vr=pdsm)
!
!
! --- RECUPERATION DE LA LISTE DES CMPS DU CATALOGUE :
!     (POUR LA GRANDEUR VARI_* , IL FAUT CONSTITUER :(V1,V2,...,VN))
    nomgd = cesk(2)
    call jelira(cesout//'.CESC', 'LONMAX', ncmpm)
    if (nomgd(1:5) .ne. 'VARI_') then
        call jeveuo(cesout//'.CESC', 'L', jcmpgd)
    else
        call wkvect('&&PEECAL.LIST_CMP', 'V V K8', ncmpm, jcmpgd)
        do i = 1, ncmpm
            nomva = 'V'
            call codent(i, 'G', nomva(2:8))
            zk8(jcmpgd-1+i) = nomva
        end do
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
    do icmp = 1, nbcmp
        nucmp=indik8(zk8(jcmpgd),nomcmp(icmp),1,ncmpm)
        val=0.d0
        vol=0.d0
        ico=0
        do ima = 1, nbma
            if (repe(2*(ima-1)+1).eq.0) cycle
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
            if (nbsp .gt. 1) then
                call utmess('F', 'UTILITAI8_60')
            endif
            do ipt = 1, nbpt
                call cesexi('C', jcesd, jcesl, ima, ipt,&
                            1, nucmp, iad)
                ASSERT(iad.ge.0)
                if (iad .eq. 0) cycle
!
                val=val+cesv(iad)
!
                if (tych .eq. 'ELGA') then
                    call cesexi('C', jpoid, jpoil, ima, ipt,&
                                1, 1, iad)
                    ASSERT(iad.gt.0)
                    volpt=poiv(iad)
                else if (tych.eq.'ELEM') then
                    ASSERT(nbpt.eq.1)
                    volpt=pdsm(ima)
                else if (tych.eq.'ELNO') then
                    ASSERT(nbpt.ge.1)
                    volpt=pdsm(ima)/nbpt
                endif
                ico=ico+1
                vol=vol+volpt
            end do
        end do
        if (ico .eq. 0) then
            valk(3)=nomcmp(icmp)
            call utmess('F', 'UTILITAI7_12', nk=3, valk=valk)
        endif
!
        if (icmp .eq. 1) zr(jintr+icmp+ind2-1)=vol
        zr(jintr+icmp+ind2)=val
        zk16(jintk+ind1+icmp-1)='INTE_'//nomcp2(icmp)
        zr(jintr+nbcmp+icmp+ind2)=val/vol
        zk16(jintk+ind1+nbcmp+icmp-1)='MOYE_'//nomcp2(icmp)
    end do
!
!
! --- ON AJOUTE LES PARAMETRES MANQUANTS DANS LA TABLE:
    call tbexip(resu, lieu, exist, k8b)
    if (.not.exist) then
        call tbajpa(resu, 1, zk16(jintk+ind1-1), 'K16')
    endif
    do icmp = 1, nbcmp*2
        call tbexip(resu, zk16(jintk+ind1+icmp-1), exist, k8b)
        if (.not.exist) then
            call tbajpa(resu, 1, zk16(jintk+ind1+icmp-1), 'R')
        endif
    end do
!
! --- ON REMPLIT LA TABLE
    nbpara=ind1+nbcmp*2
    call tbajli(resu, nbpara, zk16(jintk), [nuord], zr(jintr),&
                [cbid], valk, 0)
    call detrsd('CHAM_ELEM_S', '&&PEECAL.CESOUT')
    call jedetr('&&PEECAL.INTE_R')
    call jedetr('&&PEECAL.INTE_K')
    call jedetr('&&PEECAL.IND.MAILLE')
    call jedetr('&&PEECAL.LIST_CMP')
    call jedetr('&&PEECAL.MAILLES_FILTRE')
    call jedetr('&&PEECAL.MES_MAILLES')
!
    call jedema()
!
end subroutine

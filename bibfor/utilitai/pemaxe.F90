subroutine pemaxe(resu, nomcha, lieu, nomlie, modele,&
                  chpost, nbcmp, nomcmp, nuord, inst,&
                  iocc)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
#include "asterfort/wkvect.h"
!
    integer :: nbcmp, nuord, iocc
    character(len=8) :: nomcmp(nbcmp), modele, nomlie, lieu
    character(len=19) :: chpost, resu
    character(len=24) :: nomcha
!
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "MINMAX"
!     ROUTINE D'APPEL : PEMIMA
!
!     BUT : EXTRAIRE LE MIN ET LE MAX D'UNE CMP D'UN CHAMELEM
!           ET LES STOCKER DANS LA TABLE
!
!     IN  RESU   : NOM DE LA TABLE
!     IN  NOMCHA : NOM SYMBOLIQUE DU CHAMP DU POST-TRAITEMENT
!     IN  LIEU   : LIEU DU POST-TRAITEMENT
!         (LIEU='TOUT'/'GROUP_MA'/'MAILLE')
!     IN  NOMLIE : NOM DU LIEU
!     IN  MODELE : NOM DU MODELE
!     IN  CHPOST  : NOM DU CHAMP DU POST-TRAITEMENT
!     IN  NBCMP   : NOMBRE DE COMPOSANTES
!     IN  NOMCMP  : NOM DES COMPOSANTES
!     IN  NUORD   : NUMERO D'ORDRE
!     IN  INST    : INSTANT
!     IN  IOCC    : NUMERO DE L'OCCURENCE
!     ------------------------------------------------------------------
!
    integer :: nbma, nbmai, i,  jcesl, jcesd
    integer :: nucmp,  jcmpgd, ncmpm, iad, indma
    integer :: jmesma, ipt, nbsp, nbpt, icmp, ima, nbpara, nbno
    integer :: nmin, nmax, npara, pmax, pmin
    real(kind=8) :: vmin, vmax, inst
    complex(kind=8) :: cbid
    character(len=8) :: noma, k8b, typmcl(2), nomgd, nomva, knmin, knmax
    character(len=16) :: motcle(2)
    character(len=19) :: ligrel, cesout
    character(len=24) :: mesmai, nommai
    logical(kind=1) :: exist
! Tableaux automatiques F90
    real(kind=8) :: mima(2*nbcmp+2)
    character(len=16) :: nompar(6*nbcmp+5), mamax(2*nbcmp+3)
    integer :: ptmax(1+2*nbcmp)
    character(len=8), pointer :: cesk(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
!
    call jemarq()
!
    cbid=(0.d0,0.d0)
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrel)
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
    nommai = noma//'.NOMMAI         '
!
! --- CREATION D'UN TABLEAU D'INDICES POUR REPERER
!     LES MAILLES DU POST TRAITEMENT
    call wkvect('&&PEMAXC_IND.MAILLE', 'V V I', nbma, indma)
    if (lieu(1:4) .ne. 'TOUT') then
        mesmai = '&&PEMAXC_NUM.MAILLE'
        motcle(1) = 'GROUP_MA'
        typmcl(1) = 'GROUP_MA'
        call reliem(' ', noma, 'NU_MAILLE', 'MINMAX', iocc,&
                    1, motcle, typmcl, mesmai, nbmai)
        call jeveuo(mesmai, 'L', jmesma)
        do i = 1, nbma
            zi(indma+i-1)=0
        end do
        do i = 1, nbmai
            zi(indma+zi(jmesma+i-1)-1)=1
        end do
    else
        do i = 1, nbma
            zi(indma+i-1)=1
        end do
    endif
!
    nompar(1)  ='CHAMP_GD'
    nompar(2)='NUME_ORDRE'
    nompar(3)='INST'
    nompar(4)=lieu
    mima(1)=inst
    mamax(1)=nomcha
    mamax(2)=nomlie
!
    call tbexip(resu, lieu, exist, k8b)
    if (.not.exist) then
        call tbajpa(resu, 1, nompar(4), 'K16')
    endif
!
! --- CALCULS DES CHAMPS SIMPLES:
    cesout='&&PEMAXC_CESOUT'
!
    call celces(chpost, 'V', cesout)
    call jeveuo(cesout//'.CESV', 'L', vr=cesv)
    call jeveuo(cesout//'.CESL', 'L', jcesl)
    call jeveuo(cesout//'.CESD', 'L', jcesd)
    call jeveuo(cesout//'.CESK', 'L', vk8=cesk)
!
!
! --- RECUPERATION DE LA LISTE DES CMPS DU CATALOGUE :
!     (POUR LA GRANDEUR VARI_* , IL FAUT CONSTITUER :(V1,V2,...,VN))
    nomgd = cesk(2)
    call jelira(cesout//'.CESC', 'LONMAX', ncmpm)
    if (nomgd(1:5) .ne. 'VARI_') then
        call jeveuo(cesout//'.CESC', 'L', jcmpgd)
    else
        call wkvect('&&PEMAXC.LIST_CMP', 'V V K8', ncmpm, jcmpgd)
        do i = 1, ncmpm
            nomva = 'V'
            call codent(i, 'G', nomva(2:8))
            zk8(jcmpgd-1+i) = nomva
        end do
    endif
!
    do icmp = 1, nbcmp
        nucmp=indik8(zk8(jcmpgd),nomcmp(icmp),1,ncmpm)
        vmin=r8maem()
        vmax=-r8maem()
!
        do ima = 1, nbma
            if (zi(indma+ima-1) .ne. 1) goto 35
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            nbsp=zi(jcesd-1+5+4*(ima-1)+2)
            ASSERT(nbsp.eq.1)
            do ipt = 1, nbpt
                call cesexi('C', jcesd, jcesl, ima, ipt,&
                            1, nucmp, iad)
                if (iad .gt. 0) then
                    if (vmax .lt. cesv(iad)) then
                        vmax=cesv(iad)
                        nmax=ima
                        pmax=ipt
                    endif
                    if (vmin .gt. cesv(iad)) then
                        vmin=cesv(iad)
                        nmin=ima
                        pmin=ipt
                    endif
                endif
            end do
 35         continue
        end do
!
        nompar(4+6*(icmp-1)+1)='MAX_'//nomcmp(icmp)
        nompar(4+6*(icmp-1)+2)='MA_MAX_'//nomcmp(icmp)
        nompar(4+6*(icmp-1)+3)='PT_MAX_'//nomcmp(icmp)
        nompar(4+6*(icmp-1)+4)='MIN_'//nomcmp(icmp)
        nompar(4+6*(icmp-1)+5)='MA_MIN_'//nomcmp(icmp)
        nompar(4+6*(icmp-1)+6)='PT_MIN_'//nomcmp(icmp)
!
        mima(1+2*(icmp-1)+1)=vmax
        mima(1+2*(icmp-1)+2)=vmin
!
        call jenuno(jexnum(nommai, nmin), knmin)
        call jenuno(jexnum(nommai, nmax), knmax)
!
        mamax(2+2*(icmp-1)+1)=knmax
        mamax(2+2*(icmp-1)+2)=knmin
        ptmax(1+2*(icmp-1)+1)=pmax
        ptmax(1+2*(icmp-1)+2)=pmin
!
        call tbexip(resu, nompar(4+6*(icmp-1)+1), exist, k8b)
        if (.not.exist) then
            call tbajpa(resu, 1, nompar(4+6*(icmp-1)+1), 'R')
            call tbajpa(resu, 1, nompar(4+6*(icmp-1)+2), 'K16')
            call tbajpa(resu, 1, nompar(4+6*(icmp-1)+3), 'I')
            call tbajpa(resu, 1, nompar(4+6*(icmp-1)+4), 'R')
            call tbajpa(resu, 1, nompar(4+6*(icmp-1)+5), 'K16')
            call tbajpa(resu, 1, nompar(4+6*(icmp-1)+6), 'I')
        endif
    end do
!
    npara=6*nbcmp
    ptmax(1)=nuord
!
! --- ON REMPLIT LA TABLE
    nbpara=4+npara
    call tbajli(resu, nbpara, nompar, ptmax, mima,&
                [cbid], mamax, 0)
!
    call jedetr('&&PEMAXC_IND.MAILLE')
    call jedetr('&&PEMAXC_CESOUT')
    call jedetr('&&PEMAXC.LIST_CMP')
!
    call jedema()
!
end subroutine

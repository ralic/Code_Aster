subroutine pemaxn(resu, nomcha, lieu, nomlie, modele,&
                  chpost, nbcmp, nomcmp, nuord, inst)
!
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbcmp, nuord
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
!     BUT : EXTRAIRE LE MIN ET LE MAX D'UNE CMP D'UN CHAMNO
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
!     ------------------------------------------------------------------
!
    integer :: iret, nbma, i, jcesv, jcesl, jcesd
    integer :: jcesk, jcmpgd, ncmpm, ibid, jcesc
    integer :: icmp, nbpara, nbno
    integer :: ino, nmin, nmax, npara, nbcmpm
    real(kind=8) :: vmin, vmax, inst
    complex(kind=8) :: cbid
    character(len=8) :: noma, k8b, nomgd, nomva, knmin, knmax
    character(len=19) :: ligrel, cesout
    character(len=24) :: nomnoe
    logical :: exist
! Tableaux automatiques F90
    real(kind=8) :: mima(2*nbcmp+2)
    character(len=16) :: nompar(4*nbcmp+5), nomax(2*nbcmp+3)
!
    call jemarq()
!
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrel, iret)
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, iret)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8b, iret)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8b, iret)
!
    if (lieu(1:4) .ne. 'TOUT') call u2mess('A', 'UTILITAI3_94')
!
    nomnoe = noma//'.NOMNOE         '
    nompar(1)  ='CHAMP_GD'
    nompar(2)='NUME_ORDRE'
    nompar(3)='INST'
    nompar(4)=lieu
    mima(1)=inst
    nomax(1)=nomcha
    nomax(2)=nomlie
!
    call tbexip(resu, lieu, exist, k8b)
    if (.not.exist) then
        call tbajpa(resu, 1, nompar(4), 'K16')
    endif
!
! --- CALCULS DES CHAMPS SIMPLES:
    cesout='&&PEMAXC_CESOUT'
    call cnocns(chpost, 'V', cesout)
    call jeveuo(cesout//'.CNSV', 'L', jcesv)
    call jeveuo(cesout//'.CNSL', 'L', jcesl)
    call jeveuo(cesout//'.CNSD', 'L', jcesd)
    call jeveuo(cesout//'.CNSK', 'L', jcesk)
    call jeveuo(cesout//'.CNSC', 'L', jcesc)
!
! --- RECUPERATION DE LA LISTE DES CMPS DU CATALOGUE :
!     (POUR LA GRANDEUR VARI_* , IL FAUT CONSTITUER :(V1,V2,...,VN))
    nomgd = zk8(jcesk-1+2)
    call jelira(cesout//'.CNSC', 'LONMAX', ncmpm, k8b)
    if (nomgd(1:5) .ne. 'VARI_') then
        call jeveuo(cesout//'.CNSC', 'L', jcmpgd)
    else
        call wkvect('&&PEMAXC.LIST_CMP', 'V V K8', ncmpm, jcmpgd)
        do 25 i = 1, ncmpm
            nomva = 'V'
            call codent(i, 'G', nomva(2:8))
            zk8(jcmpgd-1+i) = nomva
25      continue
    endif
!
    npara=4*nbcmp
    nbcmpm=zi(jcesd+1)
!
    do 30 i = 1, nbcmp
        vmin=r8maem()
        vmax=-r8maem()
        icmp=indik8(zk8(jcesc),nomcmp(i),1,nbcmpm)
        ASSERT(icmp.ge.0)
        do 36 ino = 1, nbno
            if (zl(jcesl+(ino-1)*nbcmpm+icmp-1)) then
                if (vmax .lt. zr(jcesv+(ino-1)*nbcmpm+icmp-1)) then
                    vmax=zr(jcesv+(ino-1)*nbcmpm+icmp-1)
                    nmax=ino
                endif
                if (vmin .gt. zr(jcesv+(ino-1)*nbcmpm+icmp-1)) then
                    vmin=zr(jcesv+(ino-1)*nbcmpm+icmp-1)
                    nmin=ino
                endif
            endif
36      continue
        mima(1+2*(i-1)+1)=vmax
        mima(1+2*(i-1)+2)=vmin
        call jenuno(jexnum(nomnoe, nmin), knmin)
        call jenuno(jexnum(nomnoe, nmax), knmax)
        nomax(2+2*(i-1)+1)=knmax
        nomax(2+2*(i-1)+2)=knmin
!
        nompar(4+4*(i-1)+1)='MAX_'//nomcmp(i)
        nompar(4+4*(i-1)+2)='NO_MAX_'//nomcmp(i)
        nompar(4+4*(i-1)+3)='MIN_'//nomcmp(i)
        nompar(4+4*(i-1)+4)='NO_MIN_'//nomcmp(i)
!
! ---    ON AJOUTE LES PARAMETRES MANQUANTS DANS LA TABLE:
        call tbexip(resu, nompar(4+4*(i-1)+1), exist, k8b)
        if (.not.exist) then
            call tbajpa(resu, 1, nompar(4+4*(i-1)+1), 'R')
            call tbajpa(resu, 1, nompar(4+4*(i-1)+2), 'K16')
            call tbajpa(resu, 1, nompar(4+4*(i-1)+3), 'R')
            call tbajpa(resu, 1, nompar(4+4*(i-1)+4), 'K16')
        endif
!
!
30  end do
!
! --- ON REMPLIT LA TABLE
    nbpara=4+npara
    call tbajli(resu, nbpara, nompar, nuord, mima,&
                cbid, nomax, 0)
!
    call jedetr('&&PEMAXC_CESOUT')
    call jedetr('&&PEMAXC.LIST_CMP')
!
    call jedema()
!
end subroutine

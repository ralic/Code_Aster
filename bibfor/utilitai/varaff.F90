subroutine varaff(noma, gran, base, ceselz)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=8) :: noma, gran
    character(len=*) :: ceselz
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! BUT :
!  - TRAITER L'OPTION 'AFFE' DE LA COMMANDE CREA_CHAMP POUR VARI_R
!  - CREER LE CHAM_ELEM_S / ELEM  (CESELZ)
!-----------------------------------------------------------------------
    integer :: ibid, nocc, nbtou, n1
    integer :: iad, k, iocc, nbmail
    character(len=8) :: kbid, typmcl(2)
    character(len=6) :: knuva
    character(len=16) :: motclf, motcls(2)
    character(len=19) :: ceselm
    character(len=24) :: mesmai
    integer :: iarg, nvarmx, jcesd, jcesl, jcesv, jlnova, jlnovx, jlvavx
    integer :: jmesma, kvari, n2, numa, nuva, nuvamx, nbmato
    parameter  (nvarmx=10000)
    logical :: ltou
!     ------------------------------------------------------------------
    call jemarq()
!
    if (noma .eq. ' ') call u2mess('F', 'UTILITAI_10')
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmato,&
                kbid, ibid)
!
    ASSERT(gran.eq.'VARI_R')
    call wkvect('&&VARAFF.LNOVX', 'V V K8', nvarmx, jlnovx)
    call wkvect('&&VARAFF.LVAVX', 'V V R', nvarmx, jlvavx)
!
    motclf = 'AFFE'
    call getfac(motclf, nocc)
!
    mesmai = '&&VARAFF.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
!
!     0- CALCUL DU PLUS GRAND NUMERO DE VARI UTILISE (NUVAMX):
!     --------------------------------------------------------
    nuvamx=0
    do 29 iocc = 1, nocc
        call getvtx(motclf, 'NOM_CMP', iocc, iarg, nvarmx,&
                    zk8(jlnovx), n1)
        ASSERT(n1.gt.0)
        do 28, k=1,n1
        ASSERT(zk8(jlnovx-1+k)(1:1).eq.'V')
        read (zk8(jlnovx-1+k)(2:8),'(I7)') nuva
        nuvamx=max(nuvamx,nuva)
28      continue
29  end do
    ASSERT(nuvamx.gt.0)
!
!
!     1- ALLOCATION DE CESELM
!     --------------------------------------------
    call wkvect('&&VARAFF.LNOVA', 'V V K8', nuvamx, jlnova)
    do 27, k=1,nuvamx
    call codent(k, 'G', knuva)
    zk8(jlnova-1+k)='V'//knuva
    27 end do
    ceselm = ceselz
!     -- REMARQUE : LES CMPS SERONT DANS L'ORDRE V1,V2,...
    call cescre(base, ceselm, 'ELEM', noma, 'VARI_R',&
                nuvamx, zk8(jlnova), 0, -1, -nuvamx)
!
    call jeveuo(ceselm//'.CESD', 'L', jcesd)
    call jeveuo(ceselm//'.CESV', 'E', jcesv)
    call jeveuo(ceselm//'.CESL', 'E', jcesl)
!
!
!     2- BOUCLE SUR LES OCCURENCES DU MOT CLE AFFE
!     --------------------------------------------
    do 30 iocc = 1, nocc
!
        call getvtx(motclf, 'NOEUD', iocc, iarg, 0,&
                    kbid, n1)
        if (n1 .ne. 0) call u2mess('F', 'UTILITAI_12')
        call getvtx(motclf, 'GROUP_NO', iocc, iarg, 0,&
                    kbid, n1)
        if (n1 .ne. 0) call u2mess('F', 'UTILITAI_13')
!
        call getvtx(motclf, 'NOM_CMP', iocc, iarg, nvarmx,&
                    zk8(jlnovx), n1)
        call getvr8(motclf, 'VALE', iocc, iarg, nvarmx,&
                    zr(jlvavx), n2)
        ASSERT(n1.eq.n2)
!
!
!
        call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                    kbid, nbtou)
        if (nbtou .eq. 1) then
            ltou=.true.
            nbmail=nbmato
        else
            ltou=.false.
            call reliem(' ', noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcls, typmcl, mesmai, nbmail)
            ASSERT(nbmail.gt.0)
            call jeveuo(mesmai, 'L', jmesma)
        endif
!
        do 31, kvari=1,n1
        read (zk8(jlnovx-1+kvari)(2:8),'(I7)') nuva
        ASSERT(nuva.gt.0.and.nuva.le.nuvamx)
        do 32, k=1,nbmail
        if (ltou) then
            numa=k
        else
            numa=zi(jmesma-1+k)
        endif
        ASSERT(numa.gt.0.and.numa.le.nbmato)
!
        call cesexi('C', jcesd, jcesl, numa, 1,&
                    1, nuva, iad)
        ASSERT(iad.lt.0)
!
!           -- RECOPIE DE LA VALEUR:
        zl(jcesl-1-iad) = .true.
        zr(jcesv-1-iad) = zr(jlvavx-1+kvari)
32      continue
31      continue
!
        call jedetr(mesmai)
30  end do
!
!
    call jedetr('&&VARAFF.LNOVX')
    call jedetr('&&VARAFF.LNOVA')
    call jedema()
end subroutine

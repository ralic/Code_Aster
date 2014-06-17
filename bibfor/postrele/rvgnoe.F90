subroutine rvgnoe(mcf, iocc, nmaila, nlstnd, nbtrou,&
                  linoeu)
    implicit none
#include "jeveux.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/oreino.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iocc, nbtrou, linoeu(*)
    character(len=*) :: mcf
    character(len=8) :: nmaila
    character(len=24) :: nlstnd
!     ------------------------------------------------------------------
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
!     SAISIE DES NOEUDS DE L' OCCURENCE IOCC DE ACTION
!     CAS OU LE LIEU EST UNE LISTE DE GROUP_NO ET/OU NOEUD
!     ------------------------------------------------------------------
! IN   IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
! IN   NMAILA : K : NOM DU MAILLAGE CONTENANT LES GROUPES ET LES NOEUDS
! JXIN NLSTND : K : NOM OJB S V I <-- NUMERO DES NOEUDS
!     ------------------------------------------------------------------
!     CONSTRUCTION DE LA LISTE NLSTND :
!         LA LISTE ARGUMENT DE NOEUD
!         LES NOEUD DES GROUPES DE NOEUD DANS L' ORDRE DE LA LISTE
!         ARGUMENT DE GROUP_NO
!         SI 2 NOEUD CONSECUTIF DANS CETTE CONSTRUCTION SONT IDENTIQUES
!         ON N' EN GARDE QU' UN
!     ------------------------------------------------------------------
!
    integer :: nbrgpn, nbneud, aneud, agrpn, alndtp, alstnd, agneud
    integer :: i, j, k, libre, numnd, nbtnd, n1, nbn, iret, iera
    integer :: asgtu, i1, i2,  ny
    real(kind=8) :: vecty(3), tole
    character(len=8) :: courbe, crit
    character(len=24) :: nomgrn
    character(len=15) :: nrepnd
    character(len=17) :: nrepgn
    integer :: iarg
    real(kind=8), pointer :: vale(:) => null()
!
!==================== CORPS DE LA ROUTINE =============================
!
    call jemarq()
!
    nbtnd = 0
    nrepgn = nmaila//'.GROUPENO'
    nrepnd = nmaila//'.NOMNOE'
    libre = 1
!
! --- RECUPERATION DES ENTITES
!
    call getvem(nmaila, 'GROUP_NO', mcf, 'GROUP_NO', iocc,&
                iarg, 0, zk24(1), nbrgpn)
    call getvem(nmaila, 'NOEUD', mcf, 'NOEUD', iocc,&
                iarg, 0, zk8, nbneud)
    nbrgpn = -nbrgpn
    nbneud = -nbneud
    if (nbrgpn .ne. 0) then
        call wkvect('&OP0051.NOM.GRP.ND', 'V V K24', nbrgpn, agrpn)
        call getvem(nmaila, 'GROUP_NO', mcf, 'GROUP_NO', iocc,&
                    iarg, nbrgpn, zk24(agrpn), n1)
        do 10, i = 1, nbrgpn, 1
        call jelira(jexnom(nrepgn, zk24(agrpn+i-1)), 'LONUTI', n1)
        nbtnd = nbtnd + n1
10      continue
    endif
    if (nbneud .ne. 0) then
        call wkvect('&OP0051.NOM.NOEUD', 'V V K8', nbneud, aneud)
        call getvem(nmaila, 'NOEUD', mcf, 'NOEUD', iocc,&
                    iarg, nbneud, zk8( aneud), n1)
        nbtnd = nbtnd + nbneud
    endif
!
    call wkvect('&OP0051.LIST.ND.TEMP', 'V V I', nbtnd, alndtp)
    do 20, i = 1, nbtnd, 1
    zi(alndtp + i-1) = 0
    20 end do
!
    if (nbneud .ne. 0) then
        do 200, i = 1, nbneud, 1
        call jenonu(jexnom(nrepnd, zk8(aneud + i-1)), numnd)
        zi(alndtp + i-1) = numnd
200      continue
    endif
    libre = nbneud + 1
    if (nbrgpn .ne. 0) then
        do 100, i = 1, nbrgpn, 1
        nomgrn = zk24(agrpn + i-1)
        call jelira(jexnom(nrepgn, nomgrn), 'LONMAX', nbn)
        call jeveuo(jexnom(nrepgn, nomgrn), 'L', agneud)
        do 110, j = 1, nbn, 1
        zi(alndtp + libre-1 + j-1) = zi(agneud + j-1)
110      continue
        libre = libre + nbn
100      continue
    endif
!
    nbtnd = 0
    if (nbtrou .eq. 0) then
        do 250, i = 1, libre-1, 1
        nbtnd = nbtnd + min(1,abs(zi(alndtp+i-1)-zi(alndtp+i)))
250      continue
    else
        do 252, i = 1, nbtrou, 1
        do 254, j = 1, libre-1, 1
        if (linoeu(i) .eq. zi(alndtp+j-1)) then
            nbtnd = nbtnd + 1
            goto 252
        endif
254      continue
252      continue
    endif
!
    if (nbtnd .eq. 0) then
        call utmess('F', 'POSTRELE_64')
    endif
    call wkvect(nlstnd, 'V V I', nbtnd, alstnd)
    nbtnd = libre - 1
    if (nbtrou .eq. 0) then
        libre = 2
        numnd = zi(alndtp)
        zi(alstnd) = numnd
        do 300, i = 2, nbtnd, 1
        if (numnd .ne. zi(alndtp + i-1)) then
            numnd = zi(alndtp + i-1)
            zi(alstnd + libre-1) = numnd
            libre = libre + 1
        endif
300      continue
    else
        libre = 1
        do 302, i = 1, nbtnd, 1
        numnd = zi(alndtp+i-1)
        do 304, j = 1, nbtrou, 1
        if (linoeu(j) .eq. numnd) then
            do 306, k = 1, libre-1, 1
            if (numnd .eq. zi(alstnd+k-1)) goto 302
306          continue
            zi(alstnd + libre-1) = numnd
            libre = libre + 1
        endif
304      continue
302      continue
    endif
!
! --- CAS PARTICULIER
!
    call getvr8('ACTION', 'VECT_Y', iocc=iocc, nbval=3, vect=vecty,&
                nbret=ny)
    if (ny .ne. 0) then
!        VERIFICATIONS PRELIMINAIRES
        if ((nbneud.ge.2.and.nbrgpn.eq.0) .or. ( nbneud.eq.0.and.nbrgpn.eq.1)) then
            iera = 0
            if (nbrgpn .eq. 1) then
                nomgrn = zk24(agrpn + 1-1)
                call jelira(jexnom(nrepgn, nomgrn), 'LONMAX', nbn)
                if (nbn .lt. 2) then
                    call utmess('F', 'POSTRELE_21')
                endif
            endif
        else
            call utmess('F', 'POSTRELE_22')
        endif
        call jeexin('&&YAPAS '//'S1   '//'.DESC', n1)
        if (n1 .ne. 0) call jedetr('&&YAPAS '//'S1   '//'.DESC')
        courbe='&&YAPAS'
        call wkvect(courbe//'S1   '//'.DESC', 'V V R', 6, asgtu)
!       ORIGINE
        i1=zi(alstnd-1+1)
!       EXTREMITE
        i2=zi(alstnd-1+libre-1)
        call jeveuo(nmaila//'.COORDO    .VALE', 'L', vr=vale)
!       TOLERANCE
        call getvtx(mcf, 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
        call getvr8(mcf, 'PRECISION', iocc=iocc, scal=tole, nbret=n1)
!       VERIFICATION QUE LES POINTS SONT ALIGNES
        call oreino(nmaila, zi(alstnd), libre-1, i1, i2,&
                    vale, crit, tole, iera, iret)
        if (iret .ne. 0) then
            call utmess('F', 'POSTRELE_60')
        endif
        zr(asgtu-1+1)=vale(3*(i1-1)+1)
        zr(asgtu-1+2)=vale(3*(i1-1)+2)
        zr(asgtu-1+3)=vale(3*(i1-1)+3)
        zr(asgtu-1+4)=vale(3*(i2-1)+1)
        zr(asgtu-1+5)=vale(3*(i2-1)+2)
        zr(asgtu-1+6)=vale(3*(i2-1)+3)
    endif
!
    call jeexin('&OP0051.NOM.NOEUD', n1)
    if (n1 .ne. 0) call jedetr('&OP0051.NOM.NOEUD')
    call jeexin('&OP0051.NOM.GRP.ND', n1)
    if (n1 .ne. 0) call jedetr('&OP0051.NOM.GRP.ND')
    call jedetr('&OP0051.LIST.ND.TEMP')
!
    call jedema()
end subroutine

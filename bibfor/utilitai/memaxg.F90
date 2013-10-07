subroutine memaxg(nborn, born, gbil, lonvec, result)
    implicit none
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexve.h"
#include "asterfort/tbtrtb.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nborn, lonvec
    real(kind=8) :: born(*), gbil(*)
    character(len=8) :: result
! ----------------------------------------------------------------------
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
!
!      OPERATEUR :     CALC_G
!      OPTION    :     'G_MAX_GLOB'
!
!     AUTEUR : M. BONNAMY
!     ----------------------------------------------------------------
!
!     BUT: MAXIMISATION DE G SOUS CONTRAINTES BORNES
!
!     ----------------------------------------------------------------
!
!     NBORN       /IN/:NOMBRE DE BORNES
!     BORN        /IN/:CONTRAINTES BORNES
!     GBIL        /IN/:TRIANGLE INFERIEUR DE LA MATRICE G BILINEAIRE
!     LONVEC      /IN/:NOMBRE DE CHAMPS DE DEPLACEMENTS
!     RESULT     /OUT/:TABLE RESULTAT
!
! ----------------------------------------------------------------------
!
!
    integer :: i, j, ibid, nbprup, nbpar, ncomb, nfreq
    integer :: ig, ind, indold, iprov, iq, k, igq, ipa, inopr, itypr
!
    real(kind=8) :: gmax, s
    complex(kind=8) :: cbid
!
    character(len=3) :: chnu
    character(len=8) :: k8bid, tabgma
    character(len=24) :: collec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nbprup = lonvec+2
    ncomb = 2**lonvec
    call wkvect('&&MEMAXG.NORU', 'V V K16', nbprup, inopr)
    call wkvect('&&MEMAXG.TYPR', 'V V K8', nbprup, itypr)
    do 140 i = 1, lonvec
        call codent(i, 'G', chnu)
        zk16(inopr+i-1) = 'Q_'//chnu
        zk8 (itypr+i-1) = 'R'
140  end do
    zk16(inopr+lonvec) = 'G'
    zk8 (itypr+lonvec) = 'R'
    zk16(inopr+lonvec+1) = 'G_MAX'
    zk8 (itypr+lonvec+1) = 'R'
!
    tabgma = 'G_MAX'
    call tbcrsd(tabgma, 'G')
    call tbajpa(tabgma, nbprup, zk16(inopr), zk8(itypr))
!
    if (nborn .ne. (2.d0*lonvec)) then
        call utmess('F', 'RUPTURE1_14')
!
    endif
    collec = '&&MEMAXG.BORNES_Q'
    call jecrec(collec, 'V V R', 'NU', 'CONTIG', 'CONSTANT',&
                2**lonvec)
    call jeecra(collec, 'LONMAX', lonvec)
    call wkvect('&&MEMAXG.GQIJ', 'V V R8', lonvec, igq)
    call wkvect('&&MEMAXG.TABL', 'V V R8', lonvec+2, ipa)
!
!     STOCKAGE DES 2 PUISSANCE N COMBINAISON DE CHARGES
!
    do 170 i = 1, lonvec
        nfreq = 0
        ind = 0
        indold = 1
        do 180 j = 1, ncomb
            nfreq = nfreq+1
            if (i .eq. 1) call jecroc(jexnum(collec, j))
            call jeveuo(jexnum(collec, j), 'E', iq)
            if (nfreq .gt. (2**(lonvec-i))) then
                iprov = indold
                indold = ind
                ind = iprov
                nfreq = 1
            endif
            zr(iq+i-1) = born(2*(i-1)+ ind + 1)
180      continue
170  end do
!
!     BALAYAGE DES SOMMETS DE LA FORME QUADRATIQUE G = QGQ
!
    gmax = 0.d0
!
    do 190 k = 1, ncomb
!
        call jeveuo(jexnum(collec, k), 'L', iq)
!
        s = 0.d0
        do 200 i = 1, lonvec
            zr(igq+i-1) = 0.d0
            do 210 j = 1, lonvec
                if (i .lt. j) then
                    zr(igq+i-1) = zr(igq+i-1) + gbil(j*(j-1)/2 + i )* zr(iq+j-1)
                else
                    zr(igq+i-1) = zr(igq+i-1) + gbil(i*(i-1)/2 + j )* zr(iq+j-1)
                endif
210          continue
            s = s + zr(igq+i-1) * zr(iq+i-1)
            zr(ipa+i-1) = zr(iq+i-1)
200      continue
!
        if (s .gt. gmax) then
            gmax = s
        endif
!
        zr(ipa+lonvec) = s
        nbpar = lonvec+1
        call tbajli(tabgma, nbpar, zk16(inopr), [ibid], zr(ipa),&
                    [cbid], k8bid, 0)
!
190  end do
!
    call tbexve(tabgma, 'G', '&&MEMAXG.G', 'V', ncomb,&
                k8bid)
    call jeveuo('&&MEMAXG.G', 'L', ig)
!
    do 220 k = 1, ncomb
!
        if (zr(ig+k-1) .eq. gmax) then
            call jeveuo(jexnum(collec, k), 'L', iq)
            do 230 i = 1, lonvec
                zr(ipa+i-1) = zr(iq+i-1)
230          continue
            zr(ipa+lonvec) = gmax
            zr(ipa+lonvec+1) = gmax
            call tbajli(tabgma, nbprup, zk16(inopr), [ibid], zr(ipa),&
                        [cbid], k8bid, k)
        endif
!
220  end do
!
    call tbtrtb(tabgma, 'G', result, 1, zk16(inopr+lonvec),&
                'DE', 0.d0, 'ABSOLU  ')
    call detrsd('TABLE', tabgma)
!
    call jedema()
end subroutine

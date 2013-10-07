subroutine mmaxgl(nborn, born, gbil, noeu, abcur,&
                  lonvec, nnoff, result)
    implicit none
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexve.h"
#include "asterfort/tbfutb.h"
#include "asterfort/tbtrtb.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nborn, lonvec, nnoff
    real(kind=8) :: born(*), gbil(*), abcur(*)
    character(len=8) :: result, noeu(*)
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
!
!     AUTEUR : J. ANGLES
!     ----------------------------------------------------------------
!
!     BUT: MAXIMISATION DE G LOCAL SOUS CONTRAINTES BORNES
!
!     ----------------------------------------------------------------
!
!     NBORN       /IN/:NOMBRE DE BORNES
!     BORN        /IN/:CONTRAINTES BORNES
!     GBIL        /IN/:TRIANGLE INFERIEUR DE LA MATRICE G BILINEAIRE
!     NOEU        /IN/:LISTE DES NOEUDS DU FOND DE FISSURE
!     ABCUR       /IN/:ABSCISSE CURVILIGNE DU FOND DE FISSURE
!     LONVEC      /IN/:NOMBRE DE CHAMPS DE DEPLACEMENTS
!     NNOFF       /IN/:NOMBRE DE NOEUD EN FOND DE FISSURE
!     RESULT     /OUT/:TABLE RESULTAT
!
! ----------------------------------------------------------------------
!
!
    integer :: i, j, k, n, ibid, nbprup, nbpar, ncomb, nfreq, init
    integer :: ig, ind, indold, iprov, iq, igq, ipa, inopr, itypr, ni, nj
    integer :: irmax, ikmax
!
    real(kind=8) :: mgmax, gmax, s
    complex(kind=8) :: cbid
!
    character(len=3) :: chnu
    character(len=8) :: k8bid, tabgma, table(2)
    character(len=24) :: collec, chsigi
    character(len=16) :: valk
!
! ----------------------------------------------------------------------
!
    call jemarq()
    ibid=0
    cbid=(0.d0,0.d0)
!
!- RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
!-INUTILE ???
    call getvid('COMPORTEMENT', 'SIGM_INIT', iocc=1, scal=chsigi, nbret=init)
    if (init .ne. 0) then
        valk='CALC_G_MAX'
        call utmess('F', 'RUPTURE1_13', sk=valk)
    endif
!
    nbprup = lonvec+5
    ncomb = 2**lonvec
    call wkvect('&&MMAXGL.NORU', 'V V K16', nbprup, inopr)
    call wkvect('&&MMAXGL.TYPR', 'V V K8', nbprup, itypr)
    do 140 i = 1, lonvec
        call codent(i, 'G', chnu)
        zk16(inopr+i-1) = 'Q_'//chnu
        zk8 (itypr+i-1) = 'R'
140  end do
    zk16(inopr+lonvec) = 'NOEUD'
    zk8 (itypr+lonvec) = 'K8'
    zk16(inopr+lonvec+1) = 'ABSC_CURV'
    zk8 (itypr+lonvec+1) = 'R'
    zk16(inopr+lonvec+2) = 'G'
    zk8 (itypr+lonvec+2) = 'R'
    zk16(inopr+lonvec+3) = 'G_MAX'
    zk8 (itypr+lonvec+3) = 'R'
    zk16(inopr+lonvec+4) = 'MAX_G_MAX'
    zk8 (itypr+lonvec+4) = 'R'
!
    tabgma = 'G_MAX'
    table(1) = 'T1'
    table(2) = 'T2'
!
    call tbcrsd('T4', 'V')
    call tbajpa('T4', nbprup, zk16(inopr), zk8(itypr))
!
    if (nborn .ne. (2.d0*lonvec)) then
        call utmess('F', 'RUPTURE1_14')
!
    endif
    collec = '&&MMAXGL.BORNES_Q'
    call jecrec(collec, 'V V R', 'NU', 'CONTIG', 'CONSTANT',&
                2**lonvec)
    call jeecra(collec, 'LONMAX', lonvec)
    call wkvect('&&MMAXGL.GQIJ', 'V V R', lonvec, igq)
    call wkvect('&&MMAXGL.TABL', 'V V R', lonvec+4, ipa)
    call wkvect('&&MMAXGL.RMAX', 'V V R', lonvec+4, irmax)
    call wkvect('&&MMAXGL.KMAX', 'V V K8', 1, ikmax)
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
!     POUR CHAQUE NOEUD
!
    mgmax = 0.d0
!
    do 150 n = 1, nnoff
!
        call tbcrsd(tabgma, 'G')
        call tbajpa(tabgma, nbprup, zk16(inopr), zk8(itypr))
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
                        nj = j*(j-1)/2 + i
                        if (nj .eq. 1) then
                            zr(igq+i-1) = zr(igq+i-1) + gbil(n)*zr(iq+ j-1)
                        else
                            zr(igq+i-1) = zr(igq+i-1) + gbil(n+((nj-1) *nnoff))*zr(iq+j-1)
                        endif
                    else
                        ni = i*(i-1)/2 + j
                        if (ni .eq. 1) then
                            zr(igq+i-1) = zr(igq+i-1) + gbil(n)*zr(iq+ j-1)
                        else
                            zr(igq+i-1) = zr(igq+i-1) + gbil(n+((ni-1) *nnoff))*zr(iq+j-1)
                        endif
                    endif
210              continue
                s = s + zr(igq+i-1) * zr(iq+i-1)
                zr(ipa+i-1) = zr(iq+i-1)
200          continue
!
            if (s .gt. gmax) then
                gmax = s
            endif
!
            zr(ipa+lonvec) = abcur(n)
            zr(ipa+lonvec+1) = s
            nbpar = lonvec+3
!
!  CONSTRUCTION DE LA TABLE DES G
            call tbajli(tabgma, nbpar, zk16(inopr), [ibid], zr(ipa),&
                        [cbid], noeu(n), 0)
!
190      continue
!
        call tbexve(tabgma, 'G', '&&MMAXGL.G', 'V', ncomb,&
                    k8bid)
        call jeveuo('&&MMAXGL.G', 'L', ig)
!
        do 220 k = 1, ncomb
!
            if (zr(ig+k-1) .eq. gmax) then
                call jeveuo(jexnum(collec, k), 'L', iq)
                do 230 i = 1, lonvec
                    zr(ipa+i-1) = zr(iq+i-1)
230              continue
                zr(ipa+lonvec+1) = gmax
                zr(ipa+lonvec+2) = gmax
!
                if (gmax .ge. mgmax) then
                    mgmax = gmax
                    do 240 i = 1, lonvec
                        zr(irmax+i-1) = zr(ipa+i-1)
240                  continue
                    zr(irmax+lonvec) = zr(ipa+lonvec)
                    zr(irmax+lonvec+1) = zr(ipa+lonvec+1)
                    zr(irmax+lonvec+2) = zr(ipa+lonvec+2)
                    zr(irmax+lonvec+3) = mgmax
                    zk8(ikmax) = noeu(n)
                endif
!
                nbpar = lonvec+4
                call tbajli(tabgma, nbpar, zk16(inopr), [ibid], zr( ipa),&
                            [cbid], noeu(n), k)
!
!  CONSTRUCTION DE LA TABLE DES G_MAX
                call tbajli('T4', nbpar, zk16(inopr), [ibid], zr(ipa),&
                            [cbid], noeu(n), 0)
            endif
!
220      continue
!
        call tbtrtb(tabgma, 'V', table(2), 1, zk16(inopr+lonvec+2),&
                    'DE', 0.d0, 'ABSOLU  ')
        call detrsd('TABLE', tabgma)
        call jedetr('&&MMAXGL.G')
        if (n .eq. 1) then
            call copisd('TABLE', 'V', table(2), table(1))
        else
            call tbfutb('T3', 'V', 2, table, ' ',&
                        ' ', [0], [0.d0], [cbid], k8bid)
            call detrsd('TABLE', table(1))
            call copisd('TABLE', 'V', 'T3', table(1))
            call detrsd('TABLE', 'T3')
        endif
        call detrsd('TABLE', table(2))
!
150  end do
!
!  CONSTRUCTION DE LA TABLE SYNTHETISANT LES G_MAX
!  ORDONNEE SELON G_MAX DECROISSANT
!
    call tbtrtb('T4', 'V', 'T5', 1, zk16(inopr+lonvec+3),&
                'DE', 0.d0, 'ABSOLU  ')
    call tbajli('T5', nbprup, zk16(inopr), [ibid], zr(irmax),&
                [cbid], zk8(ikmax), 1)
    call detrsd('TABLE', 'T4')
!
!  CONSTRUCTION DE LA TABLE SYNTHETISANT LES G_MAX
!  ORDONNEE SELON L'ABSCISSE CURVILIGNE CROISSANTE
!
    call tbtrtb('T5', 'V', 'T4', 1, zk16(inopr+lonvec+1),&
                'CR', 0.d0, 'ABSOLU  ')
!
!  CONSTRUCTION DE LA TABLE FINALE
!
    call copisd('TABLE', 'V', table(1), 'T3')
    call detrsd('TABLE', table(1))
    call copisd('TABLE', 'V', 'T5', table(1))
    call copisd('TABLE', 'V', 'T4', table(2))
    call detrsd('TABLE', 'T4')
    call detrsd('TABLE', 'T5')
    call tbfutb('T4', 'V', 2, table, ' ',&
                ' ', [0], [0.d0], [cbid], k8bid)
    call detrsd('TABLE', table(1))
    call detrsd('TABLE', table(2))
    call copisd('TABLE', 'V', 'T4', table(1))
    call copisd('TABLE', 'V', 'T3', table(2))
    call detrsd('TABLE', 'T3')
    call detrsd('TABLE', 'T4')
    call tbfutb('T4', 'V', 2, table, ' ',&
                ' ', [0], [0.d0], [cbid], k8bid)
!
    call copisd('TABLE', 'G', 'T4', result)
!
    call detrsd('TABLE', table(1))
    call detrsd('TABLE', table(2))
    call detrsd('TABLE', 'T4')
!
    call jedema()
end subroutine

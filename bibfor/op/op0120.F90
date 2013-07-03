subroutine op0120()
    implicit none
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
!     CALCUL D'UNE MATRICE INTERSPECTRALE
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterfort/calint.h"
#include "asterfort/fft.h"
#include "asterfort/fointe.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/intimp.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rms.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!-----------------------------------------------------------------------
    integer :: i, ifft, ifm, imatr, it, j, k
    integer :: kb, kf, kk, ktabl, l, l1, l2
    integer :: lcomp1, lfon, lint, long1, long2
    integer :: lresu1, lrms, ls1, lssx, ltabl, lvalc, lvale
    integer :: nbpts, nbpts2, nda, ndd
    integer :: nfcod, nfonc, niv, nmatr
    real(kind=8) :: bmatr, dfreq, dt, durana, durdec, frefin, freini
    real(kind=8) :: pts, pts1, pts2, pts3, tinst, tinst1
    real(kind=8) :: tinst2
!-----------------------------------------------------------------------
    integer :: long, ival(2), ier
    real(kind=8) :: resu, zero
    character(len=8) :: k8b, nomu, nomref
    character(len=16) :: concep, nomcmd
    character(len=19) :: nomfon
    character(len=24) :: nomobj
    character(len=24) :: chnumi, chnumj, chfreq, chvale
!
    integer :: iarg, ispec
    integer :: lnumi, lnumj, lfreq, lrefe, nbabs, mxval, ipf
!
!     ------------------------------------------------------------------
!
!     --- INITIALISATION DES DIVERS ---
    call jemarq()
!
    call getres(nomu, concep, nomcmd)
!
    call getvr8(' ', 'INST_INIT', 0, iarg, 1,&
                tinst1, l)
    call getvr8(' ', 'INST_FIN', 0, iarg, 1,&
                tinst2, l)
    call getvis(' ', 'NB_POIN', 0, iarg, 1,&
                nbpts, l)
    call getvid(' ', 'FONCTION', 0, iarg, 0,&
                k8b, nfonc)
    nfonc = abs(nfonc)
!
!    --- VERIFICATION DU NOMBRE DE POINTS ---
    pts = log(dble(nbpts))/log(2.d0)
    pts1 = aint(pts)
    pts2 = abs(pts1-pts)
    pts3 = abs(1.d0-pts2)
    if (pts2 .ge. 1.d-06 .and. pts3 .ge. 1.d-06) then
        call u2mess('F', 'ALGORITH9_56')
    endif
!
    call infmaj()
    call infniv(ifm, niv)
!
    nomref=nomu(1:8)
!
    call wkvect(nomref//'.REFE', 'G V K16', 2, lrefe)
    zk16(lrefe) = 'DSP'
    zk16(lrefe+1) = 'TOUT'
!
    durana = tinst2 - tinst1
    call getvr8(' ', 'DUREE_ANALYSE', 0, iarg, 1,&
                durana, nda)
!
    durdec = durana
    call getvr8(' ', 'DUREE_DECALAGE', 0, iarg, 1,&
                durdec, ndd)
!
    if (nda .ne. 0) then
        bmatr = ( (tinst2-tinst1) - durana ) / durdec
        nmatr = int( abs(bmatr) + 1 )
    else
        nmatr = 1
    endif
!
    call wkvect('&&OP0120.TEMP.LFON', 'V V K8', nfonc, lfon)
    call wkvect('&&OP0120.TEMP.VALE', 'V V C', nbpts, lvale)
!
    call getvid(' ', 'FONCTION', 0, iarg, nfonc,&
                zk8(lfon), l)
!
    dt = durana / nbpts
    long = nbpts * nfonc / 2
    nfcod = nfonc * ( nfonc+1 ) / 2
    long1 = nbpts * nfcod
    long2 = nmatr * nfcod
    nbpts2 = nbpts / 2
    dfreq = 1.d0 / durana
!C
    call wkvect('&&OP0120.TEMP.VALC', 'V V C', long, lvalc)
    call wkvect('&&OP0120.TEMP.LINT', 'V V R', nbpts, lint)
    call wkvect('&&OP0120.TEMP.LSSX', 'V V R', long1, lssx)
    call wkvect('&&OP0120.TEMP.LRMS', 'V V R', long2, lrms)
!C
    do 20 imatr = 1, nmatr
        do 30 kf = 1, nfonc
            nomfon = zk8(lfon+kf-1)
            do 50 it = 1, nbpts
                tinst = tinst1 + (imatr-1)* (durdec) + (it-1)*dt
                call fointe('F ', nomfon, 1, 'INST', tinst,&
                            resu, ier)
                zero = 0.d0
                zc(lvale+it-1) = dcmplx(resu,zero)
50          continue
!       --- CALCUL DE LA TRANFORMEE DE FOURIER ---
            ifft = 1
            call fft(zc(lvale), nbpts, ifft)
!       ---
            do 60 it = 1, nbpts2
                lresu1 = lvalc + (kf-1)*nbpts2 + (it-1)
                zc(lresu1) = zc(lvale+it-1)*dt
60          continue
30      continue
        lcomp1 = 0
        do 70 j = 1, nfonc
            do 80 i = 1, j
!
                call calint(i, j, zc(lvalc), nbpts, zr(lint),&
                            long, durana)
!
                do 90 kk = 1, nbpts2
                    l1 = lint + kk - 1
                    l2 = lssx + kk - 1 + nbpts*lcomp1
                    zr(l2) = zr(l2) + zr(l1)
                    zr(l2+nbpts2) = zr(l2+nbpts2) + zr(l1+nbpts2)
90              continue
                lcomp1 = lcomp1 + 1
80          continue
70      continue
        call rms(imatr, zr(lssx), long1, zr(lrms), long2,&
                 nbpts, nfcod, dfreq, nfonc)
20  end do
    do 110 kb = 1, long1
        ls1 = lssx + kb - 1
        zr(ls1) = zr(ls1)/dble(nmatr)
110  end do
!
!     --- CREATION DES NOMS DE FONCTIONS ---
    mxval = nfonc*(nfonc+1)/2
    chvale = nomref//'.VALE'
    call jecrec(chvale, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',&
                mxval)
    chfreq = nomref//'.FREQ'
    chnumi = nomref//'.NUMI'
    chnumj = nomref//'.NUMJ'
    call wkvect(chnumi, 'G V I', mxval, lnumi)
    call wkvect(chnumj, 'G V I', mxval, lnumj)
    call wkvect(chfreq, 'G V R', nbpts2, lfreq)
!
    do 250 k = 1, nbpts2
        zr(lfreq+k-1) = (k-1)*dfreq
250  end do
!
    ktabl = 1
    ipf = 0
    do 130 j = 1, nfonc
        ival(2) = j
!
        do 140 i = 1, j
            ival(1) = i
            ipf = ipf+1
            zi(lnumi-1+ipf) = ival(1)
            zi(lnumj-1+ipf) = ival(2)
!
            if (ival(1) .eq. ival(2)) then
                nbabs = nbpts2
            else
                nbabs = 2*nbpts2
            endif
!
            call jecroc(jexnum(chvale, ipf))
            call jeecra(jexnum(chvale, ipf), 'LONMAX', nbabs, ' ')
            call jeecra(jexnum(chvale, ipf), 'LONUTI', nbabs, ' ')
            call jeveuo(jexnum(chvale, ipf), 'E', ispec)
!
            do 150 k = 1, nbpts2
                if (ival(1) .eq. ival(2)) then
                    l1 = ispec + k-1
                    l2 = lssx + nbpts* (ktabl-1) + k - 1
                    zr(l1) = zr(l2)
                else
                    l1 = ispec + (k-1)*2
                    l2 = lssx + nbpts* (ktabl-1) + k - 1
                    zr(l1) = zr(l2)
                    zr(l1+1) = zr(l2+nbpts2)
                endif
150          continue
            ktabl = ktabl + 1
140      continue
130  end do
!
    if (niv .ge. 1) then
        freini = 0.d0
        frefin = dfreq* (nbpts2-1)
        write (ifm,200)
        write (ifm,201) dfreq, freini, frefin
    endif
    if (niv .ge. 2) then
        nomobj = '&&OP0117.FONCTION'
        if (nfcod .ne. mxval) call u2mess('F', 'MODELISA2_89')
        call jeveuo(nomobj, 'L', ltabl)
        call intimp(ifm, zr(lrms), zk24(ltabl), nmatr, nfcod)
    endif
!
!
    call titre()
!
    200 format ('<PAS EN FREQUENCE>  <FREQ. INITIALE>  <FREQ. FINALE>')
    201 format (4x,d11.4,4x,d11.4,4x,d11.4)
!
    call jedema()
end subroutine

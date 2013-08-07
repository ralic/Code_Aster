subroutine op0118()
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
!    GENERATION D'UN VECTEUR DE FONCTIONS ALEATOIRES
!
!     ------------------------------------------------------------------
    implicit   none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/iniran.h"
#include "asterfort/fft.h"
#include "asterfort/gefact.h"
#include "asterfort/genale.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: npfft, dim, dim2, l, nbtir
    integer :: ifm, niv, nbpar, nbval, lvale, ldesc, nbfreq, k
    integer :: nbfc, long, ln, ln2, lonv, lvalf, lvalc, lr, lv
    integer :: lx, ly, ln4, kf, lfo, lprof, if, ifo, kt, it
    integer :: ix, iy, lnr, kk, ifft, kv
    integer :: jump
!
    integer :: lnuor
    parameter   ( nbpar = 2 )
!
    real(kind=8) :: pui2, pui2d, pui3d, freini, frefin, dfreq, tt, dt, r8b, tini
    real(kind=8) :: tfin, duree
    complex(kind=8) :: c16b
    character(len=8) ::  typar(2), nomvec
    character(len=16) :: typvec, nomcmd, nopar(2)
    character(len=19) :: nominf, nomfon
    integer :: iarg
!
    data nopar / 'NUME_ORDRE' , 'FONCTION' /
    data typar / 'I'          , 'K24'      /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
!
    call getres(nomvec, typvec, nomcmd)
!
!
!===============
! 2. LECTURE DES DONNEES LIEES A LA GENERATION
!===============
!
    call getvis(' ', 'NB_TIRAGE', 0, iarg, 1,&
                nbtir, l)
    if (l .eq. 0) nbtir = 0
!
    call getvr8(' ', 'DUREE_TIRAGE', 0, iarg, 1,&
                duree, l)
    if (l .eq. 0) duree = -1.d0
!
    call getvis(' ', 'INIT_ALEA', 0, iarg, 1,&
                jump, l)
    if (l .ne. 0) call iniran(jump)
!
!===============
! 3. LECTURE DES DONNEES LIEES A L'INTERSPECTRE, DISCRETISATION
!    DE L'INTERSPECTRE (=> CHOIX DES PARAMETRES DE LA GENERATION)
!    ET FACTORISATION DE L'INTERSPECTE
!===============
    call gefact(duree, nominf)
!
!
!===============
! 4. RECUPERATION DE L'INTERSPECTRE FACTORISE (NOMINF)
!===============
    call jelira(nominf//'.VALE', 'LONUTI', nbval)
    call jeveuo(nominf//'.VALE', 'L', lvale)
    call jeveuo(nominf//'.DESC', 'L', ldesc)
    call jeveuo(nominf//'.NUOR', 'L', lnuor)
    nbfreq = zi(ldesc)
    dim = zi(ldesc+1)
    nbfc = zi(ldesc+2)
!                        => NBFC = (DIM*(DIM+1))/2
    long = nbfreq + nbfc*2*nbfreq
    if (long .ne. nbval) then
        call u2mess('F', 'ALGORITH9_55')
    endif
!
!===============
! 5. PREPARATION GENERATION
!===============
    pui2 = log(dble(nbfreq))/log(2.d0)
    pui2d = abs( pui2 - aint( pui2 ))
    pui3d = abs( 1.d0 - pui2d )
    if (pui2d .ge. 1.d-06 .and. pui3d .ge. 1.d-06) then
        npfft = 2**int(log(dble(nbfreq))/log(2.d0))
    else
        npfft = nbfreq
    endif
!
    ln = npfft
    ln2 = ln*2
    dim2 = dim*dim
    lonv = ln2*dim
    freini = zr(lvale)
    frefin = zr(lvale+ln-1)
!
    dfreq = (frefin - freini)/(ln-1)
    tt = 1.d0 / dfreq
    dt = tt / ln2
! C'EST BIEN TT/LN2 ET NON TT/(LN2-1) CAR LA GENERATION COMMENCE
! A T=DT ET NON T=0.
    tfin = ln2*nbtir*dt
!
    call wkvect('&&OP0118.TEMP.VALF', 'V V R', lonv, lvalf)
    call wkvect('&&OP0118.TEMP.VALC', 'V V C', ln2, lvalc)
    call wkvect('&&OP0118.TEMP.VALR', 'V V C', dim2, lr)
    call wkvect('&&OP0118.TEMP.VALV', 'V V C', dim, lv)
    call wkvect('&&OP0118.TEMP.VALX', 'V V C', dim, lx)
    call wkvect('&&OP0118.TEMP.VALY', 'V V I', dim, ly)
!
!     --- CREATION DE L'OBJET NOMVEC//'.TABL' ET REMPLISSAGE ---
!
    call tbcrsd(nomvec, 'G')
    call tbajpa(nomvec, nbpar, nopar, typar)
!
!     --- CREATION DES FONCTIONS (VIDE)---
    ln4 = ln2*nbtir+1
!
    do 60 kf = 1, dim
        write (nomfon,'(A8,A3,I4.4)') nomvec, '.FO', kf
!
        call tbajli(nomvec, nbpar, nopar, zi(lnuor-1+kf), r8b,&
                    c16b, nomfon, 0)
!
        call wkvect(nomfon//'.VALE', 'G V R', ln4*2, lfo)
        call wkvect(nomfon//'.PROL', 'G V K24', 6, lprof)
        zi(ly+kf-1) = lfo
        zk24(lprof ) = 'FONCTION'
        zk24(lprof+1) = 'LIN LIN '
        zk24(lprof+2) = 'INST    '
        zk24(lprof+3) = 'TOUTRESU'
        zk24(lprof+4) = 'EC      '
        zk24(lprof+5) = nomfon
60  end do
!
    do 80 if = 1, dim
        ifo = zi(ly+if-1)
        do 30 kt = 1, ln4
            zr(ifo+kt-1) = dt* (kt-1)
30      continue
80  end do
!
!
!
!===============
! 5.  GENERATION DES FONCTIONS
!===============
    do 70 it = 1, nbtir
!
        call genale(zr(lvale), zr(lvalf), zc(lr), zc(lv), zc(lx),&
                    dim, long, lonv, ln)
!
        do 20 kf = 1, dim
            do 40 k = 1, ln
                ix = lvalf + (k-1) + (kf-1)*ln2
                iy = ix + ln
                zc(lvalc+k-1) = dcmplx(zr(ix),zr(iy))
                if (k .ne. 1) then
                    lnr = ln2 - k + 1
                    zc(lvalc+lnr) = dconjg(zc(lvalc+k-1))
                else
                    zc(lvalc+ln) = dcmplx(0.d0,0.d0)
                    zc(lvalc+k-1) = dcmplx(0.d0,0.d0)
                endif
40          continue
            do 998 kk = 1, ln2
                zc(lvalc+kk-1) = zc(lvalc+kk-1)*sqrt(ln2/dt)
998          continue
!
            ifft = -1
            call fft(zc(lvalc), ln2, ifft)
!
            ifo = zi(ly+kf-1) + ln2* (it-1) + ln4 + 1
            if (it .eq. 1) zr(ifo-1)=0.d0
!                        (VALEUR NULLE POUR T=0.)
            do 50 kv = 1, ln2
                zr(ifo+kv-1) = dble(zc(lvalc+kv-1))
50          continue
20      continue
70  end do
!
!===============
! 6. IMPRESSION
!===============
!
    if (niv .ge. 2) then
! LA GENERATION COMMENCE A T=DT MAIS ON METS LE SIGNAL COMMENCE
! A TINI=0. AVEC UNE VALEUR NULLE
        tini = 0.d0
        write (ifm,200)
        write (ifm,210) dt, tini, tfin , npfft
    endif
!
    200 format ('<-PAS DE TEMPS->   <-TEMPS INITIAL->  <-TEMPS FINAL->'//&
     &'  <-NB PT FFT->')
    210 format (1p,2x,d11.4,8x,d11.4,8x,d11.4,8x, i6)
!
    call jedema()
end subroutine

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
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/iniran.h"
#include "asterfort/fft.h"
#include "asterfort/gefact.h"
#include "asterfort/genale.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedetr.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: npfft, dim, dim2, l, nbtir
    integer :: ifm, niv, nbval, lvale, ldesc, nbfreq, k
    integer :: nbfc, long, ln, ln2, lonv, lvalf, lvalc, lr, lv
    integer :: lx, ly, ln4, kf, ifo, kt, it
    integer :: ix, iy, lnr, kk, ifft
    integer :: jump
    integer :: linst, lrefe,ibid, ifonc, inum, ispec
!
    integer :: lnuor,lnuord
!
    real(kind=8) :: pui2, pui2d, pui3d, freini, frefin, dfreq, tt, dt, tini
    real(kind=8) :: tfin, duree
    complex(kind=8) :: c16b
    character(len=8) :: nomvec
    character(len=16) :: typvec, nomcmd
    character(len=19) :: nominf
    character(len=24) :: chinst, chvale
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
    c16b=(0.d0,0.d0)
    call infmaj()
    call infniv(ifm, niv)
!
!   on récupère le nom du concept résultat ici de type interspectre
    call getres(nomvec, typvec, nomcmd)
!===============
! 2. LECTURE DES DONNEES LIEES A LA GENERATION
!===============
!
    call getvis(' ', 'NB_TIRAGE', scal=nbtir, nbret=l)
    if (l .eq. 0) nbtir = 0
!
    call getvr8(' ', 'DUREE_TIRAGE', scal=duree, nbret=l)
    if (l .eq. 0) duree = -1.d0
!
    call getvis(' ', 'INIT_ALEA', scal=jump, nbret=l)
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
! on lit les valeurs de l'interspectre factorisé
    call jelira(nominf//'.VALE', 'LONUTI', nbval)
    call jeveuo(nominf//'.VALE', 'L', lvale)
    call jeveuo(nominf//'.DESC', 'L', ldesc)
    call jeveuo(nominf//'.NUOR', 'L', lnuor)
! celui-ci est discrétisé sur nbfreq
    nbfreq = zi(ldesc)
! 
    dim = zi(ldesc+1)
    nbfc = zi(ldesc+2)
!                        => NBFC = (DIM*(DIM+1))/2
    long = nbfreq + nbfc*2*nbfreq
    if (long .ne. nbval) then
        call utmess('F', 'ALGORITH9_55')
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
    ln4 = ln2*nbtir+1
    call wkvect('&&OP0118.TEMP.VALF', 'V V R', lonv, lvalf)
    call wkvect('&&OP0118.TEMP.VALC', 'V V C', ln2, lvalc)
    call wkvect('&&OP0118.TEMP.VALR', 'V V C', dim2, lr)
    call wkvect('&&OP0118.TEMP.VALV', 'V V C', dim, lv)
    call wkvect('&&OP0118.TEMP.VALX', 'V V C', dim, lx)
    call wkvect('&&OP0118.TEMP.VALY', 'V V R', ln4*dim, ly)


!===============
! 5.  CALCUL DES FFT
!===============
    do it = 1, nbtir
!
        call genale(zr(lvale), zr(lvalf), zc(lr), zc(lv), zc(lx),&
                    dim, long, lonv, ln)
!
        do kf = 1, dim
            do k = 1, ln
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
            end do
            do kk = 1, ln2
                zc(lvalc+kk-1) = zc(lvalc+kk-1)*sqrt(ln2/dt)
            end do
!
            ifft = -1
            call fft(zc(lvalc), ln2, ifft)
! 
            ifo = ly + ln2* (it-1) + 1 + (kf-1)*ln4
            if (it .eq. 1) zr(ifo-1)=0.d0
            do inum = 1, ln2
                zr(ifo-1+inum) = dble(zc(lvalc-1+inum))
            end do
!
        end do
    end do

!===============
! 6.  GENERATION DE L'INTERSPECTRE ET REMPLISSAGE
!===============
    chinst = nomvec(1:8)//'.DISC'
    call jeexin(chinst, ibid)
    if (ibid .eq. 0) then
        call wkvect(chinst, 'G V R', ln4, linst)
        do kt = 1, ln4
                zr(linst-1+kt) = dt* (kt-1)
        end do
    endif
!
    call wkvect(nomvec(1:8)//'.REFE', 'G V K16', 3, lrefe)
    zk16(lrefe) = 'DSP'
    zk16(lrefe+1) = 'TOUT'
    zk16(lrefe+2) = 'INST'
!
    call wkvect(nomvec(1:8)//'.NUME_ORDRE', 'G V I', dim, lnuord)
    do ifonc = 1, dim
        zi(lnuord-1+ifonc) = ifonc
    end do
!
!
    chvale = nomvec(1:8)//'.VALE'
    call jecrec(chvale, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',dim)
    do ifonc = 1, dim
        call jecroc(jexnum(chvale, ifonc))
        call jeecra(jexnum(chvale, ifonc), 'LONMAX', ln4)
        call jeecra(jexnum(chvale, ifonc), 'LONUTI', ln4)
        call jeveuo(jexnum(chvale, ifonc), 'E', ispec)
        do inum = 1, ln4
            zr(ispec-1+inum) = zr(ly-1+inum+(ifonc-1)*ln4)
        end do
    end do
!
    call jedetr('&&OP0118.TEMP.VALF')
    call jedetr('&&OP0118.TEMP.VALR')
    call jedetr('&&OP0118.TEMP.VALC')
    call jedetr('&&OP0118.TEMP.VALV')
    call jedetr('&&OP0118.TEMP.VALX')
    call jedetr('&&OP0118.TEMP.VALY')

!
!===============
! 7. IMPRESSION
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

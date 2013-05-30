subroutine spdfft(nsens, nomfon, nbvin, nomfs, nbvout,&
                  method, sym, base)
    implicit none
    include 'jeveux.h'
    include 'asterfort/fft.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    character*(1) :: base
    integer :: nsens
!     ----------------------------------------------------------------
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
!     REALISATION N.GREFFET
!     CALCUL DE LA FFT OU DE LA FFT-1 (E. BOYERE 09/06/00)
!     ----------------------------------------------------------------
    character(len=16) :: method, sym
    character(len=19) :: nomfs, nomfon
    complex(kind=8) :: dcmplx
    real(kind=8) :: pas, pasfrq
    integer :: nbval, nbva, nbpts, nbpts1, nbpts2, ltra, lres, lres1, ier
    integer :: nin, nbvin, nbvout, lvar, n, lfon, i, ii, valmax
!     ----------------------------------------------------------------
    call jemarq()
!
!     ---  NOMBRE DE POINTS ----
    nbval = nbvin
    call jeveuo(nomfon, 'L', nin)
    lvar = nin
    if (nsens .eq. 1) then
        nbva = nbval/2
    else if (nsens.eq.-1) then
        nbva = nbval/3
    endif
    n = 1
100  continue
    nbpts = 2**n
    if (nbpts .lt. nbva) then
        n = n + 1
        goto 100
    endif
!     Methode de prise en compte du signal :
!     -TRONCATURE : on tronque au 2**N inferieur le plus proche de NBVA
!     -PROL_ZERO : on prolonge le signal avec des zero pour aller
!                   au 2**N le plus proche superieur a NBVA
    if ((method.eq.'TRONCATURE') .and. (nbpts.ne.nbva)) then
        nbpts = 2**(n-1)
        nbpts1 = nbpts
        nbpts2 = 2*nbpts
    else
        nbpts = 2**n
        nbpts1 = nbva
        nbpts2 = nbpts
    endif
!
    lfon = lvar + nbva
!     --- TRANSFORMATION PAR FOURIER
    if (nsens .eq. 1) then
!     --- SENS DIRECT
!     --- RECOPIE DES VARIABLES ---
        if (sym .eq. 'NON') then
            nbpts2 = (nbpts/2)
        endif
        call wkvect('&&SPDFFT.TRAVAIL', 'V V C', nbpts, ltra)
        do 199 i = 1, nbpts1
            zc(ltra+i-1) = dcmplx(zr(lfon+i-1),0.d0)
199      continue
        if (nbpts .gt. nbva) then
            do 1999 i = 1, (nbpts-nbva)
                zc(ltra+nbva+i-1) = dcmplx(0.d0,0.d0)
1999          continue
        endif
!
        call fft(zc(ltra), nbpts, 1)
        pas = zr(lvar+1)-zr(lvar)
!         NOMFS = 'FCT_FFT'
        call jeexin(nomfs, ier)
        if (ier .ne. 0) call jedetr(nomfs)
        call wkvect(nomfs, base//' V C', 2*nbpts2, lres)
        lres1 = lres + nbpts2
        pasfrq = 1.d0/((dble(nbpts))*pas)
!         NOUT = LRES
        nbvout = nbpts2
        do 198 i = 1, nbpts2
            zc(lres+i-1) = dcmplx((i-1)*pasfrq,0.d0)
198      continue
        do 200 i = 1, nbpts2
            zc(lres1+i-1) = zc(ltra+i-1)
200      continue
    else if (nsens.eq.-1) then
!     --- SENS INVERSE
!
!        Pour cas tronque
!         NBPTS=2*NBPTS
        if (sym .eq. 'NON') then
            nbpts2 = (2*nbpts)
        endif
        call wkvect('&&SPDFFT.TRAVAIL', 'V V C', (nbpts2+1), ltra)
        valmax = (nbpts2/2)
        if (nbva .lt. (nbpts2/2)) valmax = nbva
        do 201 i = 1, valmax
            ii = (2*i)-1
            zc(ltra+i-1) = dcmplx(zr(lfon+ii-1),zr(lfon+ii))
            zc(ltra+nbpts2-i+1) = dcmplx(zr(lfon+ii-1),-zr(lfon+ii))
201      continue
        zc(ltra+nbpts+1)=dcmplx(((4.d0*zr(lfon+ii-1)-zr(lfon+ii-3)&
        )/3.d0),0.d0)
        if ((nbpts.gt.nbva) .and. (sym.eq.'NON')) then
            do 2999 i = 1, (nbpts-nbva)
                zc(ltra+nbva+i-1) = dcmplx(0.d0,0.d0)
                zc(ltra+nbpts2-nbva-i+1) = dcmplx(0.d0,0.d0)
2999          continue
        endif
!
        zc(ltra+nbpts+1)=dcmplx(((4.d0*dble(zc(ltra+nbpts)) -dble(zc(&
        ltra+nbpts-1)) )/3.d0),0.d0)
!
        call fft(zc(ltra), nbpts2, -1)
        pas = zr(lvar+1)-zr(lvar)
!         NOMFS = 'FCT_FFT'
        call jeexin(nomfs, ier)
        if (ier .ne. 0) call jedetr(nomfs)
        call wkvect(nomfs, base//' V R', 2*nbpts2, lres)
!         NOUT = LRES
        nbvout = nbpts2
        lres1 = lres + nbpts2
        do 202 i = 1, nbpts2
            zr(lres+i-1) = (1.d0/(dble(nbpts2)*pas))*(i-1)
202      continue
!         PAS2 = (1.D0/ZR(LVAR+NBVA-1))*(DBLE(NBVA)/DBLE(NBPTS2))
        do 203 i = 1, nbpts2
            zr(lres1+i-1) = dble(zc(ltra+i-1))
203      continue
    endif
!
    call jedetr('&&SPDFFT.TRAVAIL')
!      CALL JEDETC('V','&&',1)
!
    call jedema()
end subroutine

subroutine mnlfft(n, x, y, h, nt,ind)
    implicit none
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
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE ALTERNATING FOURIER TRANSFORM
!     -    -   -    -           -       -
! ----------------------------------------------------------------------
!
! EFFECTUE LE PRODUIT DE DEUX SIGNAUX FREQUENTIELS X ET Y PAR LA METHODE
! AFT  : IFFT -> FFT -> IFFT
! LES COEFFICIENTS SONT RANGES AINSI : Z = [Z0 ZC1...ZCH ZS1...ZSH]
! X ET Y PEUVENT CONTENIR N VECTEURS, PAR EX : X = [Z1 Z2 ...ZN]
! ----------------------------------------------------------------------
! IN  N   : I             : NOMBRE DE DDL
! IN  X   : R8(N*(2*H+1)) : LES COEFF DE FOURIER
! OUT Y   : R8(N*NT)      : TRANSFORMEE DE FOURIER INVERSE
! IN  H   : I             : NOMBRE D'HARMONIQUES
! IN  NT  : I             : DISCRETISATION DU TEMPS (POUR LA FFT)
!                           ATTENTION ! NT DOIT ETRE UNE PUISSANCE DE 2.
!                           EN GENERAL ON PREND :
!                           NT = 2**(1 + LOG10(2H+1)/LOG10(2))
! IN  IND : I             : 0 ---> IFFT
!                           1 ---> FFT
!
! ----------------------------------------------------------------------
!
! --- DECLARATION PARAMETRES D'APPELS
!
#include "jeveux.h"
#include "blas/zdscal.h"
#include "asterfort/fft.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
    integer :: n, h, nt, ind
    real(kind=8) :: x(n*(2*h+1)), y(n*nt)
!
    integer :: k, iadd, j
    integer :: ixf, iyf, ixft
!
    call jemarq()
!
! --- INITIALISATION DES VECTEURS DE TRAVAIL ET DE L'IMAGINAIRE PUR I
!
    call wkvect('&&mnlfft.xf', 'V V C', n*nt, ixf)
    call wkvect('&&mnlfft.yf', 'V V C', nt, iyf)
    call wkvect('&&mnlfft.xft', 'V V C', nt, ixft)
    if (ind .eq. 0) then
! ---   REECRITURE COMPATIBLE AVEC LA DFT
! ---   APPLICATION DE L'IFFT
        do 10 k = 1, n
            call zdscal(nt, 0.d0, zc(ixft), 1)
!
            zc(ixft)=nt*x(k)
            do 30 j = 1, h
                zc(ixft-1+j+1)=dcmplx((nt/2.d0)*x(j*n+k),(nt/2.d0)*x(n*(h+j)+k))
                zc(ixft-1+nt-j+1)=dcmplx((nt/2.d0)*x(j*n+k),-(nt/2.d0)*x(n*(h+j)+k))
30          continue
!
            call fft(zc(ixft), nt, -1)
            do 11 j = 1, nt
                iadd = (j-1)*n+k
                y(iadd) = dble(zc(ixft-1+j))
11          continue
10      continue
!
    else if (ind.eq.1) then
! ---   APPLICATION DE LA FFT
        do 50 k = 1, n
            do 51 j = 1, nt
                zc(iyf-1+j)=dcmplx(y((j-1)*n+k),0.d0)
51          continue
!
            call fft(zc(iyf), nt, 1)
!
            x(k)=dble(zc(iyf))/nt
            do 52 j = 1, h
                x(j*n+k)=2.d0*dble(zc(iyf-1+j+1))/nt
                x((h+j)*n+k)=2.d0*aimag(zc(iyf-1+j+1))/nt
52          continue
50      continue
    endif
!
    call jedetr('&&mnlfft.xf')
    call jedetr('&&mnlfft.yf')
    call jedetr('&&mnlfft.xft')
!
    call jedema()
end subroutine

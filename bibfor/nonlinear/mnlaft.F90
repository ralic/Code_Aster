subroutine mnlaft(x, y, h, nt, pq)
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
!
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE ALTERNATING FOURIER TRANSFORM
!     -    -   -    -           -       -
! ----------------------------------------------------------------------
!
! EFFECTUE LE PRODUIT DE DEUX SIGNAUX FREQUENTIELS X ET Y PAR LA METHODE
! AFT  : IFFT -> FFT -> IFFT
! LES COEFFICIENTS SONT RANGES AINSI : Z = [Z0 ZC1...ZCH ZS1...ZSH]
! ----------------------------------------------------------------------
! IN  X  : R8(N*(2*H+1)) : VECTEUR A MULTIPLIER
! IN  Y  : R8(N*(2*H+1)) : VECTEUR A MULTIPLIER
! IN  H  : I             : NOMBRE D'HARMONIQUES
! IN  NT : I             : DISCRETISATION DU TEMPS (POUR LA FFT)
!                          ATTENTION ! NT DOIT ETRE UNE PUISSANCE DE 2.
!                          EN GENERAL ON PREND :
!                          NT = 2**(1 + LOG10(4H+1)/LOG10(2))
! OUT PQ : R8(N*(2*H+1)) : VECTEUR RESULTAT DE LA MULTIPLICATION
! ----------------------------------------------------------------------
!
! --- DECLARATION PARAMETRES D'APPELS
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/fft.h"
#include "asterfort/wkvect.h"
    integer :: h, nt
    real(kind=8) :: x(2*h+1), y(2*h+1), pq(2*h+1)
!
    integer :: ixf, iyf, ixyf
    integer :: k
!
    call jemarq()
!
    call wkvect('&&mnlaft.xf', 'V V C', nt, ixf)
    call wkvect('&&mnlaft.yf', 'V V C', nt, iyf)
    call wkvect('&&mnlaft.xft', 'V V C', nt, ixyf)
!
    zc(ixf)=dcmplx(nt*x(1),0.d0)
    zc(iyf)=dcmplx(nt*y(1),0.d0)
    do 20 k = 1, h
        zc(ixf-1+k+1)=dcmplx(nt/2.d0*x(k+1), nt/2.d0*x(h+k+1))
        zc(ixf-1+nt-k+1)=dcmplx(nt/2.d0*x(k+1),-nt/2.d0*x(h+k+1))
!
        zc(iyf-1+k+1)=dcmplx(nt/2.d0*y(k+1),nt/2.d0*y(h+k+1))
        zc(iyf-1+nt-k+1)=dcmplx(nt/2.d0*y(k+1),-nt/2.d0*y(h+k+1))
20  continue
!
    call fft(zc(ixf), nt, -1)
    call fft(zc(iyf), nt, -1)
!
    do 30 k = 1, nt
        zc(ixyf-1+k)=dcmplx(dble(zc(ixf-1+k))*dble(zc(iyf-1+k)),0.d0)
30  continue
!
    call fft(zc(ixyf), nt, 1)
!
    pq(1)=dble(zc(ixyf))/nt
    do 40 k = 1, h
        pq(k+1)=2.d0*dble(zc(ixyf-1+k+1))/nt
        pq(h+k+1)=2.d0*aimag(zc(ixyf-1+k+1))/nt
40  continue
!
    call jedetr('&&mnlaft.xf')
    call jedetr('&&mnlaft.yf')
    call jedetr('&&mnlaft.xft')
!
    call jedema()
end subroutine

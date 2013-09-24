subroutine mnlaft(x, y, h, nt, pq)
! aslint: disable=W1306

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
#include "asterfort/fft.h"
    integer :: h, nt
    real(kind=8) :: x(2*h+1), y(2*h+1), pq(2*h+1)
    integer :: k
    complex(kind=8) :: xf(nt), yf(nt), i, xyf(nt)
!
!
    i=(0.d0,1.d0)
    do 10 k = 1, nt
        xf(k)=(0.d0,0.d0)
        yf(k)=(0.d0,0.d0)
10  continue
!
    xf(1)=nt*x(1)
    yf(1)=nt*y(1)
    do 20 k = 1, h
        xf(k+1)=nt*(x(k+1)+i*x(h+k+1))/2.d0
        xf(nt-k+1)=nt*(x(k+1)-i*x(h+k+1))/2.d0
!
        yf(k+1)=nt*(y(k+1)+i*y(h+k+1))/2.d0
        yf(nt-k+1)=nt*(y(k+1)-i*y(h+k+1))/2.d0
20  continue
!
    call fft(xf, nt, -1)
    call fft(yf, nt, -1)
!
    do 30 k = 1, nt
        xyf(k)=(dble(xf(k))*dble(yf(k)))+i*0.d0
30  continue
!
    call fft(xyf, nt, 1)
!
    pq(1)=dble(xyf(1))/nt
    do 40 k = 1, h
        pq(k+1)=2.d0*dble(xyf(k+1))/nt
        pq(h+k+1)=2.d0*aimag(xyf(k+1))/nt
40  continue
!
end subroutine

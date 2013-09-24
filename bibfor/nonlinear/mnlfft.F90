subroutine mnlfft(n, x, y, h, nt,&
                  ind)
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
#include "asterfort/fft.h"
#include "asterc/iisnan.h"
    integer :: n, h, nt, ind, k, iadd, j
    real(kind=8) :: x(n*(2*h+1)), y(n*nt)
    complex(kind=8) :: xft(nt), xf(n*nt), yf(nt), i
!
! --- INITIALISATION DES VECTEURS DE TRAVAIL ET DE L'IMAGINAIRE PUR I
!
    xf = 0.d0
    yf = 0.d0
    i = (0.d0,1.d0)
    if (ind .eq. 0) then
! ---   REECRITURE COMPATIBLE AVEC LA DFT
! ---   APPLICATION DE L'IFFT
        do 10 k = 1, n
            do 40 j = 1, nt
                xft(j)=(0.d0,0.d0)
40          continue
!
            xft(1)=nt*x(k)
            do 30 j = 1, h
                xft(j+1)=(nt/2.d0)*(x(j*n+k)+i*x(n*(h+j)+k))
                xft(nt-j+1)=(nt/2.d0)*(x(j*n+k)-i*x(n*(h+j)+k))
30          continue
!
            call fft(xft, nt, -1)
            do 11 j = 1, nt
                iadd = (j-1)*n+k
                y(iadd) = dble(xft(j))
11          continue
10      continue
!
    else if (ind.eq.1) then
! ---   APPLICATION DE LA FFT
        do 50 k = 1, n
            do 51 j = 1, nt
                yf(j)=y((j-1)*n+k)+i*0.d0
51          continue
!
            call fft(yf, nt, 1)
!
            x(k)=dble(yf(1))/nt
            do 52 j = 1, h
                x(j*n+k)=2.d0*dble(yf(j+1))/nt
                x((h+j)*n+k)=2.d0*aimag(yf(j+1))/nt
52          continue
50      continue
    endif
!
end subroutine

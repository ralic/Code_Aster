subroutine dsqdis(xyzl, caraq4, df, dci, an)
    implicit  none
#include "jeveux.h"
#include "asterfort/dsxhft.h"
#include "asterfort/elref5.h"
#include "asterfort/jquad4.h"
#include "asterfort/mgauss.h"
    real(kind=8) :: xyzl(3, *), df(3, 3), dci(2, 2), an(4, 12), caraq4(*)
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
!     -------------------------------------------------------
!     MATRICE AN(4,12) DU CISAILLEMENT POUR LE DSQ
!     -------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: nc, k, ic, int, j, i, iret
    real(kind=8) :: qsi, eta, peta, meta, pqsi, mqsi, det, jacob(5)
    real(kind=8) :: l(4), c(4), s(4), x(4), y(4)
    real(kind=8) :: hft2(2, 6), tb(6, 12), ta(6, 4), dt(2, 6)
    real(kind=8) :: dib(2, 12), dia(2, 4), aw(4, 12), aa(4, 4), aai(4, 4)
!     ------------------------------------------------------------------
!
    call elref5('SE2', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
    nc = 4
!
    c(1) = caraq4(13)
    c(2) = caraq4(14)
    c(3) = caraq4(15)
    c(4) = caraq4(16)
    s(1) = caraq4(17)
    s(2) = caraq4(18)
    s(3) = caraq4(19)
    s(4) = caraq4(20)
!
    l(1) = caraq4( 9)
    l(2) = caraq4(10)
    l(3) = caraq4(11)
    l(4) = caraq4(12)
!
    x(1) = caraq4(1)
    x(2) = caraq4(2)
    x(3) = caraq4(3)
    x(4) = caraq4(4)
    y(1) = caraq4(5)
    y(2) = caraq4(6)
    y(3) = caraq4(7)
    y(4) = caraq4(8)
!
    do 100 k = 1, 72
        tb(k,1) = 0.d0
100  end do
    tb(3,2) = 0.25d0
    tb(3,5) = - 0.25d0
    tb(3,8) = 0.25d0
    tb(3,11) = - 0.25d0
    tb(6,3) = 0.25d0
    tb(6,6) = - 0.25d0
    tb(6,9) = 0.25d0
    tb(6,12) = - 0.25d0
!
    do 300 ic = 1, nc
!
        do 105 k = 1, 24
            dib(k,1) = 0.d0
105      continue
        do 110 k = 1, 8
            dia(k,1) = 0.d0
110      continue
!
        do 250 int = 1, 2
!
            if (ic .eq. 1) then
                qsi = -zr(icoopg-1+ndim*(int-1)+1)
                eta = -zr(ipoids-1+int)
            else if (ic .eq. 2) then
                qsi = zr(ipoids-1+int)
                eta = -zr(icoopg-1+ndim*(int-1)+1)
            else if (ic .eq. 3) then
                qsi = zr(icoopg-1+ndim*(int-1)+1)
                eta = zr(ipoids-1+int)
            else if (ic .eq. 4) then
                qsi = -zr(ipoids-1+int)
                eta = zr(icoopg-1+ndim*(int-1)+1)
            endif
!
            call jquad4(xyzl, qsi, eta, jacob)
!
            peta = 1.d0 + eta
            meta = 1.d0 - eta
            pqsi = 1.d0 + qsi
            mqsi = 1.d0 - qsi
!
            do 120 k = 1, 24
                ta(k,1) = 0.d0
120          continue
            ta(1,1) = - meta * c(1)
            ta(1,3) = - peta * c(3)
            ta(2,2) = - pqsi * c(2)
            ta(2,4) = - mqsi * c(4)
            ta(3,1) = qsi * c(1)
            ta(3,2) = - eta * c(2)
            ta(3,3) = - qsi * c(3)
            ta(3,4) = eta * c(4)
            ta(4,1) = - meta * s(1)
            ta(4,3) = - peta * s(3)
            ta(5,2) = - pqsi * s(2)
            ta(5,4) = - mqsi * s(4)
            ta(6,1) = qsi * s(1)
            ta(6,2) = - eta * s(2)
            ta(6,3) = - qsi * s(3)
            ta(6,4) = eta * s(4)
!
            call dsxhft(df, jacob(2), hft2)
!
!           -------- PRODUIT DCI.HFT2 ----------------------------------
            do 130 k = 1, 12
                dt(k,1) = 0.d0
130          continue
            do 140 j = 1, 6
                dt(1,j) = dt(1,j)+dci(1,1)*hft2(1,j)+dci(1,2)*hft2(2, j)
                dt(2,j) = dt(2,j)+dci(2,1)*hft2(1,j)+dci(2,2)*hft2(2, j)
140          continue
!           -------- PRODUIT DT.TB -------------------------------------
            do 160 j = 1, 12
                do 160 k = 1, 6
                    dib(1,j) = dib(1,j) + dt(1,k) * tb(k,j)
                    dib(2,j) = dib(2,j) + dt(2,k) * tb(k,j)
160              continue
!           -------- PRODUIT DT.TA -------------------------------------
            do 180 j = 1, 4
                do 180 k = 1, 6
                    dia(1,j) = dia(1,j) + dt(1,k) * ta(k,j)
                    dia(2,j) = dia(2,j) + dt(2,k) * ta(k,j)
180              continue
250      continue
        do 260 j = 1, 12
            aw(ic,j) = (c(ic)*dib(1,j) + s(ic)*dib(2,j)) * l(ic)/2.d0
260      continue
        do 270 j = 1, 4
            aa(ic,j) = - (c(ic)*dia(1,j) + s(ic)*dia(2,j)) * l(ic)/ 2.d0
270      continue
        aa(ic,ic) = aa(ic,ic) + 2.d0 / 3.d0 * l(ic)
300  end do
!     -------------- INVERSION DE AA -----------------------------------
    do 310 k = 1, 16
        aai(k,1) = 0.d0
310  end do
    do 320 i = 1, 4
        aai(i,i) = 1.d0
320  end do
    call mgauss('NFVP', aa, aai, 4, 4,&
                4, det, iret)
!
    aw(1,1) = aw(1,1) + 1.d0
    aw(1,2) = aw(1,2) - x(1)/2.d0
    aw(1,3) = aw(1,3) - y(1)/2.d0
    aw(1,4) = aw(1,4) - 1.d0
    aw(1,5) = aw(1,5) - x(1)/2.d0
    aw(1,6) = aw(1,6) - y(1)/2.d0
    aw(2,4) = aw(2,4) + 1.d0
    aw(2,5) = aw(2,5) - x(2)/2.d0
    aw(2,6) = aw(2,6) - y(2)/2.d0
    aw(2,7) = aw(2,7) - 1.d0
    aw(2,8) = aw(2,8) - x(2)/2.d0
    aw(2,9) = aw(2,9) - y(2)/2.d0
    aw(3,7) = aw(3,7) + 1.d0
    aw(3,8) = aw(3,8) - x(3)/2.d0
    aw(3,9) = aw(3,9) - y(3)/2.d0
    aw(3,10) = aw(3,10) - 1.d0
    aw(3,11) = aw(3,11) - x(3)/2.d0
    aw(3,12) = aw(3,12) - y(3)/2.d0
    aw(4,1) = aw(4,1) - 1.d0
    aw(4,2) = aw(4,2) - x(4)/2.d0
    aw(4,3) = aw(4,3) - y(4)/2.d0
    aw(4,10) = aw(4,10) + 1.d0
    aw(4,11) = aw(4,11) - x(4)/2.d0
    aw(4,12) = aw(4,12) - y(4)/2.d0
!
!     -------------- AN = AAI.AW ---------------------------------------
    do 410 k = 1, 48
        an(k,1) = 0.d0
410  end do
    do 420 i = 1, 4
        do 420 k = 1, 4
            do 420 j = 1, 12
                an(i,j) = an(i,j) + aai(i,k) * aw(k,j)
420          continue
!
end subroutine

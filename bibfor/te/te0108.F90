subroutine te0108(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/cq3d2d.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_TEXT_R'
!                          CAS COQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    real(kind=8) :: pc(3), fpl, fmo, coor2d(18), zero, coefmo, coefpl
    real(kind=8) :: dfdx(9), dfdy(9), poids, cour, cosa, sina
    real(kind=8) :: r, matnp(9), long, lamb, text, textmo, textpl, rp1, rp2, rp3
    integer :: i, k, nno, kp, npg1, gi, pi, ivectt, icoef
    integer :: itemps, itex, nnos, ndim, jgano
    integer :: ipoids, ivf, idfde, igeom
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
    zero = 0.d0
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTTR', 'E', ivectt)
    call jevech('PCOEFHR', 'L', icoef)
    call jevech('PT_EXTR', 'L', itex)
    call jevech('PTEMPSR', 'L', itemps)
!
    if (nomte .ne. 'THCOSE3 ' .and. nomte .ne. 'THCOSE2 ') then
        coefmo = zr(icoef)
        coefpl = zr(icoef+1)
        textmo = zr(itex+1)
        textpl = zr(itex+2)
!
        fmo = coefmo*textmo
        fpl = coefpl*textpl
        pc(1) = zero
        pc(2) = fmo
        pc(3) = fpl
    endif
!
    if (nomte .ne. 'THCPSE3 ' .and. nomte .ne. 'THCASE3 ' .and. nomte .ne. 'THCOSE3 ' .and.&
        nomte .ne. 'THCOSE2 ') then
!
        call cq3d2d(nno, zr(igeom), 1.d0, zero, coor2d)
!
        do 30 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        poids, dfdx, dfdy)
            do 20 gi = 1, nno
                do 10 pi = 1, 3
                    i = 3* (gi-1) + pi - 1 + ivectt
                    zr(i) = zr(i) + poids*zr(ivf+k+gi-1)*pc(pi)
10              continue
20          continue
30      continue
!
        else if (nomte.eq.'THCPSE3' .or. nomte.eq.'THCASE3')&
    then
!
        do 80 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, poids, cosa, sina)
            do 50 pi = 1, 3
!
                if (nomte .eq. 'THCASE3') then
                    r = zero
                    do 40 i = 1, nno
                        r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
40                  continue
                    poids = poids*r
                endif
!
50          continue
            do 70 gi = 1, nno
                do 60 pi = 1, 3
                    i = 3* (gi-1) + pi - 1 + ivectt
                    zr(i) = zr(i) + poids*zr(ivf+k+gi-1)*pc(pi)
60              continue
70          continue
80      continue
!
        else if (nomte.eq.'THCOSE3' .or. nomte.eq.'THCOSE2')&
    then
!
        long = (&
               zr(igeom+3)-zr(igeom))**2 + (zr(igeom+3+1)-zr(igeom+1) )**2 + (zr(igeom+3+2)-zr(ig&
               &eom+2)&
               )**2
        long = sqrt(long)/2.d0
!
!      IMPORTANT: LAMB = CONV * EPAISSEUR
!
        lamb = zr(icoef)/2.d0
        text = zr(itex)
!
        rp1 = 1.33333333333333d0
        rp2 = 0.33333333333333d0
        rp3 = 0.33333333333333d0
!
        do 100 kp = 1, npg1
!
            k = (kp-1)*nno
!
            poids = zr(ipoids-1+kp)
!
            matnp(1) = rp1*poids*zr(ivf-1+k+1)
            matnp(2) = rp2*poids*zr(ivf-1+k+1)
            matnp(3) = rp3*poids*zr(ivf-1+k+1)
!
            matnp(4) = rp1*poids*zr(ivf-1+k+2)
            matnp(5) = rp2*poids*zr(ivf-1+k+2)
            matnp(6) = rp3*poids*zr(ivf-1+k+2)
!
            if (nomte.eq.'THCOSE3') then
                matnp(7) = rp1*poids*zr(ivf-1+k+3)
                matnp(8) = rp2*poids*zr(ivf-1+k+3)
                matnp(9) = rp3*poids*zr(ivf-1+k+3)
            endif
!
            do 90 i = 1, 3*nno
                zr(ivectt-1+i) = zr(ivectt-1+i) + long*matnp(i)*lamb* text
90          continue
100      continue
!
    endif
!
end subroutine

subroutine te0502(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/rcfodi.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTIONS : 'RIGI_THER_CONV_T'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: npgmax
    parameter         ( npgmax=10 )
    character(len=24) :: decent
    real(kind=8) :: dfdx(9), dfdy(9), poids, r
    real(kind=8) :: dni(2, 9, npgmax), uloc(2, 9), ul(2, npgmax)
    real(kind=8) :: jacob(npgmax), umi(2), aire, rr
    real(kind=8) :: xr, xrr, xaux, rbid
    real(kind=8) :: s, um, xma, xm, coef, cmin, alfa, aksi, cc
    integer :: kp, i, j, k, ij, itemps, imattt
    integer :: itempi, ifon(3), ivite, igeom, imate
    integer :: iad, nbvf, jvalf, idim, jdim
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
! DEB ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVITESR', 'L', ivite)
    call jevech('PMATERC', 'L', imate)
    call jevech('PNEUK24', 'L', iad)
    decent = zk24(iad-1+1)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PMATTTR', 'E', imattt)
!
    call ntfcma(zi(imate), ifon)
    nbvf = zi(ifon(1))
    jvalf = zi(ifon(1) + 2)
    xr = 0.d0
    do 22 i = 1, nbvf
        xaux = zr(jvalf + i - 1)
        call rcfodi(ifon(1), xaux, rbid, xrr)
        if (xrr .gt. xr) then
            xr = xrr
        endif
22  end do
    rr = 0.6d0/xr
!
    k = 0
    do 10 i = 1, nno
        do 20 idim = 1, 2
            k = k+1
            uloc(idim,i) = zr(ivite+k-1)
20      continue
10  end do
!
    aire = 0.d0
    umi(1) = 0.d0
    umi(2) = 0.d0
!
    do 30 kp = 1, npg
        ul(1,kp) = 0.d0
        ul(2,kp) = 0.d0
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
!
        if (lteatt(' ','AXIS','OUI')) then
            r = 0.d0
            do 40 i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
40          continue
            poids = poids*r
        endif
!
        do 50 i = 1, nno
            ul(1,kp) = ul(1,kp) + uloc(1,i)*zr(ivf+k+i-1)
            ul(2,kp) = ul(2,kp) + uloc(2,i)*zr(ivf+k+i-1)
50      continue
!
        aire = aire + poids
        do 60 i = 1, nno
            dni(1,i,kp) = dfdx(i)
            dni(2,i,kp) = dfdy(i)
60      continue
!
        jacob(kp) = poids
        umi(1) = umi(1)+ul(1,kp)*poids
        umi(2) = umi(2)+ul(2,kp)*poids
30  end do
!
    umi(1) = umi(1)/aire
    umi(2) = umi(2)/aire
!
    ij = imattt - 1
!
    do 70 i = 1, nno
        do 80 j = 1, nno
            s = 0.d0
            do 90 kp = 1, npg
                k = (kp-1)*nno
                s = s +zr(ivf+k+i-1)*dni(1,j,kp)*ul(1,kp)*jacob(kp)* rr +zr(ivf+k+i-1)*dni(2,j,kp&
                    &)*ul(2,kp)*jacob(kp)*rr
90          continue
            ij = ij+1
            zr(ij) = zr(ij)+s
80      end do
70  end do
!
    if (decent .eq. 'OUI') then
!
!- DECENTREMENT HUGUES-BROOKS SU2
!
        um = umi(1)*umi(1) + umi(2)*umi(2)
        um = sqrt(um)
        if (um .lt. 1.d-10) goto 9999
        umi(1) = umi(1)/um
        umi(2) = umi(2)/um
!
        xma = sqrt(aire)
!
        do 100 i = 2, nno
            xm = 0.d0
            xm =xm+(zr(igeom)-zr(igeom+2*i-2))*(zr(igeom)-zr(igeom+2*&
            i-2)) +(zr(igeom+1)-zr(igeom+2*i-1))*(zr(igeom+1)-zr(&
            igeom+2*i-1))
            xm = sqrt(xm)
            if (xm .gt. xma) xma = xm
100      end do
!
        ij = imattt - 1
!
        do 110 i = 1, nno
            do 110 j = 1, nno
                s=0.d0
                do 120 kp = 1, npg
                    do 120 idim = 1, 2
                        do 120 jdim = 1, 2
!
                            coef = rr
                            cmin = 1.d0
                            alfa = um*xma/cmin*coef
                            aksi = alfa/3.d0
                            if (alfa .gt. 3.d0) aksi = 1.d0
                            cc = aksi*um*xma
!
                            s = s+(&
                                dni(idim,i,kp)*dni(jdim,j,kp)*ul( idim,kp)*ul(jdim,kp) *jacob(kp)&
                                &/(um*um)&
                                )* coef*cc
!
120                      continue
                ij = ij+1
                zr(ij) = zr(ij) + s
110          continue
    endif
!
9999  continue
end subroutine

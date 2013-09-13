subroutine te0170(option, nomte)
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
!.......................................................................
!
!     BUT: CALCUL DES MATRICES DE RIGIDITE  ELEMENTAIRES EN MECANIQUE
!          ELEMENTS DE FLUIDE ISOPARAMETRIQUES 3D
!
!          OPTION : 'RIGI_MECA '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
!-----------------------------------------------------------------------
    integer :: icompo, ideplm, ideplp, ivectu, k, l, n1
    integer :: n2, nbres, ndim, nn, nno2, nnos, nt2
!
    real(kind=8) :: r8bid
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=8) :: nomres(nbres), fami, poum
    integer :: icodre(nbres)
    character(len=16) :: nomte, option
    real(kind=8) :: valres(nbres), a(2, 2, 27, 27)
    real(kind=8) :: b(54, 54), ul(54), c(1485)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, rho, celer
    integer :: ipoids, ivf, idfde, igeom, imate, kpg, spt
    integer :: jgano, nno, kp, npg2, ik, ijkl, i, j, imatuu, jcret
!
!
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA' .or. option .eq.&
        'RIGI_MECA_TANG') then
        call jevech('PCOMPOR', 'L', icompo)
        if (zk16(icompo+3) .eq. 'COMP_ELAS') then
            call utmess('F', 'ELEMENTS2_90')
        endif
    endif
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
!
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', r8bid,&
                2, nomres, valres, icodre, 1)
    rho = valres(1)
    celer = valres(2)
!
    do 50 k = 1, 2
        do 40 l = 1, 2
            do 30 i = 1, nno
                do 20 j = 1, i
                    a(k,l,i,j) = 0.d0
20              continue
30          continue
40      continue
50  end do
!
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 80 kp = 1, npg2
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      TERME EN (P**2)/ (RHO*(CEL**2))  C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 70 i = 1, nno
            do 60 j = 1, i
                a(1,1,i,j) = a(1,1,i,j) + poids*zr(ivf+l+i-1)*zr(ivf+ l+j-1)/ rho/celer/celer
60          continue
!
70      continue
!
80  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 120 k = 1, 2
        do 110 l = 1, 2
            do 100 i = 1, nno
                ik = ((2*i+k-3)* (2*i+k-2))/2
                do 90 j = 1, i
                    ijkl = ik + 2* (j-1) + l
                    c(ijkl) = a(k,l,i,j)
90              continue
100          continue
110      continue
120  end do
    nno2 = nno*2
    nt2 = nno* (nno2+1)
!
    if (option(1:9) .ne. 'FULL_MECA' .and. option(1:9) .ne. 'RIGI_MECA') goto 150
    if (option .eq. 'RIGI_MECA_HYST') then
        call jevech('PMATUUC', 'E', imatuu)
        do 130 i = 1, nt2
            zc(imatuu+i-1) = dcmplx(c(i),0.d0)
130      continue
    else
        call jevech('PMATUUR', 'E', imatuu)
        do 140 i = 1, nt2
            zr(imatuu+i-1) = c(i)
140      continue
    endif
150  continue
    if (option(1:9) .ne. 'FULL_MECA' .and. option .ne. 'RAPH_MECA' .and. option .ne.&
        'FORC_NODA') goto 210
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    do 160 i = 1, nno2
        zr(ivectu+i-1) = 0.d0
        ul(i) = zr(ideplm+i-1) + zr(ideplp+i-1)
160  end do
!
    nn = 0
    do 180 n1 = 1, nno2
        do 170 n2 = 1, n1
            nn = nn + 1
            b(n1,n2) = c(nn)
            b(n2,n1) = c(nn)
170      continue
180  end do
    do 200 n1 = 1, nno2
        do 190 n2 = 1, nno2
            zr(ivectu+n1-1) = zr(ivectu+n1-1) + b(n1,n2)*ul(n2)
190      continue
200  end do
!
210  continue
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine

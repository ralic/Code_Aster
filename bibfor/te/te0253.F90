subroutine te0253(option, nomte)
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
!          ELEMENTS DE FLUIDE ISOPARAMETRIQUES 2D
!
!          OPTION : 'RIGI_MECA '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
!
!-----------------------------------------------------------------------
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/u2mess.h"
    integer :: icompo, ideplm, ideplp, k, l, n1, n2
    integer :: nbres, nn, nno2, nt2
    real(kind=8) :: r, r8b
!-----------------------------------------------------------------------
    parameter         ( nbres=2 )
    character(len=8) :: nomres(nbres), fami, poum
    integer :: icodre(nbres), kpg, spt
    character(len=16) :: nomte, option
    real(kind=8) :: valres(nbres), a(2, 2, 9, 9)
    real(kind=8) :: b(18, 18), ul(18), c(171)
    real(kind=8) :: dfdx(9), dfdy(9), poids, rho, celer
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: nno, kp, npg, ik, ijkl, i, j, imatuu
    integer :: ivectu, jcret, ndim, jgano, nnos
!
!
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA' .or. option .eq.&
        'RIGI_MECA_TANG') then
        call jevech('PCOMPOR', 'L', icompo)
        if (zk16(icompo+3) .eq. 'COMP_ELAS') then
            call u2mess('F', 'ELEMENTS2_90')
        endif
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', r8b,&
                2, nomres, valres, icodre, 1)
    rho = valres(1)
    celer = valres(2)
!
!     INITIALISATION DE LA MATRICE A
    do 112 k = 1, 2
        do 112 l = 1, 2
            do 112 i = 1, nno
                do 112 j = 1, i
                    a(k,l,i,j) = 0.d0
112              continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 kp = 1, npg
!
        k = (kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      TERME EN (P**2)/ (RHO*(CEL**2))  C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        if (lteatt(' ','AXIS','OUI')) then
            r = 0.d0
            do 102 i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102          continue
            poids = poids*r
        endif
!
        do 106 i = 1, nno
            do 107 j = 1, i
                a(1,1,i,j) = a(1,1,i,j) + poids * zr(ivf+k+i-1) * zr( ivf+k+j-1) / rho / celer/ce&
                             &ler
!
107          continue
!
106      continue
!
101  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 111 k = 1, 2
        do 111 l = 1, 2
            do 111 i = 1, nno
                ik = ((2*i+k-3) * (2*i+k-2)) / 2
                do 111 j = 1, i
                    ijkl = ik + 2 * (j-1) + l
                    c(ijkl) = a(k,l,i,j)
111              continue
!
    nno2 = nno*2
    nt2 = nno*(nno2+1)
!
    if (option(1:9) .ne. 'FULL_MECA' .and. option(1:9) .ne. 'RIGI_MECA') goto 9998
    if (option .eq. 'RIGI_MECA_HYST') then
        call jevech('PMATUUC', 'E', imatuu)
        do 115 i = 1, nt2
            zc(imatuu+i-1)=dcmplx(c(i),0.d0)
115      continue
    else
        call jevech('PMATUUR', 'E', imatuu)
        do 114 i = 1, nt2
            zr(imatuu+i-1)=c(i)
114      continue
    endif
9998  continue
!
    if (option .ne. 'FULL_MECA' .and. option .ne. 'RAPH_MECA' .and. option .ne. 'FORC_NODA') &
    goto 9999
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    do 113 i = 1, nno2
        zr(ivectu+i-1) = 0.d0
        ul(i)=zr(ideplm+i-1)+zr(ideplp+i-1)
113  end do
!
    nn = 0
    do 120 n1 = 1, nno2
        do 121 n2 = 1, n1
            nn = nn + 1
            b(n1,n2) = c(nn)
            b(n2,n1) = c(nn)
121      continue
120  end do
!
    do 130 n1 = 1, nno2
        do 130 n2 = 1, nno2
            zr(ivectu+n1-1) = zr(ivectu+n1-1)+b(n1,n2)*ul(n2)
130      continue
!
9999  continue
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine

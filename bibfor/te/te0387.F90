subroutine te0387(option, nomte)
! ......................................................................
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'MTAN_THER_PARO_F'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: icode, igeom2, ihechp, jgano, nbres, ndim, nnos
!
!-----------------------------------------------------------------------
    parameter (nbres=3)
    character(len=8) :: nompar(nbres), lirefe(2)
    real(kind=8) :: valpar(nbres), poids, poids1, poids2, r, r1, r2
    real(kind=8) :: z, z1, z2, hechp, nx, ny, theta, mat(6)
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: itemps, imatt, k, i, j, l, li, lj, nbelr
    logical :: laxi
!
!
    call elref2(nomte, 2, lirefe, nbelr)
    ASSERT(nbelr.eq.2)
    call elref4(lirefe(2), 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PHECHPF', 'L', ihechp)
    call jevech('PMATTTR', 'E', imatt)
    theta = zr(itemps+2)
    if (nomte(5:8) .eq. 'SE22') then
        igeom2 = igeom + 4
    else if (nomte(5:8).eq.'SE33') then
        igeom2 = igeom + 6
    endif
!
    do 40 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids1)
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom2), nx, ny, poids2)
        r = 0.d0
        r1 = 0.d0
        r2 = 0.d0
        z = 0.d0
        z1 = 0.d0
        z2 = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r1 = r1 + zr(igeom+2*i-2)*zr(ivf+l-1)
            r2 = r2 + zr(igeom2+2*i-2)*zr(ivf+l-1)
            z1 = z1 + zr(igeom+2*i-1)*zr(ivf+l-1)
            z2 = z2 + zr(igeom2+2*i-1)*zr(ivf+l-1)
10      continue
        poids = (poids1+poids2)/2.0d0
        r = (r1+r2)/2.d0
        z = (z1+z2)/2.d0
        if (laxi) poids = poids*r
        valpar(1) = r
        nompar(1) = 'X'
        valpar(2) = z
        nompar(2) = 'Y'
        valpar(3) = zr(itemps)
        nompar(3) = 'INST'
        call fointe('A', zk8(ihechp), 3, nompar, valpar,&
                    hechp, icode)
        ASSERT(icode.eq.0)
        k = 0
        do 30 i = 1, nno
            li = ivf + (kp-1)*nno + i - 1
            do 20 j = 1, i
                lj = ivf + (kp-1)*nno + j - 1
                k = k + 1
                mat(k) = poids*theta*zr(li)*zr(lj)*hechp
20          continue
30      continue
        if (nomte(5:8) .eq. 'SE22') then
            zr(imatt-1+1) = zr(imatt-1+1) + mat(1)
            zr(imatt-1+2) = zr(imatt-1+2) + mat(2)
            zr(imatt-1+3) = zr(imatt-1+3) + mat(3)
            zr(imatt-1+4) = zr(imatt-1+4) - mat(1)
            zr(imatt-1+5) = zr(imatt-1+5) - mat(2)
            zr(imatt-1+6) = zr(imatt-1+6) + mat(1)
            zr(imatt-1+7) = zr(imatt-1+7) - mat(2)
            zr(imatt-1+8) = zr(imatt-1+8) - mat(3)
            zr(imatt-1+9) = zr(imatt-1+9) + mat(2)
            zr(imatt-1+10) = zr(imatt-1+10) + mat(3)
        else if (nomte(5:8).eq.'SE33') then
            zr(imatt-1+1) = zr(imatt-1+1) + mat(1)
            zr(imatt-1+2) = zr(imatt-1+2) + mat(2)
            zr(imatt-1+3) = zr(imatt-1+3) + mat(3)
            zr(imatt-1+4) = zr(imatt-1+4) + mat(4)
            zr(imatt-1+5) = zr(imatt-1+5) + mat(5)
            zr(imatt-1+6) = zr(imatt-1+6) + mat(6)
            zr(imatt-1+7) = zr(imatt-1+7) - mat(1)
            zr(imatt-1+8) = zr(imatt-1+8) - mat(2)
            zr(imatt-1+9) = zr(imatt-1+9) - mat(4)
            zr(imatt-1+10) = zr(imatt-1+10) + mat(1)
            zr(imatt-1+11) = zr(imatt-1+11) - mat(2)
            zr(imatt-1+12) = zr(imatt-1+12) - mat(3)
            zr(imatt-1+13) = zr(imatt-1+13) - mat(5)
            zr(imatt-1+14) = zr(imatt-1+14) + mat(2)
            zr(imatt-1+15) = zr(imatt-1+15) + mat(3)
            zr(imatt-1+16) = zr(imatt-1+16) - mat(4)
            zr(imatt-1+17) = zr(imatt-1+17) - mat(5)
            zr(imatt-1+18) = zr(imatt-1+18) - mat(6)
            zr(imatt-1+19) = zr(imatt-1+19) + mat(4)
            zr(imatt-1+20) = zr(imatt-1+20) + mat(5)
            zr(imatt-1+21) = zr(imatt-1+21) + mat(6)
        endif
40  end do
end subroutine

subroutine te0277(option, nomte)
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'RESI_THER_PARO_F'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: icode, itemp, itemps, jgano, nbres, ndim, nnos
!
    real(kind=8) :: z1, z2
!-----------------------------------------------------------------------
    parameter (nbres=3)
    character(len=8) :: nompar(nbres)
    real(kind=8) :: valpar(nbres), poids, poids1, poids2, r, r1, r2
    real(kind=8) :: z, hechp, nx, ny, tpg, theta
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: iveres, i, l, li, ihechp, nbelr
    logical :: laxi
!
    character(len=8) :: lirefe(2)
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call elref2(nomte, 2, lirefe, nbelr)
    ASSERT(nbelr.eq.2)
    call elref4(lirefe(2), 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PHECHPF', 'L', ihechp)
    call jevech('PTEMPEI', 'L', itemp)
    call jevech('PRESIDU', 'E', iveres)
!
    theta = zr(itemps+2)
!
    do 30 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids1)
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom+2*nno), nx, ny, poids2)
        r1 = 0.d0
        r2 = 0.d0
        z1 = 0.d0
        z2 = 0.d0
        tpg = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r1 = r1 + zr(igeom+2*i-2)*zr(ivf+l-1)
            r2 = r2 + zr(igeom+2* (nno+i)-2)*zr(ivf+l-1)
            z1 = z1 + zr(igeom+2*i-1)*zr(ivf+l-1)
            z2 = z2 + zr(igeom+2* (nno+i)-1)*zr(ivf+l-1)
            tpg = tpg + (zr(itemp+nno+i-1)-zr(itemp+i-1))*zr(ivf+l-1)
10      continue
        if (laxi) then
            poids1 = poids1*r1
            poids2 = poids2*r2
        endif
        r = (r1+r2)/2.0d0
        z = (z1+z2)/2.0d0
        poids = (poids1+poids2)/2
        valpar(1) = r
        valpar(2) = z
        valpar(3) = zr(itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'INST'
        call fointe('A', zk8(ihechp), 3, nompar, valpar,&
                    hechp, icode)
        ASSERT(icode.eq.0)
!CDIR$ IVDEP
        do 20 i = 1, nno
            li = ivf + (kp-1)*nno + i - 1
            zr(iveres+i-1) = zr(iveres+i-1) - poids*zr(li)*hechp* theta*tpg
            zr(iveres+i-1+nno) = zr(iveres+i-1+nno) + poids*zr(li)* hechp*theta*tpg
20      continue
30  end do
end subroutine

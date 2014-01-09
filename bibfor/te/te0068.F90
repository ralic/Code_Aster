subroutine te0068(option, nomte)
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
#include "asterfort/assert.h"
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_TH_FLUX_F (OU _R)'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbres
!-----------------------------------------------------------------------
    integer :: icode, j, jgano
!-----------------------------------------------------------------------
    parameter (nbres=3)
    character(len=8) :: nompar(nbres), elrefe
    real(kind=8) :: psfn, nx, ny, valpar(nbres), poids, r, z, fluxx, fluxy
    real(kind=8) :: coorse(18), vectt(9)
    integer :: nno, nnos, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: itemps, ivectt, i, l, li, iflu, nnop2, c(6, 9), ise, nse

    ASSERT(option(11:16).ne.'FLUX_R')
    call elref1(elrefe)
    ASSERT(elrefe(1:2).eq.'SE')

    if (lteatt(' ','LUMPE','OUI'))  elrefe='SE2'

    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)

    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PFLUXVF', 'L', iflu)
    call jevech('PVECTTR', 'E', ivectt)

    call connec(nomte, nse, nnop2, c)
!
    do 10 i = 1, nnop2
        vectt(i) = 0.d0
10  end do
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 70 ise = 1, nse
!
        do 30 i = 1, nno
            do 20 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
20          continue
30      continue
!
        do 60 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            r = 0.d0
            z = 0.d0
            do 40 i = 1, nno
                l = (kp-1)*nno + i
                r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
                z = z + coorse(2* (i-1)+2)*zr(ivf+l-1)
40          continue
!
            valpar(1) = r
            nompar(1) = 'X'
            valpar(2) = z
            nompar(2) = 'Y'
            valpar(3) = zr(itemps)
            nompar(3) = 'INST'
            call fointe('FM', zk8(iflu), 3, nompar, valpar,&
                        fluxx, icode)
            call fointe('FM', zk8(iflu+1), 3, nompar, valpar,&
                        fluxy, icode)
!
!  PRODUIT  SCALAIRE   (FLUXV.NORMALE EXT)
!**
            psfn = nx*fluxx + ny*fluxy
!CDIR$ IVDEP
            do 50 i = 1, nno
                li = ivf + (kp-1)*nno + i - 1
                vectt(c(ise,i)) = vectt(c(ise,i)) + poids*zr(li)*psfn
50          continue
60      continue
70  end do
!
    do 80 i = 1, nnop2
        zr(ivectt-1+i) = vectt(i)
80  end do
!
end subroutine

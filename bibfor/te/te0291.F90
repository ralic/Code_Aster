subroutine te0291(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/uthk.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!         CALCUL DE L'INDICATEUR D'ERREUR EN ENERGIE
!         SUR UN ELEMENT AVEC LA METHODE DE ZHU-ZIENKIEWICZ.
!         OPTION : 'CALC_ESTI_ERRE'
!
! ......................................................................
!
!
!
!
    integer :: nno, kp, npg1, i, k, nnos, jgano, ndim
    integer :: ipoids, ivf, idfde, igeom, niv, nbcmp
    integer :: ibid, ierr, imate, isigm, isigno, mater
!
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, valres(2)
    real(kind=8) :: sigl11, sigl22, sigl33, sigl12, sigl13, sigl23
    real(kind=8) :: sigc11, sigc22, sigc33, sigc12, sigc13, sigc23
    real(kind=8) :: esig11, esig22, esig33, esig12, esig13, esig23
    real(kind=8) :: e, nu, eest, nor, norsig, nu0, he, r
!
    integer :: icodre(2)
    character(len=4) :: fami
    character(len=8) :: nomres(2)
!
    logical :: laxi
!
! ----------------------------------------------------------------------
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
    call jemarq()
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    mater = zi(imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
!     CHAMP DE CONTRAINTE CALCULE
    call jevech('PSIEF_R', 'L', isigm)
!     CHAMP DE CONTRAINTE LISSE
    call jevech('PSIGMA', 'L', isigno)
!
    call jevech('PERREUR', 'E', ierr)
!
    norsig = 0.d0
    zr(ierr) = 0.d0
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    do 101 kp = 1, npg1
        k=(kp-1)*nno
        if (ndim .eq. 2) then
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
            nbcmp=4
        else if (ndim.eq.3) then
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            nbcmp=6
        endif
!
        if (laxi) then
            r = 0.d0
            do 103 i = 1, nno
                r = r + zr(igeom+2*(i-1)) * zr(ivf+k+i-1)
103          continue
            poids = poids*r
        endif
!
        sigl11 = 0.d0
        sigl22 = 0.d0
        sigl33 = 0.d0
        sigl12 = 0.d0
        sigl13 = 0.d0
        sigl23 = 0.d0
        do 102 i = 1, nno
            sigl11 = sigl11 + zr(isigno-1+nbcmp*(i-1)+1) * zr(ivf+k+i- 1)
            sigl22 = sigl22 + zr(isigno-1+nbcmp*(i-1)+2) * zr(ivf+k+i- 1)
            sigl33 = sigl33 + zr(isigno-1+nbcmp*(i-1)+3) * zr(ivf+k+i- 1)
            sigl12 = sigl12 + zr(isigno-1+nbcmp*(i-1)+4) * zr(ivf+k+i- 1)
            if (ndim .eq. 3) then
                sigl13 = sigl13 + zr(isigno-1+nbcmp*(i-1)+5) * zr(ivf+ k+i-1)
                sigl23 = sigl23 + zr(isigno-1+nbcmp*(i-1)+6) * zr(ivf+ k+i-1)
            endif
!
102      continue
!
        call rcvalb(fami, kp, 1, '+', mater,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
!
!    ESTIMATION DE L'ERREUR EN NORME DE L' ENERGIE
!
        sigc11 = zr(isigm-1+nbcmp*(kp-1)+1)
        sigc22 = zr(isigm-1+nbcmp*(kp-1)+2)
        sigc33 = zr(isigm-1+nbcmp*(kp-1)+3)
        sigc12 = zr(isigm-1+nbcmp*(kp-1)+4)
!
        esig11 = sigl11-sigc11
        esig22 = sigl22-sigc22
        esig33 = sigl33-sigc33
        esig12 = sigl12-sigc12
!
        if (ndim .eq. 2) then
            eest = esig11**2+esig22**2+esig33**2 +2*(1.d0+nu)*(esig12) **2 -2*nu*esig11*esig22-2*&
                   &nu*esig11*esig33-2*nu*esig22* esig33
            zr(ierr) = zr(ierr) + eest * poids / e
!
!    NORME DE L' ENERGIE DE LA SOLUTION CALCULEE
!
            nor = sigc11**2+sigc22**2+sigc33**2 +2*(1.d0+nu)*(sigc12) **2 -2*nu*sigc11*sigc22-2*n&
                  &u*sigc11*sigc33-2*nu*sigc22* sigc33
            norsig = norsig + nor * poids / e
!
        else if (ndim.eq.3) then
!
            sigc13 = zr(isigm-1+nbcmp*(kp-1)+5)
            sigc23 = zr(isigm-1+nbcmp*(kp-1)+6)
!
            esig13 = sigl13-sigc13
            esig23 = sigl23-sigc23
!
            eest = esig11**2+esig22**2+esig33**2 +2*(1.d0+nu)*(esig12) **2 +2*(1.d0+nu)*(esig13)*&
                   &*2 +2*(1.d0+nu)*(esig23)**2 -2*nu*esig11*esig22-2*nu*esig11*esig33-2*nu*esig2&
                   &2*esig33
            zr(ierr) = zr(ierr) + eest * poids / e
!
!    NORME DE L' ENERGIE DE LA SOLUTION CALCULEE
!
            nor = sigc11**2+sigc22**2+sigc33**2 +2*(1.d0+nu)*(sigc12) **2 +2*(1.d0+nu)*(sigc13)**&
                  &2 +2*(1.d0+nu)*(sigc23)**2 -2*nu*sigc11*sigc22-2*nu*sigc11*sigc33-2*nu*sigc22*&
                  &sigc33
            norsig = norsig + nor * poids / e
        else
            ASSERT(.false.)
        endif
!
101  end do
!
    niv=1
    call uthk(nomte, zr(igeom), he, ndim, ibid,&
              ibid, ibid, ibid, niv, ibid)
!
    if ((zr(ierr)+norsig) .ne. 0.d0) then
        nu0 = 100.d0*sqrt(zr(ierr)/(zr(ierr)+norsig))
    else
        nu0 = 0.d0
    endif
!
    zr(ierr ) = sqrt(zr(ierr))
    zr(ierr+1) = nu0
    zr(ierr+2) = sqrt(norsig)
    zr(ierr-1+10)=he
!
    call jedema()
!
end subroutine

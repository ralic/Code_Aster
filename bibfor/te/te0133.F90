subroutine te0133(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/rcangm.h"
    character(len=16) :: option, nomte
!----------------------------------------------------------------------
!
!   - FONCTION REALISEE: CALCUL DU REPERE LOCAL DONNE PAR L'UTILISATEUR
!   - TYPES D'ELEMENT 3D ET C_PLAN, D_PLAN, AXIS (2D)
!
!
!----------------------------------------------------------------------
!
    integer :: jgeom, jrepl1, jrepl2, jrepl3
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: i, j
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: ux(3), uy(3), uz(3)
    real(kind=8) :: ang(7), bary(3)
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    ASSERT(nnos.le.8)
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PREPLO1', 'E', jrepl1)
    call jevech('PREPLO2', 'E', jrepl2)
    if (ndim .eq. 3) call jevech('PREPLO3', 'E', jrepl3)
!
!
!     CALCUL DU BARYCENTRE (CAS DES REPERES CYLINDRYQUES)
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do 150 i = 1, nno
        do 140 j = 1, ndim
            bary(j) = bary(j)+zr(jgeom+j+ndim*(i-1)-1)/nno
140      continue
150  end do
!
!     CALCUL DES ANGLES NAUTIQUES
    call rcangm(ndim, bary, ang)
!     CALCUL DE LA MATRICE DE PASSAGE GLOBAL->LOCAL
    call matrot(ang, pgl)
!
!     (UX,UY,UZ) : VECTEUR LOCAUX UTILISATEUR DANS LE BASE GLOBALE
!     UX EST LA PREMIERE LIGNE DE PGL
!     UY LA DEUXIEME
!     UZ LA TROISIEME
!
    ux(1) = pgl(1,1)
    ux(2) = pgl(1,2)
    ux(3) = pgl(1,3)
!
    uy(1) = pgl(2,1)
    uy(2) = pgl(2,2)
    uy(3) = pgl(2,3)
!
    uz(1) = pgl(3,1)
    uz(2) = pgl(3,2)
    uz(3) = pgl(3,3)
!
    do 12 i = 1, 2
        zr(jrepl1-1+i)=ux(i)
        zr(jrepl2-1+i)=uy(i)
12  end do
!
    if (ndim .eq. 3) then
        zr(jrepl1-1+3)=ux(3)
        zr(jrepl2-1+3)=uy(3)
        do 13 i = 1, 3
            zr(jrepl3-1+i)=uz(i)
13      continue
    endif
!
end subroutine

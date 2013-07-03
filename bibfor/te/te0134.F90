subroutine te0134(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: option, nomte
!----------------------------------------------------------------------
!
!   - FONCTION REALISEE: CALCUL DU REPERE LOCAL DONNE PAR L'UTILISATEUR
!   - TYPES D'ELEMENT : DKT,DKTG,DST,Q4G,Q4GG,COQUE_3D,GRILLE
!
!
!----------------------------------------------------------------------
!
    integer :: jgeom, jrepl1, jrepl2, jrepl3, jcacoq, iret
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: i
    real(kind=8) :: pgl(3, 3), t2ev(4), t2ve(4)
    real(kind=8) :: pulx(3), puly(3), pulz(3), ux(3), uy(3), uz(3)
    real(kind=8) :: coor(12), alpha, beta, c, s
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
    call assert(nnos.le.8)
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcacoq)
    call jevech('PREPLO1', 'E', jrepl1)
    call jevech('PREPLO2', 'E', jrepl2)
    call jevech('PREPLO3', 'E', jrepl3)
!
    do 40 i = 1, nnos*3
        coor(i) = zr(jgeom-1+i)
40  end do
!
!     CALCUL DE LA MATRICE DE PASSAGE GLOBAL -> LOCAL(INTRINSEQUE)
    if (nnos .eq. 3) then
        call dxtpgl(coor, pgl)
    else if (nnos.eq.4) then
        call dxqpgl(coor, pgl, 'S', iret)
    else
        call assert(.false.)
    endif
    alpha = zr(jcacoq+1)*r8dgrd()
    beta = zr(jcacoq+2)*r8dgrd()
!
    call coqrep(pgl, alpha, beta, t2ev, t2ve,&
                c, s)
!
!     T2EV : LA MATRICE DE PASSAGE (2X2) : UTILISATEUR -> INTRINSEQUE
!
!     PUL : LA MATRICE DE PASSAGE (3X3) : UTILISATEUR -> INTRINSEQUE
!
!         (T2EV(1) , T2EV(3) , 0 )
!     PUL=(T2EV(2) , T2EV(4) , 0 )
!         (  0     ,    0    , 1 )
!     PUL = (PULX,PULY,PULZ)
    pulx(1) = t2ev(1)
    pulx(2) = t2ev(2)
    pulx(3) = 0.0d0
!
    puly(1) = t2ev(3)
    puly(2) = t2ev(4)
    puly(3) = 0.0d0
    pulz(1) = 0.0d0
    pulz(2) = 0.0d0
    pulz(3) = 1.0d0
!
!     (UX,UY,UZ) : VECTEUR LOCAUX UTILISATEUR DANS LE BASE GLOBALE
!     (UX,UY,UZ) = INV(PGL) * PUL
!
    call utpvlg(1, 3, pgl, pulx, ux)
    call utpvlg(1, 3, pgl, puly, uy)
    call utpvlg(1, 3, pgl, pulz, uz)
!
    do 12 i = 1, 3
        zr(jrepl1-1+i)=ux(i)
        zr(jrepl2-1+i)=uy(i)
        zr(jrepl3-1+i)=uz(i)
12  end do
!
end subroutine

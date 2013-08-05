subroutine te0422(option, nomte)
    implicit  none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxefgv.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/utpvgl.h"
    character(len=16) :: option, nomte
!
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
! ======================================================================
!     CALCUL DES EFFORTS GENERALISES
!     GENERALISES POUR LES ELEMENTS DKTG, ET Q4GG
!     POUR UN MATERIAU ISOTROPE
!         OPTION TRAITEE  ==>  SIEF_ELGA
!
!     IN   K16   OPTION : NOM DE L'OPTION A CALCULER
!     IN   K16   NOMTE  : NOM DU TYPE_ELEMENT
!     ------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: i, ipg, jcara, iret
    integer :: jdepg, jeffg, jgeom
!
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), alpha, beta
    real(kind=8) :: depl(24)
    real(kind=8) :: effgt(32)
    real(kind=8) :: t2ev(4), t2ve(4), c, s
!
    character(len=4) :: fami
!
!     ------------------------------------------------------------------
!
    fami = 'RIGI'
!
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
    if (option .ne. 'SIEF_ELGA') ASSERT(.false.)
!
    do 10 i = 1, 32
        effgt(i) = 0.d0
10  end do
!
    call jevech('PGEOMER', 'L', jgeom)
!
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
!
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
    call jevech('PCACOQU', 'L', jcara)
    alpha = zr(jcara+1) * r8dgrd()
    beta = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2ev, t2ve,&
                c, s)
!
    call jevech('PDEPLAR', 'L', jdepg)
    call utpvgl(nno, 6, pgl, zr(jdepg), depl)
!
! --- CALCUL DES EFFORTS GENERALISES AUX POINTS DE CALCUL
    call jevech('PCONTRR', 'E', jeffg)
    call dxefgv(nomte, option, xyzl, pgl, depl,&
                effgt)
!
    call dxefro(npg, t2ve, effgt, zr(jeffg))
!
end subroutine

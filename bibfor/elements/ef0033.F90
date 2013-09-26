subroutine ef0033(nomte)
    implicit  none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxefgv.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
#include "asterfort/utpvgl.h"
    character(len=16) :: nomte
! ----------------------------------------------------------------------
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
!     CALCUL DE EFGE_ELNO EN LINEAIRE
!     ------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: iret, jcara
    integer :: jdepg, jeffg, jgeom
    integer :: np
!
    real(kind=8) :: alpha, beta
    real(kind=8) :: pgl(3, 3), xyzl(3, 4)
    real(kind=8) :: depl(24)
    real(kind=8) :: effgt(32)
    real(kind=8) :: t2iu(4), t2ui(4), c, s
!
    character(len=4) :: fami
!     ------------------------------------------------------------------
!
!
    fami='NOEU'
    call elref4(' ', fami, ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano)
!
!
    call r8inir(32, 0.d0, effgt, 1)
!
    call jevech('PGEOMER', 'L', jgeom)
!
    np=nno
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
    beta  = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
!
    call jevech('PDEPLAR', 'L', jdepg)
    call utpvgl(nno, 6, pgl, zr(jdepg), depl)
!
!
! --- CALCUL DES EFFORTS GENERALISES VRAIS AUX POINTS DE CALCUL
    call dxefgv(nomte, 'EFGE_ELNO', xyzl, pgl, depl, effgt)
!
! ---   PASSAGE DES EFFORTS GENERALISES DU REPERE INTRINSEQUE
! ---   A L'ELEMENT AU REPERE LOCAL DE LA COQUE
!       ---------------------------------------
    call jevech('PEFFORR', 'E', jeffg)
    call dxefro(np, t2iu, effgt, zr(jeffg))
!
end subroutine

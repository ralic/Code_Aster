subroutine ef0031(nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/cosiro.h"
#include "asterfort/dxeffi.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/ppgan2.h"
#include "asterfort/tecach.h"
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano, ind
    integer :: icompo, ichn, jgeom, jcara, iret, icontp, ibid
!
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), effgt(32), alpha, beta
    real(kind=8) :: t2iu(4), t2ui(4), c, s
!
!     ---> POUR DKT/DST EFFINT = 24
!     ---> POUR DKQ/DSQ EFFINT = 32
    real(kind=8) :: effint(32)
!
! DEB ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano)
!
! --- PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
    call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G', ibid, 'S')
!
    call jevech('PGEOMER', 'L', jgeom)
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
!
!
    call tecach('NNN', 'PCOMPOR', 'L', iret, iad=icompo)
    call jevech('PCONTRR', 'L', icontp)
    ind=8
    call dxeffi('EFGE_ELNO', nomte, pgl, zr(icontp), ind, effint)
!
    call jevech('PCACOQU', 'L', jcara)
    alpha = zr(jcara+1) * r8dgrd()
    beta  = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
!
    call dxefro(npg, t2iu, effint, effgt)
    call jevech('PEFFORR', 'E', ichn)
    call ppgan2(jgano, 1, ind, effgt, zr(ichn))
!
end subroutine

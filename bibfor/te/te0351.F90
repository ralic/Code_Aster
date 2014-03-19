subroutine te0351(option, nomte)
!
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
! ======================================================================
!    CALCUL DES FORCES NODALES POUR LES ELEMENTS QUAS4
!    => 1 POINT DE GAUSS + STABILISATION ASSUMED STRAIN
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmasf2.h"
    character(len=16) :: option, nomte
!
    character(len=8) :: typmod(2)
    integer :: nno, npg1, ipoids, ivf, idfde, igeom
    integer :: icontm, ivectu, ndim, nnos, jgano
    real(kind=8) :: work(18)
!
!
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! - TYPE DE MODELISATION
!
    if (lteatt('AXIS','OUI')) then
        typmod(1) = 'AXIS'
    else if (lteatt('C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN'
    else if (lteatt('D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN'
    else
        ASSERT(.false.)
    endif
!
    typmod(2) = 'ASSU    '
!
! - PARAMETRES
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVECTUR', 'E', ivectu)
!
    call nmasf2(nno, npg1, ipoids, ivf, idfde,&
                zr(igeom), typmod, zr(icontm), work, zr(ivectu))
!
end subroutine

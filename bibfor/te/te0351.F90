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
#include "asterfort/elref4.h"
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
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! - TYPE DE MODELISATION
!
    if (lteatt(' ','AXIS','OUI')) then
        typmod(1) = 'AXIS    '
    else if (nomte(3:4).eq.'CP') then
        typmod(1) = 'C_PLAN  '
    else if (nomte(3:4).eq.'DP') then
        typmod(1) = 'D_PLAN  '
    else
!       NOM D'ELEMENT ILLICITE
        ASSERT(nomte(3:4).eq.'CP')
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

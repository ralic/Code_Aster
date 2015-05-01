subroutine nmmabu(ndim, nno, axi, grand, dfdi,&
                  b)
!
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
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    aster_logical :: grand, axi
    integer :: ndim, nno
    real(kind=8) :: dfdi(nno, ndim), b(6, 3, nno)
!
! ----------------------------------------------------------------------
!                     CALCUL DE LA MATRICE B :  DEPS = B.DU
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO     : NOMBRE DE NOEUDS
! IN  AXI     : .TRUE. SI AXISYMETRIQUE
! IN  GRAND   : .TRUE. SI GRANDES DEFORMATIONS
! IN  DFDI    : DERIVEE DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
! OUT B       : MATRICE B : B(6,3,NNP)
! ----------------------------------------------------------------------
!
    integer :: n
    real(kind=8) :: r2
! ----------------------------------------------------------------------
!
!
    if (grand) then
        call utmess('F', 'ALGORITH7_76')
    endif
    if (axi) then
        call utmess('F', 'ALGORITH7_76')
    endif
!
    call r8inir(18*nno, 0.d0, b, 1)
    r2 = sqrt(2.d0)/2.d0
!
    ASSERT((ndim.eq.2).or.(ndim.eq.3))
!
    if (ndim .eq. 2) then
        do 10 n = 1, nno
            b(1,1,n) = dfdi(n,1)
            b(2,2,n) = dfdi(n,2)
            b(4,1,n) = r2*dfdi(n,2)
            b(4,2,n) = r2*dfdi(n,1)
 10     continue
!
    else if (ndim.eq.3) then
        do 20 n = 1, nno
            b(1,1,n) = dfdi(n,1)
            b(2,2,n) = dfdi(n,2)
            b(3,3,n) = dfdi(n,3)
            b(4,1,n) = r2*dfdi(n,2)
            b(4,2,n) = r2*dfdi(n,1)
            b(5,1,n) = r2*dfdi(n,3)
            b(5,3,n) = r2*dfdi(n,1)
            b(6,2,n) = r2*dfdi(n,3)
            b(6,3,n) = r2*dfdi(n,2)
 20     continue
!
    endif
!
end subroutine

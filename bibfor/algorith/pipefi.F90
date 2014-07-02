subroutine pipefi(npg, lgpg, mate, geom, vim,&
                  ddepl, deplm, ddepl0, ddepl1, dtau,&
                  copilo, typmod)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/nmfisa.h"
#include "asterfort/pipeba.h"
#include "asterfort/r8inir.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: mate, npg, lgpg
    real(kind=8) :: geom(2, 4), vim(lgpg, npg), ddepl(2, 4), deplm(2, 4)
    real(kind=8) :: ddepl0(2, 4), ddepl1(2, 4), dtau, copilo(5, npg)
    character(len=8) :: typmod(2)
!
!-----------------------------------------------------------------------
!  PILOTAGE PRED_ELAS POUR LES ELEMENTS DE JOINT 2D
!
! IN  : GEOM, MATE, VIM, DDEPL, DEPLM, DDEPL0, DDELP1, DTAU, NPG
! OUT : COPILO
!-----------------------------------------------------------------------
!
    aster_logical :: axi
    integer :: i, j, kpg
    real(kind=8) :: up(8), ud(8), sup(2), sud(2), b(2, 8), poids
!-----------------------------------------------------------------------
!
! INITIALISATION
!
    axi = typmod(1) .eq. 'AXIS'
!
! DEPLACEMENT U(ETA) = UP + ETA * UD
!
    call dcopy(8, deplm, 1, up, 1)
    call daxpy(8, 1.d0, ddepl, 1, up,&
               1)
    call daxpy(8, 1.d0, ddepl0, 1, up,&
               1)
    call dcopy(8, ddepl1, 1, ud, 1)
! BOUCLE SUR LES POINTS DE GAUSS :
!
    do 10 kpg = 1, npg
!
!      SAUT AU POINT DE GAUSS : SU(ETA) = SUP + ETA * SUD
        call nmfisa(axi, geom, kpg, poids, b)
        do 30 i = 1, 2
            sup(i) = 0.d0
            sud(i) = 0.d0
            do 40 j = 1, 8
                sup(i) = sup(i) + b(i,j)*up(j)
                sud(i) = sud(i) + b(i,j)*ud(j)
 40         continue
 30     continue
!
!      INITIALISATION DES COEFFICIENTS DE PILOTAGE
        call r8inir(4, 0.d0, copilo(1, kpg), 1)
        copilo(5,kpg) = r8vide()
!
!      APPEL DU PILOTAGE PRED_ELAS SPECIFIQUE A LA LOI DE COMPORTEMENT
        call pipeba(2, mate, sup, sud, vim(1, kpg),&
                    dtau, copilo(1, kpg))
!
 10 end do
!
end subroutine

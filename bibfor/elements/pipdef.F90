subroutine pipdef(ndim, nno, kpg, ipoids, ivf,&
                  idfde, geom, typmod, compor, deplm,&
                  ddepl, depl0, depl1, dfdi, fm,&
                  epsm, epsp, epsd)
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
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmepsb.h"
#include "asterfort/nmgeom.h"
#include "blas/daxpy.h"
    integer :: ndim, nno, kpg
    integer :: ipoids, ivf, idfde
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*)
    real(kind=8) :: geom(ndim, *), deplm(*)
    real(kind=8) :: ddepl(*), depl0(*), depl1(*)
    real(kind=8) :: dfdi(*)
    real(kind=8) :: epsm(6), epsp(6), epsd(6)
    real(kind=8) :: fm(3, 3)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS/DEFORMATION)
!
! CALCUL DES DEFORMATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  KPG    : NUMERO DU POINT DE GAUSS
! IN  IPOIDS : POIDS DES POINTS DE GAUSS
! IN  IVF    : VALEUR DES FONCTIONS DE FORME
! IN  IDFDE  : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM   : COORDONEES DES NOEUDS
! IN  TYPMOD : TYPE DE MODELISATION
! IN  COMPOR : COMPORTEMENT
! IN  DEPLM  : DEPLACEMENT EN T-
! IN  DDEPL  : INCREMENT DE DEPLACEMENT A L'ITERATION NEWTON COURANTE
! IN  DEPL0  : CORRECTION DE DEPLACEMENT POUR FORCES FIXES
! IN  DEPL1  : CORRECTION DE DEPLACEMENT POUR FORCES PILOTEES
! OUT DFDI   : DERIVEE DES FONCTIONS DE FORME
! OUT FM     : GRADIENT DE LA TRANSFORMATION AU TEMPS MOINS
! OUT EPSM   : DEFORMATIONS AU TEMPS MOINS
! OUT EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! OUT EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
!
!
!
!
    integer :: iffg
    aster_logical :: axi, grand
    integer :: ndimsi
    real(kind=8) :: r, deps(6)
    real(kind=8) :: t9bid(3, 3), t18bid(6, 3)
    real(kind=8) :: poids
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    axi = typmod(1) .eq. 'AXIS'
    grand = compor(3) .ne. 'PETIT'
    ndimsi = 2*ndim
!
    if (typmod(2) .eq. 'DEPLA') then
!
! ----- CALCUL DE EPSM (LINEAIRE) OU EM (GREEN)  = EPS(UM)
        call nmgeom(ndim, nno, axi, grand, geom,&
                    kpg, ipoids, ivf, idfde, deplm,&
                    .true._1, poids, dfdi, fm, epsm,&
                    r)
!
! ----- REACTUALISATION DE LA GEOMETRIE SI GRANDES DEFS
        if (grand) then
            call daxpy(ndim*nno, 1.d0, deplm, 1, geom,&
                       1)
        endif
!
! ----- CALCUL DE DEPS = EPS(DU)
        call nmgeom(ndim, nno, axi, .false._1, geom,&
                    kpg, ipoids, ivf, idfde, ddepl,&
                    .true._1, poids, dfdi, t9bid, deps,&
                    r)
!
! ----- CALCUL DE EPSP (= DEPS + EPS(DU0) )
        call nmgeom(ndim, nno, axi, .false._1, geom,&
                    kpg, ipoids, ivf, idfde, depl0,&
                    .true._1, poids, dfdi, t9bid, epsp,&
                    r)
        call daxpy(ndimsi, 1.d0, deps, 1, epsp,&
                   1)
!
! ----- CALCUL DE EPSD (DEPS = EPSP + ETA EPSD)
        call nmgeom(ndim, nno, axi, .false._1, geom,&
                    kpg, ipoids, ivf, idfde, depl1,&
                    .true._1, poids, dfdi, t9bid, epsd,&
                    r)
!
    else if (typmod(2).eq.'GRADEPSI') then
        iffg = ivf+(kpg-1)*nno
        call dfdmip(ndim, nno, axi, geom, kpg,&
                    ipoids, zr(iffg), idfde, r, poids,&
                    dfdi)
! ----- CALCUL DE EPSM
        call nmepsb(ndim, nno, axi, zr(iffg), dfdi,&
                    deplm, epsm, t18bid)
! ----- CALCUL DE DEPS = EPS(DU)
        call nmepsb(ndim, nno, axi, zr(iffg), dfdi,&
                    ddepl, deps, t18bid)
! ----- CALCUL DE EPSP (= DEPS + EPS(DU0) )
        call nmepsb(ndim, nno, axi, zr(iffg), dfdi,&
                    depl0, epsp, t18bid)
! ----- CALCUL DE EPSD (DEPS = EPSP + ETA EPSD)
        call nmepsb(ndim, nno, axi, zr(iffg), dfdi,&
                    depl1, epsd, t18bid)
        call daxpy(ndimsi, 1.d0, deps, 1, epsp,&
                   1)
    else
        ASSERT(.false.)
    endif
!
!
!
!
end subroutine

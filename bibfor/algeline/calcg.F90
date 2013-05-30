subroutine calcg(dfds, vecn, g, devg, traceg,&
                 devgii)
!
    implicit      none
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/trace.h'
    include 'blas/ddot.h'
    real(kind=8) :: dfds(6), vecn(6), g(6), devg(6), traceg, devgii
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : RECHERCHE DE LA DIRECTION D'ECOULEMENT ---------------------
! ======================================================================
! IN  : N      : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
! --- : ND     : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : DFDS   : DF/DSIG -----------------------------------------------
! --- : VECN   : VECTEUR N ---------------------------------------------
! OUT : G      : G = DF/DSIG - (DF/DSIG.VECN)VECN ----------------------
! --- : DEVG   : DEVIATEUR DE G ----------------------------------------
! --- : TRACEG : PREMIER INVARIANT DE G --------------------------------
! --- : DEVGII : NORME DU DEVIATEUR ------------------------------------
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: fact1
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- CALCUL DE G ------------------------------------------------------
! ======================================================================
    fact1=ddot(ndt,dfds,1,vecn,1)
    do 10 ii = 1, ndt
        g(ii) = dfds(ii) - fact1*vecn(ii)
10  end do
! ======================================================================
! --- CALCUL DU DEVIATEUR DE G ET DE SA NORME --------------------------
! ======================================================================
    call lcdevi(g, devg)
    devgii=ddot(ndt,devg,1,devg,1)
    devgii = sqrt (devgii)
! ======================================================================
! --- CALCUL DU PREMIER INVARIANT DE G ---------------------------------
! ======================================================================
    traceg = trace(ndi, g)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

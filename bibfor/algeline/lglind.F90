subroutine lglind(nbmat, mater, parame, ge, q,&
                  vecn, deps, devg, devgii, traceg,&
                  dy)
!
    implicit      none
    include 'asterfort/calcg.h'
    include 'asterfort/drfdrs.h'
    include 'asterfort/drudrs.h'
    include 'asterfort/hlode.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lceqvn.h'
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), parame(5), q(6), vecn(6), ge
    real(kind=8) :: deps(6), devg(6), devgii, traceg, dy(10)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- BUT : CALCUL DU PREMIER MULTIPLICATEUR PLASTIQUE (CAS GAMP = 0) --
! ======================================================================
! IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
! --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : NR     : NOMBRE DE RELATIONS NON LINEAIRES ---------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : PARAME : VARIABLES D'ECROUISSAGES ------------------------------
! --- : GE     : GE ----------------------------------------------------
! --- : Q      : DG/DS -------------------------------------------------
! --- : VECN   : VECTEUR N ---------------------------------------------
! --- : DEPS   : INCREMENT DE DEFORMATIONS DEPUIS L'INSTANT PRECEDENT --
! OUT : DEVG   : DEVIATEUR DE G ----------------------------------------
! --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
! --- : TRACEG : TRACE DU TENSEUR G ------------------------------------
! --- : DY     : INCREMENTS (SIG, I1, GAMP, EVP, DELTA) ----------------
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: gammax, mu, k, gamcjs, sigc, h0, dgamp, ddelta
    real(kind=8) :: duds(6), dfds(6), g(6), ds(6), dinv, mun, deux, trois, dix
    real(kind=8) :: devp
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( dix    = 10.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES PARAMETRES MATERIAU -----------------------------
! ======================================================================
    mu = mater( 4,1)
    k = mater( 5,1)
    sigc = mater( 9,2)
    gamcjs = mater(12,2)
! ======================================================================
! --- CALCUL DE H0, CALCUL INTERMEDIAIRE -------------------------------
! ======================================================================
    h0 = hlode(gamcjs, mun)
! ======================================================================
! --- CALCUL DE DUDS ---------------------------------------------------
! ======================================================================
    call drudrs(parame, q, h0, sigc, duds)
! ======================================================================
! --- CALCUL DE DFDS ---------------------------------------------------
! ======================================================================
    call drfdrs(q, parame, h0, sigc, ge,&
                duds, dfds)
! ======================================================================
! --- CALCUL DE G ------------------------------------------------------
! ======================================================================
    call calcg(dfds, vecn, g, devg, traceg,&
               devgii)
! ======================================================================
! --- CALCUL DU PREMIER INCREMENT DE GAMP ------------------------------
! ======================================================================
    gammax = 0.0d0
    do 10 ii = 1, ndt
        if (abs(deps(ii)) .gt. gammax) gammax = abs(deps(ii))
10  end do
    dgamp = gammax / dix
! ======================================================================
! --- CALCUL DU PREMIER DELTA ------------------------------------------
! ======================================================================
    ddelta = dgamp*sqrt(trois/deux)/devgii
! ======================================================================
! --- CALCUL DU PREMIER INCREMENT DU DEVIATEUR DES CONTRAINTES ---------
! ======================================================================
    do 20 ii = 1, ndt
        ds(ii) = mun * deux * mu * ddelta * devg(ii)
20  end do
! ======================================================================
! --- CALCUL DU PREMIER INCREMENT DU PREMIER INVARIANT DES CONTRAINTES -
! ======================================================================
    dinv = mun * trois * k * ddelta * traceg
! ======================================================================
! --- CALCUL DU PREMIER INCREMENT DE EVP -------------------------------
! ======================================================================
    devp = ddelta * traceg
! ======================================================================
! --- STOCKAGE ---------------------------------------------------------
! ======================================================================
    call lceqvn(ndt, ds(1), dy(1))
    call lceqvn(1, dinv, dy(ndt+1))
    call lceqvn(1, dgamp, dy(ndt+2))
    call lceqvn(1, devp, dy(ndt+3))
    call lceqvn(1, ddelta, dy(ndt+4))
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

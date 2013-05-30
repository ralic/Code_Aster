subroutine calcdy(mu, k, f0, devg, devgii,&
                  traceg, dfdl, delta, dy)
!
    implicit      none
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lceqvn.h'
    real(kind=8) :: mu, k, f0, devg(6), devgii, traceg
    real(kind=8) :: dfdl, delta, dy(10)
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
! --- BUT : CALCUL DE DY -----------------------------------------------
! ======================================================================
! IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
! --- : NR     : NOMBRE DE COMPOSANTES NON LINEAIRES -------------------
! --- : MU     : PARAMETRE MATERIAU ------------------------------------
! --- : K      : PARAMETRE MATERIAU ------------------------------------
! --- : F0     : VALEUR SEUIL A L'INSTANT 0 ----------------------------
! --- : DEVG   : DEVIATEUR DE G ----------------------------------------
! --- : DEVGII : NORME DU TENSEUR DEVG ---------------------------------
! --- : TRACEG : TRACE DE G --------------------------------------------
! --- : DFDL   : DF/DLAMBDA --------------------------------------------
! --- : DELTA  : DELTA LAMBDA INITIAL ----------------------------------
! OUT : DY     : INCREMENTS DE L'ITERATION COURANTE --------------------
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: ddelta, dgamp, dsn(6), dinv, devp, mun, deux, trois
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    =  -1.0d0  )
    parameter       ( deux   =   2.0d0  )
    parameter       ( trois  =   3.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- CALCUL DES INCREMENTS --------------------------------------------
! ======================================================================
    ddelta = mun * f0 / dfdl
    delta = delta + ddelta
    dgamp = delta * sqrt(deux/trois) * devgii
    do 10 ii = 1, ndt
        dsn(ii) = mun * deux * mu * delta * devg(ii)
10  end do
    dinv = mun * trois * k * delta * traceg
    devp = delta * traceg
! ======================================================================
! --- STOCKAGE ---------------------------------------------------------
! ======================================================================
    call lceqvn(ndt, dsn(1), dy(1))
    call lceqvn(1, dinv, dy(ndt+1))
    call lceqvn(1, dgamp, dy(ndt+2))
    call lceqvn(1, devp, dy(ndt+3))
    call lceqvn(1, ddelta, dy(ndt+4))
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

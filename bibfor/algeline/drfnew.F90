subroutine drfnew(devg, devgii, traceg, dfds, dfdg,&
                  mu, k, dfdl)
!
    implicit      none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "blas/ddot.h"
    real(kind=8) :: devg(6), devgii, traceg, dfds(6), dfdg, mu, k, dfdl
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
! --- BUT : CALCUL DE DF/D(DELTA_LAMBDA) POUR NEWTON -------------------
! ======================================================================
! IN  : N      : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
! --- : ND     : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : DEVG   : DEVIATEUR DE G ----------------------------------------
! --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
! --- : TRACEG : PREMIER INVARIANT DE G --------------------------------
! --- : DFDS   : DERIVEE DE F PAR RAPPORT AUX CONTRAINTES --------------
! --- : DFDG   : DERIVEE DE F PAR RAPPORT A GAMP -----------------------
! --- : MU     : PARAMETRE MATERIAU ------------------------------------
! --- : K      : PARAMETRE MATERIAU ------------------------------------
! OUT : DFDL   : DF/DLAMBDA = - DF/DSIG.(2*MU*DEV(G) + K*TRACE(G)*I)
! ------------ :                + DF/DGAMP*RAC(2/3)*GII
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: vect1(6), scal1, mun, deux, trois
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- CALCUL INTERMEDIAIRE ---------------------------------------------
! ======================================================================
    do 10 ii = 1, ndt
        vect1(ii) = deux*mu*devg(ii)
10  end do
    do 20 ii = 1, ndi
        vect1(ii) = vect1(ii) + k*traceg
20  end do
    scal1=ddot(ndt,dfds,1,vect1,1)
! ======================================================================
! --- CALCUL FINAL -----------------------------------------------------
! ======================================================================
    dfdl = mun * scal1 + dfdg*sqrt(deux/trois)*devgii
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

subroutine drudrs(parame, q, h0, sigc, dudsig)
!
    implicit      none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    real(kind=8) :: q(6), parame(5), h0, sigc, dudsig(6)
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE DU/DSIG ------------------------------------------
! ======================================================================
! IN  : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
! --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : PARAME : PARAMETRES D'ECROUISSAGES -----------------------------
! --- : Q      : DG/DSIG -----------------------------------------------
! --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
! --- : SIGC   : PARAMETRE MATERIAU ------------------------------------
! OUT : DUDSIG : DUDSIG = - M(GAMP)*K(GAMP)*Q/(RAC(6)*SIGC*H0) ---------
! ------------ :          - K(GAMP)*M(GAMP)*I/(3*SIGC) -----------------
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: mgamp, kgamp, fact1, fact2, mun, trois, six
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( six    =  6.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES VARIABLES D'ECROUISSAGES ------------------------
! ======================================================================
    kgamp = parame(3)
    mgamp = parame(4)
! ======================================================================
! --- CALCUL INTERMEDIAIRE ---------------------------------------------
! ======================================================================
    fact1 = mun*mgamp*kgamp/(sqrt(six)*sigc*h0)
    fact2 = mun*mgamp*kgamp/(trois*sigc)
! ======================================================================
! --- CALCUL FINAL -----------------------------------------------------
! ======================================================================
    do 10 ii = 1, ndt
        dudsig(ii) = fact1*q(ii)
10  end do
    do 20 ii = 1, ndi
        dudsig(ii) = dudsig(ii) + fact2
20  end do
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

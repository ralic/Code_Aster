subroutine drfdrs(q, parame, h0, sigc, rgdev,&
                  duds, dfds)
!
    implicit    none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    real(kind=8) :: q(6), parame(5), h0, sigc, rgdev, duds(6), dfds(6)
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
! IN  : N      : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
! --- : Q      : DG/DSIG -----------------------------------------------
! --- : PARAME : PARAMETRES D'ECROUISSAGE ------------------------------
! --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
! --- : SIGC   : PARAMETRE DU MODELE -----------------------------------
! --- : RGDEV  : G(S) --------------------------------------------------
! --- : DUDS   : DU/DSIG -----------------------------------------------
! OUT : DFDS   : DF/DSIG = (1/A)*((1/(SIGC*H0))**(1/A))*G**((1-A)/A)*Q -
! ------------ :         - DU/DSIG  ------------------------------------
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: agamp, fact1, fact2, fact3, fact4, un
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( un     =  1.0d0   )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES VARIABLES D'ECROUISSAGES ------------------------
! ======================================================================
    agamp = parame(2)
! ======================================================================
! --- VARIABLE INTERMEDIAIRE -------------------------------------------
! ======================================================================
    fact2 = un/agamp
    fact3 = (un/(sigc*h0))**fact2
    fact3 = fact3*fact2
    fact2 = (un-agamp)/agamp
    fact4 = rgdev**fact2
    fact1 = fact3*fact4
! ======================================================================
! --- CALCUL FINAL -----------------------------------------------------
! ======================================================================
    do 10 ii = 1, ndt
        dfds(ii) = fact1*q(ii)-duds(ii)
10  end do
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

function domrev(gamcjs, sigc, parame, rgdev, rucpla)
!
    implicit      none
#include "asterfort/hlode.h"
    real(kind=8) :: gamcjs, sigc, parame(5), rgdev, rucpla, domrev
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
! --- BUT : CALCUL DU DOMAINE DE REVERSIBILITE -------------------------
! ======================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------------
! --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
! --- : RGDEV  : FONCTION G(S) -----------------------------------------
! --- : RUCPLA : CRITERE PLASTIQUE -------------------------------------
! OUT : DOMREV : DOMAINE DE REVERSIBILITE (FORMULATION BIS) ------------
! ======================================================================
    real(kind=8) :: agamp, h0, mun, un
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       (  un    =  1.0d0  )
    agamp = parame(2)
! ======================================================================
! --- CALCUL DE H0 = (1-GAMMA_CJS)**(1/6) ------------------------------
! ======================================================================
    h0 = hlode(gamcjs, mun)
! ======================================================================
! --- CALCUL DE FBIS = (G(S)/(SIG_C*H0))**(1/A(GAMP))-U(GAMP) ----------
! ======================================================================
    domrev = (rgdev/(sigc*h0))**(un/agamp) - rucpla
! ======================================================================
end function

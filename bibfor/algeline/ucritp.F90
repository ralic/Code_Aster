function ucritp(nbmat, mater, parame, rgdev, invar1)
!
    implicit      none
    include 'asterfort/hlode.h'
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), parame(5), rgdev, invar1, ucritp
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
! --- BUT : CALCUL DU CRITERE PLASTIQUE --------------------------------
! ======================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------------
! --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
! --- : RGDEV  : FONCTION G(S) -----------------------------------------
! --- : INVAR1 : PREMIER INVARIANT DES CONTRAINTES ---------------------
! OUT : UCRITP = U(SIG,GAMP) -------------------------------------------
! ------------ = - M(GAMP)*K(GAMP)*G(S)/(RAC(6)*SIGMA_C*H0 -------------
! ------------ : - M(GAMP)*K(GAMP)*I1/(3*SIGMA_C) ----------------------
! ------------ : + S(GAMP)*K(GAMP) -------------------------------------
! ======================================================================
    real(kind=8) :: sgamp, kgamp, mgamp, mun, trois, six
    real(kind=8) :: h0, fact1, fact2, fact3, sigc, gamcjs
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( six    =  6.0d0  )
! ======================================================================
! --- RECUPERATION DES PARAMETRES MATERIAU -----------------------------
! ======================================================================
    sigc = mater( 9,2)
    gamcjs = mater(12,2)
! ======================================================================
! --- RECUPERATION DES VARIABLES D'ECROUISSAGE -------------------------
! ======================================================================
    sgamp = parame(1)
    kgamp = parame(3)
    mgamp = parame(4)
! ======================================================================
! --- CALCUL DE H0 = (1-GAMCJS)**(1/6) ---------------------------------
! ======================================================================
    h0 = hlode(gamcjs, mun)
! ======================================================================
! --- CALCUL DE U(SIG,GAMP) --------------------------------------------
! ======================================================================
    fact1 = mun*mgamp*kgamp*rgdev/(sqrt(six)*sigc*h0)
    fact2 = mun*mgamp*kgamp*invar1/(trois*sigc)
    fact3 = sgamp*kgamp
    ucritp = fact1 + fact2 + fact3
! ======================================================================
end function

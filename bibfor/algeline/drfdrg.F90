subroutine drfdrg(parame, derpar, h0, sigc, rgdev,&
                  dudg, df)
!
    implicit      none
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    real(kind=8) :: h0, sigc, rgdev, dudg, df, parame(5), derpar(4)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! --- BUT : CALCUL DE DF/DGAMP -----------------------------------------
! ======================================================================
! IN  : PARAME : PARAMETRES D'ECROUISSAGE ------------------------------
! --- : DERPAR : DERIVEES DES PARAMETRES D'ECROUISSAGE -----------------
! --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
! --- : SIGC   : PARAMETRE DU MODELE -----------------------------------
! --- : RGDEV  : G(S) --------------------------------------------------
! --- : DUDG   : DU/DGAMP ----------------------------------------------
! OUT : DF     : DF/DGAMP = - ((1/A(GAMP))**2)*   ----------------------
! ------------ :              ((G/(SIGC*H0))**(1/A(GAMP))*  ------------
! ------------ :              LOG(G/(SIGC*H0))*DA/DG-DU/DG  ------------
! ======================================================================
    real(kind=8) :: agamp, da, mun, un, fact1, fact2, fact3
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( un     =  1.0d0  )
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES VARIABLES D'ECROUISSAGE ET DE SES DERIVEES ------
! ======================================================================
    agamp = parame(2)
    da = derpar(2)
! ======================================================================
! --- CALCUL FINAL -----------------------------------------------------
! ======================================================================
    fact1 = un/agamp
    fact2 = rgdev/(sigc*h0)
    fact3 = fact2**fact1
    df = mun*fact1*fact1*fact3*log(fact2)*da - dudg
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

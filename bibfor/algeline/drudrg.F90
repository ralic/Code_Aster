subroutine drudrg(parame, derpar, h0, sigc, rgdev,&
                  invar1, dudg)
!
    implicit  none
    include 'asterfort/derpro.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    real(kind=8) :: parame(5), derpar(4), h0, sigc, rgdev, invar1, dudg
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
! --- BUT : CALCUL DE DU/DGAMP -----------------------------------------
! ======================================================================
! IN  : PARAME : VECTEUR DES VARIABLES D'ECROUISSAGE -------------------
! --- : DERIVE : VECTEUR DES DERIVEES DES VARIABLES D'ECROUISSAGE ------
! --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
! --- : SIGC   : PARAMETRE DU MODELE -----------------------------------
! --- : RGDEV  : G(S) --------------------------------------------------
! --- : INVAR1 : PREMIER INVARIANT DES CONTRAINTES ---------------------
! OUT : DUDG   : DUDG = - G/(RAC(6)*SIGC*H0)*D(KM)/DGAMP ---------------
! ------------ :        - INVAR1/(3*SIGC)*D(KM)DGAMP -------------------
! ------------ :        + D(KS)/DGAMP ----------------------------------
! ======================================================================
    real(kind=8) :: mun, trois, six, fact1, fact2
    real(kind=8) :: sgamp, kgamp, mgamp, ds, dk, dm, dkm, dks
! ======================================================================
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    =  -1.0d0  )
    parameter       ( trois  =   3.0d0  )
    parameter       ( six    =   6.0d0  )
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES VARIABLES D'ECROUISSAGES ET DE SES DERIVEES -----
! ======================================================================
    sgamp = parame(1)
    kgamp = parame(3)
    mgamp = parame(4)
    ds = derpar(1)
    dk = derpar(3)
    dm = derpar(4)
! ======================================================================
! --- CALCUL INTERMEDIAIRE ---------------------------------------------
! ======================================================================
    fact1 = mun*rgdev/(sqrt(six)*sigc*h0)
    fact2 = mun*invar1/(trois*sigc)
    call derpro(kgamp, dk, mgamp, dm, dkm)
    call derpro(kgamp, dk, sgamp, ds, dks)
! ======================================================================
! --- CALCUL FINAL -----------------------------------------------------
! ======================================================================
    dudg = (fact1+fact2)*dkm + dks
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

subroutine lkhtet(nbmat, mater, rcos3t, h0e, h0c,&
                  htheta)
!
    implicit none
#include "asterfort/lkhlod.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), rcos3t, htheta
! =================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! --- BUT : CALCUL DE H(THETA)
! =================================================================
! IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : RCOS3T : COS(3T) ------------------------------------------
! OUT : H0E    : PARAMETRE UTILIE DANS LE CRITERE------------------
!     : H0C    : PARAMETRE UTILIE DANS LE CRITERE------------------
!     : HTHETA : H(THETA ------------------------------------------
! =================================================================
    real(kind=8) :: un, deux, six
    real(kind=8) :: gamcjs, h0ext, h0c, h0e, hlode
    real(kind=8) :: fact1, fact2, fact3
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( six    =  6.0d0  )
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    h0ext = mater(4,2)
    gamcjs = mater(5,2)
! =================================================================
! ---- CALCUL DE H0C
! =================================================================
    h0c = (un - gamcjs )**(un/six)
! =================================================================
! ---- CALCUL DE H0E
! =================================================================
    h0e = (un + gamcjs )**(un/six)
! =================================================================
! ---- CALCUL DE H(THETA)
! =================================================================
    fact1 = (h0c + h0ext)/deux
    fact2 = (h0c - h0ext)/deux
!
    hlode = lkhlod(gamcjs,rcos3t)
!
    fact3 = (deux*hlode-(h0c+h0e))/(h0c-h0e)
!
    htheta = fact1 + fact2*fact3
! =================================================================
end subroutine

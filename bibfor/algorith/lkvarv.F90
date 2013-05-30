subroutine lkvarv(vintr, nbmat, mater, paravi)
!
    implicit      none
    integer :: nbmat
    real(kind=8) :: paravi(3), mater(nbmat, 2)
! ==================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ==================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE---------------------------
! ==================================================================
! --- BUT : CALCUL DES FONCTIONS D'ECROUISSAGE VISQUEUSE------------
! ==================================================================
! IN  : VINTR    : VARIABLE INTERNE (ICI XIV) ----------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE --------------------
! --- : MATER  : PARAMETRES DU MODELE ------------------------------
! OUT : PARAVI : VARIABLE D'ECROUISSAGE ----------------------------
! ------------ : AXIV, SXIV, MXIV ----------------------------------
! ==================================================================
    real(kind=8) :: m0, a0, s0, avmax, mvmax, svmax, xivmax
    real(kind=8) :: sxiv, axiv, mxiv, xiv
    real(kind=8) :: fact1, vintr
! ==================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE -------------------------
! ==================================================================
    a0 = mater( 8,2)
    s0 = mater(11,2)
    m0 = mater(12,2)
!
!
! ==================================================================
! --- ON IMPOSE A0=1 CAR POUR LA LOI LETK ON MODIFIE LES CRITERES --
! --- VISQUEUX POUR AVOIR UNE DROITE
! ==================================================================
    avmax = 1.d0
    mvmax = mater(19,2)
    svmax = s0
!
    xivmax = mater(20,2)
! ==================================================================
! CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIV < XIVMAX------
! ==================================================================
    xiv = vintr
    if (xiv .lt. xivmax) then
!
        fact1 = xiv/xivmax
!
        axiv = a0 + (avmax - a0)*fact1
!
        sxiv = s0 + (svmax - s0)*fact1
!
        mxiv = m0 + (mvmax - m0)*fact1
! ==================================================================
! CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIV >= XIVMAX   --
! ==================================================================
    else
        axiv = avmax
!
        sxiv = svmax
!
        mxiv = mvmax
! ==================================================================
! CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIE< XIP < XIULT--
! ==================================================================
    endif
! ==================================================================
! --- STOCKAGE -----------------------------------------------------
! ==================================================================
    paravi(1) = axiv
    paravi(2) = sxiv
    paravi(3) = mxiv
! ==================================================================
end subroutine

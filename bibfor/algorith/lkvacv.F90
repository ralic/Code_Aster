subroutine lkvacv(nbmat, mater, paravi, varvi)
!
    implicit      none
    integer :: nbmat
    real(kind=8) :: paravi(3), mater(nbmat, 2), varvi(4)
! ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE-------------------------
! ================================================================
! --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE CRITERE VISQUEUX --
! ================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------
!     : PARAVI : VARIABLE D'ECROUISSAGE VISQUEUSE ----------------
! ------------ : PARAVI(1)=AXIV ----------------------------------
! ------------ : PARAVI(2)=SXIV ----------------------------------
! ------------ : PARAVI(3)=MXIV ----------------------------------
! OUT : VARVI  : AVXIV, BVXIV, DVXIV ----------------------------
! ================================================================
    real(kind=8) :: sigc, gamcjs, h0c
    real(kind=8) :: avxiv, bvxiv, kvxiv, dvxiv
    real(kind=8) :: un, deux, trois, six
! ================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------
! ================================================================
    parameter       ( un     =  1.0d0   )
    parameter       ( deux   =  2.0d0   )
    parameter       ( trois  =  3.0d0   )
    parameter       ( six    =  6.0d0   )
! ================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE -----------------------
! ================================================================
    sigc = mater(3,2)
    gamcjs = mater(5,2)
! ================================================================
!---- CALCUL DE Kd(XIP)-------------------------------------------
! ================================================================
    kvxiv = (deux/trois)**(un/deux/paravi(1))
! ================================================================
!---- CALCUL DE Ad(XIP)-------------------------------------------
! ================================================================
    h0c = (un - gamcjs)**(un/six)
    avxiv = -paravi(3) * kvxiv/sqrt(six)/sigc/h0c
! ================================================================
!---- CALCUL DE Bd(XIP)-------------------------------------------
! ================================================================
    bvxiv = paravi(3) * kvxiv/trois/sigc
! ================================================================
!---- CALCUL DE Dd(XIP)-------------------------------------------
! ================================================================
    dvxiv = paravi(2) * kvxiv
! ================================================================
! --- STOCKAGE ---------------------------------------------------
! ================================================================
    varvi(1) = avxiv
    varvi(2) = bvxiv
    varvi(3) = dvxiv
    varvi(4) = kvxiv
! ================================================================
end subroutine

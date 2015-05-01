subroutine lkvacp(nbmat, mater, paraep, varpl)
!
    implicit      none
    integer :: nbmat
    real(kind=8) :: paraep(3), mater(nbmat, 2), varpl(4)
! ===============================================================
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
! ===============================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE------------------------
! ===============================================================
! --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE ------------------
! ===============================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE -----------------
! --- : MATER  : PARAMETRES DU MODELE ---------------------------
!     : PARAEP : VARIABLE D'ECROUISSAGE -------------------------
! ------------ : PARAEP(1)=AXIP ---------------------------------
! ------------ : PARAEP(2)=SXIP ---------------------------------
! ------------ : PARAEP(3)=MXIP ---------------------------------
! OUT : VARPL  : ADXIP, BDXIP, DDXIP, KDXIP ---------------------
! ===============================================================
    real(kind=8) :: sigc, gamcjs, h0c
    real(kind=8) :: adxip, bdxip, ddxip, kdxip
    real(kind=8) :: un, deux, trois, six
! ===============================================================
! --- INITIALISATION DE PARAMETRES ------------------------------
! ===============================================================
    parameter       ( un     =  1.0d0   )
    parameter       ( deux   =  2.0d0   )
    parameter       ( trois  =  3.0d0   )
    parameter       ( six    =  6.0d0   )
!
! ===============================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ----------------------
! ===============================================================
    sigc = mater(3,2)
    gamcjs = mater(5,2)
! ===============================================================
!---- CALCUL DE Kd(XIP)------------------------------------------
! ===============================================================
    kdxip = (deux/trois)**(un/deux/paraep(1))
! ===============================================================
!---- CALCUL DE Ad(XIP)------------------------------------------
! ===============================================================
    h0c = (un - gamcjs)**(un/six)
    adxip = -paraep(3) * kdxip/sqrt(six)/sigc/h0c
! ===============================================================
!---- CALCUL DE Bd(XIP)------------------------------------------
! ===============================================================
    bdxip = paraep(3) * kdxip/trois/sigc
! ===============================================================
!---- CALCUL DE Dd(XIP)------------------------------------------
! ===============================================================
    ddxip = paraep(2) * kdxip
! ===============================================================
! --- STOCKAGE --------------------------------------------------
! ===============================================================
    varpl(1) = adxip
    varpl(2) = bdxip
    varpl(3) = ddxip
    varpl(4) = kdxip
! ===============================================================
end subroutine

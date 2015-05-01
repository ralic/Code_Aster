subroutine lkdfdx(nbmat, mater, ucrip, invar, s,&
                  paraep, varpl, derpar, dfdxip)
    implicit      none
#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lkhtet.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2)
    real(kind=8) :: ucrip, invar, s(6), paraep(3), varpl(4), derpar(3)
    real(kind=8) :: dfdxip
! ====================================================================
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
! --- BUT : CALCUL DE DF/DSIG ----------------------------------------
! ====================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ----------------------
! --- : MATER  : PARAMETRES DU MODELE --------------------------------
!     : UCRIP  : PARTIE SOUS LA PUISSANCE DANS LE CRITERE ------------
!     : INVAR  : INVARIANT TCONTRAINTES ------------------------------
!     : S      : DEVIATEUR DES CONTRAINTES ---------------------------
!     : PARAEP : VARIABLE D'ECROUISSAGE ------------------------------
! ------------ : PARAEP(1)=AXIP --------------------------------------
! ------------ : PARAEP(2)=SXIP --------------------------------------
! ------------ : PARAEP(3)=MXIP --------------------------------------
!     : VARPL  : VARPL(1) = ADXIP ------------------------------------
!                VARPL(2) = BDXIP ------------------------------------
!                VARPL(3) = DDXIP ------------------------------------
!                VARPL(4) = KDXIP ------------------------------------
!     : DERPAR : DERPAR(1) = DAD -------------------------------------
!                DERPAR(2) = DSD -------------------------------------
!                DERPAR(3) = DMD  ------------------------------------
! OUT : DFDXIP : dF/dXIP ---------------------------------------------
! ====================================================================
    common /tdim/   ndt , ndi
    integer :: ndi, ndt
    real(kind=8) :: pref, sigc, rcos3t, h0c, h0e, htheta
    real(kind=8) :: sii
    real(kind=8) :: un, lgleps, zero
    real(kind=8) :: dfdad, dfdsd, dfdmd
    real(kind=8) :: fact1, fact3, fact4, fact5
!      REAL*8  DEUX,  TROIS
! ====================================================================
! --- INITIALISATION DE PARAMETRES -----------------------------------
! ====================================================================
    parameter       ( zero    =  0.0d0   )
    parameter       ( un      =  1.0d0   )
!      PARAMETER       ( DEUX    =  2.0D0   )
!      PARAMETER       ( TROIS   =  3.0D0   )
    parameter       ( lgleps  =  1.0d-8  )
! ====================================================================
! ====================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ---------------------------
! ====================================================================
    sigc = mater(3,2)
    pref = mater(1,2)
! =================================================================
! --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
! ================================================================
    call lcprsc(s, s, sii)
    sii = sqrt (sii)
! =================================================================
! --- CALCUL DE h(THETA), H0E ET H0C, -----------------------------
! =================================================================
    rcos3t = cos3t (s, pref, lgleps)
    call lkhtet(nbmat, mater, rcos3t, h0e, h0c,&
                htheta)
! =================================================================
! --- CALCUL DE d(F)/d(sd)
! =================================================================
    fact1 = - paraep(1) * varpl(4) * sigc * h0c
    if (ucrip .gt. zero) then
        dfdsd = fact1 * (ucrip)**(paraep(1) - un)
    else
        dfdsd = zero
    endif
! =================================================================
! --- CALCUL DE d(F)/d(md)
! =================================================================
    if (ucrip .gt. zero) then
        fact3 = - paraep(1) * sigc * h0c
        fact4 = varpl(1) * sii * htheta / paraep(3)
        fact5 = varpl(2) * invar / paraep(3)
        dfdmd = fact3 * (fact4 + fact5) * (ucrip)**(paraep(1) - un)
    else
        dfdmd = zero
    endif
! =================================================================
! --- CALCUL DE d(F)/d(ad)
! =================================================================
!      FACT6 = SIGC * H0C * DERPAR(1)
!      FACT7 = PARAEP(2)/DEUX/PARAEP(1)
!      FACT8 = LOG(DEUX/TROIS)*(DEUX/TROIS)**(UN/DEUX/PARAEP(1))
!
!      DFDAD = FACT6*(UCRIP)**PARAEP(1)*
!     &        (LOG(UCRIP)-(FACT7*FACT8/UCRIP))
! VERSION CIH
    if (ucrip .gt. zero) then
        dfdad = - sigc*h0c*log(ucrip/varpl(4))*(ucrip)**paraep(1)
    else
        dfdad = zero
    endif
    dfdxip = derpar(1)*dfdad + derpar(2)*dfdsd + derpar(3)*dfdmd
! =================================================================
!
end subroutine

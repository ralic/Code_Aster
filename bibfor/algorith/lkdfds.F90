subroutine lkdfds(nbmat, mater, s, para, var,&
                  ds2hds, ucri, dfdsig)
    implicit      none
#include "asterc/r8prem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lkhtet.h"
#include "asterfort/r8inir.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2)
    real(kind=8) :: s(6), para(3), var(4), ucri
    real(kind=8) :: ds2hds(6), dfdsig(6)
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
! --- BUT : CALCUL DE DF/DSIG -------------------------------------
! =================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE -------------------
! --- : MATER  : PARAMETRES DU MODELE -----------------------------
!     : S      : TENSEUR DU DEVIATEUR DES CONTRAINTES -------------
!     : PARA   : VARIABLE D'ECROUISSAGE ---------------------------
! ------------ : PARA(1)=AXI --------------------------------------
! ------------ : PARA(2)=SXI --------------------------------------
! ------------ : PARA(3)=MXI --------------------------------------
!     : VAR  : ADXI, BDXI, DDXI -----------------------------------
!     : DS2HDS: d(sII*h(THETA))/dsig ------------------------------
!     : UCRI  : LE TERME SOUS LA PUISSANCE DANS LE CRITERE --------
! OUT : DFDSIG : dF/dsig ------------------------------------------
! =================================================================
    integer :: ndi, ndt, i
    real(kind=8) :: pref, sigc, rcos3t, h0c, h0e, htheta
    real(kind=8) :: zero, un, lgleps
    real(kind=8) :: a(6), kron(6)
    real(kind=8) :: fact1, fact3
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( zero    =  0.0d0   )
    parameter       ( un      =  1.0d0   )
    parameter       ( lgleps  =  1.0d-8  )
! =================================================================
    common /tdim/   ndt , ndi
! =================================================================
    data    kron    /un  ,un  ,un  ,zero  ,zero  ,zero/
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    sigc = mater(3,2)
    pref = mater(1,2)
! =================================================================
! --- CALCUL DE h(THETA), H0E ET H0C, -----------------------------
! =================================================================
    rcos3t = cos3t (s, pref, lgleps)
    call lkhtet(nbmat, mater, rcos3t, h0e, h0c,&
                htheta)
! =================================================================
! --- CALCUL DES TERMES INTERMEDIARES
! =================================================================
    fact1 = para(1) * sigc * h0c
    fact3 = para(1) - un
! =================================================================
! --- RESULTAT FINAL
! =================================================================
    call r8inir(6, 0.d0, a, 1)
    call r8inir(6, 0.d0, dfdsig, 1)
!
    do 10 i = 1, ndt
        a(i) = var(1) * ds2hds(i) + var(2)* kron (i)
10  end do
!
    do 20 i = 1, ndt
        if (ucri .le. r8prem()) then
            dfdsig(i) = ds2hds(i)
        else
            dfdsig(i) = ds2hds(i) - fact1*((ucri)**fact3)*a(i)
        endif
20  end do
! =================================================================
end subroutine

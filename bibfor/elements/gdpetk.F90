subroutine gdpetk(tetag, tetapg, petikm, petik)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LE
!           VECTEUR-COURBURE AUX POINTS DE GAUSS, EN AXES GENERAUX,
!           VECTEUR DENOMME 'PETIT K'.
!
!     IN  : TETAG     : VECTEUR-INCREMENT DE ROTATION A GAUCHE
!           TETAPG    : DERIVEE DE TETAG PAR RAPPORT A L'ABS. CURVILIGNE
!           PETIKM    : VECTEUR-COURBURE A L'ITERATION PRECEDENTE
!
!     OUT : PETIK     : VECTEUR-COURBURE ACTUEL
! ------------------------------------------------------------------
    implicit none
    include 'asterfort/antisy.h'
    include 'asterfort/axial.h'
    include 'asterfort/marota.h'
    include 'asterfort/promat.h'
    include 'asterfort/provec.h'
    include 'asterfort/transp.h'
    include 'blas/ddot.h'
    real(kind=8) :: tetag(3), tetapg(3), petikm(3), petik(3), petik1(3)
    real(kind=8) :: petik2(3), v1(3), amat1(3, 3), amat2(3, 3), amat3(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: coef1, coef2, coef3, demi, epsil, prosca
    real(kind=8) :: teta1, teta2, un
!-----------------------------------------------------------------------
    epsil = 1.d-8
    demi = 5.d-1
    un = 1.d0
!
!*** PETIK1: VECTEUR BETA (SIMO: 'A THREE-DIMENSIONAL FINITE-STRAIN ROD
!***                       MODEL-PART 2'.)
    teta2=ddot(3,tetag,1,tetag,1)
    if (abs(teta2) .lt. epsil) goto 11
    teta1 = sqrt (teta2)
    call provec(tetag, tetapg, v1)
    coef1 = sin(teta1) / teta1
    prosca=ddot(3,tetag,1,tetapg,1)
    coef2 = (un-coef1) * prosca / teta2
    coef3 = demi * (sin(demi*teta1)/(demi*teta1))**2
    do 2 i = 1, 3
        petik1(i) = coef1*tetapg(i) + coef2*tetag(i) + coef3*v1(i)
 2  end do
    goto 20
!
!*** TETAG EST TRES PETIT ET BETA VAUT PRATIQUEMENT TETAPRIM
11  continue
    do 12 i = 1, 3
        petik1(i) = tetapg(i)
12  end do
!
20  continue
!
!
    call marota(tetag, amat1)
    call antisy(petikm, un, amat2)
    call promat(amat1, 3, 3, 3, amat2,&
                3, 3, 3, amat3)
    call transp(amat1, 3, 3, 3, amat2,&
                3)
    call promat(amat3, 3, 3, 3, amat2,&
                3, 3, 3, amat1)
    call axial(amat1, petik2)
!
    do 32 i = 1, 3
        petik(i) = petik1(i) + petik2(i)
32  end do
end subroutine

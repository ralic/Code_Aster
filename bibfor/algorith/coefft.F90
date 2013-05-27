subroutine coefft(cothe, coeff, dcothe, dcoeff, x,&
                  dtime, coeft, nmat, coel)
    implicit none
!       ===============================================================
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
!       ---------------------------------------------------------------
!       INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE PAR
!       UNE METHODE DE RUNGE KUTTA AVEC REDECOUPAGE AUTOMATIQUE DU PAS
!       DE TEMPS : CALCUL DES PARAMETRES MATERIAU A UN INSTANT DONNE
!       ---------------------------------------------------------------
!       IN  COTHE  :  COEFFICIENTS MATERIAU ELASTIQUE A T
!           COEFF  :  COEFFICIENTS MATERIAU INELASTIQUE A T
!           DCOTHE :  INTERVALLE COEFFICIENTS MATERIAU ELAST POUR DT
!           DCOEFF :  INTERVALLE COEFFICIENTS MATERIAU INELAST POUR DT
!           X      :  INSTANT COURANT
!           DTIME  :  INTERVALLE DE TEMPS
!           NMAT   :  NOMNRE MAXI DE COEF MATERIAU
!       OUT COEFT  :  COEFFICIENTS MATERIAU INELASTIQUE A T+DT
!C          COEL   :  COEFFICIENTS  ELASTIQUES ELASTIQUE A T+DT
!       ---------------------------------------------------------------
    include 'asterfort/r8inir.h'
    integer :: nmat, i, ncoe, ncoel
    real(kind=8) :: cothe(nmat), dcothe(nmat), coel(nmat)
    real(kind=8) :: hsdt, dtime, x
    real(kind=8) :: coeff(nmat), dcoeff(nmat), coeft(nmat)
!
    hsdt=x/dtime
!
    call r8inir(nmat, 0.d0, coel, 1)
!
    if (cothe(nmat) .eq. 0) then
        do 12 i = 1, 3
            coel(i)=cothe(i)+hsdt*dcothe(i)
12      continue
        coel(nmat)=0.d0
    else if (cothe(nmat).eq.1) then
!          MATERIAU ISOTROPE, OU ANISOTROPE, MATRICE DE HOOKE
        coel(nmat)=1.d0
        ncoel=75
        do 11 i = 1, ncoel
            coel(i)=cothe(i)+hsdt*dcothe(i)
11      continue
    endif
!
!       POUR GAGNER DU TEMPS CPU
    if (coeff(nmat) .eq. 0) then
        ncoe=nmat
    else
        ncoe=nint(coeff(nmat))
    endif
!
    do 10 i = 1, ncoe
        coeft(i)=coeff(i)+hsdt*dcoeff(i)
10  continue
!
end subroutine

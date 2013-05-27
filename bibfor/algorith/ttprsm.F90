subroutine ttprsm(ndim, ddeple, ddeplm, dlagrf, coeffr,&
                  tau1, tau2, mprojt, inadh, rese,&
                  nrese, coeffp, lpenaf, dvitet)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/assert.h'
    integer :: ndim
    real(kind=8) :: ddeple(3), ddeplm(3), dlagrf(2)
    real(kind=8) :: coeffr, coeffp
    real(kind=8) :: tau1(3), tau2(3), mprojt(3, 3)
    integer :: inadh
    real(kind=8) :: rese(3), nrese
    logical :: lpenaf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! ETAT D'ADHERENCE DU POINT DE CONTACT
!
! ----------------------------------------------------------------------
!
!
!  CALCUL DE P_B(0,1)(LAMDBA+ RHO C [[DELTA X]])
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DDEPLE : INCREMENT DEPDEL DU DEPL. DU POINT DE CONTACT
! IN  DDEPLM : INCREMENT DEPDEL DU DEPL. DU PROJETE DU POINT DE CONTACT
! IN  COEFFR : COEF_REGU_FROT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT INADH  : INDICE D'ADHERENCE
!               1 - ADHERENT
!               0 - GLISSANT
! OUT RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFFR*VITESSE
! OUT NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! OUT DVITET : VITESSE DE GLISSEMENT
!
! ----------------------------------------------------------------------
!
    integer :: i, k
    real(kind=8) :: dvite(3), dvitet(3)
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    nrese = 0.d0
    do 10 i = 1, 3
        rese(i) = 0.d0
        dvitet(i) = 0.d0
10  end do
!
! --- CALCUL DU SAUT DE "VITESSE" [[DELTA X]]
!
    do 12 i = 1, ndim
        dvite(i) = ddeple(i) - ddeplm(i)
12  end do
!
! --- PROJECTION DU SAUT SUR LE PLAN TANGENT
!
    do 21 i = 1, ndim
        do 22 k = 1, ndim
            dvitet(i) = mprojt(i,k)*dvite(k)+dvitet(i)
22      continue
21  end do
!
! --- SEMI-MULTIPLICATEUR DE FROTTEMENT RESE
!
    if (lpenaf) then
        do 32 i = 1, 3
            rese(i) = coeffp*dvitet(i)
32      continue
    else
        if (ndim .eq. 2) then
            do 30 i = 1, 2
                rese(i) = dlagrf(1)*tau1(i)+coeffr*dvitet(i)
30          continue
        else if (ndim.eq.3) then
            do 31 i = 1, 3
                rese(i) = dlagrf(1)*tau1(i)+ dlagrf(2)*tau2(i)+ coeffr*dvitet(i)
31          continue
        else
            call assert(.false.)
        endif
    endif
!
! -- CALCUL DU COEF D'ADHERENCE
!
    do 40 i = 1, 3
        nrese = rese(i)*rese(i) + nrese
40  end do
    nrese = sqrt(nrese)
!
! --- ON TESTE SI    NRESE < 1 OU NON
! --- SI OUI ADHERENCE
!
    if (nrese .le. 1.d0) then
        inadh = 1
    else
        inadh = 0
    endif
!
end subroutine

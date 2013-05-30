subroutine crirup(fami, imat, ndim, npg, lgpg,&
                  option, compor, sigp, vip, vim,&
                  instam, instap)
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
    implicit none
!
!
! SOUS ROUTINE PERMETTANT L ELVALUATION DU CRITERE
! METHODE MISE EN OEUVRE : MOYENNE SUR LES POINTS DE GAUSS PUIS
! EVALUATION DE LA CONTRAINTE PRINCIPALE MAXIMALE
! ------------------------------------------------------------------
! IN  FAMI  :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!    IMAT   :  ADRESSE DU MATERIAU CODE
!    NDIM   :  DIMENSION DE L'ESPACE
!    NPG    :  NOMBRE DE POINTS DE GAUSS
!    LGPG   :  NOMBRE TOTAL DE VI APRES AJOUT DES VI RUPTURE
!   OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG ,FULL_MECA ,RAPH_MECA
!   COMPOR  : CARTE DECRIVANT LES PARAMETRES K16 DU COMPORTEMENT
!     VIM   : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
!    INSTAM : INSTANT PRECEDENT
! OUT  SIGP : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
!     VIP   : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
!    INSTAP : INSTANT DE CALCUL
!-------------------------------------------------------------------
! DECLALRATION DES VARIABLES UTILES
    include 'asterc/r8prem.h'
    include 'asterfort/fgequi.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    integer :: npg, kpg, i, ndim, lgpg, imat, ivp, cerr
!
    real(kind=8) :: sigmoy(6), sigp(2*ndim, npg), equi(20), sc, prin1
    real(kind=8) :: vip(lgpg, npg), vim(lgpg, npg), pm, pp, dp, instam, instap
    real(kind=8) :: dt
!
    character(len=*) :: fami
    character(len=16) :: option, compor(*)
! ---------------------------------------------------------------------
!
! CALCUL DU TENSEUR DES CONTRAINTES MOYEN PUIS DIAGONALISATION
    if (option(1:9) .ne. 'FULL_MECA' .and. option(1:9) .ne. 'RAPH_MECA') then
        goto 999
    endif
!
    call rcvalb(fami, 1, 1, '+', imat,&
                ' ', 'CRIT_RUPT', 0, ' ', 0.d0,&
                1, 'SIGM_C', sc, cerr, 1)
    call r8inir(6, 0.d0, sigmoy, 1)
!
!     CALCUL DU TENSEUR MOYEN
    do 200 i = 1, 2*ndim
        do 300 kpg = 1, npg
            sigmoy(i)=sigmoy(i)+sigp(i,kpg)/npg
300      continue
200  end do
!
!     EVALUATION DE LA CONTRAINTE PRINCIPALE MAXIMALE
    call fgequi(sigmoy, 'SIGM', ndim, equi)
    prin1=max(equi(3),equi(4))
    prin1=max(equi(5),prin1)
!
! CALCUL DE P MOYEN
    pp=0
    pm=0
    if (ndim .eq. 2) then
        ivp=9
    else
        ivp=13
    endif
    do 400 kpg = 1, npg
        pm=pm+vim(ivp,kpg)/npg
        pp=pp+vip(ivp,kpg)/npg
400  end do
    dp=pp-pm
    dt=instap-instam
!
!     EVALUATION DE LA VITESSE DE DEFORMATION PLASTIQUE : DP/DT
!     ET DE DIFFERENTES ENERGIES :
!     V(LGPG-4) : ENERGIE DISSIPEE, DP*SIGMOY_EG
!     V(LGPG-3) : ENREGIE DISSIPEE CUMULEE A CHAQUE PAS,
!     V(LGPG-2) : PUISSANCE DISSIPEE, DP/DT*SIGMOY_EG
!     V(LGPG-1) : PUISSANCE DISSIPEE CUMULEE A CHAQUE PAS,
    do 500 kpg = 1, npg
        vip(lgpg-5,kpg)=dp/dt
        vip(lgpg-4,kpg)=dp*equi(1)
        vip(lgpg-3,kpg)=dp*equi(1)+vim(lgpg-3,kpg)
        vip(lgpg-2,kpg)=dp*equi(1)/dt
        vip(lgpg-1,kpg)=dp*equi(1)/dt+vim(lgpg-1,kpg)
500  end do
!
!     CRITERE DE RUPTURE
    if (prin1 .gt. sc) then
! LA CONTRAINTE PRINCIPALE MAXI DEPASSE LE SEUIL
        do 600 kpg = 1, npg
            vip(lgpg,kpg)=1.d0
600      continue
    else if (abs(vim(lgpg,1)-1.d0).lt.r8prem()) then
! LA MAILLE ETAIT DEJA CASSEE. ELLE LE RESTE
        do 601 kpg = 1, npg
            vip(lgpg,kpg)=1.d0
601      continue
    else
! MAILLE SAINE
        do 602 kpg = 1, npg
            vip(lgpg,kpg)=0.d0
602      continue
    endif
!
999  continue
end subroutine

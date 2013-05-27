subroutine lcumef(option, dep, depm, an, bn,&
                  cn, epsm, epsrm, epsrp, depsi,&
                  epsfm, sigi, nstrs, sigt)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ROUTINE APPELE DANS FLU
! LCUMEF     SOURCE    BENBOU   01/03/26
!
!_______________________________________________________________________
!
! ROUTINE QUI CALCUL L INCREMENT DE CONTRAINTES (ELASTIQUE)
!  CORRIGE PAR LE FLUAGE TOTAL (PROPRE + DESSICCATION)
!
! IN  DEP      : MATRICE ELASTIQUE DE HOOKE A L'INSTANT T+DT
! IN  DEPM     : MATRICE ELASTIQUE DE HOOKE A L'INSTANT T
! IN  AN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE (CF LCUMMD)
! IN  BN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE (CF LCUMMD)
! IN  CN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE (CF LCUMMD)
! IN  EPSM     : DEFORMATION TEMPS MOINS
! IN  EPSRM    : DEFORMATION DE RETRAIT TEMPS MOINS
! IN  EPSRP    : DEFORMATION DE RETRAIT TEMPS PLUS
! IN  EPSFM    : DEFORMATION DE FLUAGE TEMPS MOINS
! IN  DEPSI    : INCREMENT DE DEFORMATION TOTALE
! IN  SIGI     : CONTRAINTES INITIALES
! IN  NSTRS    : DIMENSION DES VECTEURS CONTRAINTE ET DEFORMATION
! OUT SIGT     : CONTRAINTES AU TEMPS PLUS
!_______________________________________________________________________
!
    implicit none
! MODIFI DU 18 DECEMBRE 2002 - YLP SUPPRESSION DE LA DECLARATION DE
! LA VARIABLE ISING APPELEE DANS INVERMAT (RESORBEE PAR MGAUSS)
    include 'asterfort/lcinve.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/r8inir.h'
    integer :: i, j, k, nstrs
! MODIFI DU 6 JANVIER 2003 - YLP SUPPRESSION DES DECLARATIONS
! IMPLICITES DE TABLEAUX
!      REAL*8   AN(NSTRS),BN(NSTRS,NSTRS),CN(NSTRS,NSTRS)
!      REAL*8   DEP(NSTRS,NSTRS),SIGI(NSTRS),DSIGT(NSTRS),DEPSI(NSTRS)
    real(kind=8) :: an(6), bn(6, 6), cn(6, 6)
    real(kind=8) :: dep(6, 6), sigi(6), sigt(6), depsi(6), epsm(6), epsfm(6)
! MODIFI DU 18 AOUT 2004 - YLP AJOUT DES VARIABLES DE
!                              DEFORMATION DE RETRAIT
    real(kind=8) :: epsrm, epsrp
! MODIFI DU 6 JANVIER 2002 - YLP SUPPRESSION DES DECLARATIONS
! IMPLICITES DES TABLEAUX
!      REAL*8   EFLU(NSTRS,NSTRS),DEFELA(NSTRS),TEMP(NSTRS,NSTRS)
!      REAL*8   DEFLUN(NSTRS),TEMP2(NSTRS,NSTRS)
    real(kind=8) :: temp(6, 6)
    real(kind=8) :: deflun(6), temp2(6, 6), temp3(6, 6), rtemp(6)
! MODIFI DU 18 AOUT 2004 - YLP AJOUT DE LA VARIABLE KRON
    real(kind=8) :: kron(6), bnsigi(6), depsc(6)
    real(kind=8) :: depsr(6), depm(6, 6), epsm2(6)
    character(len=16) :: option(2)
!-----------------------------------------------------------------------
    integer :: iret
    real(kind=8) :: det
!-----------------------------------------------------------------------
    data     kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! INITIALISATION DES VARIABLES
!
    do 11 i = 1, nstrs
        sigt(i) = 0.d0
        deflun(i) = 0.d0
        do 12 j = 1, nstrs
            temp(i,j) = 0d0
12      continue
11  end do
!
    do 5 i = 1, nstrs
        do 5 j = 1, nstrs
            do 5 k = 1, nstrs
                temp(i,j) = temp(i,j) + cn(i,k) * dep(k,j)
 5          continue
!
! CONSTRUCTION DE LA MATRICE D ELASTICITE CORRIGE PAR LE FLUAGE
!
!
!  EQUATION : (3.1-2)
!
!
!
    do 10 i = 1, nstrs
        do 66 j = 1, nstrs
            temp2(i,j) = temp(i,j)
66      continue
        temp2(i,i) = 1.d0 + temp2(i,i)
10  continue
! EN COMMENTAIRES CI-DESSOUS, L'ANCIENNE VERSION UTILISANT UNE ROUTINE
! D'INVERSION FOURNIE AVEC LE PAQUET DES SOURCES INITIALES. ELLE A ETE
! DEBRANCHEE AU PROFIT DE LA ROUTINE MGAUSS PRE-EXISTANT DANS CODE_ASTER
!        CALL LCUMIN(TEMP,NSTRS,ISING)
!
    do 16 i = 1, nstrs
        do 15 j = 1, nstrs
            if (i .eq. j) then
                temp3(i,j) = 1.d0
            else
                temp3(i,j) = 0.d0
            endif
15      continue
16  continue
!
!
! MODIFI DU 6 JANVIER - YLP
!       CALL MGAUSS(TEMP, TEMP2, NSTRS, NSTRS, NSTRS, ZERO, LGTEST)
!        CALL MGAUSS(TEMP2, TEMP3, 6, NSTRS, NSTRS, ZERO, LGTEST)
    call mgauss('NFVP', temp2, temp3, 6, nstrs,&
                nstrs, det, iret)
!
    if ((option(2).eq.'MAZARS') .or. (option(2).eq.'ENDO_ISOT_BETON')) then
!
        call r8inir(6, 0.d0, rtemp, 1)
        do 17 i = 1, nstrs
            rtemp(i)=an(i)
            do 17 j = 1, nstrs
! MODIFI DU 18 AOUT 2004 - YLP AJOUT DE LA CORRECTION DES
! DEFORMATIONS TOTALES PAR LES DEFORMATIONS DE RETRAIT
!             RTEMP(I) = RTEMP(I)+BN(I,J)*SIGI(J)+
!     &                           TEMP(I,J)*(EPSM(J)+DEPSI(J)-EPSFM(J))
                rtemp(i) = rtemp(i)+bn(i,j)*sigi(j)+ temp(i,j)*(epsm( j)+depsi(j)-epsfm(j) -epsrm&
                           &*kron(j))
17          continue
!
! CALCUL DE (1 + CN)-1 * E0
!
        do 20 i = 1, nstrs
            do 20 j = 1, nstrs
                deflun(i)=deflun(i)+temp3(i,j)*rtemp(j)
20          continue
!
        do 60 i = 1, nstrs
            do 60 j = 1, nstrs
! MODIFI DU 18 AOUT 2004 - YLP AJOUT DE LA CORRECTION DES
! DEFORMATIONS TOTALES PAR LES DEFORMATIONS DE RETRAIT
!          SIGT(I) = SIGT(I) +
!     &               DEP(I,J) * (EPSM(J)+DEPSI(J)-EPSFM(J)-DEFLUN(J))
                sigt(i) = sigt(i) + dep(i,j) * (epsm(j)+depsi(j)- epsrp*kron(j) -epsfm(j)-deflun(&
                          &j))
60          continue
!
    else
! --- MODELE BETON_UMLV_FP SEUL - ECRITURE EN INCREMENTALE
! --- CONSTRUCTION VECTEUR DEFORMATION (RETRAIT + THERMIQUE)
        call lcinve(0.d0, depsr)
        do 110 i = 1, nstrs
            depsr(i) = kron(i)*(epsrp-epsrm)
110      continue
! --- CALCUL DE BN:SIGI = BNSIGI -> TENSEUR ORDRE 2
        call lcprmv(bn, sigi, bnsigi)
! --- CALCUL DE SIGT
        do 120 i = 1, nstrs
            depsc(i) = depsi(i)-an(i)-bnsigi(i)-depsr(i)
120      continue
! --- PRODUIT MATRICE*VECTEUR : (E(T+))*DEPSC
        call lcprmv(dep, depsc, epsm2)
!
! --- PRODUIT MATRICE*VECTEUR : (E(T+))*(E(T-))^(-1)*SIGI
        do 130 i = 1, nstrs
            do 140 j = 1, nstrs
                if (i .eq. j) then
                    temp2(i,j) = 1.d0
                else
                    temp2(i,j) = 0.d0
                endif
140          continue
130      continue
!
        call mgauss('NFVP', depm, temp2, 6, nstrs,&
                    nstrs, det, iret)
        call lcprmm(temp2, dep, temp)
        call lcprmv(temp, sigi, rtemp)
!
        do 150 i = 1, nstrs
            epsm2(i) = epsm2(i) + rtemp(i)
150      continue
        call lcprmv(temp3, epsm2, sigt)
    endif
!
end subroutine

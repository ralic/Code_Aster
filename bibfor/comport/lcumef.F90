subroutine lcumef(rela_plas, dep, depm, an, bn,&
                  cn, epsm, epsrm, epsrp, depsi,&
                  epsfm, sigi, nstrs, sigt)
!
implicit none
!
#include "asterfort/lcinve.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprmv.h"
#include "asterfort/mgauss.h"
#include "asterfort/r8inir.h"
!
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
!
    character(len=16), intent(in) :: rela_plas


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
    integer :: i, j, k, nstrs
    real(kind=8) :: an(6), bn(6, 6), cn(6, 6)
    real(kind=8) :: dep(6, 6), sigi(6), sigt(6), depsi(6), epsm(6), epsfm(6)
    real(kind=8) :: epsrm, epsrp
    real(kind=8) :: temp(6, 6)
    real(kind=8) :: deflun(6), temp2(6, 6), temp3(6, 6), rtemp(6)
    real(kind=8) :: bnsigi(6), depsc(6)
    real(kind=8) :: depsr(6), depm(6, 6), epsm2(6)
    integer :: iret
    real(kind=8) :: det
    real(kind=8), parameter :: kron(6) = (/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
!
! INITIALISATION DES VARIABLES
!
    do i = 1, nstrs
        sigt(i) = 0.d0
        deflun(i) = 0.d0
        do j = 1, nstrs
            temp(i,j) = 0d0
        end do
    end do
!
    do i = 1, nstrs
        do j = 1, nstrs
            do k = 1, nstrs
                temp(i,j) = temp(i,j) + cn(i,k) * dep(k,j)
            end do
        end do
    end do
!
! CONSTRUCTION DE LA MATRICE D ELASTICITE CORRIGE PAR LE FLUAGE
!
!
!  EQUATION : (3.1-2)
!
!
!
    do i = 1, nstrs
        do j = 1, nstrs
            temp2(i,j) = temp(i,j)
        end do
        temp2(i,i) = 1.d0 + temp2(i,i)
    end do
! EN COMMENTAIRES CI-DESSOUS, L'ANCIENNE VERSION UTILISANT UNE ROUTINE
! D'INVERSION FOURNIE AVEC LE PAQUET DES SOURCES INITIALES. ELLE A ETE
! DEBRANCHEE AU PROFIT DE LA ROUTINE MGAUSS PRE-EXISTANT DANS CODE_ASTER
!        CALL LCUMIN(TEMP,NSTRS,ISING)
!
    do i = 1, nstrs
        do j = 1, nstrs
            if (i .eq. j) then
                temp3(i,j) = 1.d0
            else
                temp3(i,j) = 0.d0
            endif
        end do
    end do
    call mgauss('NFVP', temp2, temp3, 6, nstrs,&
                nstrs, det, iret)
!
    if ((rela_plas.eq.'MAZARS') .or. (rela_plas.eq.'ENDO_ISOT_BETON')) then
        call r8inir(6, 0.d0, rtemp, 1)
        do i = 1, nstrs
            rtemp(i)=an(i)
            do j = 1, nstrs
                rtemp(i) = rtemp(i)+bn(i,j)*sigi(j)+&
                           temp(i,j)*(epsm( j)+depsi(j)-epsfm(j) -epsrm*kron(j))
            end do
        end do
!
! CALCUL DE (1 + CN)-1 * E0
!
        do i = 1, nstrs
            do j = 1, nstrs
                deflun(i)=deflun(i)+temp3(i,j)*rtemp(j)
            end do
        end do
!
        do i = 1, nstrs
            do j = 1, nstrs
                sigt(i) = sigt(i) + dep(i,j) * (epsm(j)+depsi(j)- epsrp*kron(j) -epsfm(j)-deflun(j))
            end do
        end do
    else
! --- MODELE BETON_UMLV_FP SEUL - ECRITURE EN INCREMENTALE
! --- CONSTRUCTION VECTEUR DEFORMATION (RETRAIT + THERMIQUE)
        call lcinve(0.d0, depsr)
        do i = 1, nstrs
            depsr(i) = kron(i)*(epsrp-epsrm)
        end do
! --- CALCUL DE BN:SIGI = BNSIGI -> TENSEUR ORDRE 2
        call lcprmv(bn, sigi, bnsigi)
! --- CALCUL DE SIGT
        do i = 1, nstrs
            depsc(i) = depsi(i)-an(i)-bnsigi(i)-depsr(i)
        end do
! --- PRODUIT MATRICE*VECTEUR : (E(T+))*DEPSC
        call lcprmv(dep, depsc, epsm2)
!
! --- PRODUIT MATRICE*VECTEUR : (E(T+))*(E(T-))^(-1)*SIGI
        do i = 1, nstrs
            do j = 1, nstrs
                if (i .eq. j) then
                    temp2(i,j) = 1.d0
                else
                    temp2(i,j) = 0.d0
                endif
            end do
        end do
!
        call mgauss('NFVP', depm, temp2, 6, nstrs,&
                    nstrs, det, iret)
        call lcprmm(temp2, dep, temp)
        call lcprmv(temp, sigi, rtemp)
!
        do i = 1, nstrs
            epsm2(i) = epsm2(i) + rtemp(i)
        end do
        call lcprmv(temp3, epsm2, sigt)
    endif
!
end subroutine

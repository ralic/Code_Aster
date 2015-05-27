subroutine glrc_integ_loc(lambda, deuxmu, seuil, alf,&
                          alfmc, gmt, gmc, cof1,&
                          vim, q2d, qff, tr2d, eps33,&
                          de33d1, de33d2, ksi2d, dksi1, dksi2,&
                          da1, da2, kdmax, told, codret,&
                          emp)
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
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "asterfort/glrc_calc_eps33.h"
    integer :: kdmax, codret
    real(kind=8) :: vim(*), gmt, gmc, tr2d, eps33
    real(kind=8) :: lambda, deuxmu, seuil, alf, qff(2), told
    real(kind=8) :: de33d1, de33d2, ksi2d, dksi1, dksi2, da1, da2, treps, treps2
    real(kind=8) :: cof1(2), q2d(2)
!
! ----------------------------------------------------------------------
!
!      CALCUL D'ENDOMMAGEMENT (DA1 ET DA2) ET DE LA COMPOSANTE DE
!       DEFORMATION EPS33, APPELE PAR "LCGLDM" (LOI GLRC_DM)
!
! IN:
!       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
!       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
!       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!       SEUIL   : INITIAL MEMBRANE
!       ALF     : PARAMETRE DE SEUIL FLEXION
!       VIM     : VARIABLES INTERNES EN T-
!       Q2D     : PARTIE CONSTANTE DU RESIDU MEMBRANE
!       QFF(2)  : PARTIE CONSTANTE DU RESIDU FLEXION
!       TR2D    : EPS11 + EPS22
!       COF1    : PARAMETRE
! OUT:
!       DA1     : ENDOMMAGEMENT SUR LA PARTIE 1 DE LA PLAQUE
!       DA2     : ENDOMMAGEMENT SUR LA PARTIE 2 DE LA PLAQUE
!       KSI2D   : VALEUR DE LA FONCTION CARACTERISTIQUE DE L'ENDOM.
!       EPS33   : COMPOSANTE 33 DE LA DEFORMATION
!       DE33D1  : DERIVEE DE EPS33 PAR RAPPORT A DA1
!       DE33D2  : DERIVEE DE EPS33 PAR RAPPORT A DA2
!       ELAS    : .TRUE. SI ELASTIQUE
!       ELAS1   : .TRUE. SI DA1 == VIM(1)
!       ELAS2   : .TRUE. SI DA2 == VIM(2)
!       CODRET  : CODE RETOUR DE L'INTEGRATION INTEGRATION DU
!                 0 => PAS DE PROBLEME
!                 1 => ABSENCE DE CONVERGENCE
! ----------------------------------------------------------------------
!
    aster_logical :: lconv1, lconv2
!
    integer :: i
!
    real(kind=8) :: qm1, qm2
    real(kind=8) :: rd1, rd2, dr1d, dr2d, dd1, dd2, seuilr
    real(kind=8) :: alfmc, emp(2), cof2(2), dq2d(2)
!
    call glrc_calc_eps33(lambda, deuxmu, alfmc, gmt, gmc,&
                         tr2d, da1, da2, eps33, de33d1,&
                         de33d2, ksi2d, dksi1, dksi2, cof1,&
                         q2d, emp, cof2, dq2d)
!
    treps = tr2d + eps33
    treps2 = treps**2
!
!----------CONTRIBUTION DE MEMBRANE-----
    qm1 = 0.5d0*cof1(1)*treps2+q2d(1)
    qm2 = 0.5d0*cof1(2)*treps2+q2d(2)
    rd1 = qm1/(1.0d0 + da1)**2 - seuil
    rd2 = qm2/(1.0d0 + da2)**2 - seuil
!
!----CONTRIBUTION DES COURBURES---------
    rd1 = rd1 + qff(1)/(alf + da1)**2
    rd2 = rd2 + qff(2)/(alf + da2)**2
!
    seuilr = max(seuil,1.0d-6)
!
!-----VERIFIER SI LE SEUIL EST ATTEINT
    lconv1 = rd1 .lt. 0.0d0
    lconv2 = rd2 .lt. 0.0d0
!
    if ((.not. lconv1 .and. da1.ge.vim(1)) .or. (.not. lconv2 .and. da2.ge.vim(2))) then
!
        do i = 1, kdmax
!
            dd1 = 0.0d0
            dd2 = 0.0d0
!
            if (rd1 .ge. 0.0d0) then
                dr1d = (cof1(1)*treps*de33d1+0.5d0*cof2(1)*treps2+ dq2d(1))/(1.d0 + da1)**2&
                      - 2.d0*qm1 /(1.d0 + da1)**3 - 2.d0*qff(1)/(alf + da1)**3
                if (abs(dr1d) .lt. 1.0d-14) then
                    dd1 = 0.0d0
                else
                    dd1 = - rd1/dr1d
                endif
            endif
!
            if (rd2 .ge. 0.0d0) then
                dr2d = (cof1(2)*treps*de33d2+0.5d0*cof2(2)*treps2+ dq2d(2)) /(1.d0 + da2)**2&
                      - 2.d0*qm2 /(1.d0 + da2)**3 - 2.d0*qff(2)/(alf + da2)**3
!
                if (abs(dr2d) .lt. 1.0d-14) then
                    dd2 = 0.0d0
                else
                    dd2 = - rd2/dr2d
                endif
            endif
!
            if (((abs(dd1*rd1) .lt. told*seuilr) .or. ((rd1 .lt. 0.0d0 .and. da1 .le. vim(1))))&
                .and.&
                ((abs(dd2*rd2) .lt. told* seuilr) .or. ((rd2 .lt. 0.0d0 .and. da2 .le. vim(2)))&
                )) goto 10
!
            da1 = da1 + dd1
            da2 = da2 + dd2
!
            if (da1 .lt. 0.0d0 .and. rd1 .lt. 0.0d0) da1 = vim(1)
            if (da2 .lt. 0.0d0 .and. rd2 .lt. 0.0d0) da2 = vim(2)
!
            call glrc_calc_eps33(lambda, deuxmu, alfmc, gmt, gmc,&
                                 tr2d, da1, da2, eps33, de33d1,&
                                 de33d2, ksi2d, dksi1, dksi2, cof1,&
                                 q2d, emp, cof2, dq2d)
!
            treps = tr2d + eps33
            treps2 = treps**2
!
!----------CONTRIBUTION DE MEMBRANE-----
            qm1 = 0.5d0*cof1(1)*treps2+q2d(1)
            qm2 = 0.5d0*cof1(2)*treps2+q2d(2)
            rd1 = qm1/(1.0d0 + da1)**2 - seuil
            rd2 = qm2/(1.0d0 + da2)**2 - seuil
!
!----CONTRIBUTION DES COURBURES---------
            rd1 = rd1 + qff(1)/(alf + da1)**2
            rd2 = rd2 + qff(2)/(alf + da2)**2
!
        enddo
!
!    NON CONVERGENCE POUR LE NOMBRE MAXIMAL D ITERATION PRESCRIT
        codret = 1
!
 10     continue
    endif
end subroutine

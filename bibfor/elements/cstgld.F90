subroutine cstgld(lamf, muf, alf, gf, emp,&
                  efp, qff)
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
!
    implicit none
    real(kind=8) :: lamf, muf, alf, gf, trot, trot2
    real(kind=8) :: emp(2), efp(2)
!
!----------------------------------------------------------------------
!        CALCUL DES CONSTANTES INDEPENDANTES DE DA1, DA2 ET EPS33
!
!     IN :
!         LAMBDA : PARAMETRE D ELASTICITE - MEMBRANE
!         MU     : PARAMETRE D ELASTICITE - MEMBRANE
!         LAMF   : PARAMETRE D ELASTICITE - FLEXION
!         MUF    : PARAMETRE D ELASTICITE - FLEXION
!         ALF    : PARAMETRE DE SEUIL FLEXION
!         GF     : PARAMETRE GAMMA POUR LA FLEXION
!         GMT    : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!         GMC    : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!         DELTA  : PARAMETRE DE COUPLAGE MEMBRANE-FLEXION
!         TROT   : TRACE DE KAPPA (FLEXION)
!         EMP(2) : VALEURS PROPRES DE EPS 2D
!         EFP(2) : VALEURS PROPRES DE KAPPA
!
!     OUT :
!         QFF(2) : TF
!         QM     : TM
!         COF1   : INTERMEDIAIRE DE CALCUL
!         Q2D    : INTERMEDIAIRE DE CALCUL
!         GI(2)  : PARTIE DE LA DERIVEE DE KSI(EMP) PAR RAPPORT A DA
!         GTR2   : INTERMEDIAIRE DE CALCUL
!         GI(2)  : INTERMEDIAIRE DE CALCUL
!----------------------------------------------------------------------
!
    integer :: k
    real(kind=8) :: qff(2), gf1, gf2
!
!-- ICI ON SUPPOSE QUE GF1=GF2, CE QUI N EST PAS NECESSAIRE
    gf1 = gf
    gf2 = gf
!
    trot = efp(1) + efp(2)
    trot2 = trot**2
!
! -------- CALCUL DE QFF --------------------
!
    if (trot .gt. 0.0d0) then
        qff(1) = 0.0d0
        qff(2) = 0.5d0*lamf*trot2
    else
        qff(1) = 0.5d0*lamf*trot2
        qff(2) = 0.0d0
    endif
!
    do 4510, k = 1,2
    if (efp(k) .gt. 0.0d0) then
        qff(2) = qff(2) + muf*efp(k)**2
    else
        qff(1) = qff(1) + muf*efp(k)**2
    endif
    4510 end do
!
    qff(1) = alf*qff(1)*(1.0d0 - gf1)
    qff(2) = alf*qff(2)*(1.0d0 - gf2)
!
end subroutine

subroutine cjsmat(mod, imat, tempf, materf, ndt,&
                  ndi, nvi, nivcjs)
    implicit none
!
!       ================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!       CJS        : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
!                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
!                    MATER(*,1) = E , NU , ALPHA
!                    MATER(*,2) = BETA_CJS, RM, N_CJS, KP, RC, A_CJS,
!                                 B_CJS, C_CJS , GAMMA_CJS, MU_CJS,
!                                 PCO, PA
!                    VARIABLES INTERNES : Q, R, X, SIGNE, ETAT
!               ( SIGNE = SIGNE(S:DEPSDP) )
!                (ETAT: ELASTIC = 0, ISOTRO = 1, DEVIAT = 2, ISODEV = 3)
!       ----------------------------------------------------------------
!       IN  IMAT   :  ADRESSE DU MATERIAU CODE
!           MOD    :  TYPE DE MODELISATION
!           TEMPF  :  TEMPERATURE  A T+DT
!       OUT MATERF :  COEFFICIENTS MATERIAU A T+DT
!                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
!                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NVI    :  NB DE VARIABLES INTERNES
!           NIVCJS :  NIVEAU 1, 2 OU 3 DE LA LOI CJS
!       ----------------------------------------------------------------
    include 'asterfort/cjsnvi.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nvi, imat
    real(kind=8) :: materf(14, 2), tempf
    character(len=8) :: mod, nomc(17)
    character(len=4) :: nivcjs
    integer :: cerr(17)
!       ----------------------------------------------------------------
!
! -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
    call cjsnvi(mod, ndt, ndi, nvi)
!
! -     RECUPERATION MATERIAU -----------------------------------------
!
!
    nomc(1) = 'E        '
    nomc(2) = 'NU       '
    nomc(3) = 'ALPHA    '
    nomc(4) = 'BETA_CJS '
    nomc(5) = 'RM       '
    nomc(6) = 'N_CJS    '
    nomc(7) = 'KP       '
    nomc(8) = 'RC       '
    nomc(9) = 'A_CJS    '
    nomc(10)= 'B_CJS    '
    nomc(11)= 'C_CJS    '
    nomc(12)= 'GAMMA_CJS'
    nomc(13)= 'MU_CJS   '
    nomc(14)= 'PCO      '
    nomc(15)= 'PA       '
    nomc(16)= 'Q_INIT   '
    nomc(17)= 'R_INIT   '
!
!
! -     RECUPERATION MATERIAU A TEMPF (T+DT)
!
    call rcvala(imat, ' ', 'ELAS', 1, 'TEMP',&
                tempf, 3, nomc(1), materf(1, 1), cerr(1),&
                0)
    if (cerr(3) .ne. 0) materf(3,1) = 0.d0
    call rcvala(imat, ' ', 'CJS', 1, 'TEMP',&
                tempf, 12, nomc(4), materf(1, 2), cerr(4),&
                2)
    call rcvala(imat, ' ', 'CJS', 1, 'TEMP',&
                tempf, 2, nomc(16), materf(13, 2), cerr(16),&
                0)
    if (cerr(16) .eq. 1) then
        materf(13,2) = 0.d0
    endif
    if (cerr(17) .eq. 1) then
        materf(14,2) = 0.d0
    endif
!
    if (materf(3,2) .eq. 0.d0) then
        nivcjs='CJS1'
! - POUR CJS1, PAR DEFAUT RC=RM/2
!   ET POUR EVITER DES NAN DANS LES LOG ON PREND PC0 = 1.
!
        materf(5,2) = materf(2,2) / 2.d0
        materf(11,2) = 1.d0
!
    else if (materf(3,2) .ne. 0.d0 .and. materf(6,2) .ne. 0.d0) then
        nivcjs='CJS2'
! - POUR CJS2  POUR EVITER DES NAN DANS LES LOG
!   ON PREND PC0 = 1
        materf(11,2) = 1.d0
!
    else if (materf(3,2) .ne. 0.d0 .and. materf(6,2) .eq. 0.d0) then
        nivcjs='CJS3'
!
    else
        call u2mess('F', 'ALGORITH2_16')
    endif
!
end subroutine

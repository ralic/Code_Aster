subroutine rsvmat(fami, kpg, ksp, mod, imat,&
                  nmat, materd, materf, matcst, ndt,&
                  ndi, nr, nvi, vind)
    implicit none
!       ================================================================
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
!       ----------------------------------------------------------------
!       ROUSS_VISC : RECUPERATION DU MATERIAU A TEMPD ET TEMPF
!                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
!                    MATER(*,1) = E , NU , ALPHA
!                    MATER(*,2) = D , SIG1 , PORO_INIT, PORO_CRIT
!                                            PORO_ACCE, PORO_LIMI
!                                            D_SIGM_EPSI_NORM, BETA
!                    VARIABLES INTERNES : P , B , E
!       ----------------------------------------------------------------
!       IN  IMAT   :  ADRESSE DU MATERIAU CODE
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION  DE MATER
!           TEMPD  :  TEMPERATURE  A T
!           TEMPF  :  TEMPERATURE  A T+DT
!       OUT MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
!                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rslnvi.h"
#include "asterfort/utmess.h"
    integer :: i, imat, nmat, ndt, ndi, nr, nvi
    integer :: jprol, jvale, nbvale, kpg, ksp, iret
!
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), tempd, tempf
    real(kind=8) :: epsi, vind(nvi), f0
    real(kind=8) :: resu
!
    character(len=8) :: mod, nomc(16), type
    integer :: cerr(16)
    character(len=3) :: matcst
    character(len=*) :: fami
!
    data epsi       /1.d-15/
!       ----------------------------------------------------------------
!
! -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
    call rslnvi(mod, ndt, ndi, nr, nvi)
!
! -   RECUPERATION MATERIAU ------------------------------------------
!
!
    nomc(1) = 'E        '
    nomc(2) = 'NU       '
    nomc(3) = 'ALPHA    '
    nomc(4) = 'B_ENDOGE'
    nomc(5) = 'K_DESSIC'
    nomc(6) = 'D        '
    nomc(7) = 'SIGM_1   '
    nomc(8) = 'PORO_INIT'
    nomc(9) = 'PORO_CRIT'
    nomc(10)= 'PORO_ACCE'
    nomc(11)= 'PORO_LIMI'
    nomc(12)= 'D_SIGM_EPSI_NORM'
    nomc(13)= 'BETA'
    nomc(14)= 'SIGM_0'
    nomc(15)= 'EPSI_0'
    nomc(16)= 'M'
!
! -     RECUPERATION MATERIAU A TEMPD (T)
!
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                5, nomc(1), materd(1, 1), cerr(1), 0)
    if (cerr(3) .ne. 0) materd(3,1) = 0.d0
    if (cerr(4) .ne. 0) materd(4,1) = 0.d0
    if (cerr(5) .ne. 0) materd(5,1) = 0.d0
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ROUSSELIER', 0, ' ', [0.d0],&
                8, nomc(6), materd(1, 2), cerr(6), 2)
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'VISC_SINH', 0, ' ', [0.d0],&
                3, nomc(14), materd(9, 2), cerr(14), 2)
!
!         RECUPERATION DE E(TEMPD) VIA LES COURBES DE TRACTION MONOTONES
!         SIG = F(EPS,TEMPD) ENTREES POINT PAR POINT  (MOT CLE TRACTION)
!         > ECRASEMENT DU E RECUPERE PAR MOT CLE ELAS
!
    call rcvarc(' ', 'TEMP', '-', fami, kpg, ksp, tempd, iret)
    call rctype(imat, 1, 'TEMP', [tempd], resu, type)
    if ((type.eq.'TEMP') .and. (iret.eq.1)) then
        call utmess('F', 'CALCULEL_31')
    endif
    call rctrac(imat, 1, 'SIGM', resu, jprol,&
                jvale, nbvale, materd(1, 1))
!
! -     RECUPERATION MATERIAU A TEMPF (T+DT)
!
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                5, nomc(1), materf(1, 1), cerr(1), 0)
    if (cerr(3) .ne. 0) materf(3,1) = 0.d0
    if (cerr(4) .ne. 0) materf(4,1) = 0.d0
    if (cerr(5) .ne. 0) materf(5,1) = 0.d0
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'ROUSSELIER', 0, ' ', [0.d0],&
                8, nomc(6), materf(1, 2), cerr(6), 2)
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'VISC_SINH', 0, ' ', [0.d0],&
                3, nomc(14), materf(9, 2), cerr(14), 2)
!
!         RECUPERATION DE E(TEMPF) VIA LES COURBES DE TRACTION MONOTONES
!         SIG = F(EPS,TEMP) ENTREES POINT PAR POINT  (MOT CLE TRACTION)
!         > ECRASEMENT DU E RECUPERE PAR MOT CLE ELAS
!
    call rcvarc(' ', 'TEMP', '+', fami, kpg, ksp, tempf, iret)
    call rctype(imat, 1, 'TEMP', [tempf], resu, type)
    if ((type.eq.'TEMP') .and. (iret.eq.1)) then
        call utmess('F', 'CALCULEL_31')
    endif
    call rctrac(imat, 1, 'SIGM', resu, jprol,&
                jvale, nbvale, materf(1, 1))
!
! -     MATERIAU CONSTANT ? ------------------------------------------
!
!       PRINT * ,'MATERD = ',MATERD,'MATERF = ',MATERF
    matcst = 'OUI'
    do 30 i = 1, 5
        if (abs ( materd(i,1) - materf(i,1) ) .gt. epsi) then
            matcst = 'NON'
            goto 50
        endif
30  continue
    do 40 i = 1, 10
        if (abs ( materd(i,2) - materf(i,2) ) .gt. epsi) then
            matcst = 'NON'
            goto 50
        endif
40  continue
!
! ---- INITIALISATION DE LA POROSITE INITIALE -------------------------
50  continue
    if (vind(2) .eq. 0.d0) then
        f0 = materf(3,2)
        vind(2) = f0
        if (f0 .lt. 0.d0) then
            call utmess('F', 'ALGORITH10_52')
        else if (f0.ge.1.d0) then
            call utmess('F', 'ALGORITH10_50')
        endif
    endif
!
! ----ET C EST TOUT ---------
end subroutine

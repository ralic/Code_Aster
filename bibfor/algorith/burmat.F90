subroutine burmat(fami, kpg, ksp, mod, imat,&
                  nmat, materd, materf, matcst, ndt,&
                  ndi, nr, nvi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: alexandre.foucault at edf.fr
!       ----------------------------------------------------------------
!       RECUPERATION DU MATERIAU A TEMPF ET TEMPD
!       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!           MOD    :  TYPE DE MODELISATION
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION 1 DE MATER
!       OUT MATERD :  COEFFICIENTS MATERIAU A T    (TEMPD )
!           MATERF :  COEFFICIENTS MATERIAU A T+DT (TEMPF )
!                     MATER(*,I) = CARACTERISTIQUES MATERIAU
!                                    I = 1  CARACTERISTIQUES ELASTIQUES
!                                    I = 2  CARACTERISTIQUES VISQUEUSES
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON OU 'NAP' SI NAPPE DANS 'VECMAT.F'
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
!
!     NOM                         a t-                 a t+ (t-+dt)
!     -------------------------------------------------------------
!     E                           MATERD(1,1)          MATERF(1,1)
!     NU                          MATERD(2,1)          MATERF(2,1)
!     ALPHA                       MATERD(3,1)          MATERF(3,1)
!     BENDO                       MATERD(4,1)          MATERF(4,1)
!     KDESS                       MATERD(5,1)          MATERF(5,1)
!     HYGRO                       MATERD(6,1)          MATERF(6,1)
!
!     KRS                         MATERD(1,2)          MATERF(1,2)
!     ETARS                       MATERD(2,2)          MATERF(2,2)
!     ETAIS                       MATERD(3,2)          MATERF(3,2)
!     KRD                         MATERD(4,2)          MATERF(4,2)
!     ETARD                       MATERD(5,2)          MATERF(5,2)
!     ETAID                       MATERD(6,2)          MATERF(6,2)
!     KAPPA                       MATERD(7,2)          MATERF(7,2)
!     QSR_K                       MATERD(8,2)          MATERF(8,2)
!     TEMP_0_C                    MATERD(9,2)          MATERF(9,2)
!     ETAFD                       MATERD(10,2)         MATERF(10,2)
!     ----------------------------------------------------------------
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/burnvi.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
#include "asterfort/rcvarc.h"
    integer :: kpg, ksp, imat, nmat, ndt, ndi, nr, nvi
   integer :: cerr(16), ii,IRET
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: epsi,td,tf,tempm
    character(len=3) :: matcst
    character(len=*) :: fami
    character(len=8) :: mod
    character(len=16) :: nomc(16)
!
! === ============================================
!
!     RECUPERATION PROPRIETES MATERIAU ELASTIQUE
!
! === ============================================
!
    nomc(1) = 'E        '
    nomc(2) = 'NU       '
    nomc(3) = 'ALPHA    '
    nomc(4) = 'B_ENDOGE'
    nomc(5) = 'K_DESSIC'
!
! === ===========
!     INSTANT T-
! === ===========
!
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                5, nomc(1), materd(1, 1), cerr(1), 0)
    if (cerr(3) .ne. 0) materd(3,1) = 0.d0
    if (cerr(4) .ne. 0) materd(4,1) = 0.d0
    if (cerr(5) .ne. 0) materd(5,1) = 0.d0
!
! === ===========
!     INSTANT T+
! === ===========
!
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                5, nomc(1), materf(1, 1), cerr(1), 0)
    if (cerr(3) .ne. 0) materf(3,1) = 0.d0
    if (cerr(4) .ne. 0) materf(4,1) = 0.d0
    if (cerr(5) .ne. 0) materf(5,1) = 0.d0
!
! === =============================================
!
!     RECUPERATION PROPRIETES MATERIAU HYGROMETRIE
!
! === =============================================
!
    nomc(6)='FONC_DESORP'
!
! === ===========
!     INSTANT T-
! === ===========
!
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomc(6), materd(6, 1), cerr(6), 0)
    if (cerr(6) .ne. 0) then
        call utmess('F', 'ALGORITH4_94')
    endif
!
! === ===========
!     INSTANT T+
! === ===========
!
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomc(6), materf(6, 1), cerr(6), 0)
    if (cerr(6) .ne. 0) then
        call utmess('F', 'ALGORITH4_94')
    endif
!
! === ===============================================
!
!     RECUPERATION PROPRIETES MATERIAU FLUAGE PROPRE
!
! === ===============================================
!
    nomc(7)='K_RS'
    nomc(8)='ETA_RS'
    nomc(9)='ETA_IS'
    nomc(10)='K_RD'
    nomc(11)='ETA_RD'
    nomc(12)='ETA_ID'
    nomc(13)='KAPPA'
    nomc(14)='QSR_K'
    nomc(15)='TEMP_0_C'



!
! === =================
!     INSTANT T- ET T+
! === =================
!

    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'BETON_BURGER', 0, ' ', [0.d0],&
                9, nomc(7), materd(1, 2), cerr(7), 0)
!
!   teste sur la valeur de QSR_K
    if (int(materd(8,2)) .ne. int(0.)) then
        call rcvarc(' ','TEMP','-',FAMI,KPG,KSP,TD,IRET)
        call rcvarc(' ','TEMP','+',FAMI,KPG,KSP,TF,IRET)
        tempm=(td+tf)/2.
        do 10 ii = 1, 6
            materf(ii,2) = materd(ii,2)*exp(materd(8,2)*&
            (1./(tempm+273.)-1./(materd(9,2)+273.)))
10      end do
!       expression pour kappa differente des autres parametres 
        materf(7,2) = materd(7,2)/(exp(materd(8,2)*&
            (1./(tempm+273.)-1./(materd(9,2)+273.))))
    else
        do 20 ii = 1, 7
            materf(ii,2) = materd(ii,2)
20      end do
    endif
!
        materf(8,2)=materd(8,2)
        materf(9,2)=materd(9,2)

!
! === ===================================================
!
!     RECUPERATION PROPRIETES MATERIAU FLUAGE DESSICATION
!
! === ===================================================
!
    nomc(16)='ETA_FD'
!
! === =================
!     INSTANT T- ET T+
! === =================
!
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'BETON_BURGER', 0, ' ', [0.d0],&
                1, nomc(16), materd(10, 2), cerr(16), 0)
    if (cerr(16) .ne. 0) then
        materd(10,2) = -1.d0
        materf(10,2) = -1.d0
    else
        materf(10,2) = materd(10,2)
    endif
!
!
! --- MATERIAU CONSTANT?
!
!
    matcst = 'OUI'
    epsi=r8prem()
    do 30 ii = 1, nmat
        if (abs(materd(ii,1)-materf(ii,1) ) .gt. epsi*materd(ii,1)) then
            matcst = 'NON'
            goto 9999
        endif
30  end do
    do 40 ii = 1, nmat
        if (abs(materd(ii,2)-materf(ii,2) ) .gt. epsi*materd(ii,2)) then
            matcst = 'NON'
            goto 9999
        endif
40  end do
!
9999  continue
!
! === ===================================================
!
!     RECUPERATION NOMBRE DE COMPOSANTES DES CONTRAINTES
!                  NOMBRE DE VARIABLES INTERNES
!                  NOMBRE D'INCONNUES OBTENUES PAR NEWTON
!
! === ===================================================
    call burnvi(mod, ndt, ndi, nr, nvi)
!
end subroutine

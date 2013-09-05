subroutine lklmat(mod, imat, nbmat, tempd, materd,&
                  materf, matcst, ndt, ndi, nvi,&
                  indal)
!
    implicit     none
#include "asterfort/lklnvi.h"
#include "asterfort/matini.h"
#include "asterfort/rcvala.h"
#include "asterfort/u2mess.h"
    integer :: ndt, ndi, nvi, imat, nbmat
    real(kind=8) :: materd(nbmat, 2), materf(nbmat, 2), tempd
    character(len=3) :: matcst
    character(len=8) :: mod
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! |---------------------------------------------------------------|
! |-- BUT : RECUPERATION DES DONNEES MATERIAU POUR LA LOI DE -----|
! |------ : COMPORTEMENT LETK VISCOPLASTIQUE(MECANIQUE DES ROCHES)|
! |---------------------------------------------------------------|
! |----- NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES ------|
! |----- MATER(*,1) = E, NU, MU, K -------------------------------|
! |----- MATER(*,2) = GAMMA_ULT, GAMMA_E, M_ULT, M_E, A_E, -------|
! |---------------- : M_PIC, A_PIC, ETA, SIGMA_C, GAMMA, ---------|
! |---------------- : KSI, GAMMA_CJS, SIGMA_P1, SIGMA_P2, PA -----|
! |----- VARIABLE INTERNE : GAMMA_P, EPS_P-  ---------------------|
! |---------------------------------------------------------------|
! =================================================================
! IN  : MOD    : TYPE DE MODELISATION -----------------------------
! --- : IMAT   : ADRESSE DU MATERIAU CODE -------------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : TEMPD  : TEMPERATURE BIDON --------------------------------
! OUT : MATERD : COEFFICIENTS MATERIAU A T ------------------------
! --- : MATERF : COEFFICIENTS MATERIAU A T+DT ---------------------
! ------------ : MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ------------ : MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : MATCST : 'OUI' --------------------------------------------
! --- : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR -----------
! --- : NDI    : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR --------
! --- : NVI    : NB DE VARIABLES INTERNES -------------------------
! --- : INDAL  : INDICATEUR SUR ALPHA
! =================================================================
    integer :: ii, indal
    real(kind=8) :: e, nu, mu, k
    real(kind=8) :: zero, un, deux, trois
    real(kind=8) :: mu0v, xi0v, s0, a0, var1, var2
    real(kind=8) :: mpic, apic, sigmp1, sigc, me, ae, cohere
    integer :: cerr(32)
    character(len=8) :: nomc(32)
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( zero   =  0.0d0  )
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
! =================================================================
! --- NB DE COMPOSANTES / VARIABLES INTERNES ----------------------
! =================================================================
    call lklnvi(mod, ndt, ndi, nvi)
! =================================================================
! --- DEFINITION DES CHAMPS ---------------------------------------
! =================================================================
    nomc(1) = 'E        '
    nomc(2) = 'NU       '
    nomc(3) = 'ALPHA    '
    nomc(4) = 'PA       '
    nomc(5) = 'NELAS    '
    nomc(6) = 'SIGMA_C  '
    nomc(7) = 'H0_EXT   '
    nomc(8) = 'GAMMA_CJS'
    nomc(9) = 'XAMS     '
    nomc(10) = 'ETA      '
    nomc(11) = 'A_0      '
    nomc(12) = 'A_E      '
    nomc(13) = 'A_PIC    '
    nomc(14) = 'S_0      '
    nomc(15) = 'M_0      '
    nomc(16) = 'M_E      '
    nomc(17) = 'M_PIC    '
    nomc(18) = 'M_ULT    '
    nomc(19) = 'XI_ULT   '
    nomc(20) = 'XI_E     '
    nomc(21) = 'XI_PIC   '
    nomc(22) = 'MV_MAX   '
    nomc(23) = 'XIV_MAX'
    nomc(24) = 'A        '
    nomc(25) = 'N        '
    nomc(26) = 'SIGMA_P1 '
    nomc(27) = 'MU0_V    '
    nomc(28) = 'XI0_V    '
    nomc(29) = 'MU1      '
    nomc(30) = 'XI1      '
!
    call matini(nbmat, 2, 0.d0, materd)
!
! =================================================================
! --- RECUPERATION DES PARAMETRES MATERIAU ------------------------
! =================================================================
    call rcvala(imat, ' ', 'ELAS', 1, 'TEMP',&
                [tempd], 3, nomc(1), materd(1, 1), cerr(1), 0)
    indal=1
    if (cerr(3) .ne. 0) indal=0
!
    call rcvala(imat, ' ', 'LETK', 1, 'TEMP',&
                [tempd], 27, nomc(4), materd(1, 2), cerr(4), 0)
! =================================================================
! - CALCUL DES MODULES DE CISAILLEMENT ET DE DEFORMATION VOLUMIQUE-
! =================================================================
    e = materd(1,1)
    nu = materd(2,1)
    mu = e / (deux*(un+nu))
    k = e / (trois*(un-deux*nu))
! =================================================================
! --- STOCKAGE DES MODULES CALCULES COMME PARAMETRES MATERIAU -----
! =================================================================
    materd(4,1) = mu
    materd(5,1) = k
! =================================================================
! - VERIFICATIONS -------------------------------------------------
! =================================================================
    mu0v = materd(24,2)
    xi0v = materd(25,2)
    s0 = materd(11,2)
    a0 = materd(8,2)
    if (s0 .eq. zero) call u2mess('F', 'COMPOR1_26')
    if (mu0v .eq. xi0v) call u2mess('F', 'COMPOR1_26')
!
    var1 = un/(s0**a0)
    var2 = (un+mu0v)/(mu0v-xi0v)
!
    if ((mu0v.gt.xi0v) .and. (var1) .gt. (var2)) then
        call u2mess('F', 'COMPOR1_26')
    endif
! =================================================================
! --- VERIFICATION DE LA COHERENCE DES PARAMETRES : ---------------
! --- SIGMA_C, SIGMA_P1, M_PIC, A_PIC, A_E ET M_E -----------------
! =================================================================
    mpic = materd(14,2)
    apic = materd(10,2)
    sigmp1 = materd(23,2)
    sigc = materd( 3,2)
    me = materd(13,2)
    ae = materd( 9,2)
    cohere =&
     &        abs(sigc/sigmp1*((mpic*sigmp1/sigc+1)**(apic/ae))-me)
    if (cohere .gt. 1.0d-2) then
        call u2mess('F', 'ALGORITH5_12')
    endif
! =================================================================
! --- DEFINITION D'UN MATERIAU FINAL ------------------------------
! =================================================================
    do 10 ii = 1, nbmat
        materf(ii,1) = materd(ii,1)
        materf(ii,2) = materd(ii,2)
10  end do
    matcst = 'OUI'
! =================================================================
end subroutine

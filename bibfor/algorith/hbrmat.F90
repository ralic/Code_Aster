subroutine hbrmat(mod, imat, nbmat, tempd, materd,&
                  materf, matcst, ndt, ndi, nr,&
                  nvi)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit     none
    include 'asterfort/rcvala.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nvi, nr, imat, nbmat
    real(kind=8) :: materf(nbmat, 2), materd(nbmat, 2), tempd
    character(len=3) :: matcst
    character(len=8) :: mod
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU POUR LA LOI DE HOEK BROWN ------
! |--------------------------------------------------------------------|
! |----- NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES -----------|
! |----- MATER(*,1) = E, NU, ALPHA, MU, K -----------------------------|
! |----- MATER(*,2) = GRUP, GRES, SEND --------------------------------|
! |---------------- : SRUP, MEND, MRUP, BETA, ALPHAHB,  ---------------|
! |---------------- : PPHI1, BRES, AP, DP, CP, SIGBD, PPHI2, PPHI0 ----|
! |----- VARIABLE INTERNE : GAMMAP-, EPSP-  ---------------------------|
! |--------------------------------------------------------------------|
! ======================================================================
! IN  IMAT   :  ADRESSE DU MATERIAU CODE -------------------------------
!     NBMAT  :  NOMBRE DE PARAMETRES MATERIAU --------------------------
!     TEMPD  :  TEMPERATURE BIDON --------------------------------------
!     MOD    :  TYPE DE MODELISATION -----------------------------------
! OUT MATERD :  COEFFICIENTS MATERIAU A T ------------------------------
!     MATERF :  COEFFICIENTS MATERIAU A T+DT ---------------------------
!               MATER(*,1) = CARACTERISTIQUES   ELASTIQUES -------------
!               MATER(*,2) = CARACTERISTIQUES   PLASTIQUES -------------
!     MATCST : 'OUI' ---------------------------------------------------
!     NDT    :  NB TOTAL DE COMPOSANTES TENSEURS -----------------------
!     NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS -------------------
!     NR     :  NOMBRE D'EQUATION DU SYSTEME NL  -----------------------
!     NVI    :  NB DE VARIABLES INTERNES -------------------------------
! ======================================================================
    real(kind=8) :: e, nu, mu, k, mrup, srup, alpha
    real(kind=8) :: ap, dp, cp, sigbd, bres, gres, grup
    real(kind=8) :: un, deux, eps
    real(kind=8) :: cohere
    integer :: cerr(14)
    character(len=8) :: nomc(14)
    integer :: ii
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( eps    =  1.0d-6  )
! =================================================================
    do 12 ii = 1, nbmat
        materd(ii,1) = 0.d0
        materd(ii,2) = 0.d0
        materf(ii,1) = 0.d0
        materf(ii,2) = 0.d0
12  end do
! =================================================================
! --- DEFINITION DES CHAMPS ---------------------------------------
! =================================================================
    nomc(1) = 'E         '
    nomc(2) = 'NU        '
    nomc(3) = 'ALPHA     '
    nomc(4) = 'GAMMA_RUP '
    nomc(5) = 'GAMMA_RES '
    nomc(6) = 'S_END     '
    nomc(7) = 'S_RUP     '
    nomc(8) = 'M_END     '
    nomc(9) = 'M_RUP     '
    nomc(10) = 'BETA      '
    nomc(11) = 'ALPHAHB   '
    nomc(12) = 'PHI_RUP   '
    nomc(13) = 'PHI_RES   '
    nomc(14) = 'PHI_END   '
! =================================================================
! --- RECUPERATION DES PARAMETRES MATERIAU ------------------------
! =================================================================
    materf(3,1) = 0.0d0
    materf(11,2) = 0.0d0
    call rcvala(imat, ' ', 'ELAS', 0, ' ',&
                0.d0, 2, nomc(1), materf(1, 1), cerr(1),&
                1)
    call rcvala(imat, ' ', 'ELAS', 0, ' ',&
                0.d0, 1, nomc(3), materf(3, 1), cerr(3),&
                0)
    call rcvala(imat, ' ', 'HOEK_BROWN', 0, ' ',&
                0.d0, 10, nomc(4), materf(1, 2), cerr(4),&
                1)
    call rcvala(imat, ' ', 'HOEK_BROWN', 0, ' ',&
                0.d0, 1, nomc(14), materf(11, 2), cerr(14),&
                0)
! =================================================================
! - CALCUL DES MODULES DE CISAILLEMENT ET DE DEFORMATION VOLUMIQUE-
! =================================================================
    e = materf(1,1)
    nu = materf(2,1)
    mu = e / (deux*(un+nu))
    k = e / (3.0d0*(un-deux*nu))
! =================================================================
! --- STOCKAGE DES PARAMETRES ELASTIQUES CALCULES -----------------
! =================================================================
    materf(4,1) = mu
    materf(5,1) = k
! =================================================================
! - CALCUL DES COEFFICIENTS PARABOLIQUES ET SIGMABD --------------
! =================================================================
    materf(15,2) = materf(10,2)
    materf(16,2) = materf(11,2)
    grup = materf(1,2)
    gres = materf(2,2)
    mrup = materf(6,2)
    srup = materf(4,2)
    alpha = materf(8,2)
    bres = materf(7,2) - sqrt(srup)
    ap = -bres / (grup - gres)**2
    dp = deux*bres*gres / (grup - gres)**2
    cp = bres*grup*(grup-deux*gres) / (grup - gres)**2
    cohere = ap*grup**2+dp*grup+cp
    if (abs(cohere) .gt. eps) then
        call u2mess('F', 'ALGORITH3_90')
    endif
    cohere = ap*gres**2+dp*gres+cp
    if (abs(cohere-bres) .gt. eps) then
        call u2mess('F', 'ALGORITH3_90')
    endif
    sigbd = ( (mrup) +sqrt((mrup)**2 + 4.0d0*((un-alpha)**2)*srup)) / (deux*(un-alpha)**2 )
!  =================================================================
! --- STOCKAGE DES PARAMETRES PLASTIQUES CALCULES -----------------
! =================================================================
    materf(10,2) = bres
    materf(11,2) = ap
    materf(12,2) = dp
    materf(13,2) = cp
    materf(14,2) = sigbd
    do 10 ii = 1, nbmat
        materd(ii,1) = materf(ii,1)
        materd(ii,2) = materf(ii,2)
10  end do
    matcst = 'OUI'
! ======================================================================
! --- NOMBRE DE COMPOSANTES --------------------------------------------
! ======================================================================
    if (mod(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
    else if ((mod(1:6).eq.'D_PLAN') .or. (mod(1:4).eq.'AXIS')) then
        ndt = 4
        ndi = 3
    else if ((mod(1:6).eq.'C_PLAN') .or. (mod(1:2).eq.'1D')) then
        call u2mess('F', 'ALGORITH3_92')
    else
        call u2mess('F', 'ALGORITH2_20')
    endif
! ======================================================================
! --- NOMBRE DE VARIABLES INTERNES -------------------------------------
! ======================================================================
    nvi = 3
! =================================================================
! - NOMBRE DE CONDITIONS NON-LINEAIRES ----------------------------
! =================================================================
    nr = ndt + 3
! ======================================================================
end subroutine

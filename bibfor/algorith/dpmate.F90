subroutine dpmate(mod, imat, materf, ndt, ndi,&
                  nvi, typedp)
! ======================================================================
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
    include 'asterc/r8vide.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nvi, imat, typedp
    real(kind=8) :: materf(5, 2)
    character(len=8) :: mod
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU POUR LA LOI DE DRUCKER PRAGER --
! ======================================================================
!       IN  IMAT   :  ADRESSE DU MATERIAU CODE
!       OUT MATERF :  COEFFICIENTS MATERIAU A T+DT
!                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
!                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
! ======================================================================
    real(kind=8) :: trois, deux, un, six, alpha, sy, syult, c, a, phi
    real(kind=8) :: typed, tabtmp(4), coe, dilat, psi
    integer :: icodre(8)
    character(len=8) :: nomc(8)
! ======================================================================
    parameter ( six    =  6.0d0 )
    parameter ( trois  =  3.0d0 )
    parameter ( deux   =  2.0d0 )
    parameter ( un     =  1.0d0 )
! ======================================================================
! --- DEFINITION PARAMETRES MATERIAU ELAS ------------------------------
! ======================================================================
    nomc(1) = 'E'
    nomc(2) = 'NU'
    nomc(3) = 'ALPHA'
! ======================================================================
! --- RECUPERATION DONNEES MATERIAU ELAS -------------------------------
! ======================================================================
    materf(3,1) = 0.0d0
    call rcvala(imat, ' ', 'ELAS', 0, ' ',&
                0.d0, 2, nomc(1), materf(1, 1), icodre,&
                1)
    call rcvala(imat, ' ', 'ELAS', 0, ' ',&
                0.d0, 1, nomc(3), materf(3, 1), icodre,&
                0)
! ======================================================================
! --- DEFINITION PARAMETRES MATERIAU DRUCKER ---------------------------
! ======================================================================
    nomc(4) = 'ALPHA'
    nomc(5) = 'SY'
    nomc(6) = 'P_ULTM'
! ======================================================================
! --- RECUPERATION MATERIAU SUIVANT LE TYPE D ECROUISSAGE --------------
! ======================================================================
    typed = r8vide()
    call rcvala(imat, ' ', 'DRUCK_PRAGER', 0, ' ',&
                0.d0, 1, 'TYPE_DP', typed, icodre,&
                0)
    if (typed .eq. 1.0d0) then
! ======================================================================
! --- CAS LINEAIRE -----------------------------------------------------
! ======================================================================
        typedp = 1
        nomc(7) = 'H'
        call rcvala(imat, ' ', 'DRUCK_PRAGER', 0, ' ',&
                    0.d0, 4, nomc(4), tabtmp(1), icodre,&
                    1)
! ======================================================================
! --- POUR DES COMMODITES DE PROGRAMMATION ON DEFINIT LES PARAMETRES ---
! --- MATERIAU DE LA FACON SUIVANTE ------------------------------------
! ======================================================================
        materf(1,2) = tabtmp(2)
        materf(2,2) = tabtmp(4)
        materf(3,2) = tabtmp(1)
        materf(4,2) = tabtmp(3)
        coe = materf(2,2) + trois * materf(1,1) * ( un/deux/(un+ materf(2,1)) + materf(3,2)*mater&
              &f(3,2)/(un-deux*materf(2,1)))
        if (coe .le. 0.0d0) then
            call u2mess('F', 'ALGORITH3_37')
        endif
    else if (typed.eq.2.0d0) then
! ======================================================================
! --- CAS PARABOLIQUE --------------------------------------------------
! ======================================================================
        typedp = 2
        nomc(7) = 'SY_ULTM'
        call rcvala(imat, ' ', 'DRUCK_PRAGER', 0, ' ',&
                    0.d0, 4, nomc(4), tabtmp(1), icodre,&
                    1)
! ======================================================================
! --- POUR DES COMMODITES DE PROGRAMMATION ON DEFINIT LES PARAMETRES ---
! --- MATERIAU DE LA FACON SUIVANTE ------------------------------------
! ======================================================================
        alpha = tabtmp(1)
        sy = tabtmp(2)
        syult = tabtmp(4)
        phi = atan2 ( (trois*alpha / deux / sqrt(( deux*alpha + 1.0d0 )*(1.0d0-alpha))), 1.0d0 )
!           PHI   = ASIN ( TROIS * ALPHA / ( DEUX + ALPHA ) )
        c = ( trois - sin(phi) ) * sy / six / cos(phi)
        a = sqrt ( syult / sy )
        materf(1,2) = a
        materf(2,2) = phi
        materf(3,2) = c
        materf(4,2) = tabtmp(3)
        nomc(8) = 'DILAT'
        call rcvala(imat, ' ', 'DRUCK_PRAGER', 0, ' ',&
                    0.d0, 1, nomc(8), dilat, icodre,&
                    1)
        psi = atan2 ( (trois*dilat / deux / sqrt(( deux*dilat + 1.0d0 )*(1.0d0-dilat))), 1.0d0 )
        materf(5,2) = psi
    endif
! ======================================================================
! --- NOMBRE DE COMPOSANTES --------------------------------------------
! ======================================================================
    if (mod(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
    else if ((mod(1:6).eq.'D_PLAN') .or. (mod(1:4).eq.'AXIS')) then
        ndt = 4
        ndi = 3
    endif
! ======================================================================
! --- NOMBRE DE VARIABLES INTERNES -------------------------------------
! ======================================================================
    nvi = 3
! ======================================================================
end subroutine

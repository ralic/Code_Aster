subroutine inithm(imate, yachai, yamec, phi0, em,&
                  alpha0, k0, cs, biot, t,&
                  epsv, depsv, epsvm)
    implicit      none
    include 'asterfort/rcvala.h'
    logical :: yachai
    integer :: imate, yamec
    real(kind=8) :: phi0, em, alpha0, k0, cs, biot, epsvm, epsv, depsv
! ======================================================================
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
! --- RECUPERATION DE VALEURS MECANIQUES -------------------------------
! --- SI PAS MECA ALORS ON POSE BIOT = PHI0 POUR ANNULER LES TERMES ----
! --- DANS LES FORMULATIONS AVEC MECA. IDEM POUR ALPHA = 0 -------------
! ======================================================================
    integer :: nelas
    parameter  ( nelas=4 )
    real(kind=8) :: elas(nelas), young, nu, t
    character(len=8) :: ncra1(nelas)
    integer :: icodre(nelas)
    real(kind=8) :: eps
    parameter  ( eps = 1.d-21 )
! ======================================================================
! --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
! ======================================================================
    data ncra1/'E','NU','ALPHA','RHO'/
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
    if ((yamec.eq.1) .or. yachai) then
        call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                    t, 3, ncra1(1), elas(1), icodre,&
                    1)
        young = elas(1)
        nu = elas(2)
        alpha0 = elas(3)
        k0 = young / 3.d0 / (1.d0-2.d0*nu)
        cs = (1.0d0-biot) / k0
    else
! =====================================================================
! --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0 OU EM -----------------
! =====================================================================
        alpha0 = 0.0d0
        cs = em
        k0 = 0.0d0
        if (em .lt. eps) then
            biot = phi0
        endif
    endif
! =====================================================================
! --- CALCUL EPSV AU TEMPS MOINS --------------------------------------
! =====================================================================
    epsvm = epsv - depsv
! =====================================================================
end subroutine

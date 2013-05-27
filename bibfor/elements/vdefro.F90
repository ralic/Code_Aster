subroutine vdefro(np, matev, tensel, tenloc)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!.======================================================================
    implicit none
!
!      VDEFRO   -- PASSAGE DU VECTEUR DES EFFORTS GENERALISES
!                  OU DU VECTEUR DES DEFORMATIONS-COURBURES
!                  DU REPERE INTRINSEQUE AUX NOEUDS
!                  OU AUX POINTS D'INTEGRATION DE L'ELEMENT
!                  AU REPERE UTILISATEUR POUR LES ELEMENTS DE
!                  COQUE EPAISSE 3D .
!
!                 CETTE ROUTINE EST ANALOGUE A DXEFRO QUI EST
!                 OPERATIONELLE POUR LES ELEMENTS DE PLAQUE
!                 A L'EXCEPTION DES MATRICES DE PASSAGE QUI
!                 SONT DEFINIES EN DES POINTS DE L'ELEMENT.
!
!   ARGUMENT        E/S   TYPE         ROLE
!    NP             IN     I        NOMBRE DE POINTS OU SONT CALCULES
!                                   LES TENSEURS (I.E. IL S'AGIT DES
!                                   NOEUDS OU DES POINTS D'INTEGRATION
!                                   DE L'ELEMENT)
!    MATEV(2,2,10)  IN     R        MATRICES DE PASSAGE DES REPERES
!                                   INTRINSEQUES AUX POINTS  DE
!                                   L'ELEMENT AU REPERE UTILISATEUR
!    TENSEL(1)      IN     R        VECTEUR DES EFFORTS GENERALISES
!                                   OU DES DEFORMATIONS-COURBURES
!                                   DANS LE REPERE INTRINSEQUE A
!                                   L'ELEMENT I.E.
!                                       NXX NYY NXY MXX MYY MXY VX VY
!                                   OU  EXX EYY EXY KXX KYY KXY GAX GAY
!    TENLOC(1)      OUT    R        VECTEUR DES EFFORTS GENERALISES
!                                   OU DES DEFORMATIONS-COURBURES
!                                   DANS LE REPERE UTILISATEUR
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'asterfort/utbtab.h'
    real(kind=8) :: matev(2, 2, 1), tensel(1), tenloc(1)
! -----  VARIABLES LOCALES
    real(kind=8) :: nelem(4), melem(4), xab(2, 2)
    real(kind=8) :: nlocal(4), mlocal(4)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- BOUCLE SUR LES POINTS OU SONT CALCULES LES VECTEURS
! --- (I.E. LES NOEUDS OU LES POINTS D'INTEGRATION) :
!     ============================================
!-----------------------------------------------------------------------
    integer :: i, np
!-----------------------------------------------------------------------
    do 10 i = 1, np
!
        nelem(1) = tensel(1+8*(i-1))
        nelem(2) = tensel(3+8*(i-1))
        nelem(3) = tensel(3+8*(i-1))
        nelem(4) = tensel(2+8*(i-1))
!
        melem(1) = tensel(4+8*(i-1))
        melem(2) = tensel(6+8*(i-1))
        melem(3) = tensel(6+8*(i-1))
        melem(4) = tensel(5+8*(i-1))
!
        call utbtab('ZERO', 2, 2, nelem, matev(1, 1, i),&
                    xab, nlocal)
        call utbtab('ZERO', 2, 2, melem, matev(1, 1, i),&
                    xab, mlocal)
!
        tenloc(1+8*(i-1)) = nlocal(1)
        tenloc(2+8*(i-1)) = nlocal(4)
        tenloc(3+8*(i-1)) = nlocal(2)
!
        tenloc(4+8*(i-1)) = mlocal(1)
        tenloc(5+8*(i-1)) = mlocal(4)
        tenloc(6+8*(i-1)) = mlocal(2)
!
        tenloc(7+8*(i-1)) = tensel(&
                            7+8*(i-1)) * matev(1, 1, i) + tensel(8+8*(i-1)) * matev(2, 1, i)
        tenloc(8+8*(i-1)) = tensel(&
                            7+8*(i-1)) * matev(1, 2, i) + tensel(8+8*(i-1)) * matev(2, 2, i)
!
10  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine

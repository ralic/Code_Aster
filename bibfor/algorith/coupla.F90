subroutine coupla(np1, nbm, indic, tpfl, veci1,&
                  vgap, vecr4, vecr1, vecr2, vecr5,&
                  vecr3, masg, puls, locflc, amflu0,&
                  amfluc, xsi0)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! DESCRIPTION : PRISE EN COMPTE DU COUPLAGE EN CAS DE REGIME
! -----------   DE VIBRATION NON-LINEAIRE (IMPACT-FROTTEMENT)
!
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterf_types.h"
#include "asterfort/coefmo.h"
#include "asterfort/matini.h"
!
    integer :: np1, nbm, indic
    character(len=8) :: tpfl
    integer :: veci1(*)
    real(kind=8) :: vgap, vecr4(*), vecr1(*), vecr2(*), vecr5(*), vecr3(*)
    real(kind=8) :: masg(*), puls(*)
    aster_logical :: locflc(*)
    real(kind=8) :: amflu0(np1, *), amfluc(np1, *), xsi0(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
    real(kind=8) :: xcf, r8b1(2), r8b2
    complex(kind=8) :: c16b
    aster_logical :: lk
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
! 1.  CALCUL DE LA MATRICE D'AMORTISSEMENT AJOUTE ADIMENSIONALISEE
!     ------------------------------------------------------------
    call matini(np1, np1, 0.d0, amfluc)
!
!.... LK = .FALSE. INDIQUE QU'ON NE CALCULE PAS LES TERMES DE RAIDEUR
!
    lk = .false.
    do 10 i = 1, nbm
        if (locflc(i)) then
            call coefmo(tpfl, lk, nbm, i, indic,&
                        r8b1, puls(i), vgap, xsi0(i), veci1,&
                        vecr1, vecr2, vecr3, vecr4, vecr5,&
                        r8b2, c16b, xcf)
            amfluc(i,i) = xcf/masg(i)
        endif
 10 end do
!
! 2.  CALCUL DU SAUT DE MATRICE D'AMORTISSEMENT AJOUTE ADIMENSIONALISEE
!     PAR RAPPORT A LA MATRICE DE REFERENCE EN VOL
!     --------------------------------------------
    do 20 j = 1, nbm
        do 21 i = 1, nbm
            amfluc(i,j) = amfluc(i,j) - amflu0(i,j)
 21     continue
 20 end do
!
! --- FIN DE COUPLA.
end subroutine

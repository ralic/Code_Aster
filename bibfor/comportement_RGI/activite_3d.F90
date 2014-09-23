subroutine activite_3d(gam1,gam2,temp,casol,nasol,ohsol)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     choix des coefficients d'activité
!=====================================================================
        implicit none
      real(kind=8) :: gam1
      real(kind=8) :: gam2
      real(kind=8) :: temp
      real(kind=8) :: casol
      real(kind=8) :: nasol
      real(kind=8) :: ohsol

      real(kind=8) :: elec
      real(kind=8) :: at

 !     calcul de la constante fonction de t
 !     dans l'équation de debye hückel      
      at=1.8d-6/(((7.73d-7*temp)**3)**0.5d0)
      
 !     electroneutralité simplifiée (1)    (3 ci-dessous a verifier ?)  
      elec=nasol+3.d0*casol
 !     détermination de la concentration en hydroxydes à partir de (1)      
      ohsol=nasol+2.d0*casol
      
 !     calcul des coefficients d'activité suivant les valences des ions      
      gam1=10.d0**(-at*((elec**0.5d0/(1.d0+(elec**0.5d0)))-0.3d0*elec))
      gam2=10.d0**(-at*4.d0*((elec**0.5d0/(1.d0+(elec**0.5d0)))&
     -0.3d0*elec))      
end subroutine

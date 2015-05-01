function xcalc_heav(id_no, hea_se, iflag)
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
! BUT : CALCULER LA FONCTION HEAVISIDE : 2 | 0
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!   - ID_NO  : IDENTIFIANT <ENTIER> DU DOMAINE DE LA FONCTION HEAVISIDE
!   - HEA_SE : IDENTIFIANT <ENTIER> DU DOMAINE AUQUEL APPARTIENT LE SOUS-ELEMENT/SOUS-FACETTE
!   - IFLAG  : ENTIER UTILE POUR MODIFIER L ENICHISSIMENT MONO-HEAVISIDE EN -2|0
!               => CE FLAG DOIT ETRE NETTOYER UNE FOIS QUE LA DEFINITION TOPOLOGIQUE MONO/MULTI
!                  SERA UNIFORMISEE
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
!-----------------------------------------------------------------------
    integer :: id_no, hea_se
    integer :: iflag
    real(kind=8) :: xcalc_heav
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
    if ( id_no.eq.hea_se) then
      xcalc_heav=2.d0
    else
      xcalc_heav=0.d0
    endif
!  ASTUCE POUR OBTENIR UN SAUT DE DEPLACEMENT = (+2) * H1X POUR UNE  FISSURE
!    REMARQUE: ON RAJOUTE CETTE CONVENTION PARCE QUE LA DEFINITION DE LA 
!        TOPOLOGIE SIGNE HEAVISIDE PAR FACETTE EST DIFFERENTE DU SIMPLE AU MULTI HEAVISIDE       
!    * NFISS=1 : ON A SIGNE(ESCLAVE)=-1, AVEC L ENRICHISSEMENT STANDARD XFEM
!                => MAIS CETTE INFORMATION INDIPENSABLE POUR CALCULER LE SAUT DE DEPLACEMENT,
!                   N EST PAS CONSTRUITE EXPLICITEMENT,
!                   DU COUP, L INFO EST REDEFINIE A CHAQUE FOIS DANS LE CODE ...
!                   => ON FIXE LE SAUT A +2 POUR NE PAS GERER DIRECTEMENT CE PROBLEME
!    * NFISS>1 : LA TOPOLOGIE EST CONSTRUITE EXPLICITEMENT ET STOCKEE DANS TOPOFAC.HEA
!                => CETTE INFO EST CODEE ENSUITE DANS TOPONO.HFA 
!                   => ET TRANSPORTEE DE MANIERE TRANSPARENTE
     if (iflag.eq.-999) xcalc_heav=1.d0*xcalc_heav      
     if (iflag.eq.999) xcalc_heav=-1.d0*xcalc_heav      
!
end function

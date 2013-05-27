subroutine tecart(carte)
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
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/cmpcar.h'
    include 'asterfort/expcar.h'
    character(len=*) :: carte
! ----------------------------------------------------------------------
!     IN : CARTE (K19) : NOM DE LA CARTE A MODIFIER
!
!     OUT: CARTE: LA CARTE EST MODIFIEE.
! ---------------------------------------------------------------------
!     BUT : CETTE ROUTINE  PERMET UNE SURCHARGE "FINE"
!           DES GRANDEURS AFFECTEES SUR UNE CARTE:
!
!         1) PAR DEFAUT (SI ON N'APPELLE PAS "TECART") UNE MAILLE SERA
!            AFFECTEE PAR LA DERNIERE GRANDEUR QU'ON LUI AURA ASSIGNEE
!            (DERNIER "NOCART" LA CONCERNANT)
!             LA GRANDEUR EST ICI A CONSIDERER DANS SON ENSEMBLE :
!             C'EST L'ENSEMBLE DES CMPS NOTES PAR "NOCART".
!             LA NOUVELLE GRANDEUR ECRASE COMPLETEMENT UNE EVENTUELLE
!             ANCIENNE GRANDEUR SUR TOUTES LES MAILLES AFFECTEES.
!
!         2) SI ON APPELLE "TECART", LA NOUVELLE GRANDEUR AFFECTEE
!            N'ECRASE QUE LES CMPS QUE L'ON AFFECTE VRAIMENT :
!           LES CMPS NON AFFECTEES PAR "NOCART" SONT ALORS TRANSPARENTES
!
!         REMARQUE :
!           SI L'ON VEUT DONNER UNE VALEUR PAR DEFAUT SUR LES CMPS
!           IL SUFFIT DE FAIRE UN "NOCART" DES LE DEBUT EN NOTANT
!           TOUTES LES CMPS SUR "TOUT" LE MAILLAGE.
!
!
!
! ---------------------------------------------------------------------
!    EXEMPLE :
!
!      GMA1 = (MA1,MA2)
!      GMA2 = (MA2,MA3)
!
!      CALL ALCART()
!      CALL NOCART(GMA1,('DX','DY'),(1.,2.))
!      CALL NOCART(GMA2,('DX','DZ'),(4.,5.))
!
!    1) ON NE FAIT PAS TECART() :
!          MAILLE   'DX'   'DY'  'DZ'  ("X" VEUT DIRE "N'EXISTE PAS")
!           MA1      1.     2.    X
!           MA2      X      4.    5.
!           MA3      X      4.    5.
!
!    2) ON FAIT TECART()
!          MAILLE   'DX'   'DY'  'DZ'
!           MA1      1.     2.    X
!           MA2      1.     4.    5.
!           MA3      X      4.    5.
!
! ----------------------------------------------------------------------
    character(len=19) :: carte2
! DEB --
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    carte2= carte
!
!     -- ON ETEND LA CARTE:
    call expcar(carte2)
!
!     -- ON COMPRIME LA CARTE:
    call cmpcar(carte2)
!
end subroutine

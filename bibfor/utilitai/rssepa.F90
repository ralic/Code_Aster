subroutine rssepa(result, nuordr, modele, mate, carele,&
                  excit)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/rsadpa.h'
    integer :: nuordr
    character(len=8) :: result, modele, carele, mate
    character(len=19) :: excit
!----------------------------------------------------------------------
!     BUT: ECRIRE DANS LA SD RESULTAT LES PARAMETRES MODELE, MATE,
!          CARELE ET EXCIT POUR LE NUME_ORDRE NUORDR
!
!     IN      RESULT : NOM DE LA SD RESULTAT
!     IN      IORDR  : NUMERO D'ORDRE
!     IN      MODELE : NOM DU MODELE
!     IN      MATE   : NOM DU CHAMP MATERIAU
!     IN      CARELE : NOM DE LA CARACTERISTIQUE ELEMENTAIRE
!     IN      EXCIT  : NOM DE LA SD INFO_CHARGE
!
!
!     VARIABLES LOCALES
    integer :: jpara
    character(len=8) :: k8b
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     ======================================================
! --- STOCKAGE DE NOMS DE CONCEPT A PARTIR DE LA SD RESULTAT
!     ======================================================
!
!     STOCKAGE DU NOM DU MODELE
!     -------------------------
    call rsadpa(result, 'E', 1, 'MODELE', nuordr,&
                0, jpara, k8b)
    zk8(jpara)=modele
!
!     STOCKAGE DU NOM DU CHAMP MATERIAU
!     ---------------------------------
    call rsadpa(result, 'E', 1, 'CHAMPMAT', nuordr,&
                0, jpara, k8b)
    zk8(jpara)=mate(1:8)
!
!     STOCKAGE DU NOM DE LA CARACTERISTIQUE ELEMENTAIRE
!     -------------------------------------------------
    call rsadpa(result, 'E', 1, 'CARAELEM', nuordr,&
                0, jpara, k8b)
    zk8(jpara)=carele
!
!     STOCKAGE DU NOM DE LA SD INFO_CHARGE
!     ------------------------------------
    call rsadpa(result, 'E', 1, 'EXCIT', nuordr,&
                0, jpara, k8b)
    zk24(jpara)=excit(1:19)
!
    call jedema()
end subroutine

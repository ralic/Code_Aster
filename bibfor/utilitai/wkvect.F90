subroutine wkvect(nom, carac, dim, ldec)
    implicit none
    include 'asterfort/jecreo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: nom, carac
    integer :: dim, ldec
!     ------------------------------------------------------------------
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
!
!     ------------------------------------------------------------------
!     CREATION D'UN VECTEUR
!     ------------------------------------------------------------------
! IN  NOM   : CH*24 : NOM (COMPLET)  DU VECTEUR
! IN  CARAC : CH    : DESCRIPTION DES CARACTERISTIQUES POUR JECREO
! IN  DIM   : IS    : TAILLE DU VECTEUR
! OUT LDEC  : IS    : DECALAGE
!     ------------------------------------------------------------------
    character(len=4) :: cbid
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jecreo(nom, carac)
    call jeecra(nom, 'LONMAX', dim, cbid)
    call jeecra(nom, 'LONUTI', dim, cbid)
    call jeveuo(nom, 'E', ldec)
end subroutine

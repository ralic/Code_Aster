module cara_elem_carte_type
!
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
!
! --------------------------------------------------------------------------------------------------
!
!   Définition des "user_type" utilisés que par AFFE_CARA_ELEM.
!
! --------------------------------------------------------------------------------------------------
!
!   cara_elem_carte_type      : Pour la gestion automatique des cartes
!       nom_carte   : nom de la carte.                          nomu//ACE_CARTE
!       adr_cmp     : adresse jeveux des composantes.           jeveut(nom_carte//'.NCMP')
!       adr_val     : adresse jeveux des valeurs.               jeveut(nom_carte//'.VALV')
!       nbr_cmp     : nombre de composantes.
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
!
    type cara_elem_carte
        integer             :: adr_cmp
        integer             :: adr_val
        integer             :: nbr_cmp
        character(len=19)   :: nom_carte
    end type cara_elem_carte
!
end module

module cara_elem_info_type
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
!   cara_elem_info  : Variable globale définit pour tout AFFE_CARA_ELEM
!       nomu        : nom du concept en sortie de la commande.  getres(nomu , x , x )
!       concept     : nom du concept résultat.                  getres( x , concept , x )
!       commande    : nom de la commande.                       getres( x , x , commande )
!       modele      : nom du modèle.
!       maillage    : nom du maillage.
!       nbnoeu      : nombre de noeud du maillage.
!       nbmail      : nombre de maille du maillage.
!       dimmod      : dimension topologique du modèle.          dismoi('DIM_GEOM' sur 'MODELE')
!       ivr         : Pour faire des vérifications de cohérences et des impressions
!           ivr(1)=1    : vérification MAILLES
!           ivr(2)      : libre
!           ivr(3)=niv  : niveau d'impression
!           ivr(4)=ifm  : unité d'impression
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
!
    type cara_elem_info
        character(len= 8)   :: nomu
        character(len=16)   :: concept
        character(len=16)   :: commande
        character(len= 8)   :: modele
        character(len= 8)   :: maillage
        integer             :: nbmail
        integer             :: nbnoeu
        integer             :: dimmod
        integer             :: ivr(4)
    end type cara_elem_info
!
end module

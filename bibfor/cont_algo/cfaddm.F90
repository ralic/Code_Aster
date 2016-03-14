subroutine cfaddm(ds_contact, l_frot      , node_slav_indx, i_cont_link,&
                  model_ndim, nb_node_mast, nods_mast_indx, coef_node  , tau1,&
                  tau2, norm, jeu, coornp)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfcoef.h"
#include "asterfort/cfcoem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer :: node_slav_indx, i_cont_link
    integer :: nb_node_mast, model_ndim
    integer :: nods_mast_indx(*)
    real(kind=8) :: coef_node(*)
    real(kind=8) :: jeu, coornp(3)
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    aster_logical :: l_frot
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! ON AJOUTE UNE LIAISON MAITRE/ESCLAVE OU NODALE
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  LCTFD  : FROTTEMENT
! IN  LCTF3D : FROTTEMENT EN 3D
! IN  POSNOE : INDICE DANS CONTNO DU NOEUD ESCLAVE
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  NBNOM  : NOMBRE DE NOEUDS MAITRES CONCERNES (MAX: 9)
! IN  POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
! IN  COEFNO : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!               MAITRES
! IN  TAU1   : TANGENTE LOCALE DIRECTION 1
! IN  TAU2   : TANGENTE LOCALE DIRECTION 2
! IN  NORM   : NORMALE LOCALE
! IN  JEU    : JEU DANS LA DIRECTION DE LA NORMALE CHOISIE (PM.NORM)
! IN  COORNP : COORDONNNES DE LA PROJECTION DU NOEUD ESCLAVE
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_tangco, sdcont_approj
    real(kind=8), pointer :: v_sdcont_tangco(:) => null()
    real(kind=8), pointer :: v_sdcont_approj(:) => null()
    character(len=24) :: sdcont_jeuite
    real(kind=8), pointer :: v_sdcont_jeuite(:) => null()
    real(kind=8) :: coef_cont(30), coef_fric_x(30), coef_fric_y(30)
    integer :: nb_dof_tot
    integer :: dof_indx(30)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access to contact datastructure
!
    sdcont_tangco = ds_contact%sdcont_solv(1:14)//'.TANGCO'
    sdcont_jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    sdcont_approj = ds_contact%sdcont_solv(1:14)//'.APPROJ'
    call jeveuo(sdcont_tangco, 'E', vr = v_sdcont_tangco)
    call jeveuo(sdcont_jeuite, 'E', vr = v_sdcont_jeuite)
    call jeveuo(sdcont_approj, 'E', vr = v_sdcont_approj)
!
! - Compute coefficients for pairing
!
    call cfcoef(ds_contact    , model_ndim , nb_node_mast, nods_mast_indx, coef_node,&
                node_slav_indx, norm       , tau1        , tau2          , coef_cont,&
                coef_fric_x   , coef_fric_y, nb_dof_tot  , dof_indx)
!
! - Update gaps
!
    v_sdcont_jeuite(3*(i_cont_link-1)+1) = jeu
    v_sdcont_jeuite(3*(i_cont_link-1)+2) = 0.d0
    v_sdcont_jeuite(3*(i_cont_link-1)+3) = 0.d0
!
! - Save tangents
!
    v_sdcont_tangco(6*(i_cont_link-1)+1) = tau1(1)
    v_sdcont_tangco(6*(i_cont_link-1)+2) = tau1(2)
    v_sdcont_tangco(6*(i_cont_link-1)+3) = tau1(3)
    v_sdcont_tangco(6*(i_cont_link-1)+4) = tau2(1)
    v_sdcont_tangco(6*(i_cont_link-1)+5) = tau2(2)
    v_sdcont_tangco(6*(i_cont_link-1)+6) = tau2(3)
!
! - Save projection
!
    v_sdcont_approj(3*(i_cont_link-1)+1) = coornp(1)
    v_sdcont_approj(3*(i_cont_link-1)+2) = coornp(2)
    v_sdcont_approj(3*(i_cont_link-1)+3) = coornp(3)
!
! - Set coefficients for pairing
!
    call cfcoem(ds_contact , l_frot      , node_slav_indx, i_cont_link,&
                nb_dof_tot , nb_node_mast, nods_mast_indx, dof_indx   ,&
                coef_cont  , coef_fric_x, coef_fric_y)
!
    call jedema()
end subroutine

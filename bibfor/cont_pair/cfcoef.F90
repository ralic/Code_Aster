subroutine cfcoef(ds_contact    , model_ndim , nb_node_mast, nods_mast_indx, coef_node,&
                  node_slav_indx, norm       , tau1        , tau2          , coef_cont,&
                  coef_fric_x   , coef_fric_y, nb_dof_tot  , dof_indx)
!
use NonLin_Datastructure_type
!
implicit none
!
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
    integer, intent(in) :: model_ndim
    integer, intent(in) :: nb_node_mast
    integer, intent(in) :: nods_mast_indx(9)
    integer, intent(in) :: node_slav_indx
    real(kind=8), intent(in) :: coef_node(9)
    real(kind=8), intent(in) :: norm(3)
    real(kind=8), intent(in) :: tau1(3)
    real(kind=8), intent(in) :: tau2(3)
    real(kind=8), intent(out) :: coef_cont(30)
    real(kind=8), intent(out) :: coef_fric_x(30)
    real(kind=8), intent(out) :: coef_fric_y(30)
    integer, intent(out) :: dof_indx(30)
    integer, intent(out) :: nb_dof_tot
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! CALCULE LES COEFFICIENTS DES RELATIONS LINEAIRES ET DONNE LES NUMEROS
!  DES DDL ASSOCIES
!
! --------------------------------------------------------------------------------------------------
!
! IN  NDIMG  : DIMENSION DE L'ESPACE (2 OU 3)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBNOM  : NOMBRE DE NOEUDS MAITRES CONCERNES
! IN  POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
! IN  POSNOE : INDICES DANS CONTNO DU NOEUD ESCLAVE
! IN  COEFNO : COEFFICIENTS DES FONCTIONS DE FORME APRES PROJECTION
!                SUR LA MAILLE MAITRE
! IN  NORM   : NORMALE
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT COEF   : COEFFICIENTS LIES AU NOEUD ESCLAVE ET AUX NOEUDS MAITRES
!              (DIRECTION NORMALE)
! OUT COEFX  : COEFFICIENTS LIES AU NOEUD ESCLAVE ET AUX NOEUDS MAITRES
!              (PROJECTION SUR LA PREMIERE TANGENTE)
! OUT COEFY  : COEFFICIENTS LIES AU NOEUD ESCLAVE ET AUX NOEUDS MAITRES
!              (PROJECTION SUR LA SECONDE TANGENTE)
! OUT NBDDLT : NOMBRE DE DDLS CONCERNES (ESCLAVES + MAITRES)
! OUT DDL    : NUMEROS DES DDLS ESCLAVE ET MAITRES CONCERNES
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_dime, i_node_mast
    integer :: jdecal, nb_dof_mast, nb_dof_slav, jdecdl
    character(len=24) :: sdcont_ddlco, sdcont_nbddl
    integer, pointer :: v_sdcont_nbddl(:) => null()
    integer, pointer :: v_sdcont_ddlco(:) => null()
    integer :: node_mast_indx
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    coef_cont(1:30)   = 0.d0
    coef_fric_x(1:30) = 0.d0
    coef_fric_y(1:30) = 0.d0
    dof_indx (1:30)   = 0
!
! - Access to contact datastructure
!
    sdcont_nbddl = ds_contact%sdcont_solv(1:14)//'.NBDDL'
    sdcont_ddlco = ds_contact%sdcont_solv(1:14)//'.DDLCO'
    call jeveuo(sdcont_nbddl, 'L', vi = v_sdcont_nbddl)
    call jeveuo(sdcont_ddlco, 'L', vi = v_sdcont_ddlco)
!
! - For slave nodes
!
    jdecdl      = v_sdcont_nbddl(node_slav_indx)
    nb_dof_slav = v_sdcont_nbddl(node_slav_indx+1) - v_sdcont_nbddl(node_slav_indx)
!
    do i_dime = 1, model_ndim
        coef_cont(i_dime)   = 1.d0 * norm(i_dime)
        coef_fric_x(i_dime) = 1.d0 * tau1(i_dime)
        coef_fric_y(i_dime) = 1.d0 * tau2(i_dime)
        dof_indx(i_dime)    = v_sdcont_ddlco(jdecdl+i_dime)
    end do
    jdecal = nb_dof_slav
!
! - For master nodes
!
    do i_node_mast = 1, nb_node_mast
        node_mast_indx = nods_mast_indx(i_node_mast)
        jdecdl         = v_sdcont_nbddl(node_mast_indx)
        nb_dof_mast    = v_sdcont_nbddl(node_mast_indx+1) - v_sdcont_nbddl(node_mast_indx)
        do i_dime = 1, nb_dof_mast
            coef_cont(jdecal+i_dime)   = coef_node(i_node_mast) * norm(i_dime)
            coef_fric_x(jdecal+i_dime) = coef_node(i_node_mast) * tau1(i_dime)
            coef_fric_y(jdecal+i_dime) = coef_node(i_node_mast) * tau2(i_dime)
            dof_indx(jdecal+i_dime)    = v_sdcont_ddlco(jdecdl+i_dime)
        end do
        jdecal = jdecal + nb_dof_mast
    end do
!
! - Total number of dof
!
    nb_dof_tot = jdecal
!
    call jedema()
end subroutine

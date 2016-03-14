subroutine cfcoem(ds_contact, l_frot      , node_slav_indx, i_cont_link,&
                  nb_dof_tot, nb_node_mast, nods_mast_indx, dof_indx   ,&
                  coef_cont , coef_fric_x , coef_fric_y)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisd.h"
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
    integer, intent(in) :: node_slav_indx
    integer, intent(in) :: i_cont_link
    integer, intent(in) :: nb_dof_tot
    integer, intent(in) :: nb_node_mast
    integer, intent(in) :: nods_mast_indx(9)
    integer, intent(in) :: dof_indx(30)
    real(kind=8), intent(in) :: coef_cont(30)
    real(kind=8), intent(in) :: coef_fric_x(30)
    real(kind=8), intent(in) :: coef_fric_y(30)
    aster_logical, intent(in) :: l_frot
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL)
!
! COEFFICIENTS RELATIONS LINEAIRES APPARIEMENT
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  node_slav_indx   : index of slave node (in contact datastructure)
! IN  POSNOE : INDICE DANS CONTNO DU NOEUD ESCLAVE
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
! IN  NBDDLT : NOMBRE DE DDL NOEUD ESCLAVE+NOEUDS MAITRES
! IN  nb_node_mast  : NOMBRE DE NOEUDS MAITRES CONCERNES (MAX: 9)
! IN  POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
! IN  DDL    : NUMEROS DES DDLS ESCLAVE ET MAITRES CONCERNES
! IN  COEF   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
! IN  COFX   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!                POUR LA PREMIERE DIRECTION TANGENTE
! IN  COFY   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!                POUR LA SECONDE DIRECTION TANGENTE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_dof_slav, nb_dof_mast, nb_slav_maxi
    integer :: jdecal, jdecdl, node_mast_indx
    integer :: i_dof_mast, i_dof_slav, i_node_mast
    character(len=24) :: sdcont_apcoef, sdcont_apcofr
    real(kind=8), pointer :: v_sdcont_apcoef(:) => null()
    real(kind=8), pointer :: v_sdcont_apcofr(:) => null()
    character(len=24) :: sdcont_nbddl, sdcont_apddl
    integer, pointer :: v_sdcont_nbddl(:) => null()
    integer, pointer :: v_sdcont_apddl(:) => null()
    character(len=24) :: sdcont_appoin
    integer, pointer :: v_sdcont_appoin(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access to contact datastructure
!
    sdcont_nbddl  = ds_contact%sdcont_solv(1:14)//'.NBDDL'
    sdcont_apddl  = ds_contact%sdcont_solv(1:14)//'.APDDL'
    sdcont_apcoef = ds_contact%sdcont_solv(1:14)//'.APCOEF'
    sdcont_apcofr = ds_contact%sdcont_solv(1:14)//'.APCOFR'
    sdcont_appoin = ds_contact%sdcont_solv(1:14)//'.APPOIN'
    call jeveuo(sdcont_nbddl , 'L', vi = v_sdcont_nbddl)
    call jeveuo(sdcont_apddl , 'E', vi = v_sdcont_apddl)
    call jeveuo(sdcont_appoin, 'E', vi = v_sdcont_appoin)
    call jeveuo(sdcont_apcoef, 'E', vr = v_sdcont_apcoef)
    if (l_frot) then
        call jeveuo(sdcont_apcofr, 'E', vr = v_sdcont_apcofr)
    endif
!
! - Get parameters
!
    nb_slav_maxi = cfdisd(ds_contact%sdcont_solv,'NESMAX')
    nb_dof_slav  = v_sdcont_nbddl(node_slav_indx+1) - v_sdcont_nbddl(node_slav_indx)
!
! - Set total number of links
!
    v_sdcont_appoin(i_cont_link+1) = v_sdcont_appoin(i_cont_link) + nb_dof_tot
!
! - Set contact link for slave node
!
    jdecal = v_sdcont_appoin(i_cont_link)
    do i_dof_slav = 1, nb_dof_slav
        v_sdcont_apcoef(jdecal+i_dof_slav) = coef_cont(i_dof_slav)
        v_sdcont_apddl(jdecal+i_dof_slav)  = dof_indx(i_dof_slav)
    end do
!
! - Set friction link for slave node
!
    if (l_frot) then
        do i_dof_slav = 1, nb_dof_slav
            v_sdcont_apcofr(jdecal+i_dof_slav)                 = coef_fric_x(i_dof_slav)
            v_sdcont_apcofr(jdecal+30*nb_slav_maxi+i_dof_slav) = coef_fric_y(i_dof_slav)
        end do
    endif
!
! - Set contact/friction link for master node
!
    jdecal = jdecal + nb_dof_slav
    jdecdl = nb_dof_slav
    do i_node_mast = 1, nb_node_mast
        node_mast_indx = nods_mast_indx(i_node_mast)
        nb_dof_mast    = v_sdcont_nbddl(node_mast_indx+1) - v_sdcont_nbddl(node_mast_indx)
        do i_dof_mast = 1, nb_dof_mast
            v_sdcont_apcoef(jdecal+i_dof_mast) = coef_cont(jdecdl+i_dof_mast)
            v_sdcont_apddl(jdecal+i_dof_mast)  = dof_indx(jdecdl+i_dof_mast)
        end do
        if (l_frot) then
            do i_dof_mast = 1, nb_dof_mast
                v_sdcont_apcofr(jdecal+i_dof_mast)                 = coef_fric_x(jdecdl+i_dof_mast)
                v_sdcont_apcofr(jdecal+30*nb_slav_maxi+i_dof_mast) = coef_fric_y(jdecdl+i_dof_mast)
            end do
        endif
        jdecal = jdecal + nb_dof_mast
        jdecdl = jdecdl + nb_dof_mast
    end do
!
    call jedema()
end subroutine

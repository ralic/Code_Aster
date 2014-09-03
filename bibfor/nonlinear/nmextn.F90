subroutine nmextn(field_disc, type_extr_cmp, type_extr_elem, type_extr, nb_node,&
                  nb_elem   , nb_cmp       , nb_poin       , nb_spoi  , nb_extr)
!
implicit none
!
#include "asterfort/nmexto.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=4), intent(in) :: field_disc
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_poin
    integer, intent(in) :: nb_spoi
    integer, intent(in) :: nb_cmp
    character(len=8), intent(in) :: type_extr
    character(len=8), intent(in) :: type_extr_elem
    character(len=8), intent(in) :: type_extr_cmp
    integer, intent(out) :: nb_extr
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities 
!
! Count number of extractions
!
! --------------------------------------------------------------------------------------------------
!
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  type_extr        : type of extraction
! In  type_extr_elem   : type of extraction by element
! In  type_extr_cmp    : type of extraction for components
! In  nb_node          : number of nodes
! In  nb_elem          : number of elements
! In  nb_poin          : number of points (Gauss)
! In  nb_spoi          : number of subpoints
! In  nb_cmp           : number of components
! Out nb_extr          : number of extractions
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nfor, npoin, nlieu
!
! --------------------------------------------------------------------------------------------------
!
    nb_extr = 0
!
! - Number of components to extract
!
    call nmexto('COMP' , field_disc, type_extr_cmp, type_extr_elem, type_extr,&
                nb_node, nb_elem   , nb_cmp       , nb_poin       , nb_spoi  ,&
                nfor)
!
! - Number of points to extract
!
    call nmexto('POIN' , field_disc, type_extr_cmp, type_extr_elem, type_extr,&
                nb_node, nb_elem   , nb_cmp       , nb_poin       , nb_spoi  ,&
                npoin)
!
! - Number of localization to extract
!
    call nmexto('LIEU' , field_disc, type_extr_cmp, type_extr_elem, type_extr,&
                nb_node, nb_elem   , nb_cmp       , nb_poin       , nb_spoi  ,&
                nlieu)
!
! - Total of extraction
!
    nb_extr = nlieu * npoin * nfor
!
end subroutine

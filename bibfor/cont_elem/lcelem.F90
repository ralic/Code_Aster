subroutine lcelem(nomte         , elem_dime     ,&
                  l_axis        , l_elem_frot   ,&
                  nb_dof        , nb_lagr       , indi_lagc   ,&
                  elem_slav_code, elga_fami_slav, nb_node_slav,&
                  elem_mast_code, elga_fami_mast, nb_node_mast)
!
implicit none
!
#include "asterf_types.h" 
#include "asterfort/assert.h"
#include "asterfort/lteatt.h"
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
! aslint: disable=W1501
!
    character(len=16), intent(in) :: nomte
    integer, intent(out) :: elem_dime
    aster_logical, intent(out) :: l_axis
    aster_logical, intent(out) :: l_elem_frot
    integer, intent(out) :: nb_dof
    integer, intent(out) :: nb_lagr
    integer, intent(out) :: indi_lagc(10)
    character(len=8), intent(out) :: elem_slav_code
    character(len=8), intent(out) :: elga_fami_slav
    integer, intent(out) :: nb_node_slav
    character(len=8), intent(out) :: elem_mast_code
    character(len=8), intent(out) :: elga_fami_mast
    integer, intent(out) :: nb_node_mast
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Get informations about contact element
!
! --------------------------------------------------------------------------------------------------
!
! In  nomte            : type of finite element
! Out elem_dime        : dimension of elements
! Out l_axis           : .true. for axisymmetric element
! Out l_elem_frot      : .true. for friction element
! Out nb_dof           : total number of dof on contact element
! Out nb_lagr          : total number of Lagrangian dof on contact element
! Out indi_lagc        : PREVIOUS node where Lagrangian dof is present (1) or not (0)
! Out elem_slav_code   : code element for slave side from contact element
! Out elga_fami_slav   : name of integration scheme for slave side from contact element
! Out nb_node_slav     : number of nodes of for slave side from contact element
! Out elem_mast_code   : code element for master side from contact element
! Out elga_fami_mast   : name of integration scheme for master side from contact element
! Out nb_node_mast     : number of nodes of for master side from contact element
!
! --------------------------------------------------------------------------------------------------
!
    l_axis          = lteatt('AXIS','OUI')
    l_elem_frot     = lteatt('FROTTEMENT','OUI')
    elem_dime       = 0
    indi_lagc(1:10) = 0
    nb_dof          = 0
    nb_lagr         = 0
    elem_slav_code  = ' '
    elga_fami_slav  = ' '
    nb_node_slav    = 0
    elem_mast_code  = ' '
    elga_fami_mast  = ' '
    nb_node_mast    = 0
    ASSERT(.not.l_elem_frot)
!
! - 2D / SEG2
!
    if (nomte(1:7) .eq. 'LCS2S2C') then
        elem_dime      = 2
        elem_slav_code = 'SE2'
        nb_node_slav   = 2
        elem_mast_code = 'SE2'
        nb_node_mast   = 2
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(3)   = 1    
        elga_fami_slav = 'FPG3'
        elga_fami_mast = 'FPG3'
    elseif (nomte(1:7) .eq. 'LCS2S3C') then
        elem_dime      = 2
        elem_slav_code = 'SE2'
        nb_node_slav   = 2
        elem_mast_code = 'SE3'
        nb_node_mast   = 3
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(3)   = 1    
        elga_fami_slav = 'FPG3'
        elga_fami_mast = 'FPG3'
    else if (nomte(1:7) .eq. 'LCS2S2D') then
        elem_dime      = 2
        elem_slav_code = 'SE2'
        nb_node_slav   = 2
        elem_mast_code = 'SE2'
        nb_node_mast   = 2
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1  
        elga_fami_slav = 'FPG3'
        elga_fami_mast = 'FPG3'
    else if (nomte(1:7) .eq. 'LCS2S3D') then
        elem_dime      = 2
        elem_slav_code = 'SE2'
        nb_node_slav   = 2
        elem_mast_code = 'SE3'
        nb_node_mast   = 3
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1  
        elga_fami_slav = 'FPG3'
        elga_fami_mast = 'FPG3' 
    else if (nomte(1:7) .eq. 'LCS2S2E') then
        elem_dime      = 2
        elem_slav_code = 'SE2'
        nb_node_slav   = 2
        elem_mast_code = 'SE2'
        nb_node_mast   = 2
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1    
        elga_fami_slav = 'FPG3'
        elga_fami_mast = 'FPG3' 
    else if (nomte(1:7) .eq. 'LCS2S3E') then
        elem_dime      = 2
        elem_slav_code = 'SE2'
        nb_node_slav   = 2
        elem_mast_code = 'SE3'
        nb_node_mast   = 3
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1    
        elga_fami_slav = 'FPG3'
        elga_fami_mast = 'FPG3'
!
! - 2D / SEG3
!
    else if (nomte(1:7) .eq. 'LCS3S2C') then
        elem_dime      = 2
        elem_slav_code = 'SE3'
        nb_node_slav   = 3
        elem_mast_code = 'SE2'
        nb_node_mast   = 2
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1    
        elga_fami_slav = 'FPG4'
        elga_fami_mast = 'FPG4'
    else if (nomte(1:7) .eq. 'LCS3S3C') then
        elem_dime      = 2
        elem_slav_code = 'SE3'
        nb_node_slav   = 3
        elem_mast_code = 'SE3'
        nb_node_mast   = 3
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1    
        elga_fami_slav = 'FPG4'
        elga_fami_mast = 'FPG4'
!
! - 3D/QUAD4
!
    else if (nomte .eq. 'LACQ4Q4D') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4Q4E') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4T3D') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4T6D') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4Q8D') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4Q9D') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4T3E') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4T6E') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4Q8E') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ4Q9E') then
        elem_dime      = 3
        elem_slav_code = 'QU4'
        nb_node_slav   = 4
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
!
! - 3D/QUAD8
!
    else if (nomte .eq. 'LACQ8Q8D') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8T3D') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8T6D') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8Q4D') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8Q9D') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 2
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8Q8E') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8Q9E') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8T3E') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8T6E') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ8Q4E') then
        elem_dime      = 3
        elem_slav_code = 'QU8'
        nb_node_slav   = 8
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 4
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(2)   = 1
        indi_lagc(3)   = 1
        indi_lagc(4)   = 1
        indi_lagc(5)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
!
! - 3D/TRIA3
!
    else if (nomte .eq. 'LACT3T3D') then
        elem_dime      = 3
        elem_slav_code = 'TR3'
        nb_node_slav   = 3
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav ='FPG6'
        elga_fami_mast ='FPG6'
    else if (nomte .eq. 'LACT3T6D') then
        elem_dime      = 3
        elem_slav_code = 'TR3'
        nb_node_slav   = 3
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT3Q4D') then
        elem_dime      = 3
        elem_slav_code = 'TR3'
        nb_node_slav   = 3
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT3Q8D') then
        elem_dime      = 3
        elem_slav_code = 'TR3'
        nb_node_slav   = 3
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT3Q9D') then
        elem_dime      = 3
        elem_slav_code = 'TR3'
        nb_node_slav   = 3
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
!
! - 3D/TRIA6
!
    else if (nomte .eq. 'LACT6T6D') then
        elem_dime      = 3
        elem_slav_code = 'TR6'
        nb_node_slav   = 6
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT6T3D') then
        elem_dime      = 3
        elem_slav_code = 'TR6'
        nb_node_slav   = 6
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT6Q4D') then
        elem_dime      = 3
        elem_slav_code = 'TR6'
        nb_node_slav   = 6
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT6Q8D') then
        elem_dime      = 3
        elem_slav_code = 'TR6'
        nb_node_slav   = 6
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACT6Q9D') then
        elem_dime      = 3
        elem_slav_code = 'TR6'
        nb_node_slav   = 6
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(4)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
!
! - 3D/QUAD9
!
    else if (nomte .eq. 'LACQ9Q9D') then
        elem_dime      = 3
        elem_slav_code = 'QU9'
        nb_node_slav   = 9
        elem_mast_code = 'QU9'
        nb_node_mast   = 9
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(10)  = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ9T3D') then
        elem_dime      = 3
        elem_slav_code = 'QU9'
        nb_node_slav   = 9
        elem_mast_code = 'TR3'
        nb_node_mast   = 3
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(10)  = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ9T6D') then
        elem_dime      = 3
        elem_slav_code = 'QU9'
        nb_node_slav   = 9
        elem_mast_code = 'TR6'
        nb_node_mast   = 6
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(10)   = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ9Q4D') then
        elem_dime      = 3
        elem_slav_code = 'QU9'
        nb_node_slav   = 9
        elem_mast_code = 'QU4'
        nb_node_mast   = 4
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(10)  = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else if (nomte .eq. 'LACQ9Q8D') then
        elem_dime      = 3
        elem_slav_code = 'QU9'
        nb_node_slav   = 9
        elem_mast_code = 'QU8'
        nb_node_mast   = 8
        nb_lagr        = 1
        nb_dof         = nb_node_mast*elem_dime + nb_node_slav*elem_dime+nb_lagr
        indi_lagc(10)  = 1
        elga_fami_slav = 'FPG6'
        elga_fami_mast = 'FPG6'
    else
        ASSERT(.false.)
    endif
!
    ASSERT(nb_node_slav .le. 10)
    ASSERT(nb_dof .le. 81)
    ASSERT((elem_dime .eq. 2).or.(elem_dime .eq. 3))
!
end subroutine

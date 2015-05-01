subroutine xlagsc(nb_dim, nb_node_mesh, nb_edge, nb_edge_max, algo_lagr,&
                  jtabno, jtabin      , jtabcr , crack      , sdline_crack,&
                  l_pilo)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/xrell1.h"
#include "asterfort/xrell2.h"
#include "asterfort/xsella.h"
!
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, intent(in) :: jtabno
    integer, intent(in) :: jtabin
    integer, intent(in) :: jtabcr
    integer, intent(in) :: nb_node_mesh
    integer, intent(in) :: nb_edge
    integer, intent(in) :: nb_dim
    integer, intent(in) :: nb_edge_max
    integer, intent(in) :: algo_lagr
    character(len=14), intent(in) :: sdline_crack
    character(len=8), intent(in) :: crack
    aster_logical, intent(in) :: l_pilo
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact definition
!
! Create list of linear relations for equality between Lagrange
!
! --------------------------------------------------------------------------------------------------
!
! (VOIR BOOK VI 15/07/05) :
!    - DETERMINATION DES NOEUDS
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! --------------------------------------------------------------------------------------------------
!
! In  crack          : name of crack 
! In  nb_dim         : dimension of space
! In  nb_node_mesh   : number of (physical) nodes in mesh
! In  nb_edge        : number of cut edges
! In  nb_edge_max    : number maximum of edges
! In  algo_lagr      : type of Lagrange multiplier space selection
! In  sdline_crack   : name of datastructure of linear relations for crack
! In  jtabno         : adress of table of nodes for edges (middle et vertex nodes)
! In  jtabin         : adress of table of intersection points
! In  jtabcr         : adress of table of score
! In  l_pilo         : .true. if creation of linear relations for continuation method (PILOTAGE)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jpino
    integer :: nb_node_sele
    aster_logical :: l_create_group
!
! --------------------------------------------------------------------------------------------------
!
    l_create_group = .false.
    if (algo_lagr .eq. 1) then
!
! ----- Table of nodes
!
        call wkvect('&&XLAGSP.PICKNO', 'V V I', nb_edge_max, jpino)
!
! ----- Selection of edges
!
        call xsella(crack       , nb_node_mesh, nb_edge, zi(jtabno), zi(jpino),&
                    nb_node_sele)
!
! ----- Create list of linear relations (algorithm 1)
!
        call xrell1(zi(jtabno)  , nb_edge, zi(jpino), nb_node_sele, sdline_crack)

    else if (algo_lagr.eq.2) then
!
! ----- Create list of linear relations (algorithm 2)
!
        call xrell2(zi(jtabno)    , nb_dim      , nb_edge, zr(jtabin), zr(jtabcr),&
                    l_create_group, sdline_crack, l_pilo)
    endif
!
    call jedetr('&&XLAGSP.PICKNO')

end subroutine

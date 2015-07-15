subroutine nmcmat(matr_type_ , calc_opti_    , asse_opti_    , l_calc        , l_asse     ,&
                  nb_matr    , list_matr_type, list_calc_opti, list_asse_opti, list_l_calc,&
                  list_l_asse)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: matr_type_
    character(len=*), intent(in) :: calc_opti_    
    character(len=*), intent(in) :: asse_opti_
    aster_logical, intent(in) :: l_calc
    aster_logical, intent(in) :: l_asse
    integer, intent(inout) :: nb_matr
    character(len=6), intent(inout)  :: list_matr_type(20)
    character(len=16), intent(inout) :: list_calc_opti(20) 
    character(len=16), intent(inout) :: list_asse_opti(20)
    aster_logical, intent(inout) :: list_l_asse(20)
    aster_logical, intent(inout) :: list_l_calc(20)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Computation
!
! Mangement of list of matrix to compute/assembly
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  matr_type        : type of matrix
!                MERIGI  - MATRICE POUR RIGIDITE
!                MEDIRI  - MATRICE POUR CL DIRICHLET LAGRANGE
!                MEGEOM  - MATRICE POUR NON-LIN. GEOMETRIQUE
!                MEAMOR  - MATRICE POUR AMORTISSEMENT
!                MEMASS  - MATRICE POUR MASSE
!                MESUIV  - MATRICE POUR CHARGEMENT SUIVEUR
!                MESSTR  - MATRICE POUR SOUS-STRUCTURES
!                MECTCC  - MATRICE POUR CONTACT CONTINU
!                MECTCF  - MATRICE POUR FROTTEMENT CONTINU
!                MEXFEC  - MATRICE POUR CONTACT XFEM
!                MEXFEF  - MATRICE POUR FROTTEMENT XFEM
!                MEXFTC  - MATRICE POUR CONTACT XFEM (GRD GLIS)
!                MEXFTF  - MATRICE POUR FROTT. XFEM (GRD GLIS)
! In  calc_opti        : option for matr_elem
! In  asse_opti        : option for matr_asse
! In  l_calc           : .true. to compute matr_elem
! In  l_asse           : .true. to assembly matr_elem in matr_asse
! IO  nb_matr          : number of matrix in list
! IO  list_matr_type   : list of matrix
! IO  list_calc_opti   : list of options for matr_elem
! IO  list_asse_opti   : list of options for matr_asse
! IO  list_l_calc      : list of flags to compute matr_elem
! IO  list_l_asse      : list of flags to assembly matr_elem in matr_asse
!
! --------------------------------------------------------------------------------------------------
!
    nb_matr = nb_matr + 1
    ASSERT(nb_matr.le.20)
    list_matr_type(nb_matr) = matr_type_
    list_calc_opti(nb_matr) = calc_opti_
    list_asse_opti(nb_matr) = asse_opti_
    list_l_asse(nb_matr)    = l_asse
    list_l_calc(nb_matr)    = l_calc
!
end subroutine

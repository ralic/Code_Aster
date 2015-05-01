subroutine char_crea_ligf(mesh, ligrch, nb_elem_late, nb_noel_maxi)
!
    implicit none
!
#include "asterfort/jecrec.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/wkvect.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=19), intent(in) :: ligrch
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_elem_late
    integer, intent(in) :: nb_noel_maxi
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Create <LIGREL> on late elements
!
! --------------------------------------------------------------------------------------------------
!
! In  ligrch       : name of <LIGREL> for load
! In  mesh         : name of mesh
! In  nb_late_elem : number of "late" elements
! In  nb_noel_maxi : maximum number of nodes on "late" elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_grel, iret
    integer :: lont_liel, lont_nema, nb_node_late
    character(len=8), pointer :: p_ligrch_lgrf(:) => null()
    integer, pointer :: p_ligrch_nbno(:) => null()
    integer, pointer :: p_ligrch_lgns(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jeexin(ligrch//'.LIEL', iret)
    if (iret .eq. 0) then
!
! ----- General objects
!
        call wkvect(ligrch//'.LGRF', 'G V K8', 2, vk8 = p_ligrch_lgrf)
        p_ligrch_lgrf(1) = mesh
        call wkvect(ligrch//'.NBNO', 'G V I', 1, vi = p_ligrch_nbno)
        p_ligrch_nbno(1) = 0
!
! ----- Number of GRoup of ELements
!
        nb_grel      = nb_elem_late
!
! ----- LGNS object
!
        nb_node_late = nb_elem_late*nb_noel_maxi
        call wkvect(ligrch//'.LGNS', 'G V I', nb_node_late, vi = p_ligrch_lgns)
!
! ----- LIEL object
!
        lont_liel    = 2*nb_elem_late  
        call jecrec(ligrch//'.LIEL', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nb_grel)
        call jeecra(ligrch//'.LIEL', 'LONT', ival = lont_liel)
!
! ----- NEMA object
!
        lont_nema    = 2*nb_elem_late*(nb_noel_maxi+1)
        call jecrec(ligrch//'.NEMA', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nb_grel)
        call jeecra(ligrch//'.NEMA', 'LONT', ival = lont_nema)
!
    endif
end subroutine

subroutine numddl(nume_ddlz, base, nb_matr, list_matr, renumz)
!
implicit none
!
#include "asterfort/as_deallocate.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/nueffe.h"
#include "asterfort/numoch.h"
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
! aslint: disable=W1306
!
    character(len=2), intent(in) :: base
    character(len=*), intent(in) :: nume_ddlz
    character(len=*), intent(in) :: renumz
    character(len=*), intent(in) :: list_matr(*)
    integer, intent(in) :: nb_matr
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Numbering - Create NUME_EQUA objects
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_ddl       : name of nume_ddl object
! In  base           : JEVEUX base to create objects
!                      base(1:1) => PROF_CHNO objects
!                      base(2:2) => NUME_DDL objects
! In  list_matr      : list of elementary matrixes
! In  nb_matr        : number of elementary matrixes
! In  renum          : method for renumbering equation
!                       SANS/RCMKs
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_matr_elem(nb_matr)
    integer :: nb_ligr
    character(len=24), pointer :: list_ligr(:) => null()
    integer :: i_matr
!
! --------------------------------------------------------------------------------------------------
!
    do i_matr = 1, nb_matr
        list_matr_elem(i_matr) = list_matr(i_matr)
    end do
!
! - Create list of LIGREL for numbering
!
    call numoch(list_matr_elem, nb_matr, list_ligr, nb_ligr)
!
! - Numbering - Create NUME_EQUA objects
!
    call nueffe(nb_ligr, list_ligr, base, nume_ddlz, renumz)
!
    AS_DEALLOCATE(vk24 = list_ligr)
!
end subroutine

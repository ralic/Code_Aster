!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine gcour3(resu, noma, coorn, lnoff, trav1,&
                      trav2, trav3, chfond, connex, grlt, liss, &
                      basfon, nbre, milieu,&
                      ndimte, typdis, nomfis)
        character(len=8) :: resu
        character(len=8) :: noma
        character(len=24) :: coorn
        integer :: lnoff
        character(len=24) :: trav1
        character(len=24) :: trav2
        character(len=24) :: trav3
        character(len=24) :: chfond
        aster_logical :: connex
        character(len=19) :: grlt
        character(len=24) :: liss
        character(len=24) :: basfon
        integer :: nbre
        aster_logical :: milieu
        integer :: ndimte
        character(len=16) :: typdis
        character(len=8) :: nomfis
    end subroutine gcour3
end interface

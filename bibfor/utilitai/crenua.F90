subroutine crenua(nuagez    , gran_name, nb_point, nb_dim, nb_cmp_max,&
                  l_crea_nual)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
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
    character(len=*), intent(in) :: nuagez
    character(len=*), intent(in) :: gran_name
    integer, intent(in) :: nb_point
    integer, intent(in) :: nb_dim
    integer, intent(in) :: nb_cmp_max
    aster_logical, intent(in) :: l_crea_nual
!
! --------------------------------------------------------------------------------------------------
!
! Create NUAGE datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  nuage       : name of NUAGE datastructure
! In  gran_name   : name of GRANDEUR
! In  nb_point    : number of ppoints in NUAGE
! In  nb_dim      : dimension of model
! In  nb_cmp_max  : number maxi of components on point
! In  l_crea_nual : .true. if create .NUAL
!
! --------------------------------------------------------------------------------------------------
!
    integer :: length
    character(len=4) :: type_scal
    character(len=19) :: nuage
    integer :: jnuai, jnual, jnuav, jnuax
!
! --------------------------------------------------------------------------------------------------
!
    nuage = nuagez
!
! - Create .NUAX
!
    length = nb_dim * nb_point
    call wkvect(nuage//'.NUAX', 'V V R', length, jnuax)
!
! - Create .NUAI
!
    length = 5 + nb_cmp_max
    call wkvect(nuage//'.NUAI', 'V V I', length, jnuai)
!
! - Create .NUAV
!
    call dismoi('TYPE_SCA', gran_name, 'GRANDEUR', repk=type_scal)
    length = nb_cmp_max * nb_point
    if (type_scal(1:1) .eq. 'R') then
        call wkvect(nuage//'.NUAV', 'V V R', length, jnuav)
    else if (type_scal(1:1) .eq. 'C') then
        call wkvect(nuage//'.NUAV', 'V V C', length, jnuav)
    else
        ASSERT(.false.)
    endif
!
! - Create .NUAL
!
    if (l_crea_nual) then
        length = nb_cmp_max * nb_point
        call wkvect(nuage//'.NUAL', 'V V L', length, jnual)
    endif
!
end subroutine

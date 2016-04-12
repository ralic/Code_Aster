    subroutine mat_proto(fami, kpg, ksp, poum, imate, itface, nprops, props)
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
!     but: Calculer les coef. materiau pour l'interface umat
!       in   fami    : famille de point de gauss (rigi,mass,...)
!       in   kpg,ksp : numero du (sous)point de gauss
!       in   poum    : '+' /'-'
!       in   nprops  : en entree : dimension du tableau props
!       in   itface  : nom de l'interface de prototypage
!       out  nprops  : en sortie : nombre de coefficients recuperes dans props
!            props(*): coeficients du materiau
! ======================================================================
    implicit none

#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/r8nnem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcadlv.h"
#include "asterfort/assert.h"
#include "asterfort/rcvarc.h"
#include "asterfort/get_meta_phasis.h"
#include "asterfort/get_meta_id.h"
#include "asterfort/get_elas_id.h"

    character(len=*), intent(in) :: fami
    integer, intent(in)          :: kpg
    integer, intent(in)          :: ksp
    character(len=1), intent(in) :: poum
    integer, intent(in)          :: imate
    character(len=*), intent(in) :: itface
    integer, intent(inout)       :: nprops
    real(kind=8), intent(out)    :: props(*)
!----------------------------------------------------------------------------

    integer      :: i, jadr,icodre, ncoef
    real(kind=8) :: rundef

!
    real(kind=8) :: phase(5), zalpha
    integer     :: elas_id, meta_id, nb_phasis
    character(len=16) :: elas_keyword
!----------------------------------------------------------------------------
    rundef=r8nnem()

!   -- Seulement UMAT et MFRONT
    ASSERT(itface.eq.'UMAT' .or. itface.eq.'MFRONT')

!   -- mise a "undef" du tableau resultat :
    ASSERT(nprops.gt.0 .and. nprops.le.197)
    call r8inir(nprops, rundef, props, 1)

!   -- recuperation des valeurs et recopie dans props :
    call get_elas_id(imate, elas_id, elas_keyword)
!    
    if (elas_keyword.eq.'ELAS_META') then
        call get_meta_id(meta_id, nb_phasis)
        call get_meta_phasis(fami     , poum  , kpg   , ksp , meta_id,&
                             nb_phasis, phase, zcold_ = zalpha)
!
        call rcadlv(fami, kpg, ksp, poum, imate, ' ', itface, &
                    'LISTE_COEF',1, ['META'], [zalpha], jadr, ncoef, icodre, 1)
    else
        call rcadlv(fami, kpg, ksp, poum, imate, ' ', itface,  &
                    'LISTE_COEF',0, [' '], [0.d0], jadr, ncoef, icodre, 1)       
    endif
!
    if (ncoef.le.nprops) then
        do i=1,ncoef
            props(i)=zr(jadr-1+i)
        enddo
        nprops=ncoef
    else
        nprops=-ncoef
        ASSERT(.false.)
    endif

    end

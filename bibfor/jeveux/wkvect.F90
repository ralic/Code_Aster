subroutine wkvect(nom, carac, dim, jadr, &
              & vl,vi,vi4,vr,vc,vk8,vk16,vk24,vk32,vk80)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
    use iso_c_binding, only:  c_loc, c_ptr, c_f_pointer
! aslint: disable=C1308
    implicit none

#include "jeveux.h"
#include "asterfort/jecreo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/assert.h"
#include "asterfort/jgetptc.h"

    character(len=*), intent(in) :: nom
    character(len=*), intent(in) :: carac
    integer, intent(in) :: dim
    integer, intent(out), optional :: jadr

    logical,           pointer, optional, intent(inout) :: vl(:)
    integer,           pointer, optional, intent(inout) :: vi(:)
    integer(kind=4),   pointer, optional, intent(inout) :: vi4(:)
    real(kind=8),      pointer, optional, intent(inout) :: vr(:)
    complex(kind=8),   pointer, optional, intent(inout) :: vc(:)
    character(len=8),  pointer, optional, intent(inout) :: vk8(:)
    character(len=16), pointer, optional, intent(inout) :: vk16(:)
    character(len=24), pointer, optional, intent(inout) :: vk24(:)
    character(len=32), pointer, optional, intent(inout) :: vk32(:)
    character(len=80), pointer, optional, intent(inout) :: vk80(:)

!
! ------------------------------------------------------------------
! Creation d'un vecteur jeveux
! ------------------------------------------------------------------
! in  nom   : ch*24 : nom du vecteur jeveux
! in  carac : ch    : descrivion des caracteristiques pour jecreo
! in  dim   : is    : taille du vecteur
! out jadr  : is    : adresse de l'objet dans ZI, ZR ...
! out vl   : l     : vecteur de logiques
! out vi   : i     : vecteur d'entiers
! ...
! Deux usages differents :
! ------------------------
! 1) recuperation de l'adresse de l'objet 'XXX' (jxxx) dans les COMMON zi, zr, ... :
!    call wkvect('XXX', 'V V R', 10, jxxx)
! 2) recuperation du contenu de l'objet 'XXX' dans le vecteur XXX
!    call wkvect('XXX', 'V V R', 10, vr=XXX)
!-----------------------------------------------------------------------
      integer :: jad
      character(len=8) :: ktyp
      type(c_ptr) :: pc
!---------------------------------------------------------------------------
      call jecreo(nom, carac)
      call jeecra(nom, 'LONMAX', ival=dim)
      call jeecra(nom, 'LONUTI', ival=dim)
      call jeveuo(nom, 'E', jad)


!     -- cas : on veut l'adresse
      if (present(jadr)) then
          jadr=jad
          goto 999
      endif


!     -- cas : on veut un pointeur
      call jelira(nom,'TYPELONG',cval=ktyp)
      if (present(vl)) then
          ASSERT(ktyp.eq.'L')
          call jgetptc(jad,pc,vl=zl(1))
          call c_f_pointer(pc,vl,[dim])

      elseif (present(vi)) then
          ASSERT(ktyp.eq.'I')
          call jgetptc(jad,pc,vi=zi(1))
          call c_f_pointer(pc,vi,[dim])

      elseif (present(vi4)) then
          ASSERT(ktyp.eq.'S')
          call jgetptc(jad,pc,vi4=zi4(1))
          call c_f_pointer(pc,vi4,[dim])

      elseif (present(vr)) then
          ASSERT(ktyp.eq.'R')
          call jgetptc(jad,pc,vr=zr(1))
          call c_f_pointer(pc,vr,[dim])

      elseif (present(vc)) then
          ASSERT(ktyp.eq.'C')
          call jgetptc(jad,pc,vc=zc(1))
          call c_f_pointer(pc,vc,[dim])

      elseif (present(vk8)) then
          ASSERT(ktyp.eq.'K8')
          call jgetptc(jad,pc,vk8=zk8(1))
          call c_f_pointer(pc,vk8,[dim])

      elseif (present(vk16)) then
          ASSERT(ktyp.eq.'K16')
          call jgetptc(jad,pc,vk16=zk16(1))
          call c_f_pointer(pc,vk16,[dim])

      elseif (present(vk24)) then
          ASSERT(ktyp.eq.'K24')
          call jgetptc(jad,pc,vk24=zk24(1))
          call c_f_pointer(pc,vk24,[dim])

      elseif (present(vk32)) then
          ASSERT(ktyp.eq.'K32')
          call jgetptc(jad,pc,vk32=zk32(1))
          call c_f_pointer(pc,vk32,[dim])

      elseif (present(vk80)) then
          ASSERT(ktyp.eq.'K80')
          call jgetptc(jad,pc,vk80=zk80(1))
          call c_f_pointer(pc,vk80,[dim])

      else
          ASSERT(.false.)
      endif

999   continue

end subroutine

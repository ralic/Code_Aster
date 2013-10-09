subroutine calirg(mesh, nbno, list_node, tran, cent,&
                  l_angl_naut, angl_naut, geom_defo, l_rota, matr_rota)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matrot.h"
#include "asterfort/parotr.h"
#include "asterfort/wkvect.h"
!
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
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nbno
    character(len=24), intent(in) :: list_node
    logical, intent(in) :: l_angl_naut
    real(kind=8), intent(in) :: angl_naut(3)
    real(kind=8), intent(in) :: cent(3)
    real(kind=8), intent(in) :: tran(3)
    character(len=*) :: geom_defo
    logical, intent(out) :: l_rota
    real(kind=8), intent(out) :: matr_rota(3, 3)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Apply transformation for translation/rotation
!
! --------------------------------------------------------------------------------------------------
!
!
! In  noma         : mesh
! In  ndim         : space dimension
! In  l_tran       : .true. if TRAN defined (translation)
! In  tran         : vector defining translation
! In  l_cent       : .true. if center defined (rotation)
! In  cent         : vector defining center
! In  l_angl_naut  : .true. if angl defined (rotation)
! In  angl_naut    : angle defining rotation
! In  nbno         : number of nodes to transform
! In  list_node    : list of nodes to transform
! In  geom_defo    : new coordinates of mesh after transformation
!                    WARNING: defined on ALL mesh nodes (>= nbno)
! Out lrota        : .true. if rotation
! Out matr_rota    : rotation matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jgeom_init, jgeom_defo
    integer :: nnomx, nume_node, jlino
    integer :: i, j, ino, idim
    real(kind=8) :: coor2(3), zero, un
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    zero = 0.00
    un = 1.d0
    l_rota = .false.
!
    do i = 1, 3
        do j = 1, 3
            if (i .eq. j) then
                matr_rota(i,i) = un
            else
                matr_rota(i,j) = zero
                matr_rota(j,i) = zero
            endif
        end do
    end do
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nnomx)
!
! - Rotation matrix
!
    if (l_angl_naut) then
        call matrot(angl_naut, matr_rota)
        l_rota = .true.
    endif
!
! - Translation
!
    call wkvect(geom_defo, 'V V R', 3*nnomx, jgeom_defo)
    call jeveuo(mesh//'.COORDO    .VALE', 'L', jgeom_init)
    call jeveuo(list_node, 'L', jlino)
!
    do ino = 1, nbno
        nume_node = zi(jlino+ino-1)
        call parotr(mesh, jgeom_init, nume_node, 0, cent,&
                    matr_rota, tran, coor2)
        do idim = 1, 3
            zr(jgeom_defo+3*(nume_node-1)+idim-1) = coor2(idim)
        enddo
    end do
!
    call jedema()
end subroutine

subroutine as_deallocate(vl, vi, vi4, vr, vc, vk8,&
                         vk16, vk24, vk32, vk80)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
#include "jeveux_private.h"
#include "asterc/hpalloc.h"
#include "asterfort/assert.h"
#include "asterfort/jeimpm.h"
#include "asterfort/jjldyn.h"
#include "asterfort/jxlocs.h"
#include "asterfort/utmess.h"
!
    logical, pointer, optional :: vl(:)
    integer, optional, pointer :: vi(:)
    integer(kind=4), optional, pointer :: vi4(:)
    real(kind=8), optional, pointer :: vr(:)
    complex(kind=8), optional, pointer :: vc(:)
    character(len=8), optional, pointer :: vk8(:)
    character(len=16), optional, pointer :: vk16(:)
    character(len=24), optional, pointer :: vk24(:)
    character(len=32), optional, pointer :: vk32(:)
    character(len=80), optional, pointer :: vk80(:)
!
! ----------------------------------------------------------------------
! DESALLOUER un vecteur de travail
!
! INOUT vl      : vecteur de logiques
! INOUT vi      : vecteur d'entiers
! INOUT vi4     : vecteur d'entiers 4
! INOUT vr      : vecteur de reels 8
! INOUT vc      : vecteur de complexes 16
! INOUT vk8     : vecteur de k8
! INOUT vk16    : vecteur de k16
! ...
! ----------------------------------------------------------------------
!
!   -- commons jeveux :
!   --------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio, cuvtrav
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2), cuvtrav
! ----------------------------------------------------------------------
    integer :: ierr, lonty, lsic, lonvec
    character(len=4) :: typv
!
! DEB ------------------------------------------------------------------
    if (present(vi)) then
        typv='I'
        lonty=lois
    else if (present(vi4)) then
        typv='S'
        lonty=4
    else if (present(vl)) then
        typv='L'
        lonty=lois
    else if (present(vr)) then
        typv='R'
        lonty=8
    else if (present(vc)) then
        typv='C'
        lonty=16
    else if (present(vk8)) then
        typv='K8'
        lonty=8
    else if (present(vk16)) then
        typv='K16'
        lonty=16
    else if (present(vk24)) then
        typv='K24'
        lonty=24
    else if (present(vk32)) then
        typv='K32'
        lonty=32
    else if (present(vk80)) then
        typv='K80'
        lonty=80
    else
        ASSERT(.false.)
    endif
!
!   -- taille du vecteur :
!   ----------------------------
    lonvec=0
    if (typv .eq. 'I') then
        if (associated(vi)) lonvec=size(vi)
    else if (typv.eq.'S') then
        if (associated(vi4)) lonvec=size(vi4)
    else if (typv.eq.'L') then
        if (associated(vl)) lonvec=size(vl)
    else if (typv.eq.'R') then
        if (associated(vr)) lonvec=size(vr)
    else if (typv.eq.'C') then
        if (associated(vc)) lonvec=size(vc)
    else if (typv.eq.'K8') then
        if (associated(vk8)) lonvec=size(vk8)
    else if (typv.eq.'K16') then
        if (associated(vk16)) lonvec=size(vk16)
    else if (typv.eq.'K24') then
        if (associated(vk24)) lonvec=size(vk24)
    else if (typv.eq.'K32') then
        if (associated(vk32)) lonvec=size(vk32)
    else if (typv.eq.'K80') then
        if (associated(vk80)) lonvec=size(vk80)
!
    else
        ASSERT(.false.)
    endif
!
!   -- on desalloue le vecteur :
!   ----------------------------
    ierr=1
    if (typv .eq. 'I') then
        if (associated(vi)) deallocate(vi, stat=ierr)
    else if (typv.eq.'S') then
        if (associated(vi4)) deallocate(vi4, stat=ierr)
    else if (typv.eq.'L') then
        if (associated(vl)) deallocate(vl, stat=ierr)
    else if (typv.eq.'R') then
        if (associated(vr)) deallocate(vr, stat=ierr)
    else if (typv.eq.'C') then
        if (associated(vc)) deallocate(vc, stat=ierr)
    else if (typv.eq.'K8') then
        if (associated(vk8)) deallocate(vk8, stat=ierr)
    else if (typv.eq.'K16') then
        if (associated(vk16)) deallocate(vk16, stat=ierr)
    else if (typv.eq.'K24') then
        if (associated(vk24)) deallocate(vk24, stat=ierr)
    else if (typv.eq.'K32') then
        if (associated(vk32)) deallocate(vk32, stat=ierr)
    else if (typv.eq.'K80') then
        if (associated(vk80)) deallocate(vk80, stat=ierr)
!
    else
        ASSERT(.false.)
    endif
!
!   -- Si le deallocate s'est bien passe, c'est que le vecteur etait alloue.
!      il faut "rendre" la memoire a JEVEUX
!   -------------------------------------------------
    if (ierr .eq. 0) then
        lsic=lonvec*lonty/lois
        mcdyn=mcdyn-lsic
        cuvtrav=cuvtrav-lsic
    endif
!
end subroutine

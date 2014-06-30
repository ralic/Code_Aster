subroutine as_allocate(size, vl, vi, vi4, vr,&
                       vc, vk8, vk16, vk24, vk32, &
                       vk80, strdbg)
use module_allocate
! person_in_charge: jacques.pellet at edf.fr
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
! aslint: disable=W0104,W1304
    implicit none
#include "asterf_debug.h"
#include "jeveux_private.h"
#include "asterfort/assert.h"
#include "asterfort/jeimpm.h"
#include "asterfort/jjldyn.h"
#include "asterfort/jxlocs.h"
#include "asterfort/utmess.h"
!
    integer :: size
    logical(kind=1),           pointer, optional, intent(out) :: vl(:)
    integer,           pointer, optional, intent(out) :: vi(:)
    integer(kind=4),   pointer, optional, intent(out) :: vi4(:)
    real(kind=8),      pointer, optional, intent(out) :: vr(:)
    complex(kind=8),   pointer, optional, intent(out) :: vc(:)
    character(len=8),  pointer, optional, intent(out) :: vk8(:)
    character(len=16), pointer, optional, intent(out) :: vk16(:)
    character(len=24), pointer, optional, intent(out) :: vk24(:)
    character(len=32), pointer, optional, intent(out) :: vk32(:)
    character(len=80), pointer, optional, intent(out) :: vk80(:)
!
    character(len=*) :: strdbg
!
! ----------------------------------------------------------------------
! ALLOUER un vecteur de travail de longueur size
!
! IN    size  : nombre d'elements du vecteur
! INOUT vl      : vecteur de logiques
! INOUT vi      : vecteur d'entiers
! INOUT vi4     : vecteur d'entiers 4
! INOUT vr      : vecteur de reels 8
! INOUT vc      : vecteur de complexes 16
! INOUT vk8     : vecteur de k8
! INOUT vk16    : vecteur de k16
! ...
! ----------------------------------------------------------------------
    integer, save :: iprem=0
    integer :: lonty, lsic, unmega, ltot, ival(4)
    character(len=3) :: tsca
    logical(kind=1) :: alloc


    if (iprem .eq. 0) then
        cuvtrav=0.d0
        iprem=1
        call init_slvec(slvec,1000)
    endif
!
    if (present(vi)) then
        tsca='I'
        lonty=lois
    else if (present(vi4)) then
        tsca='S'
        lonty=4
    else if (present(vl)) then
        tsca='L'
        lonty=lois
    else if (present(vr)) then
        tsca='R'
        lonty=8
    else if (present(vc)) then
        tsca='C'
        lonty=16
    else if (present(vk8)) then
        tsca='K8'
        lonty=8
    else if (present(vk16)) then
        tsca='K16'
        lonty=16
    else if (present(vk24)) then
        tsca='K24'
        lonty=24
    else if (present(vk32)) then
        tsca='K32'
        lonty=32
    else if (present(vk80)) then
        tsca='K80'
        lonty=80
    else
        ASSERT(.false.)
    endif
    lsic=size*lonty/lois
!
!
!   -- on verifie que le vecteur n'est pas deja alloue :
!      (on ne peut plus le faire depuis issue21985)

!   -----------------------------------------------------
    if (.false._1) then
        alloc=.false.
        if (tsca .eq. 'I') then
            alloc=associated(vi)
        else if (tsca.eq.'S') then
            alloc=associated(vi4)
        else if (tsca.eq.'L') then
            alloc=associated(vl)
        else if (tsca.eq.'R') then
            alloc=associated(vr)
        else if (tsca.eq.'C') then
            alloc=associated(vc)
        else if (tsca.eq.'K8') then
            alloc=associated(vk8)
        else if (tsca.eq.'K16') then
            alloc=associated(vk16)
        else if (tsca.eq.'K24') then
            alloc=associated(vk24)
        else if (tsca.eq.'K32') then
            alloc=associated(vk32)
        else if (tsca.eq.'K80') then
            alloc=associated(vk80)
!
        else
            ASSERT(.false.)
        endif
!       erreur de programmation (ou consequence d'un try/except dans le .comm) :
!       ASSERT(.not.alloc)
    endif
!
!
!   -- a-t-on encore de la place pour l'allocation ?
!   -------------------------------------------------
    if (mcdyn+lsic .gt. vmxdyn) then
        call jjldyn(2, -2, ltot)
        if (mcdyn+lsic .gt. vmxdyn) then
            call jjldyn(0, -1, ltot)
        endif
!       -- on depasse la limite => meme message que jjalls:
        if (mcdyn+lsic .gt. vmxdyn) then
            unmega=1048576
            ival(1)=(lsic*lois)/unmega
            ival(2)=nint(vmxdyn*lois)/unmega
            ival(3)=nint(mcdyn*lois)/unmega
            ival(4)=(ltot*lois)/unmega
            call utmess('E', 'JEVEUX_62', ni=4, vali=ival)
            call jeimpm(6)
            call utmess('F', 'JEVEUX_62', ni=4, vali=ival)
        endif
    endif
!
!   -- on alloue le vecteur et on l'initialise a "zero" :
!   -----------------------------------------------------

!   -- recherche de l'indice dans slvec :

    if (tsca .eq. 'I') then
        call allocate_slvec(lon1=size,vi=vi)
    else if (tsca.eq.'S') then
        call allocate_slvec(lon1=size,vi4=vi4)
    else if (tsca.eq.'L') then
        call allocate_slvec(lon1=size,vl=vl)
    else if (tsca.eq.'R') then
        call allocate_slvec(lon1=size,vr=vr)
    else if (tsca.eq.'C') then
        call allocate_slvec(lon1=size,vc=vc)
    else if (tsca.eq.'K8') then
        call allocate_slvec(lon1=size,vk8=vk8)
    else if (tsca.eq.'K16') then
        call allocate_slvec(lon1=size,vk16=vk16)
    else if (tsca.eq.'K24') then
        call allocate_slvec(lon1=size,vk24=vk24)
    else if (tsca.eq.'K32') then
        call allocate_slvec(lon1=size,vk32=vk32)
    else if (tsca.eq.'K80') then
        call allocate_slvec(lon1=size,vk80=vk80)
!
    else
        ASSERT(.false.)
    endif
!
    DEBUG_ALLOCATE('alloc', strdbg, size)
!   -- actualisation de mcdyn :
!   ---------------------------
    mcdyn=mcdyn+lsic
    cuvtrav=cuvtrav+lsic
!
end subroutine

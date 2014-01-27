subroutine as_allocate(size, vl, vi, vi4, vr, vc,&
                       vk8, vk16, vk24, vk32, vk80)
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
! aslint: disable=W1304
    implicit none
#include "jeveux_private.h"
#include "asterfort/assert.h"
#include "asterfort/jeimpm.h"
#include "asterfort/jjldyn.h"
#include "asterfort/jxlocs.h"
#include "asterfort/utmess.h"
!
    integer :: size
    logical, pointer, optional :: vl(:)
    integer, pointer, optional :: vi(:)
    integer(kind=4), pointer, optional :: vi4(:)
    real(kind=8), pointer, optional :: vr(:)
    complex(kind=8), pointer, optional :: vc(:)
    character(len=8), pointer, optional :: vk8(:)
    character(len=16), pointer, optional :: vk16(:)
    character(len=24), optional, pointer :: vk24(:)
    character(len=32), optional, pointer :: vk32(:)
    character(len=80), optional, pointer :: vk80(:)
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
!
!   -- commons jeveux :
!   --------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio, cuvtrav
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2), cuvtrav
! ----------------------------------------------------------------------
    integer :: lonty, lsic, ltot, ival(4), unmega
    character(len=4) :: typv
    logical :: alloc
    integer :: iprem=0
    save iprem
!
! -------------------------------------------------------------------
    if (iprem .eq. 0) then
        cuvtrav=0.d0
        iprem=1
    endif
!
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
    lsic=size*lonty/lois
!
!
!   -- on verifie que le vecteur n'est pas deja alloue :
!   -----------------------------------------------------
    alloc=.false.
    if (typv .eq. 'I') then
        alloc=associated(vi)
    else if (typv.eq.'S') then
        alloc=associated(vi4)
    else if (typv.eq.'L') then
        alloc=associated(vl)
    else if (typv.eq.'R') then
        alloc=associated(vr)
    else if (typv.eq.'C') then
        alloc=associated(vc)
    else if (typv.eq.'K8') then
        alloc=associated(vk8)
    else if (typv.eq.'K16') then
        alloc=associated(vk16)
    else if (typv.eq.'K24') then
        alloc=associated(vk24)
    else if (typv.eq.'K32') then
        alloc=associated(vk32)
    else if (typv.eq.'K80') then
        alloc=associated(vk80)
!
    else
        ASSERT(.false.)
    endif
! erreur de programmation :
    ASSERT(.not.alloc)
!
!
!   -- a-t-on encore de la place pour l'allocation ?
!   -------------------------------------------------
    if (mcdyn+lsic .gt. vmxdyn) then
        call jjldyn(2, -2, ltot)
        if (mcdyn+lsic .gt. vmxdyn) then
            call jjldyn(0, -1, ltot)
        endif
! on depasse la limite => meme message que jjalls:
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
    if (typv .eq. 'I') then
        allocate(vi(size))
        vi(:)=0
    else if (typv.eq.'S') then
        allocate(vi4(size))
        vi4(:)=0
    else if (typv.eq.'L') then
        allocate(vl(size))
        vl(:)=.false.
    else if (typv.eq.'R') then
        allocate(vr(size))
        vr(:)=0.d0
    else if (typv.eq.'C') then
        allocate(vc(size))
        vc(:)=dcmplx(0.d0,0.d0)
    else if (typv.eq.'K8') then
        allocate(vk8(size))
        vk8(:)=' '
    else if (typv.eq.'K16') then
        allocate(vk16(size))
        vk16(:)=' '
    else if (typv.eq.'K24') then
        allocate(vk24(size))
        vk24(:)=' '
    else if (typv.eq.'K32') then
        allocate(vk32(size))
        vk32(:)=' '
    else if (typv.eq.'K80') then
        allocate(vk80(size))
        vk80(:)=' '
!
    else
        ASSERT(.false.)
    endif
!
!   -- actualisation de mcdyn :
!   ---------------------------
    mcdyn=mcdyn+lsic
    cuvtrav=cuvtrav+lsic
!
end subroutine

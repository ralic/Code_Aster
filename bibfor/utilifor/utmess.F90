subroutine utmess(typ, idmess, nk, valk, sk, &
                  ni, vali, si, nr, valr, &
                  sr, num_except, fname)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mathieu.courtois at edf.fr
!
! All messages (informations, warnings, errors) should be printed through this subroutine.
! Only the first two arguments are compulsory.
! Example: call utmess('A', 'SUPERVIS_1')
!
! To pass a single value, just use sk/si/sr=value.
! To pass more values, use nk/ni/nr=<number of values> + valk/vali/valr=<array of values>.
! Example: call utmess('A', 'MECANONLINE_34', nr=2, valr=[a, b])
!
! If 'fname' is provided, it must be valid filename and the message will be written
! into this file instead of standard MESSAGE, RESULTAT, ERREUR files.
!
! See comments in utmess_core for details
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/utmess_core.h"
!
    character(len=*), intent(in) :: typ
    character(len=*), intent(in) :: idmess
    integer, intent(in), optional :: nk
    character(len=*), intent(in), optional, target :: valk(*)
    character(len=*), intent(in), optional :: sk
    integer, intent(in), optional :: ni
    integer, intent(in), optional, target :: vali(*)
    integer, intent(in), optional :: si
    integer, intent(in), optional :: nr
    real(kind=8), intent(in), optional, target :: valr(*)
    real(kind=8), intent(in), optional :: sr
    integer, intent(in), optional :: num_except
    character(len=*), optional :: fname
!
!   common used to pass the exception number
!   TODO: add this argument to utmess_core
    integer :: nexcep
    common /utexc/ nexcep
!
!   working variables
    integer :: unk, uni, unr
    character(len=256), target :: uvk(1)
!   because it is not supported by older versions of gfortran, we use two different
!   calls to utmess_core
!    character(len=:), pointer :: ptrk(:)
    logical :: use_valk
    integer, target :: uvi(1)
    integer, pointer :: ptri(:)
    real(kind=8), target :: uvr(1)
    real(kind=8), pointer :: ptrr(:)
    character(len=256) :: ufname
!
    ASSERT(ENSEMBLE2(nk,valk))
    ASSERT(ENSEMBLE2(ni,vali))
    ASSERT(ENSEMBLE2(nr,valr))
    ASSERT(EXCLUS2(valk,sk))
    ASSERT(EXCLUS2(vali,si))
    ASSERT(EXCLUS2(valr,sr))
    ASSERT(absent(num_except) .or. typ == 'Z')
    if (present(num_except)) then
        nexcep = num_except
    endif
!   associate pointers to valk or sk
    unk = 1
    uvk(1) = ' '
    use_valk = .false.
    if (AU_MOINS_UN2(sk,valk)) then
        if (present(nk)) then
            unk = nk
            use_valk = .true.
        else
            unk = 1
            uvk(1) = sk
        endif
    endif
!   associate pointers to vali or si
    uni = 1
    uvi(1) = 0
    ptri => uvi(1:1)
    if (AU_MOINS_UN2(si,vali)) then
        if (present(ni)) then
            uni = ni
            ptri => vali(1:ni)
        else
            uni = 1
            uvi(1) = si
            ptri => uvi(1:1)
        endif
    endif
!   associate pointers to valr or sr
    unr = 1
    uvr(1) = 0.d0
    ptrr => uvr(1:1)
    if (AU_MOINS_UN2(sr,valr)) then
        if (present(nr)) then
            unr = nr
            ptrr => valr(1:nr)
        else
            unr = 1
            uvr(1) = sr
            ptrr => uvr(1:1)
        endif
    endif
!
    ufname = ' '
    if (present(fname)) then
        ufname = fname
    endif
!
    if (use_valk) then
        call utmess_core(typ, idmess, unk, valk, uni, &
                         ptri, unr, ptrr, ufname)
    else
        call utmess_core(typ, idmess, unk, uvk, uni, &
                         ptri, unr, ptrr, ufname)
    endif
!
end subroutine utmess

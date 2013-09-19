subroutine verift(fami, kpg, ksp, poum, imate,&
                  materi, compor, iret, ndim, epsth,&
                  vepsth, tmoins, tplus, trefer)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: fami, poum, compor
    character(len=8), intent(in) :: materi
    integer, intent(in) :: kpg, ksp, imate
    integer, optional, intent(in) :: ndim
    integer, intent(out) :: iret
    real(kind=8), optional, intent(out) :: epsth
    real(kind=8), optional, intent(out) :: vepsth(*)
    real(kind=8), optional, intent(out) :: tmoins, tplus, trefer
!
! --------------------------------------------------------------------------------------------------
!
!  FAMI     : famille de points de gauss
!  KPG      : numero du point de gauss
!  KSP      : numero du sous-point de gauss
!  POUM     : '+' si temperature en temps +
!             '-' si temperature en temps -
!             'T' si temperature en temps + et -
! IMATE     : materiau
! COMPOR    : comportement
! NDIM      : 1 si isotrope
!             2 si isotrope transverse (ou metallurgique)
!             3 si orthotrope
! EPSTH     : incrément de dilatation thermique si poum=T
!             déformation thermique si poum = + ou -
! IRET      : code retour concernant la temperature 0 si ok
!                                                   1 si nook
!
! --------------------------------------------------------------------------------------------------
!
    integer :: codrem(3), codrep(3), ndimloc
    character(len=8) :: nomres(3), valek(2)
    integer :: iret1, iret2, iret3, ind, somire, iadzi, iazk24
    real(kind=8) :: tm, tref, tp, valrep(3), valrem(3), tpoum, epsth3(3)
! --------------------------------------------------------------------------------------------------
    if ( present(ndim) ) then
        ASSERT( present(vepsth) )
        ASSERT( .not.present(epsth) )
        ndimloc = ndim
    else
        ASSERT( present(epsth) )
        ASSERT( .not.present(vepsth) )
        ndimloc = 1
    endif
!
    iret = 0
    iret1 = 0
    iret2 = 0
    iret3 = 0
!   s'il n'y a pas de température, epsth=0.
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret1)
    if (iret1 .ne. 0) then
        do ind = 1, ndimloc
            epsth3(ind) = 0.d0
        enddo
        iret = 1
        goto 9999
    endif
!
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret1)
    if (iret1 .eq. 1) then
        call tecael(iadzi, iazk24)
        valek(1) = zk24(iazk24-1+3) (1:8)
        call utmess('F', 'CALCULEL_8', sk=valek(1))
    endif
    if (present(trefer)) then
        trefer = tref
    endif
!
    if (compor .eq. 'ELAS_META') then
        if (ndimloc .eq. 2) then
            nomres(1) = 'C_ALPHA'
            nomres(2) = 'F_ALPHA'
        else
            ASSERT(.false.)
        endif
    else
        if      (ndimloc.eq.1) then
            nomres(1) = 'ALPHA'
        else if (ndimloc.eq.2) then
            nomres(1) = 'ALPHA_L'
            nomres(2) = 'ALPHA_N'
        else if (ndimloc.eq.3) then
            nomres(1) = 'ALPHA_L'
            nomres(2) = 'ALPHA_T'
            nomres(3) = 'ALPHA_N'
        else
            ASSERT(.false.)
        endif
    endif
!
    if (poum .eq. 'T') then
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, tm, iret2)
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    materi, compor, 0, ' ', [0.d0],&
                    ndimloc, nomres, valrem, codrem, 0)
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, tp, iret3)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    materi, compor, 0, ' ', [0.d0],&
                    ndimloc, nomres, valrep, codrep, 0)
!
        somire = iret2 + iret3
        if (somire .eq. 0) then
            do ind = 1, ndimloc
                if ((codrem(ind).ne.0) .or. (codrep(ind).ne.0)) then
                    call tecael(iadzi, iazk24)
                    valek(1)= zk24(iazk24-1+3) (1:8)
                    valek(2)=nomres(ind)
                    call utmess('F', 'CALCULEL_32', nk=2, valk=valek)
                endif
            enddo
!
            do ind = 1, ndimloc
                epsth3(ind) = valrep(ind)*(tp-tref)-valrem(ind)*(tm- tref)
            enddo
!
            if (present(tmoins)) then
                tmoins = tm
            endif
            if (present(tplus)) then
                tplus = tp
            endif
        else
            do ind = 1, ndimloc
                epsth3(ind) = 0.d0
            enddo
        endif
    else
        call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                    ksp, tpoum, iret2)
        call rcvalb(fami, kpg, ksp, poum, imate,&
                    materi, compor, 0, ' ', [0.d0],&
                    ndimloc, nomres, valrem, codrem, 0)
!
        if (iret2 .eq. 0) then
            do ind = 1, ndimloc
                if (codrem(ind) .ne. 0) then
                    call tecael(iadzi, iazk24)
                    valek(1)= zk24(iazk24-1+3) (1:8)
                    valek(2)=nomres(ind)
                    call utmess('F', 'CALCULEL_32', nk=2, valk=valek)
                endif
            enddo
!
            do ind = 1, ndimloc
                epsth3(ind) = valrem(ind)*(tpoum-tref)
            enddo
!
            if ( (poum.eq.'-') .and. present(tmoins) ) then
                tmoins = tpoum
            endif
            if ( (poum.eq.'+') .and. present(tplus)) then
                tplus = tpoum
            endif
        else
            do ind = 1, ndimloc
                epsth3(ind) = 0.d0
            enddo
        endif
    endif
!
    if ((iret2+iret3) .ge. 1) iret = 1
!
9999  continue
    if ( present(ndim) ) then
        vepsth(1:ndim) = epsth3(1:ndim)
    else
        epsth = epsth3(1)
    endif
end subroutine

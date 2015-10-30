subroutine dtmproj(sd_dtm_, sd_int_, oldcase, buffdtm, buffint)
    implicit none
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
! person_in_charge: hassan.berro at edf.fr
!
! dtmproj : Project the integration state onto the basis for the non-linear case
!           in sd_dtm/NL_CASE, given that the preceding case is oldcase 
! 
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmupmat.h"
#include "asterfort/intbuff.h"
#include "asterfort/intget.h"
#include "asterfort/intinivec.h"
#include "asterfort/intsav.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pmavec.h"
#include "asterfort/pmtvec.h"
#include "asterfort/prmama.h"
#include "asterfort/trlds.h"
#include "asterfort/transp.h"
#include "asterfort/rrlds.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "blas/dcopy.h"

!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer          , intent(in) :: oldcase
    integer, pointer              :: buffdtm(:)
    integer, pointer              :: buffint(:)
!
!   -0.2- Local variables
    integer               :: newcase, nbmode, i, j
    integer               :: level, iret, lvec, iret2
    character(len=7)      :: case0k7, case1k7
    character(len=8)      :: sd_dtm, sd_int
    character(len=24)     :: mat_jv, invphi_jv, fadd_jv

    real(kind=8), pointer :: phi0_v(:)     => null()
    real(kind=8), pointer :: phi1_v(:)     => null()
    real(kind=8), pointer :: transd_v(:)   => null()
    real(kind=8), pointer :: transf_v(:)   => null()
    real(kind=8), pointer :: field(:)      => null()
    real(kind=8), pointer :: resfield(:)   => null()
    real(kind=8), pointer :: mat(:)        => null()
    real(kind=8), pointer :: mat_a(:)      => null()
    real(kind=8), pointer :: mat_a_0(:)    => null()
    real(kind=8), pointer :: fnl_add(:)    => null()
    real(kind=8), pointer :: mgen(:)       => null()
    real(kind=8), pointer :: kgen(:)       => null()
    real(kind=8), pointer :: agen(:)       => null()

    real(kind=8), pointer :: phi1t_phi1_f(:)     => null()
    real(kind=8), pointer :: phi1t_phi1_f_inv(:) => null()

#define ptpi(row,col) phi1t_phi1_f_inv((row-1)*nbmode+col)

!
!   0 - Initializations
    sd_dtm  = sd_dtm_
    sd_int  = sd_int_
!
    call dtmget(sd_int, _NL_CASE, iscal=newcase, buffer=buffdtm)
    call dtmget(sd_int, _NB_MODES, iscal=nbmode, buffer=buffdtm)

    call dtmcase_coder(newcase, case1k7)
    call jeveuo(sd_dtm // '.PRJ_BAS.' //case1k7, 'E', vr=phi1_v)

    ! write(*,*) '*******************************************************************************'
    ! write(*,*) ' PRJ_BAS >', case1k7, '< =', phi1_v

!
!   1 - According to oldcase, determine the transformation basis for displacement vectors
!       - if oldcase == 0 (no chocs), then use A = [Phi1]_t . ( [Phi1]_t . [Phi1] ) ^ -1
!       - if oldcase != 0 (a previous projection is used), then use A x [Phi0]
!
!   --- Calculate A = [Phi1]_t . ( [Phi1]_t . [Phi1] ) ^ -1

    invphi_jv = sd_dtm //'.INV_PHI.'//case1k7
    call jeexin(invphi_jv, iret)
    if (iret.eq.0) then
        call wkvect(invphi_jv, 'V V R', nbmode*nbmode, vr=mat_a)

        AS_ALLOCATE(vr=phi1t_phi1_f, size=nbmode*nbmode)
        AS_ALLOCATE(vr=phi1t_phi1_f_inv, size=nbmode*nbmode)
        do i = 1, nbmode
            ptpi(i,i) = 1.d0
            do j = i+1, nbmode
                ptpi(i,j) = 0.d0
            end do
        end do
        call prmama(3, phi1_v, nbmode, nbmode, nbmode,&
                    phi1_v, nbmode, nbmode, nbmode, phi1t_phi1_f,&
                    nbmode, nbmode, nbmode, iret2)
        call trlds(phi1t_phi1_f, nbmode, nbmode, iret2)
        if (iret2.ne.0) then
            ! write(*,*) "Error in triangularizing matrix Phi-t Phi (basis) on line", iret2
            ! write(*,*) "phi1_v = ", phi1_v
            ! write(*,*) "phi1t_phi1_f = ", phi1t_phi1_f
            ASSERT(.false.)
        end if
        call rrlds(phi1t_phi1_f, nbmode, nbmode, phi1t_phi1_f_inv, nbmode)
        call prmama(4, phi1t_phi1_f_inv, nbmode, nbmode, nbmode,&
                    phi1_v, nbmode, nbmode, nbmode, mat_a,&
                    nbmode, nbmode, nbmode, iret) 
        AS_DEALLOCATE(vr=phi1t_phi1_f)
        AS_DEALLOCATE(vr=phi1t_phi1_f_inv)
    else
        call jeveuo(invphi_jv, 'E', vr=mat_a)
    end if

    if (oldcase .eq. 0) then
        call jeveuo(invphi_jv, 'E', vr=transd_v)
    else
        call dtmcase_coder(oldcase, case0k7)
        call jeveuo(sd_dtm // '.PRJ_BAS.' //case0k7, 'E', vr=phi0_v)
        AS_ALLOCATE(nbmode*nbmode, vr=transd_v)
        ! write(*,*) ' old PRJ_BAS >', case0k7, '< =', phi0_v

        call prmama(1, mat_a, nbmode, nbmode, nbmode,&
                    phi0_v, nbmode, nbmode, nbmode, transd_v,&
                    nbmode, nbmode, nbmode, iret)
        ! write(*,*) ' phi1_v^(-1) x phi0_v = ', transd_v
    end if

    ! write(*,*) '*******************************************************************************'
    ! write(*,*) ' DEPL/VITE/etc. transformation basis : ', transd_v

!   2 - According to oldcase, determine the transformation basis for forces
!       - if oldcase == 0 (no chocs), then use [Phi1]_t
!       - if oldcase != 0 (a previous projection is used), then use [Phi1]_t x B
!            where B = [Phi0] . ( [Phi0] . [Phi0]_t ) ^ -1
!
!   --- Note that B = ([Phi0]_t)^-1 = (A0)_t since we always have a square <Phi> matrix

    if (oldcase .eq. 0) then
        call jeveuo(sd_dtm // '.PRJ_BAS.' //case1k7, 'E', vr=transf_v)
    else
        AS_ALLOCATE(nbmode*nbmode, vr=transf_v)
        call jeveuo(sd_dtm //'.INV_PHI.'//case0k7, 'E', vr=mat_a_0)
        call prmama(1, mat_a_0, nbmode, nbmode, nbmode,&
                    phi1_v, nbmode, nbmode, nbmode, transf_v,&
                    nbmode, nbmode, nbmode, iret)
    end if

    ! write(*,*) ' Force transformation basis : ', transf_v
    ! write(*,*) '*******************************************************************************'

!   3 - Update all DEPL, VITE, ACCE fields in sd_int
!
    call intget(sd_int, IND_ARCH, iscal=level, buffer=buffint)
    AS_ALLOCATE(nbmode, vr=field)
    do i = 1, level
        call intget(sd_int, DEPL, iocc=i, rvect=field, buffer=buffint)
        call intget(sd_int, DEPL, iocc=i, vr=resfield, buffer=buffint)
        call pmavec('ZERO', nbmode, transd_v, field, resfield)
        ! nullify(resfield)

        call intget(sd_int, VITE, iocc=i, rvect=field, buffer=buffint)
        call intget(sd_int, VITE, iocc=i, vr=resfield, buffer=buffint)
        call pmavec('ZERO', nbmode, transd_v, field, resfield)
        ! nullify(resfield)

        call intget(sd_int, ACCE, iocc=i, rvect=field, buffer=buffint)
        call intget(sd_int, ACCE, iocc=i, vr=resfield, buffer=buffint)
        call pmavec('ZERO', nbmode, transd_v, field, resfield)    
        ! nullify(resfield)

        call intget(sd_int, FORCE_EX, iocc=i, rvect=field, buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=i, vr=resfield, buffer=buffint)
        call pmtvec('ZERO', nbmode, transf_v, field, resfield)
        ! nullify(resfield)
    end do

    AS_DEALLOCATE(vr=field)
    fadd_jv = sd_dtm // '.ADDED_F.'//case1k7
    call jeveuo(fadd_jv, 'E', vr=field)
    call dtmget(sd_dtm, _F_NL_ADD, vr=fnl_add, buffer=buffdtm)
    call pmtvec('ZERO', nbmode, transf_v, field, fnl_add)
    call dcopy(nbmode, fnl_add, 1, field, 1)

!   4 - Update matrices
!
    call intget(sd_int, MASS_FUL, iocc=1, savejv=mat_jv, lonvec=iret, buffer=buffint)
    if (iret.gt.0) call jedetr(mat_jv)
    call intget(sd_int, RIGI_FUL, iocc=1, savejv=mat_jv, lonvec=iret, buffer=buffint)
    if (iret.gt.0) call jedetr(mat_jv)

    if (newcase .ne. 0) then
        call jeveuo(sd_dtm // '.PRJ_MAS.' //case1k7, 'L', vr=mat)
        call intsav(sd_int, MASS_DIA, nbmode, iocc=1, rvect=mat)
        nullify(mat)
        call jeveuo(sd_dtm // '.PRJ_RIG.' //case1k7, 'L', vr=mat)
        call intsav(sd_int, RIGI_DIA, nbmode, iocc=1, rvect=mat)
        nullify(mat)
!       --- SINCE MASS = IDENTITY MATRIX => NO NEED TO MULTIPLY BY M^-1 FOR AMOR_DIA
        call jeveuo(sd_dtm // '.PRJ_AMO.' //case1k7, 'L', vr=mat)
        call intsav(sd_int, AMOR_DIA, nbmode, iocc=1, rvect=mat)
        nullify(mat)

        if (nbmode .gt. 1) then
            call jeveuo(sd_dtm // '.PRJ_AM2.' //case1k7, 'L', vr=mat)
            call intsav(sd_int, AMOR_FUL, nbmode*nbmode, iocc=1, rvect=mat, buffer=buffint)
        else
            call intget(sd_int, AMOR_FUL, iocc=1, savejv=mat_jv, lonvec=iret, buffer=buffint)
            if (iret.gt.0) call jedetr(mat_jv)
        end if
    else 
        call intget(sd_int, AMOR_FUL, iocc=1, savejv=mat_jv, lonvec=iret, buffer=buffint)
        if (iret.gt.0) call jedetr(mat_jv)

!        --- Mass
         call dtmget(sd_dtm, _MASS_FUL, lonvec=lvec)
         if (lvec.gt.0) then
            call intinivec(sd_int, MASS_FUL, lvec, iocc=1, vr=mgen)
            call dtmget(sd_dtm, _MASS_FUL, rvect=mgen, buffer=buffdtm)
         endif
         call dtmget(sd_dtm, _MASS_DIA, lonvec=lvec, buffer=buffdtm)
         call intget(sd_int, MASS_DIA, iocc=1, vr=mgen)
         call dtmget(sd_dtm, _MASS_DIA, rvect=mgen, buffer=buffdtm)
!
!       --- Stifness
        call dtmget(sd_dtm, _RIGI_FUL, lonvec=lvec)
        if (lvec.gt.0) then
            call intinivec(sd_int, RIGI_FUL, lvec, iocc=1, vr=kgen)
            call dtmget(sd_dtm, _RIGI_FUL, rvect=kgen, buffer=buffdtm)
        endif
        call dtmget(sd_dtm, _RIGI_DIA, lonvec=lvec, buffer=buffdtm)
        call intget(sd_int, RIGI_DIA, iocc=1, vr=kgen)
        call dtmget(sd_dtm, _RIGI_DIA, rvect=kgen, buffer=buffdtm)
!
!       --- Damping
        call dtmget(sd_dtm, _AMOR_FUL, lonvec=lvec)
        if (lvec.gt.0) then
            call intinivec(sd_int, AMOR_FUL, lvec, iocc=1, vr=agen)
            call dtmget(sd_dtm, _AMOR_FUL, rvect=agen, buffer=buffdtm)
        endif
        call dtmget(sd_dtm, _AMOR_DIA, lonvec=lvec)
        call intinivec(sd_int, AMOR_DIA, lvec, iocc=1, vr=agen)
        call dtmget(sd_dtm, _AMOR_DIA, rvect=agen, buffer=buffdtm)
    end if

    nullify(buffint)
    call intbuff(sd_int, buffint, level=level)

    ! call utimsd(6, 2, .false._1, .true._1, sd_int, 1, 'V')

    call intsav(sd_int, MAT_UPDT, 1, iscal=1, buffer=buffint)

    if (oldcase .ne. 0) then
        AS_DEALLOCATE(vr=transd_v)
        AS_DEALLOCATE(vr=transf_v)
    end if

end subroutine
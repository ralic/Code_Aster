subroutine dtmforc_choc(nl_ind , sd_dtm_, sd_nl_, buffdtm, buffnl,&
                        time   , depl  , vite, fext_nl, fext_tgt)
    implicit none
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmforc_choc : Calculates the stop/choc forces at the current step (t)
!
!       nl_ind           : nonlinearity index (for sd_nl access)
!       sd_dtm_, buffdtm : dtm data structure and its buffer
!       sd_nl_ , buffnl  : nl  data structure and its buffer
!       time             : current instant t
!       depl, vite       : structural modal displacement and velocity at "t"
!       fext_nl          : projected total non-linear force
!       fext_tgt         : projected tangential component of the fext_nl
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/distno.h"
#include "asterfort/dtmget.h"
#include "asterfort/fnorm.h"
#include "asterfort/fointe.h"
#include "asterfort/ftang.h"
#include "asterfort/gloloc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/locglo.h"
#include "asterfort/nlget.h"
#include "asterfort/nlsav.h"
#include "asterfort/tophys.h"
#include "asterfort/tophys_ms.h"
#include "asterfort/togene.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

!
!   -0.1- Input/output arguments
    integer               , intent(in)  :: nl_ind
    character(len=*)      , intent(in)  :: sd_dtm_
    character(len=*)      , intent(in)  :: sd_nl_
    integer     , pointer , intent(in)  :: buffdtm  (:)
    integer     , pointer , intent(in)  :: buffnl   (:)
    real(kind=8)          , intent(in)  :: time
    real(kind=8), pointer , intent(in)  :: depl     (:)
    real(kind=8), pointer , intent(in)  :: vite     (:)
    real(kind=8), pointer , intent(out) :: fext_nl  (:)
    real(kind=8), pointer , intent(out) :: fext_tgt (:)
!
!   -0.2- Local variables
    aster_logical     :: multi_support
    integer           :: i, iex, nbexci, ier, par_coorno(2)
    integer           :: nbno, ino, adherance, nbmode, im
    integer           :: start, finish
    real(kind=8)      :: sina, cosa, sinb, cosb, sing
    real(kind=8)      :: cosg, depglo(3), vitglo(3), deploc(6), vitloc(6)
    real(kind=8)      :: ddeplo(3), dvitlo(3), xjeu, knorm, cnorm
    real(kind=8)      :: ktang, cfrotd, cfrots, dist1, dist2
    real(kind=8)      :: ctang, dnorm, cost, sint, fn
    real(kind=8)      :: flocal(3), fgloba(3), eps, oldft(2), vnorm
    real(kind=8)      :: oldxl(3), oldvt(2), ftange(2), vtang(2)
    character(len=8)  :: sd_dtm, sd_nl, monmot, obst_typ
    character(len=19) :: nomres
!
    integer         , pointer :: vindx(:)    => null()
    real(kind=8)    , pointer :: coor_no(:)  => null()
    real(kind=8)    , pointer :: origob (:)  => null()
    real(kind=8)    , pointer :: coedep (:)  => null()
    real(kind=8)    , pointer :: coevit (:)  => null()
    real(kind=8)    , pointer :: psidel (:)  => null()
    real(kind=8)    , pointer :: psidel1(:)  => null()
    real(kind=8)    , pointer :: psidel2(:)  => null()
    real(kind=8)    , pointer :: sincos_angle_a(:) => null()
    real(kind=8)    , pointer :: sincos_angle_b(:) => null()
    real(kind=8)    , pointer :: sincos_angle_g(:) => null()
    real(kind=8)    , pointer :: sign_dyz(:)   => null()
    real(kind=8)    , pointer :: dplmod (:)   => null()
    real(kind=8)    , pointer :: dplmod1(:)   => null()
    real(kind=8)    , pointer :: dplmod2(:)   => null()
    real(kind=8)    , pointer :: vint(:)      => null()
    character(len=8), pointer :: nofdep(:)  => null()
    character(len=8), pointer :: nofvit(:)  => null()
!
    data par_coorno  /_COOR_NO1, _COOR_NO2/
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_nl  = sd_nl_
    eps = r8prem()
!
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)
!
    call nlget(sd_nl, _INTERNAL_VARS      , vr=vint, buffer=buffnl)
    call nlget(sd_nl, _INTERNAL_VARS_INDEX, vi=vindx, buffer=buffnl)
    start  = vindx(nl_ind)
!
    call vecini(nbmode, 0.d0, fext_nl)
    call vecini(nbmode, 0.d0, fext_tgt)
!
    adherance   = 0
    vnorm       = 0.d0
    fn          = 0.d0
    oldft(1:2)  = 0.d0
    oldxl(1:3)  = 0.d0
    oldvt(1:2)  = 0.d0
    deploc(1:6) = 0.d0
    ftange(1:2) = 0.d0
    vtang (1:2) = 0.d0
!
!
    call dtmget(sd_dtm, _MULTI_AP, kscal=monmot, buffer=buffdtm)
    multi_support = monmot(1:3) .eq. 'OUI'
    if (multi_support) then
        call dtmget(sd_dtm, _CALC_SD , kscal=nomres, buffer=buffdtm)
        call dtmget(sd_dtm, _NB_EXC_T, iscal=nbexci, buffer=buffdtm)

        call jeveuo(nomres//'.FDEP','L', vk8=nofdep)
        call jeveuo(nomres//'.FVIT','L', vk8=nofvit)

        AS_ALLOCATE(vr=coedep, size=nbexci)
        AS_ALLOCATE(vr=coevit, size=nbexci)
        do iex = 1, nbexci
            coedep(iex) = 0.d0
            coevit(iex) = 0.d0
            if (nofdep(iex) .ne. ' ') then
                call fointe('F', nofdep(iex), 1, ['INST'], [time],&
                            coedep(iex), ier)
            endif
            if (nofvit(iex) .ne. ' ') then
                call fointe('F', nofvit(iex), 1, ['INST'], [time],&
                            coevit(iex), ier)
            endif
        enddo
    endif

    call nlget(sd_nl, _COOR_ORIGIN_OBSTACLE, iocc=nl_ind, vr=origob        , buffer=buffnl)
    call nlget(sd_nl, _SINCOS_ANGLE_A      , iocc=nl_ind, vr=sincos_angle_a, buffer=buffnl)
    call nlget(sd_nl, _SINCOS_ANGLE_B      , iocc=nl_ind, vr=sincos_angle_b, buffer=buffnl)
    call nlget(sd_nl, _SINCOS_ANGLE_G      , iocc=nl_ind, vr=sincos_angle_g, buffer=buffnl)
    call nlget(sd_nl, _SIGN_DYZ            , iocc=nl_ind, vr=sign_dyz      , buffer=buffnl)
    sina = sincos_angle_a(1)
    cosa = sincos_angle_a(2)
    sinb = sincos_angle_b(1)
    cosb = sincos_angle_b(2)
    sing = sincos_angle_g(1)
    cosg = sincos_angle_g(2)

    nbno = 1
    call nlget(sd_nl, _OBST_TYP      , iocc=nl_ind, kscal=obst_typ, buffer=buffnl)
    call nlget(sd_nl, _MODAL_DEPL_NO1, iocc=nl_ind, vr=dplmod1, buffer=buffnl)
    if (multi_support) call nlget(sd_nl, _PSI_DELT_NO1, vr=psidel1, buffer=buffnl)

    if (obst_typ(1:2).eq.'BI') then
        nbno = 2
        call nlget(sd_nl, _MODAL_DEPL_NO2, iocc=nl_ind, vr=dplmod2, buffer=buffnl)
        if (multi_support) call nlget(sd_nl, _PSI_DELT_NO2, vr=psidel2, buffer=buffnl)
    end if

    do ino = 1, nbno
!       --- Point toward the modal displacement for the concerned node / 1 or 2 /
        dplmod => dplmod1
        if (multi_support) psidel => psidel1
        if (ino.eq.2) then
            dplmod => dplmod2
            if (multi_support) psidel => psidel2
        end if

!       --- Conversion of generalized displacements/velocities
!           back to the physical (global) basis
        if (multi_support) then
            call tophys_ms(dplmod, psidel, coedep, depl, depglo)
            call tophys_ms(dplmod, psidel, coevit, vite, vitglo)
        else
            call tophys(dplmod, depl, depglo)
            call tophys(dplmod, vite, vitglo)        
        endif

        nullify(coor_no)
        call nlget(sd_nl, par_coorno(ino), iocc=nl_ind, vr=coor_no, buffer=buffnl)
        do i = 1, 3
            depglo(i) = depglo(i) + coor_no(i)
        end do

    !   --- Conversion of these vectors to the local basis
        call gloloc(depglo,origob          ,sina,cosa,sinb,cosb,sing,cosg, deploc(1+(ino-1)*3))
        call gloloc(vitglo,[0.d0,0.d0,0.d0],sina,cosa,sinb,cosb,sing,cosg, vitloc(1+(ino-1)*3))
    end do

    if (nbno.eq.2) then
        do i = 1, 3
            ddeplo(i) = deploc(i) - deploc(3+i)
            dvitlo(i) = vitloc(i) - vitloc(3+i)
        end do
    else 
        do i = 1, 3
            ddeplo(i) = deploc(i)
            dvitlo(i) = vitloc(i)
        end do
    end if
!
    call nlget(sd_nl, _GAP            , iocc=nl_ind, rscal=xjeu  , buffer=buffnl)
    call nlget(sd_nl, _STIF_NORMAL    , iocc=nl_ind, rscal=knorm , buffer=buffnl)
    call nlget(sd_nl, _DAMP_NORMAL    , iocc=nl_ind, rscal=cnorm , buffer=buffnl)
    call nlget(sd_nl, _RIGI_TANGENTIAL, iocc=nl_ind, rscal=ktang , buffer=buffnl)
    call nlget(sd_nl, _DAMP_TANGENTIAL, iocc=nl_ind, rscal=ctang , buffer=buffnl)
    call nlget(sd_nl, _FRIC_DYNAMIC   , iocc=nl_ind, rscal=cfrotd, buffer=buffnl)
    call nlget(sd_nl, _FRIC_STATIC    , iocc=nl_ind, rscal=cfrots, buffer=buffnl)
    call nlget(sd_nl, _DIST_NO1       , iocc=nl_ind, rscal=dist1 , buffer=buffnl)
    call nlget(sd_nl, _DIST_NO2       , iocc=nl_ind, rscal=dist2 , buffer=buffnl)
!
    dnorm = 0.d0
    call distno(deploc, sign_dyz, obst_typ, xjeu, dist1,&
                dist2, dnorm, cost, sint)
!
    if (dnorm .le. 0.d0) then
        call vecini(3, 0.d0, fgloba)
        call vecini(3, 0.d0, flocal)

!       --- Calculation of the normal force in the local reference
        call fnorm(dnorm, dvitlo, knorm, cnorm, cost,&
                   sint, fn, flocal, vnorm)


!       --- Conversion to the global (physical) reference
        call locglo(flocal, sina, cosa, sinb, cosb,&
                    sing, cosg, fgloba)

!       --- Generalized force on the first node  
        call togene(dplmod1, fgloba, fext_nl)
!       --- Generalized force on the second node
        if (nbno.eq.2) then
            call togene(dplmod2, fgloba, fext_nl, coef=-1.d0)
        endif

!       --- Friction
        if (( abs(cfrots) .gt. eps ) .or. ( abs(cfrotd) .gt. eps )) then

!           --- Very elevated velocities => divergence detected
            if (max(dvitlo(1),dvitlo(2),dvitlo(3)).gt.(0.5d0*sqrt(r8maem()))) then
                call utmess('F', 'ALGORITH17_1', sr=time)
            end if

!           --- Internal variables, retrieval           
            adherance = nint(vint(start+12))
            oldft(1)  =      vint(start+13)
            oldft(2)  =      vint(start+14)
            oldxl(1)  =      vint(start+15)
            oldxl(2)  =      vint(start+16)
            oldxl(3)  =      vint(start+17)
            oldvt(1)  =      vint(start+18)
            oldvt(2)  =      vint(start+19)

!           --- Calculation of the tangential (frictional) force
            call ftang(fn, ddeplo, dvitlo, cfrotd, cfrots, &
                       ktang, ctang, adherance, oldvt, oldft,&
                       oldxl, cost, sint, ftange, flocal, vtang)          

!       --- Conversion to the global (physical) reference
            call locglo(flocal, sina, cosa, sinb, cosb,&
                        sing, cosg, fgloba)
!       --- Generalized force on the first node
            call togene(dplmod1, fgloba, fext_tgt)

!           --- Generalized force on the second node
            if (nbno.eq.2) then
                call togene(dplmod2, fgloba, fext_tgt, coef=-1.d0)
            endif

            do im = 1, nbmode
                fext_nl (im) = fext_nl(im) + fext_tgt(im)
            end do
        end if
    else
        do i=1, 3
            oldxl(i)=ddeplo(i)
        end do
    endif

! --------------------------------------------------------------------------------------------------
!   --- Internal variables, storage
!
    finish = vindx(nl_ind+1)
    ASSERT((finish-start).eq.NBVARINT_CHOC)

!   --- Normal and tangential forces (local)
    vint(start   ) = fn
    vint(start+1 ) = ftange(1)
    vint(start+2 ) = ftange(2)

!   local displacement of node 1
    vint(start+3 ) = deploc(1)
    vint(start+4 ) = deploc(2)
    vint(start+5 ) = deploc(3)

!   local displacement of node 2
    vint(start+6 ) = deploc(4)
    vint(start+7 ) = deploc(5)
    vint(start+8 ) = deploc(6)

!   --- Normal and tangential velocities (local)
    vint(start+9 ) = vnorm
    vint(start+10) = vtang(1)
    vint(start+11) = vtang(2)

!   --- Other internal variables
    vint(start+12) = 1.d0 * adherance
    vint(start+13) = oldft(1)
    vint(start+14) = oldft(2)
    vint(start+15) = oldxl(1)
    vint(start+16) = oldxl(2)
    vint(start+17) = oldxl(3)
    vint(start+18) = oldvt(1)
    vint(start+19) = oldvt(2)

    if (multi_support) then
        AS_DEALLOCATE(vr=coedep)
        AS_DEALLOCATE(vr=coevit)
    end if

end subroutine

subroutine dtmallo(sd_dtm_)
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
! dtmallo : Memory allocation for a dynamic simulation on a projected basis.
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/codent.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdlibe.h"
#include "asterfort/nlget.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

!
!   -0.1- Input/output arguments
    character(len=*)         , intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer           :: nbsauv, nbmode, iret, nbnli
    integer           :: jordr, jdisc, jptem, jdepl
    integer           :: jvite, jacce, jvint
    integer           :: adapt, iarch_sd, iret1, iret2, nltreat
    real(kind=8)      :: dt, dtmin, dtmax, deltadt, epsi
    character(len=4)  :: intk1, intk0
    character(len=8)  :: sd_dtm, nomres, basemo, riggen, masgen
    character(len=8)  :: amogen, sd_nl
    character(len=16) :: schema
!
    character(len=8), pointer :: inticho(:) => null()
    character(len=8), pointer :: noeucho(:) => null()
    character(len=8), pointer :: fonred(:)  => null()
    character(len=8), pointer :: fonrev(:)  => null()
    character(len=8), target  :: blanc(1)
!
!   0 - Initializations
    call jemarq()
    sd_dtm = sd_dtm_

    epsi     = 100.d0*r8prem()
    blanc(1) = ' '
    inticho  => blanc
    noeucho  => blanc
    fonred   => blanc
    fonrev   => blanc

!
!   1 - Retrieval of the necessary information
    call dtmget(sd_dtm, _CALC_SD ,kscal=nomres)
    call dtmget(sd_dtm, _ARCH_NB ,iscal=nbsauv)
    call dtmget(sd_dtm, _SCHEMA  ,kscal=schema)
    call dtmget(sd_dtm, _BASE_MOD,kscal=basemo)
    call dtmget(sd_dtm, _NB_MODES,iscal=nbmode)
    call dtmget(sd_dtm, _RIGI_MAT,kscal=riggen)
    call dtmget(sd_dtm, _MASS_MAT,kscal=masgen)
    call dtmget(sd_dtm, _NL_TREAT,iscal=nltreat) 

    amogen = ' '
    call dtmget(sd_dtm, _AMOR_MAT,lonvec=iret)
    if (iret.gt.0) call dtmget(sd_dtm, _AMOR_MAT,kscal=amogen)

    call dtmget(sd_dtm, _DT      ,rscal=dt)
!
    sd_nl = ' '
    call dtmget(sd_dtm, _NB_NONLI,iscal=nbnli)
    if (nbnli.gt.0) call dtmget(sd_dtm, _SD_NONL, kscal=sd_nl)
!

    call dtmget(sd_dtm, _IND_ALOC, lonvec=iret1)
    if (iret1.ne.0) then
        call dtmget(sd_dtm, _ADAPT   , iscal=adapt)
        call dtmget(sd_dtm, _IARCH_SD, iscal=iarch_sd)
    else
        adapt = 0
        iarch_sd = 0

        if ((schema(1:5).eq.'RUNGE').or.(schema(1:5).eq.'ADAPT').or.(schema(1:6).eq.'DEVOGE')) then
            call getvr8('INCREMENT'   , 'PAS     ' , iocc=1, scal=dt)
            call getvr8('SCHEMA_TEMPS', 'PAS_MINI' , iocc=1, scal=dtmin, nbret=iret1)
            call getvr8('SCHEMA_TEMPS', 'PAS_MAXI' , iocc=1, scal=dtmax, nbret=iret2)
            if (iret1.ne.1) dtmin = 1.d-6* dt
            if (iret2.ne.1) dtmax = 1.d6 * dt
            deltadt = abs(dtmax/dtmin - 1)
            if (deltadt.gt.epsi) then 
                adapt = 1
                iarch_sd = 1
            end if
        end if

        if (nltreat.eq.1) iarch_sd = 1

        call dtmsav(sd_dtm, _ADAPT, 1, iscal=adapt)
        call dtmsav(sd_dtm, _IARCH_SD, 1, iscal=iarch_sd)
    end if



    if (iarch_sd.gt.0) then
        call codent(iarch_sd, 'D0', intk1)
        if (iarch_sd.gt.1) then
            call codent(iarch_sd-1, 'D0', intk0)
            call jelira('&&AD'//intk0//'           .ORDR','LONMAX', nbsauv)
            nbsauv = nint(nbsauv * 1.5d0)
            call dtmsav(sd_dtm, _ARCH_NB , 1, iscal=nbsauv)
            call dtmsav(sd_dtm, _ARCH_STO, 4, ivect=[0,0,0,0])
            call mdlibe('&&AD'//intk0, nbnli)
        else
            nbsauv = nint(nbsauv * 0.25d0)
            call dtmsav(sd_dtm, _ARCH_NB , 1, iscal=nbsauv)
        end if
        call mdallo('&&AD'//intk1, 'TRAN', nbsauv, sauve='VOLA', method=schema,&
                    base=basemo, nbmodes=nbmode, rigi=riggen, mass=masgen, amor=amogen,&
                    dt=dt, nbnli=nbnli, checkarg=.false._1,&
                    jordr=jordr, jdisc=jdisc, jptem=jptem, jdepl=jdepl, jvite=jvite,&
                    jacce=jacce, jvint=jvint, sd_nl_=sd_nl)
    else 
        call mdallo(nomres(1:8), 'TRAN', nbsauv, sauve='GLOB', method=schema,&
                    base=basemo, nbmodes=nbmode, rigi=riggen, mass=masgen, amor=amogen,&
                    dt=dt, nbnli=nbnli, checkarg=.false._1,&
                    jordr=jordr, jdisc=jdisc, jptem=jptem, jdepl=jdepl, jvite=jvite,&
                    jacce=jacce, jvint=jvint, sd_nl_=sd_nl)
    end if

    call dtmsav(sd_dtm, _IND_ALOC, 7, ivect = [jordr,jdisc,jptem,jdepl,jvite,jacce,jvint])

    call jedema()
end subroutine

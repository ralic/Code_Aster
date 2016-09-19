subroutine dtmprep_noli(sd_dtm_)
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
!1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmprep_noli : Retreives information regarding 9 types of localized
!            nonlinearities for a transient DYNA_VIBRA calculation
!            on reduced basis (TRAN//GENE)
!   --------------------------------------------------------------------------------------
!       (1)    Stops(chocs)        / CHOC
!       (2)    Anti sismic devices / ANTI_SISM
!       (3)    Viscous dampers     / DIS_VISC
!       (4)    Nonlinear springs   / DIS_ECRO_TRAC
!       (5)    Buckling            / FLAMBAGE
!       (6)    Cracked rotor       / ROTOR_FISS
!       (7)    F(V) relationship   / RELA_EFFO_VITE
!       (8)    F(X) relationship   / RELA_EFFO_DEPL
!       (9)    Lubrication         / COUPLAGE_EDYOS
!   --------------------------------------------------------------------------------------
!
!   Note : Information about these 6 nonlinearity types are read using mdchoc
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtminivec.h"
#include "asterfort/dtmprep_noli_choc.h"
#include "asterfort/dtmprep_noli_flam.h"
#include "asterfort/dtmprep_noli_ants.h"
#include "asterfort/dtmprep_noli_decr.h"
#include "asterfort/dtmprep_noli_dvis.h"
#include "asterfort/dtmprep_noli_rede.h"
#include "asterfort/dtmprep_noli_revi.h"
#include "asterfort/dtmprep_noli_rotf.h"
#include "asterfort/dtmprep_verichoc.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nlget.h"
#include "asterfort/nlsav.h"
#include "asterfort/nlvint.h"
#include "asterfort/utmess.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer          :: nbchoc, nbnli, iret, i, nbmode
    integer          :: nbvint, j, nlcase, nbcomp, icomp
    integer          :: nltype_i, ivchoc
    character(len=7) :: casek7
    character(len=8) :: sd_dtm, monmot, sd_nl
    character(len=16):: nltreat_k, nltypes(_NL_NB_TYPES), nltype_k
    character(len=19):: nomres
!
    real(kind=8)     , pointer :: basev0(:)   => null()
!
    data  nltypes /'DIS_CHOC        ', 'FLAMBAGE        ', 'ANTI_SISM       ',&
                   'DIS_VISC        ', 'DIS_ECRO_TRAC   ', 'ROTOR_FISS      ',&
                   'PALIER_EDYOS    ', 'RELA_EFFO_DEPL  ', 'RELA_EFFO_VITE  '/
!
#define base0(row,col) basev0((row-1)*nbmode+col)
!
!
!   -0.3- Initializations
    call jemarq()
    sd_dtm = sd_dtm_
!
    call getfac('COMPORTEMENT', nbcomp)
    if (nbcomp.eq.0) then 
        call dtmsav(sd_dtm, _NB_NONLI, 1, iscal=0)
        call dtmsav(sd_dtm, _NL_TREAT, 1, iscal=0)
        goto 999
    end if
!
    sd_nl = '&&OP29NL'
    call dtmsav(sd_dtm, _SD_NONL        , 1, kscal=sd_nl)
    call nlsav (sd_nl, _MAX_LEVEL       , 1, iscal=0)
    call nlsav (sd_nl, _NB_CHOC         , 1, iscal=0)
    call nlsav (sd_nl, _NB_FLAMB        , 1, iscal=0)
    call nlsav (sd_nl, _NB_ANTSI        , 1, iscal=0)
    call nlsav (sd_nl, _NB_DIS_VISC     , 1, iscal=0)
    call nlsav (sd_nl, _NB_DIS_ECRO_TRAC, 1, iscal=0)
    call nlsav (sd_nl, _NB_R_FIS        , 1, iscal=0)
    call nlsav (sd_nl, _NB_PALIE        , 1, iscal=0)
    call nlsav (sd_nl, _NB_REL_FX       , 1, iscal=0)
    call nlsav (sd_nl, _NB_REL_FV       , 1, iscal=0)
!
    do icomp = 1, nbcomp

        call getvtx('COMPORTEMENT', 'RELATION', iocc=icomp, scal=nltype_k)
        do nltype_i = 1, _NL_NB_TYPES
            if (nltype_k.eq.nltypes(nltype_i)) goto 5
        enddo
5       continue
        if (nltype_i.gt._NL_NB_TYPES) then
            ASSERT(.false.)
        endif
!
        select case (nltype_i)
!
            case(NL_CHOC)
                call dtmprep_noli_choc(sd_dtm, sd_nl, icomp)
!
            case(NL_BUCKLING)
                call dtmprep_noli_flam(sd_dtm, sd_nl, icomp)
!
            case(NL_ANTI_SISMIC)
                call dtmprep_noli_ants(sd_dtm, sd_nl, icomp)
!
            case(NL_DIS_VISC)
                call dtmprep_noli_dvis(sd_dtm, sd_nl, icomp)
!
            case(NL_DIS_ECRO_TRAC)
                call dtmprep_noli_decr(sd_dtm, sd_nl, icomp)
!
            case(NL_CRACKED_ROTOR)
                call dtmprep_noli_rotf(sd_dtm, sd_nl, icomp)
!
!             case(NL_LUBRICATION)
!                 call dtmprep_noli_pali(sd_dtm, sd_nl, icomp)
! 
             case(NL_FX_RELATIONSHIP)
                 call dtmprep_noli_rede(sd_dtm, sd_nl, icomp)
! 
             case(NL_FV_RELATIONSHIP)
                 call dtmprep_noli_revi(sd_dtm, sd_nl, icomp)
!
            case default
                ASSERT(.false.)
!
        end select
    end do

    call nlvint(sd_nl)
    ! call utimsd(6, 2, .false._1, .true._1, sd_nl, 1, 'V')

    ! ASSERT(.false.)

    call dtmget(sd_dtm, _NB_NONLI, iscal=nbnli)
    call dtmsav(sd_dtm, _NL_TREAT, 1, iscal=0)
    if (nbnli .ne. 0) then

        call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
        call nlget(sd_nl, _NB_CHOC, iscal=nbchoc)

!       --- Explicit or implicit treatment of choc non-linearities
        if (nbchoc.gt.0) then
            call getvtx(' ', 'TRAITEMENT_NONL', iocc=1, scal=nltreat_k)
            if (nltreat_k(1:9).eq.'IMPLICITE') then

                if (nbchoc.gt.41) call utmess('F', 'DYNAMIQUE_28', si=41)

                call dtmsav(sd_dtm, _NL_TREAT, 1, iscal=1)
                call dtminivec(sd_dtm, _F_NL_ADD, nbmode)
                call dtminivec(sd_dtm, _IMP_DEPL, nbmode)
                call dtminivec(sd_dtm, _IMP_VITE, nbmode)
                call dtminivec(sd_dtm, _IMP_ACCE, nbmode)
                call dtminivec(sd_dtm, _IMP_FEXT, nbmode)

                nlcase = 0
                call dtmcase_coder (nlcase, casek7)
                call wkvect(sd_dtm // '.PRJ_BAS.'//casek7, 'V V R', nbmode*nbmode, vr=basev0)
                do i = 1, nbmode
                    base0(i,i) = 1.d0
                    do j = i+1, nbmode
                        base0(i,j) = 0.d0
                    end do
                end do

                call nlget(sd_nl, _INTERNAL_VARS, lonvec=nbvint)
                call dtminivec(sd_dtm, _NL_SAVE0, nbvint)
            end if
        end if

!
        call dtmget(sd_dtm, _MULTI_AP, kscal=monmot)
        if (monmot(1:3).eq.'OUI') then
            call dtmget(sd_dtm, _CALC_SD , kscal=nomres)
            call jeexin(nomres//'.IPSD', iret)
            if (iret .eq. 0) then
                ASSERT(.false.)
            end if
        end if


        call getfac('VERI_CHOC', ivchoc)
        if (ivchoc .ne. 0) then
            call dtmprep_verichoc(sd_dtm, sd_nl)
        end if

    endif

999 continue
    call jedema()
end subroutine

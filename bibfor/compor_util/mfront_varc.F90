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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    subroutine mfront_varc(fami, kpg, ksp, imate, ifm, niv, idbg, &
                           lvarc, nbvarc, nwkin, wkin, temp, dtemp, predef, dpred, &
                           neps, epsth, depsth)
!     but: variables de commande et deformation thermique pour interface MFront
!       in   fami    famille de point de gauss (rigi,mass,...)
!            kpg,ksp numero du (sous)point de gauss
!            imate   adresse du materiau code
!       out  epsth1  deformation thermique a t 
!            depst1   increment de deformation eventuellement tournee
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
    implicit none
#include "asterc/r8nnem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rccoma.h"
#include "asterfort/verift.h"
!
    integer           :: imate, kpg, ksp, i, iret, iret2,  codret(3), npred
    integer           :: ifm, niv, idbg, ndimloc, neps, nbvarc, j, nwkin
    parameter          ( npred = 8)
    real(kind=8)      :: predef(npred),dpred(npred),vrcm,vrcp,valres(3),valrem(3),wkin(nwkin)
    real(kind=8)      :: hydrm,hydrp,sechm,sechp,sref,epsbp,epsbm,bendom,kdessm,bendop,kdessp
    real(kind=8)      :: tm,tp,tref,epsth(neps),depsth(neps),temp,dtemp
    character(len=8)  :: lvarc(npred), materi
    character(len=*)  :: fami
    character(len=16) :: nomres(3)
    character(len=32) :: mcmate
    real(kind=8)      :: epsthp, epsthm
!
    materi = ' '


    call r8inir(neps, 0.d0, depsth, 1)
    call r8inir(neps, 0.d0, epsth, 1)
    call r8inir(npred, r8nnem(), predef, 1)
    call r8inir(npred, r8nnem(), dpred, 1)


!   APPEL DE RCVARC POUR LE PASSAGE A UMAT DE LA TEMPERATURE
    if ((nwkin.eq.4).and.(wkin(1).lt.-0.5)) then
         tm=wkin(2)
         tp=wkin(3)
         tref=wkin(4)
    else
    call rcvarc(' ', 'TEMP', '-', fami, kpg, ksp, tm, iret)
    if (iret .ne. 0) tm=0.d0
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg, ksp, tref, iret)
    if (iret .ne. 0) tref=0.d0
    call rcvarc(' ', 'TEMP', '+', fami, kpg,  ksp, tp, iret)
    if (iret .ne. 0) tp=0.d0
    endif
!
    call rccoma(imate, 'ELAS', 1, mcmate, iret2)
    ASSERT(iret2.eq.0)
!
    if (mcmate .eq. 'ELAS') then
        nomres(1) = 'ALPHA'
        nomres(2) = 'ALPHA'
        nomres(3) = 'ALPHA'
        ndimloc=3
    else if (mcmate.eq.'ELAS_ORTH'.or.mcmate.eq.'ELAS_ISTR') then
        nomres(1) = 'ALPHA_L'
        nomres(2) = 'ALPHA_T'
        nomres(3) = 'ALPHA_N'
        ndimloc=3
    endif

    if (mcmate.eq.'ELAS_META') then
        ndimloc=3
        call verift(fami, kpg, ksp, '-', imate,epsth_meta_= epsthm)
        call verift(fami, kpg, ksp, '+', imate,epsth_meta_= epsthp)
        do i = 1, ndimloc
            depsth(i) = epsthp - epsthm
            epsth(i)  = epsthm
        enddo
    else
        call rcvalb(fami, kpg, ksp, '-', imate,materi, mcmate, 0, ' ', [0.d0],&
                    ndimloc, nomres, valrem, codret, 0)
        call rcvalb(fami, kpg, ksp, '+', imate,materi, mcmate, 0, ' ', [0.d0],&
                    ndimloc, nomres, valres, codret, 0)

        do i = 1, ndimloc
            if ( codret(i).eq.0 ) then
                depsth(i) = valres(i)*(tp-tref)-valrem(i)*(tm- tref)
                epsth(i)  = valrem(i)*(tm-tref)
            else
                depsth(i) = 0
                epsth(i) = 0
            endif
        enddo
    endif
!
    do 30 i = 1, nbvarc
        if ( lvarc(i).eq.'SECH' ) then
!           APPEL DE RCVARC POUR EXTRAIRE TOUTES LES VARIABLES DE COMMANDE
!           SECHAGE
            call rcvarc(' ', lvarc(i), '-', fami, kpg, ksp, vrcm, iret)
            if (iret .eq. 0) then
                predef(i)=vrcm
                call rcvarc('F', lvarc(i), '+', fami, kpg, ksp, vrcp, iret2)
                dpred(i)=vrcp-vrcm
!               RETRAIT DESSICATION
                nomres(1)='K_DESSIC'
                call rcvalb(fami, kpg, ksp, '-', imate, ' ', 'ELAS', 0, ' ',&
                          & [0.d0], 1, nomres, valres, codret, 1)
                kdessm = valres(1)
                call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'ELAS', 0, ' ',&
                          & [0.d0], 1, nomres, valres, codret, 1)
                kdessp = valres(1)
                call rcvarc(' ', 'SECH', 'REF', fami, kpg,   ksp, sref, iret2)
                if (iret2 .ne. 0) sref=0.d0
                sechm=predef(i)
                sechp=predef(i)+dpred(i)
                epsbm=-kdessm*(sref-sechm)
                epsbp=-kdessp*(sref-sechp)
                do j = 1, ndimloc
                   epsth(j)=epsth(j)+epsbm
                   depsth(j)=depsth(j)+epsbp-epsbm
                enddo
            endif
        endif
        if ( lvarc(i).eq.'HYDR' ) then
!           HYDRATATION
            call rcvarc(' ', lvarc(i), '-', fami, kpg, ksp, vrcm, iret)
            if (iret .eq. 0) then
                predef(i)=vrcm
                call rcvarc('F', lvarc(i), '+', fami, kpg, ksp, vrcp, iret2)
                dpred(i)=vrcp-vrcm
!               RETRAIT ENDOGENE
                nomres(1)='B_ENDOGE'
                call rcvalb(fami, kpg, ksp, '-', imate, ' ', 'ELAS', 0, ' ', [0.d0], &
                           & 1, nomres, valres, codret, 1)
                bendom = valres(1)
                call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'ELAS', 0, ' ', [0.d0], &
                           & 1, nomres, valres, codret, 1)
                bendop = valres(1)
                hydrm=predef(i)
                hydrp=predef(i)+dpred(i)
                epsbm=-bendom*hydrm
                epsbp=-bendop*hydrp
                do j = 1, ndimloc
                   epsth(j)=epsth(j)+epsbm
                   depsth(j)=depsth(j)+epsbp-epsbm
                enddo
            endif
        endif
        call rcvarc(' ', lvarc(i), '-', fami, kpg, ksp, vrcm, iret)
        if (iret .eq. 0) then
            predef(i)=vrcm
            call rcvarc('F', lvarc(i), '+', fami, kpg, ksp, vrcp, iret2)
            dpred(i)=vrcp-vrcm
        endif
 30 continue
!
    temp=tm
    dtemp=tp-tm

    if ((niv.ge.2) .and. (idbg.eq.1)) then
        write(ifm,*)'TEMPERATURE ET INCREMENT'
        write(ifm,'(2(1X,E11.4))') temp,dtemp
        write(ifm,*) 'AUTRES VARIABLES DE COMMANDE ET INCREMENTS'
        do i = 1, nbvarc
            write(ifm,'(A8,2(1X,E11.4))') lvarc(i),predef(i),dpred(i)
        enddo
   endif

    end

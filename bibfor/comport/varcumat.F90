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
    subroutine varcumat(fami, kpg, ksp, imate, ifm, niv, idbg,  temp, dtemp, &
                        predef, dpred, neps, epsth, depsth)
!     but: variables de commande pour interface umat
!       in   fami    famille de point de gauss (rigi,mass,...)
!            kpg,ksp numero du (sous)point de gauss
!            imate   adresse du materiau code
!       out  epsth1  deformation thermique a t 
!            depst1   increment de deformation eventuellement tournee
! ======================================================================
    implicit none
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rccoma.h"
#include "asterfort/verift.h"
!
    integer           :: imate, kpg, ksp, i, iret, iret2,  codret(3), npred
    integer           :: ifm, niv, idbg, ndimloc, neps
    parameter          ( npred = 8)
    real(kind=8)      :: predef(npred),dpred(npred),vrcm,vrcp,valres(3),valrem(3)
    real(kind=8)      :: hydrm,hydrp,sechm,sechp,sref,epsbp,epsbm,bendom,kdessm,bendop,kdessp
    real(kind=8)      :: tm,tp,tref,epsth(neps),depsth(neps),temp,dtemp
    character(len=8)  :: lvarc(npred), materi
    character(len=*)  :: fami
    character(len=16) :: nomres(3)
    character(len=32) :: mcmate
    data lvarc/'SECH','HYDR','IRRA','NEUT1','NEUT2','CORR','ALPHPUR','ALPHBETA'/
    materi = ' '

    call r8inir(neps, 0.d0, depsth, 1)
    call r8inir(neps, 0.d0, epsth, 1)
    call r8inir(npred, r8nnem(), predef, 1)
    call r8inir(npred, r8nnem(), dpred, 1)


!   APPEL DE RCVARC POUR LE PASSAGE A UMAT DE LA TEMPERATURE
    call rcvarc(' ', 'TEMP', '-', fami, kpg, ksp, tm, iret)
    if (iret .ne. 0) tm=0.d0
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg, ksp, tref, iret)
    if (iret .ne. 0) tref=0.d0
    call rcvarc(' ', 'TEMP', '+', fami, kpg,  ksp, tp, iret)
    if (iret .ne. 0) tp=0.d0

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
    else if (mcmate.eq.'ELAS_META') then
        nomres(1) = 'C_ALPHA'
!         nomres(2) = 'F_ALPHA' inutilise par META_LEMA_ANI
        nomres(2) = 'C_ALPHA'
        nomres(3) = 'C_ALPHA'
        ndimloc=3
    endif
    call rcvalb(fami, kpg, ksp, '-', imate,materi, mcmate, 0, ' ', [0.d0],&
             &  ndimloc, nomres, valrem, codret, 0)
    call rcvalb(fami, kpg, ksp, '+', imate,materi, mcmate, 0, ' ', [0.d0],&
             &  ndimloc, nomres, valres, codret, 0)

    do i = 1, ndimloc
        depsth(i) = valres(i)*(tp-tref)-valrem(i)*(tm- tref)
        epsth(i)  = valrem(i)*(tm-tref)
    enddo
!
! APPEL DE RCVARC POUR EXTRAIRE TOUTES LES VARIABLES DE COMMANDE
!     SECHAGE
    call rcvarc(' ', lvarc(1), '-', fami, kpg, ksp, vrcm, iret)
    if (iret .eq. 0) then
        predef(1)=vrcm
        call rcvarc('F', lvarc(1), '+', fami, kpg, ksp, vrcp, iret2)
        dpred(1)=vrcp-vrcm
!        RETRAIT DESSICATION
        nomres(1)='K_DESSIC'
        call rcvalb(fami, kpg, ksp, '-', imate, ' ', 'ELAS', 0, ' ', [0.d0],&
                  & 1, nomres, valres, codret, 1)
        kdessm = valres(1)
        call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'ELAS', 0, ' ', [0.d0],&
                  & 1, nomres, valres, codret, 1)
        kdessp = valres(1)
        call rcvarc(' ', 'SECH', 'REF', fami, kpg,   ksp, sref, iret2)
        if (iret2 .ne. 0) sref=0.d0
        sechm=predef(1)
        sechp=predef(1)+dpred(1)
        epsbm=-kdessm*(sref-sechm)
        epsbp=-kdessp*(sref-sechp)
        do i = 1, ndimloc
           epsth(i)=epsth(i)+epsbm
           depsth(i)=depsth(i)+epsbp-epsbm
        enddo
    endif
!     HYDRATATION
    call rcvarc(' ', lvarc(2), '-', fami, kpg, ksp, vrcm, iret)
    if (iret .eq. 0) then
        predef(2)=vrcm
        call rcvarc('F', lvarc(2), '+', fami, kpg, ksp, vrcp, iret2)
        dpred(2)=vrcp-vrcm
!        RETRAIT ENDOGENE
        nomres(1)='B_ENDOGE'
        call rcvalb(fami, kpg, ksp, '-', imate, ' ', 'ELAS', 0, ' ', [0.d0], &
                   & 1, nomres, valres, codret, 1)
        bendom = valres(1)
        call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'ELAS', 0, ' ', [0.d0], &
                   & 1, nomres, valres, codret, 1)
        bendop = valres(1)
        hydrm=predef(2)
        hydrp=predef(2)+dpred(2)
        epsbm=-bendom*hydrm
        epsbp=-bendop*hydrp
        do i = 1, ndimloc
           epsth(i)=epsth(i)+epsbm
           depsth(i)=depsth(i)+epsbp-epsbm
        enddo
    endif
    do 30 i = 3, npred
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
        do i = 1, npred
            write(ifm,'(A8,2(1X,E11.4))') lvarc(i),predef(i),dpred(i)
        enddo
    endif

    end

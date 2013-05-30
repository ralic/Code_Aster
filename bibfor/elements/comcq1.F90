subroutine comcq1(fami, kpg, ksp, mod, imate,&
                  compor, carcri, instm, instp, eps,&
                  deps, tempm, tempp, sigm, vim,&
                  option, angmas, sigp, vip, dsde,&
                  codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_21
!  VARIABLE ENTREE/SORTIE
    implicit none
    include 'asterfort/nm1dci.h'
    include 'asterfort/nm1dis.h'
    include 'asterfort/nmcomp.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/verifg.h'
    include 'asterfort/verift.h'
    character(len=*) :: fami
    character(len=16) :: option, compor(*), valkm(2)
    integer :: codret, kpg, ksp, mod, imate
    real(kind=8) :: tempm, tempp, angmas(3), sigm(4), eps(4), deps(4)
    real(kind=8) :: vim(*), vip(*), sigp(4), dsde(6, 6), carcri(*), lc(1)
    real(kind=8) :: instm, instp, ep, em, depsth, etg, depsm, wkout(1)
    character(len=8) :: typmod(2), nompar
    integer :: codres
!
!
!  VARIABLE LOCALE
    integer :: iret
    call r8inir(36, 0.d0, dsde, 1)
    call r8inir(4, 0.d0, sigp, 1)
!
    codret=0
!     INTEGRATION DE LA LOI DE COMPORTEMENT POUR LES COQUE_1D :
!     COQUE_C_PLAN : COMPORTEMNT 1D
!     COQUE_AXIS ET COQUE_D_PLAN : COMPORTEMENT C_PLAN
!
!
    if (mod .gt. 0) then
!
!     EN COQUE_D_PLAN ET COQUE_AXIS, DIRECTION Y : CPLAN (EVT DEBORST)
!     DIRECTION Z : EPSZZ CONNU
!
        typmod(1) = 'C_PLAN  '
        typmod(2) = '        '
        call nmcomp(fami, kpg, ksp, 2, typmod,&
                    imate, compor, carcri, instm, instp,&
                    4, eps, deps, 4, sigm,&
                    vim, option, angmas, 1, lc,&
                    sigp, vip, 36, dsde, 1,&
                    wkout, codret)
    else
!
!         EN COQUE_C_PLAN : COMPORTEMENT 1D.
!
        if ((compor(5).eq.'DEBORST') .or. (compor(1).eq.'SANS')) then
            typmod(1) = 'COMP1D'
            typmod(2) = '        '
            call nmcomp(fami, kpg, ksp, 2, typmod,&
                        imate, compor, carcri, instm, instp,&
                        4, eps, deps, 4, sigm,&
                        vim, option, angmas, 1, lc,&
                        sigp, vip, 36, dsde, 1,&
                        wkout, codret)
!
        else
!
!            COMPORTEMENT INTEGRE EN 1D
!
!            CARACTERISTIQUES ELASTIQUES A TMOINS
            nompar = 'TEMP'
            call rcvalb('RIGI', kpg, 1, '-', imate,&
                        ' ', 'ELAS', 1, nompar, tempm,&
                        1, 'E', em, codres, 1)
!
! ---        CARACTERISTIQUES ELASTIQUES A TPLUS
            call rcvalb('RIGI', kpg, 1, '+', imate,&
                        ' ', 'ELAS', 1, nompar, tempp,&
                        1, 'E', ep, codres, 1)
            if (compor(1) .eq. 'ELAS') then
                call verifg('RIGI', kpg, 3, 'T', imate,&
                            'ELAS', 1, depsth, iret)
                sigp(1) = ep* (sigm(1)/em+deps(1)-depsth)
                dsde(1,1) = ep
                dsde(2,2) = ep
                else if ((compor(1).eq.'VMIS_ISOT_LINE') .or. (compor(1)&
            .eq.'VMIS_ISOT_TRAC')) then
                call verift(fami, kpg, 1, 'T', imate,&
                            'ELAS', 1, depsth, iret)
                depsm=deps(1)-depsth
                call nm1dis(fami, kpg, ksp, imate, em,&
                            ep, sigm, depsm, vim, option,&
                            compor, ' ', sigp(1), vip, etg)
                dsde(1,1) = etg+1.d-6*ep
!               DSDE(1,1) = ETG
                dsde(2,2) = ep
                sigp(2)=0.d0
            else if (compor(1).eq.'VMIS_CINE_LINE') then
                call verift(fami, kpg, 1, 'T', imate,&
                            'ELAS', 1, depsth, iret)
                depsm=deps(1)-depsth
                call nm1dci(fami, kpg, ksp, imate, em,&
                            ep, sigm, depsm, vim, option,&
                            ' ', sigp(1), vip, ep)
                dsde(1,1) = etg
                dsde(2,2) = ep
            else
                valkm(1) = compor(1)
                valkm(2) = 'COMP_INCR'
                call u2mesk('F', 'ALGORITH6_81', 2, valkm)
            endif
!
        endif
!
    endif
!
end subroutine

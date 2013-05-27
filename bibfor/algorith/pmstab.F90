subroutine pmstab(sigm, sigp, epsm, deps, nbvari,&
                  vim, vip, iforta, instam, instap,&
                  iter, nbpar, nompar, table, vr,&
                  igrad, valimp, imptgt, dsidep, nomvi,&
                  nbvita)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    implicit none
!-----------------------------------------------------------------------
!           OPERATEUR    CALC_POINT_MAT STOCKAGE DANS LA TBLE RESULTAT
!-----------------------------------------------------------------------
    include 'asterfort/fgequi.h'
    include 'asterfort/tbajli.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    integer :: nbvari, nbpar, i, iter, iforta, igrad, ncmp, imptgt, nbvita
    character(len=4) :: nomeps(6), nomsig(6), nomgrd(9)
    character(len=8) :: k8b, table, vk8(2), nomvi(*)
    character(len=16) :: nompar(*)
    real(kind=8) :: deps(9), sigm(6), sigp(6), epsp(9), epsm(9), epst(9)
    real(kind=8) :: vim(*), vip(*), vr(*), equi(17), valimp(9), sigt(6)
    real(kind=8) :: rac2, instam, instap, dsidep(*)
    complex(kind=8) :: cbid
    data nomeps/'EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'/
    data nomsig/'SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'/
    data nomgrd/'F11','F12','F13','F21','F22','F23','F31','F32','F33'/
!
    rac2=sqrt(2.d0)
    if (igrad .ne. 0) then
        ncmp=9
    else
        ncmp=6
    endif
!
!     STOCKAGE DE LA SOLUTION DANS LA TABLE
    if (ncmp .eq. 6) then
        call dcopy(ncmp, epsm, 1, epsp, 1)
        call daxpy(ncmp, 1.d0, deps, 1, epsp,&
                   1)
        call dcopy(ncmp, epsp, 1, epsm, 1)
        call dcopy(ncmp, epsp, 1, epst, 1)
        call dscal(3, 1.d0/rac2, epst(4), 1)
    else
        call dcopy(ncmp, valimp, 1, epst, 1)
        call dcopy(ncmp, valimp, 1, epsm, 1)
    endif
    call dcopy(nbvari, vip, 1, vim, 1)
    instam=instap
    call dcopy(6, sigp, 1, sigm, 1)
    call dcopy(6, sigp, 1, sigt, 1)
    call dscal(3, 1.d0/rac2, sigt(4), 1)
    call fgequi(sigt, 'SIGM_DIR', 3, equi)
!
    if (iforta .eq. 0) then
!
        call dcopy(ncmp, epst, 1, vr(2), 1)
        call dcopy(6, sigt, 1, vr(ncmp+2), 1)
        vr(ncmp+8)=equi(16)
        vr(ncmp+9)=equi(1)
        call dcopy(nbvita, vip, 1, vr(1+ncmp+6+2+1), 1)
        vr(1)=instap
        vr(nbpar)=iter
!        ajout KTGT
        if (imptgt .eq. 1) then
            call dcopy(36, dsidep, 1, vr(1+6+6+3+nbvari), 1)
        endif
        call tbajli(table, nbpar, nompar, i, vr,&
                    cbid, k8b, 0)
!
    else
!
        vr(1)=instap
        vk8(1)='EPSI'
        do 551 i = 1, ncmp
            vr(2)=epst(i)
            if (igrad .eq. 0) then
                vk8(2)=nomeps(i)
            else
                vk8(2)=nomgrd(i)
            endif
            call tbajli(table, nbpar, nompar, 0, vr,&
                        cbid, vk8, 0)
!
551      continue
        vk8(1)='SIGM'
        do 552 i = 1, 6
            vr(2)=sigt(i)
            vk8(2)=nomsig(i)
            call tbajli(table, nbpar, nompar, 0, vr,&
                        cbid, vk8, 0)
!
552      continue
        vk8(1)='SIEQ'
        vr(2)=equi(1)
        vk8(2)='VMIS'
        call tbajli(table, nbpar, nompar, 0, vr,&
                    cbid, vk8, 0)
!
        vr(2)=equi(16)
        vk8(2)='TRACE'
        call tbajli(table, nbpar, nompar, 0, vr,&
                    cbid, vk8, 0)
!
        vk8(1)='VARI'
        do 553 i = 1, nbvita
            vr(2)=vip(i)
            vk8(2)=nomvi(i)
!            VK8(2)(1:1)='V'
!            CALL CODENT(I,'G',VK8(2)(2:8))
            call tbajli(table, nbpar, nompar, 0, vr,&
                        cbid, vk8, 0)
553      continue
!
    endif
!
!
end subroutine

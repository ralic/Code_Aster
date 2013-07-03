subroutine pmsta1(sigm, sigp, deps, vim, vip,&
                  nbvari, nbvita, iforta, nbpar, nompar,&
                  vr, igrad, typpar, nomvi, sddisc,&
                  liccvg, itemax, conver, actite)
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
    implicit none
!-----------------------------------------------------------------------
!           OPERATEUR    CALC_POINT_MAT STOCKAGE DANS LA TBLE RESULTAT
!-----------------------------------------------------------------------
#include "jeveux.h"
!
#include "asterfort/detrsd.h"
#include "asterfort/fgequi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/pmevdr.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: nbpar, i, nbvari, igrad, ncmp, nbvita, iforta, liccvg(5)
    integer :: actite, jvari
    logical :: itemax, conver
    character(len=4) :: nomeps(6), nomsig(6), nomgrd(9)
    character(len=8) :: k8b, typpar(*), nomvi(*), vk8(2)
    character(len=16) :: nompar(*)
    character(len=19) :: tabinc, sddisc
    complex(kind=8) :: cbid
    real(kind=8) :: deps(9), sigm(6), sigp(6), vim(*), vip(*), vr(*), rac2
    real(kind=8) :: dsig(6)
    real(kind=8) :: depst(9), equi(17)
    data nomeps/'EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'/
    data nomsig/'SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'/
    data nomgrd/'F11','F12','F13','F21','F22','F23','F31','F32','F33'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
    rac2=sqrt(2.d0)
    if (igrad .ne. 0) then
        ncmp=9
    else
        ncmp=6
    endif
!
!     CALCUL DES INCREMENTS POUR NMEVDR
!
    call dcopy(ncmp, deps, 1, depst, 1)
    if (igrad .eq. 0) call dscal(3, 1.d0/rac2, depst(4), 1)
!
    call dcopy(6, sigp, 1, dsig, 1)
    call daxpy(6, -1.d0, sigm, 1, dsig,&
               1)
    call dscal(3, 1.d0/rac2, dsig, 1)
    call fgequi(dsig, 'SIGM_DIR', 3, equi)
!
!
    if (iforta .eq. 0) then
!
        tabinc='&&OP0033.TABINC'
        call detrsd('TABLE', tabinc)
        call tbcrsd(tabinc, 'V')
        call tbajpa(tabinc, nbpar, nompar, typpar)
!
!        VR CONTIENT L'ACCROISSEMENT DE VARIABLES INTERNES
!        ATTENTION, VR EST LIMITE A  9999 VALEURS
        call dcopy(nbvita, vip, 1, vr(1+ncmp+6+3), 1)
        call daxpy(nbvita, -1.d0, vim, 1, vr(1+ncmp+6+3),&
                   1)
!
        call dcopy(ncmp, depst, 1, vr(2), 1)
        call dcopy(6, dsig, 1, vr(ncmp+2), 1)
        vr(ncmp+8)=equi(16)
        vr(ncmp+9)=equi(1)
!
        call tbajli(tabinc, nbpar, nompar, 0, vr,&
                    cbid, k8b, 0)
!
    else
!
        tabinc='&&OPB033.TABINC'
        call detrsd('TABLE', tabinc)
        call tbcrsd(tabinc, 'V')
        call tbajpa(tabinc, nbpar, nompar, typpar)
!
        call wkvect('&&OP0033.VARI', 'V V R8', nbvita, jvari)
!
!        VR CONTIENT L'ACCROISSEMENT DE VARIABLES INTERNES
        call dcopy(nbvita, vip, 1, zr(jvari), 1)
        call daxpy(nbvita, -1.d0, vim, 1, zr(jvari),&
                   1)
!
        vr(1)=0.d0
        vk8(1)='EPSI'
        do 551 i = 1, ncmp
            vr(2)=depst(i)
            if (igrad .eq. 0) then
                vk8(2)=nomeps(i)
            else
                vk8(2)=nomgrd(i)
            endif
            call tbajli(tabinc, nbpar, nompar, 0, vr,&
                        cbid, vk8, 0)
!
551      continue
        vk8(1)='SIGM'
        do 552 i = 1, 6
            vr(2)=dsig(i)
            vk8(2)=nomsig(i)
            call tbajli(tabinc, nbpar, nompar, 0, vr,&
                        cbid, vk8, 0)
!
552      continue
        vk8(1)='SIEQ'
        vr(2)=equi(1)
        vk8(2)='VMIS'
        call tbajli(tabinc, nbpar, nompar, 0, vr,&
                    cbid, vk8, 0)
!
        vr(2)=equi(16)
        vk8(2)='TRACE'
        call tbajli(tabinc, nbpar, nompar, 0, vr,&
                    cbid, vk8, 0)
!
        vk8(1)='VARI'
        do 553 i = 1, nbvita
            vr(2)=zr(jvari-1+i)
            vk8(2)=nomvi(i)
            call tbajli(tabinc, nbpar, nompar, 0, vr,&
                        cbid, vk8, 0)
553      continue
!
    endif
!
!     VERIFICATION DES EVENT-DRIVEN
!
    call pmevdr(sddisc, tabinc, liccvg, itemax, conver,&
                actite)
!
    call jedetr('&&OP0033.VARI')
!
    call jedema()
end subroutine

subroutine gerpas(fami, kpg, ksp, rela_comp, mod,&
                  imat, matcst, nbcomm, cpmono, nbphas,&
                  nvi, nmat, y, pas, itmax,&
                  eps, toly, cothe, coeff, dcothe,&
                  dcoeff, coel, pgl, angmas, neps,&
                  epsd, detot, x, nfs, nsg,&
                  nhsr, numhsr, hsr, iret)
! aslint: disable=W1306,W1504
    implicit none
!     ================================================================
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
!
!     INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE
!     PAR UNE METHODE DE RUNGE KUTTA
!
!     GESTION AUTOMATIQUE DES PAS DE TEMPS (REDECOUPAGE SI NON CV)
!     -
!       IN   FAMI :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!         KPG,KSP :  NUMERO DU (SOUS)POINT DE GAUSS
!         LOI     :  NOM DU MODELE DE COMPORTEMENT
!         MOD     :  TYPE DE MODELISATION
!         IMAT    :  CODE DU MATERIAU CODE
!         MATCST  : 'OUI' SI MATERIAU CST ENTRE T ET T+DT
!                   'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
!                   'NON' SINON
!         N       :  NOMBRE DE VARIABLES INTERNES
!         NMAT    :  NOMBRE DE PARAMETRES MATERIAU
!     VAR Y       :  VARIABLES INTERNES (VIND AU DEPART, SOLU A LA FIN)
!         PAS     :  INTERVALLE DE TEMPS TF-TD = DTIME
!         EPS     :  PARAMETRE DE CONVERGENCE TOLER=CRIT(3)
!         TOLY    :  POUR CALCUL ERREUR RELATIVE=YMFS
!         COTHE   :  COEFFICIENTS MATERIAU ELAS A T
!         COEFF   :  COEFFICIENTS MATERIAU INELAS A T
!         DCOTHE  :  COEFFICIENTS MATERIAU ELAS A T+DT
!         DCOEFF  :  COEFFICIENTS MATERIAU INELAS A T+DT
!         SIGI    :  CONTRAINTES A L'INSTANT COURANT
!         EPSD    :  DEFORMATION TOTALE A T
!         DETOT   :  INCREMENT DE DEFORMATION TOTALE
!         NFS     :  NOMBRE MAX DE FAMILLES DE SYSTEMES DE GLISSEMENT
!         NSG     :  NOMBRE MAX DE DE SYSTEMES DE GLISSEMENT MONOCRISTAL
!     OUT X       :  INSTANT COURANT
!     -
#include "asterfort/calcmm.h"
#include "asterfort/calcms.h"
#include "asterfort/rk21co.h"
#include "asterfort/rkcah1.h"
#include "asterfort/rkcah2.h"
    integer :: nmat, imat, nbcomm(nmat, 3), ne, ny, na, nvi, kpok, ip, i, neps
    integer :: nbphas, nfs, kpg, ksp, itmax, iret, nsg, nhsr, numhsr(*), irota
    character(len=16) :: rela_comp
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: mod
    character(len=3) :: matcst
    character(len=*) :: fami
    real(kind=8) :: coel(nmat), hsr(nsg, nsg, nhsr), x, pas, h, toly, xr, w, wz
    real(kind=8) :: eps
    real(kind=8) :: cothe(nmat), dcothe(nmat), coeff(nmat), dcoeff(nmat)
    real(kind=8) :: epsd(neps), detot(neps), pgl(3, 3), angmas(3)
!     TABLEAUX AUTOMATIQUES F90
    real(kind=8) :: y(nvi), wk(3*nvi), ymfs(nvi)
!      POUR GAGNER EN TEMPS CPU. ATTENTION TABLEAU POUVANT ETRE GROS
!      UTILISE SEULEMENT POUR POLYCRISTAL LCMMOP
    real(kind=8) :: toutms(nbphas*nfs*nsg*7)
!
    if (rela_comp(1:8) .eq. 'POLYCRIS') then
        call calcms(nbphas, nbcomm, cpmono, nmat, pgl,&
                    coeff, angmas, nfs, nsg, toutms)
    endif
    if (rela_comp(1:8) .eq. 'MONOCRIS') then
        irota=0
        call calcmm(nbcomm, cpmono, nmat, pgl, nfs,&
                    nsg, toutms, nvi, y,&
                    irota)
    endif
!
    iret=0
!
    ne=0
    ny=nvi
    na=ny+nvi
    kpok=1
    x=0.0d0
!
    h=pas
!
    ip=0
!
    do i = 1, nvi
        ymfs(i)=max(toly,abs(y(i)))
    end do
!
40  continue
    if ((x+h) .ge. pas) then
        h=pas-x
        ip=1
    endif
!
!     WK(3*NVI) CONTIENT EE, PUIS Y, PUIS A=F(Y)
    do i = 1, nvi
        wk(ny+i)=y(i)
    end do
!
    xr=x
60  continue
!
!
    call rk21co(fami, kpg, ksp, rela_comp, mod,&
                imat, matcst, nbcomm, cpmono, nfs,&
                nsg, toutms, nvi, nmat, y,&
                kpok, wk(ne+1), wk(na+1), h, pgl,&
                nbphas, cothe, coeff, dcothe, dcoeff,&
                coel, x, pas, neps, epsd,&
                detot, nhsr, numhsr, hsr, itmax,&
                eps, iret)
    if (iret .gt. 0) then
        goto 999
    endif
!
    w=abs(wk(1))/ymfs(1)
    do i = 2, nvi
        wz=abs(wk(i))/ymfs(i)
        if (wz .gt. w) w=wz
    end do
!
    if (w .le. eps) then
!        CONVERGENCE DU PAS DE TEMPS COURANT
        kpok=1
        if (ip .eq. 1) then
!           PAS DE TEMPS FINAL ATTEINT, SOLUTION OK
            goto 999
        else
!           CALCUL DU NOUVEAU PAS DE TEMPS H (AUGMENTATION)
            call rkcah1(rela_comp, y, pas, nvi, w,&
                        wk, h, eps, iret)
            if (iret .gt. 0) then
                goto 999
            else
                goto 40
            endif
        endif
    else
!        W.GT.EPS : NON CV
        kpok=0
!        ON REPART DE LA SOLUTION Y PRECEDENTE
        do i = 1, nvi
            y(i)=wk(ny+i)
        end do
        x=xr
        ip=0
!        CALCUL DU NOUVEAU PAS DE TEMPS H (DIMINUTION)
        call rkcah2(rela_comp, y, pas, nvi, w,&
                    wk, h, eps, iret)
        if (iret .gt. 0) then
            goto 999
        else
            goto 60
        endif
!
    endif
!
999 continue
!
end subroutine

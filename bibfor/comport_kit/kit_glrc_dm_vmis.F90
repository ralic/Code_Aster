subroutine kit_glrc_dm_vmis(imate, compor, epsm, deps, vim, option, sigm, sig, vip, dsidep, crit,&
                            iret)
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/crgdm.h"
#include "asterfort/jevech.h"
#include "asterfort/lcgldm.h"
#include "asterfort/nmcine.h"
#include "asterfort/nmisot.h"
#include "asterfort/r8inir.h"
#include "asterfort/rrlds.h"
#include "asterfort/trlds.h"
    integer :: imate, iret
    real(kind=8) :: epsm(6), deps(6), vim(*), ep
    real(kind=8) :: r8bid(6), crit(*)
    real(kind=8) :: sigm(*), sig(*), vip(*), dsidep(6, *)
    character(len=16) :: option, compor
! ----------------------------------------------------------------------
!
!      LOI GLOBALE COUPLEE POUR LES PLAQUES/COQUES DKTG
!      - GLRC_DM ET VMIS_CINE_LINE
! IN:
!       VIM     : VARIABLES INTERNES EN T-
!       OPTION  : OPTION NON LINEAIRE A CALCULER
!                'RAPH_MECA' ,'FULL_MECA', OU 'RIGI_MECA_TANG'
!       EP      : EPAISSEUR
! OUT:
!       SIG     : CONTRAINTE
!       VIP     : VARIABLES INTERNES EN T+
!       DSIDEP  : MATRICE TANGENTE
! ----------------------------------------------------------------------
!
!
    logical(kind=1) :: rigi, resi, lbid
    integer :: i, j, k, ierr, nvv, icp, ncpmax, nsgmax, isg, icara
    real(kind=8) :: emmp(6), demp(6), cel(6, 6), celinv(6, 6), celdam(6, 6)
    real(kind=8) :: emel(6)
    real(kind=8) :: tandam(6, 6), tanepl(6, 6), sigpd(6), deda(6), residu
    real(kind=8) :: crbid(11), inbid, sigpp(6), rac2, emda(6)
    real(kind=8) :: empl(6), depzz, eps2d(6), deps2d(6), d22, d21eps
    real(kind=8) :: tan3d(6, 6)
    real(kind=8) :: sig2dm(6), sig2dp(6), scm(4), sigpeq, critcp, signul, prec
    real(kind=8) :: ddemp(6), tanloc(6, 6), tanpom(6, 6), precr
    real(kind=8) :: lambda, deuxmu, lamf, deumuf, gt, gc, gf, seuil, alpha
    real(kind=8) :: alfmc
    character(len=8) :: typmod(2)
!
    rac2 = sqrt(2.d0)
! ---EPAISSEUR TOTALE :
    call jevech('PCACOQU', 'L', icara)
    ep = zr(icara)
!
! -- OPTION
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
!
!-----NOMBRE DE VARIABLES INTERNES DU MODELE, SANS CELLE POUR
!-----LA CONTRAINTE PLANE
    if (compor(1:14) .eq. 'VMIS_CINE_LINE') then
        nvv = 20
    else if (compor(1:10) .eq. 'VMIS_ISOT_') then
        nvv = 15
    endif
!
!-----TOLERANCE POUR LA CONTRAINTE HORS PLAN
!
!
    critcp = crit(3)
    signul = crit(3)
    ncpmax = nint(crit(9))
!
    if (ncpmax .le. 1) then
        ncpmax = 15
    endif
!
    prec = crit(8)
!
    if (resi) then
        nsgmax = nint(crit(1))
    else
        nsgmax = 1
    endif
!
!-----LECTURE DES PARAMETRES D ENDOMMAGEMENT
    call crgdm(imate, 'GLRC_DM         ', lambda, deuxmu, lamf,&
               deumuf, gt, gc, gf, seuil,&
               alpha, alfmc, ep, .false._1, 1,&
               lbid, r8bid(1), r8bid(2), r8bid(3), r8bid(4),&
               r8bid(5), r8bid(6))
!
!-----OBTENTION DU MODULE ELASTIQUE INITIAL
    call r8inir(6, 0.d0, demp, 1)
    call r8inir(7, 0.d0, vip, 1)
!
    call lcgldm(demp, demp, vip, 'RIGI_MECA_TANG  ', demp,&
                vip, cel, lambda, deuxmu, lamf,&
                deumuf, gt, gc, gf, seuil,&
                alpha, alfmc, crit, iret)
!
    do j = 1, 6
        do i = 1, 6
            celdam(i,j) = cel(i,j)
        end do
    end do
!
!-----TRIANGULATION DU MODULE ELASTIQUE
    call trlds(celdam, 6, 6, ierr)
    ASSERT(ierr.eq.0)
!
!-----INVERSION DU MODULE ELASTIQUE
    call r8inir(36, 0.d0, celinv, 1)
    do j = 1, 6
        celinv(j,j) = 1.0d0
    end do
!
    call rrlds(celdam, 6, 6, celinv, 6)
!
!-----CALCUL DE LA DEFORMATION ELASTIQUE A L INSTANT -
    call r8inir(6, 0.d0, emel, 1)
    do j = 1, 6
        do i = 1, 6
            emel(i) = emel(i) + celinv(i,j)*sigm(j)
        end do
    end do
!
!-----INITIALISATION DES VARIABLES DEPS^(-P), EPS^(-P) ET EMDA
    call r8inir(6, 0.d0, demp, 1)
    do i = 1, 6
        ddemp(i) = deps(i)
        emmp(i) = vim(nvv-6 + i)
        emda(i) = emmp(i) - emel(i)
        empl(i) = epsm(i) - emda(i)
    end do
!
!-------DEBUT BOUCLE INTERNE
    do isg = 1, nsgmax
!
!       CORRECTION DE LA VARIABLE PRINCIPALE
        do i = 1, 6
            demp(i) = demp(i) + ddemp(i)
        end do
!
!-------CALCUL DE L ENDOMMAGEMENT
        call r8inir(6, 0.d0, sigpd, 1)
        call lcgldm(emmp, demp, vim, 'FULL_MECA       ', sigpd,&
                    vip, tandam, lambda, deuxmu, lamf,&
                    deumuf, gt, gc, gf, seuil,&
                    alpha, alfmc, crit, iret)
!
!-------CALCUL DE L INCREMENT DE LA DEFORMATION ELASTIQUE
!        PUIS DEPS - DEPS^D
        call r8inir(6, 0.d0, deda, 1)
        do j = 1, 6
            do i = 1, 6
                deda(i) = deda(i) + celinv(i,j)*sigpd(j)
            end do
        end do
        do i = 1, 6
            deda(i) = deps(i) - (demp(i) - (deda(i)- emel(i)))
        end do
!
!-------CALCUL DE LA PLASTICITE
!
!-------BOUCLE POUR SATISFAIRE LA CONDITION DE CONTRAINTES PLANES
!
        depzz=vip(nvv+1) -vip(nvv+2)*deps(1)-vip(nvv+3)*deps(2)-vip(nvv+4)*deps(4)/rac2
!
        do icp = 1, ncpmax
            eps2d(1) = empl(1)
            eps2d(2) = empl(2)
            eps2d(3) = 0.0d0
            eps2d(4) = empl(3)/rac2
            eps2d(5) = 0.d0
            eps2d(6) = 0.d0
            deps2d(1) = deda(1)
            deps2d(2) = deda(2)
            deps2d(3) = depzz
            deps2d(4) = deda(3)/rac2
            deps2d(5) = 0.d0
            deps2d(6) = 0.d0
!
            sig2dm(1) = sigm(1)/ep
            sig2dm(2) = sigm(2)/ep
            sig2dm(3) = 0.0d0
            sig2dm(4) = sigm(3)/ep*rac2
            sig2dm(5) = 0.0d0
            sig2dm(6) = 0.0d0
!
!---------VMIS_CINE_LINE--------------------
            call r8inir(6, 0.d0, sig2dp, 1)
            if (compor(1:14) .eq. 'VMIS_CINE_LINE') then
                call nmcine('RIGI', 1, 1, 3, imate,&
                            compor, crbid(1:10), inbid, inbid, eps2d,&
                            deps2d, sig2dm, vim(8), 'FULL_MECA       ', sig2dp,&
                            vip(8), tan3d, iret)
!
!---------VMIS_ISOT_LINE--------------------
            else if (compor(1:14) .eq. 'VMIS_ISOT_LINE') then
!     --    POUR POUVOIR UTILISER NMISOT
            typmod(1) = '3D  '
            typmod(2) = '        '
            call nmisot('RIGI', 1, 1, 3, typmod,&
                        imate,  'VMIS_ISOT_LINE  ', crbid, deps2d, sig2dm,&
                        vim(8), 'FULL_MECA       ', sig2dp, vip(8), tan3d,&
                        r8bid(1), r8bid(2), iret)
        endif
!
            d22 = tan3d(3,3)
!
            if (prec .gt. 0d0) then
                sigpeq=0.d0
                do j = 1, 4
                    sigpeq = sigpeq + sig2dp(j)**2
                end do
                sigpeq = sqrt(sigpeq)
                if (sigpeq .lt. signul) then
                    precr = critcp
                else
                    precr = critcp*sigpeq
                endif
            else
                precr = abs(prec)
            endif
!
            if ((icp.ge.ncpmax .or. abs(sig2dp(3)).lt.precr)) goto 100
!
            depzz = depzz - sig2dp(3)/d22
!
        end do
100     continue
!
        if (abs(sig2dp(3)) .gt. precr) then
            iret = 1
        else
            iret = 0
        endif
!
        d21eps = tan3d(3,1)*deda(1)+tan3d(3,2)*deda(2) + tan3d(3,4)* deda(4)/rac2
!
        vip(nvv+1)=depzz+d21eps/d22-sig2dp(3)/d22
        vip(nvv+2)=tan3d(3,1)/d22
        vip(nvv+3)=tan3d(3,2)/d22
        vip(nvv+4)=tan3d(3,4)/d22
!
        scm(1) = -tan3d(1,3)*sig2dp(3)/d22
        scm(2) = -tan3d(2,3)*sig2dp(3)/d22
        scm(3) = 0.d0
        scm(4) = -tan3d(4,3)*sig2dp(3)/d22*rac2
!
        do j = 1, 4
            sig2dp(j)=sig2dp(j)+scm(j)
        end do
!
        do j = 1, 6
            if (j .ne. 3) then
                do i = 1, 6
                    if (i .ne. 3) then
                        tan3d(j,i) = tan3d(j,i) - 1.d0/tan3d(3,3)*tan3d(j,3)* tan3d(3,i)
                    endif
                end do
            endif
        end do
!
!-------COPIE DES CONTRAINTES ET
!-------DE LA MATRICE TANGENTE POUR LE MODULE PLASTIQUE
!
!       PARTIE MEMBRANE (ELASTO-PLASTIQUE)
        call r8inir(36, 0.d0, tanepl, 1)
        do j = 1, 2
            do i = 1, 2
                tanepl(i,j) = tan3d(i,j)*ep
            end do
            tanepl(3,j) = tan3d(4,j)*ep/rac2
            tanepl(j,3) = tan3d(j,4)*ep/rac2
        end do
        tanepl(3,3) = tan3d(4,4)*ep/2.0d0
!
!       PARTIE FLEXION (ELASTIQUE)
        do j = 4, 6
            do i = 4, 6
                tanepl(i,j) = cel(i,j)
            end do
        end do
!
!       CONTRAINTES EN MEMBRANE (ELASTO-PLASTIQUE)
        sigpp(1) = sig2dp(1)*ep
        sigpp(2) = sig2dp(2)*ep
        sigpp(3) = sig2dp(4)*ep/rac2
!
!       CONTRAINTES EN MEMBRANE (ELASTIQUE)
        call r8inir(3, 0.d0, sigpp(4), 1)
        do i = 4, 6
            do j = 4, 6
                sigpp(j) = sigpp(j) + tanepl(j,i)*deda(i)
            end do
            sigpp(i) = sigpp(i) + sigm(i)
        end do
!
!
!-------CALCUL DU RESIDU
        residu = 0.0d0
        do i = 1, 6
            sigpp(i) = sigpp(i) - sigpd(i)
            residu = residu + sigpp(i)*ddemp(i)
            ddemp(i) = sigpp(i)
        end do
!
        if (abs(residu) .lt. critcp) goto 200
!
!-------CONSTRUCTION DU MODULE TANGENT LOCAL
!       ET ITERATION NEWTON
!
!-------Cp*Ce^(-1)*Cd
        call r8inir(36, 0.d0, tanloc, 1)
        call r8inir(36, 0.d0, tanpom, 1)
        do i = 1, 6
            do j = 1, 6
                do k = 1, 6
                    tanloc(i,j) = tanloc(i,j) + celinv(k,i)*tandam(k, j)
                end do
            end do
        end do
!
        do i = 1, 6
            do j = 1, 6
                do k = 1, 6
                    tanpom(i,j) = tanpom(i,j) + tanepl(i,k)*tanloc(k, j)
                end do
            end do
        end do
!
!-------Cd + Cp - Cp*Ce^(-1)*Cd
        do i = 1, 6
            do j = 1, 6
                tanloc(i,j) = tanepl(i,j) + tandam(i,j) - tanpom(i,j)
            end do
        end do
!
!-------TRIANGULATION DE (Cd + Cp - Cp*Ce^(-1)*Cd)
        call trlds(tanloc, 6, 6, ierr)
        ASSERT(ierr.eq.0)
!
!-------RESOLUTION DU DDEMP
        call rrlds(tanloc, 6, 6, ddemp, 1)
!
    end do
!-----FIN DE BOUCLE INTERNE
200 continue
!
!-----TEST DE CV SIG = SIGPD = SIDPP
    if (abs(residu) .lt. critcp .and. iret .eq. 0) then
        iret = 0
    else
        iret = 1
    endif
!
    do j = 1, 6
        sig(j) = sigpd(j)
        vip(nvv-6 + j) = emmp(j) + demp(j)
    end do
!
!-----CALCUL DE LA MATRICE TANGENTE
    if (rigi) then
!
!-------INVERSION DU MODULE DE PLASTICITE
!
!-------TRIANGULATION
        call trlds(tanepl, 6, 6, ierr)
        ASSERT(ierr.eq.0)
!
        call r8inir(36, 0.d0, tanpom, 1)
        do j = 1, 6
            tanpom(j,j) = 1.0d0
        end do
!
        call rrlds(tanepl, 6, 6, tanpom, 6)
!
!       Cp^(-1) - Ce^(-1)
        do j = 1, 6
            do i = 1, 6
                tanpom(i,j) = tanpom(i,j) - celinv(i,j)
            end do
        end do
!
!-------INVERSION DU MODULE D ENDOMMAGEMENT
!
!-------TRIANGULATION
        call trlds(tandam, 6, 6, ierr)
        ASSERT(ierr.eq.0)
!
        call r8inir(36, 0.d0, tanepl, 1)
        do j = 1, 6
            tanepl(j,j) = 1.0d0
        end do
!
        call rrlds(tandam, 6, 6, tanepl, 6)
!
!       Cd^(-1) + Cp^(-1) - Ce^(-1)
        do j = 1, 6
            do i = 1, 6
                tanpom(i,j) = tanpom(i,j) + tanepl(i,j)
            end do
        end do
!
!-------INVERSION DU (Cd^(-1) + Cp^(-1) - Ce^(-1))
!
!-------TRIANGULATION
        call trlds(tanpom, 6, 6, ierr)
        ASSERT(ierr.eq.0)
!
        call r8inir(36, 0.d0, dsidep, 1)
        do j = 1, 6
            dsidep(j,j) = 1.0d0
        end do
!
        call rrlds(tanpom, 6, 6, dsidep, 6)
!
    endif
!-----FIN RIGI
!
end subroutine

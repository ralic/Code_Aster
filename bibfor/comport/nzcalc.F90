subroutine nzcalc(crit, phasp, nz, fmel, seuil,&
                  dt, trans, rprim, deuxmu, eta,&
                  unsurn, dp, iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/nzfpri.h"
#include "asterfort/utmess.h"
#include "asterfort/zeroco.h"
    integer :: nz, iret
    real(kind=8) :: seuil, dt, trans, rprim, deuxmu, crit(3), phasp(5), fmel
    real(kind=8) :: eta(5), unsurn(5), dp
!-------------------------------------------------------------
! CALCUL DE DP PAR RESOLUTION DE L'EQUATION SCALAIRE FPRIM=0
! FPRIM=F1-F(I) TEL QUE F1 : DROITE DECROISSANTE
!                       F(I):A*(DP**UNSURN(I))
! ------------------------------------------------------------
! IN CRIT : CRITERES DE CONVERGENCE LOCAUX
! IN FPRIM   : FONCTION SEUIL
! IN DT    :TP-TM
!
! OUT DP
!     IRET   CODE RETOUR DE LA RESOLUTION DE L'EQUATION SCALAIRE
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!-------------------------------------------------------------
    real(kind=8) :: fprim, fplas, fdevi, dpu(5), fp(5), fd(5), eps, dpnew, fpnew
    real(kind=8) :: spetra, r0, coefa(5), dpplas, dpmin, dpmax, x(4), y(4)
    integer :: iter, i
!
!
! INITIALISATION
    eps =1.d-6
    spetra = 1.5d0*deuxmu*trans +1.d0
    r0 = 1.5d0*deuxmu + rprim*spetra
!
!
! ESSAI PLASTIQUE PUR
!
    dpplas = seuil/r0
    dp = dpplas
!
    call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                nz, fmel, eta, unsurn, dt,&
                dp, fplas, fp, fd, fprim,&
                fdevi)
    if (abs(fprim) / (1.d0+seuil) .lt. crit(3)) goto 9999
!
    dpmax = dpplas
    dpmin = 0.d0
!
!
! RECHERCHE DES BORNES PAR RESOLUTION DE F1-FI=0 POUR CHAQUE I
! METHODE DE POINT FIXE COMBINEE AVEC NEWTON
!
    do 10 i = 1, nz
        if (phasp(i) .gt. eps) then
            dp=dpplas
!
            call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                        nz, fmel, eta, unsurn, dt,&
                        dp, fplas, fp, fd, fprim,&
                        fdevi)
            if (abs(fprim) / (1.d0+seuil) .lt. crit(3)) goto 9999
            if (abs(fp(i)) / (1.d0+seuil) .lt. crit(3)) goto 99
!
            dp= 0.d0
            coefa(i) = (eta(i)*spetra) / dt**unsurn(i)
            do 11 iter = 1, int(crit(1))
                call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                            nz, fmel, eta, unsurn, dt,&
                            dp, fplas, fp, fd, fprim,&
                            fdevi)
                if (abs(fprim) / (1+seuil) .lt. crit(3)) goto 9999
                if (abs(fp(i)) / (1+seuil) .lt. crit(3)) goto 99
!
                dp = (fplas/coefa(i) )**(1.d0/unsurn(i))
                if (dp .gt. dpplas) dp = dpplas
!
!
                call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                            nz, fmel, eta, unsurn, dt,&
                            dp, fplas, fp, fd, fprim,&
                            fdevi)
                if (abs(fprim) / (1+seuil) .lt. crit(3)) goto 9999
                if (abs(fp(i)) / (1+seuil) .lt. crit(3)) goto 99
                dp = dp - fp(i) / fd(i)
11          continue
            iret = 1
            goto 9999
99          continue
            dpu(i)=dp
            if (dpmin .eq. 0.d0) then
                dpmin=dpu(i)
                dpmax=dpu(i)
            else
                dpmin=min(dpmin,dpu(i))
                dpmax=max(dpmax,dpu(i))
            endif
        endif
10  continue
!
!
!  RESOLUTION DE FPRIM=0 - METHODE SECANTE AVEC NEWTON SI BESOIN
!
!------EXAMEN DE LA SOLUTION DP=DPMIN
    dp =dpmin
    call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                nz, fmel, eta, unsurn, dt,&
                dp, fplas, fp, fd, fprim,&
                fdevi)
    if (fprim .lt. 0.d0) then
        call utmess('F', 'ALGORITH9_12')
    endif
    x(2) = dp
    y(2) = fprim
!
!------EXAMEN DE LA SOLUTION DP=DPMAX
!
    dp = dpmax
    call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                nz, fmel, eta, unsurn, dt,&
                dp, fplas, fp, fd, fprim,&
                fdevi)
!
    x(1) = dp
    y(1) = fprim
!
!------CALCUL DE DP : EQUATION SCALAIRE FPRIM = 0 AVEC DPMIN< DP < DPMAX
    x(3) = x(1)
    y(3) = y(1)
    x(4) = x(2)
    y(4) = y(2)
    do 100 iter = 1, int(crit(1))
!
!
        call zeroco(x, y)
        dp = x(4)
        call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                    nz, fmel, eta, unsurn, dt,&
                    dp, fplas, fp, fd, fprim,&
                    fdevi)
        if (abs(fprim)/(1.d0+seuil) .lt. crit(3)) goto 9999
!
        dpnew=dp-fprim/fdevi
        if ((dpnew .ge. dpmin) .and. (dpnew .le. dpmax)) then
            call nzfpri(deuxmu, trans, rprim, seuil, phasp,&
                        nz, fmel, eta, unsurn, dt,&
                        dpnew, fplas, fp, fd, fpnew,&
                        fdevi)
!
!
            if (abs(fpnew)/(1.d0+seuil) .lt. crit(3)) then
                dp=dpnew
                goto 9999
            endif
            if (abs(fpnew)/(1.d0+seuil) .lt. abs(fprim)/(1.d0+seuil)) then
                dp=dpnew
                fprim = fpnew
            endif
!
        endif
        y(4)=fprim
        x(4)=dp
100  continue
    iret = 1
!
9999  continue
end subroutine

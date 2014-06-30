subroutine mddp54(neqgen, depl, vite, acce, fext,&
                  dt, dtsto, nbexci, idescf, nomfon,&
                  coefm, liad, inumor, nbchoc, logcho,&
                  dplmod, parcho, noecho, saucho, nbrede,&
                  dplred, fonred, saured, saredi, nbrevi,&
                  dplrev, fonrev, saurev, sarevi, nofdep,&
                  nofvit, nofacc, psidel, monmot, nbrfis,&
                  fk, dfk, angini, foncp, nbpal,&
                  vrotat, typal, finpal, cnpal, prdeff,&
                  conv, fsauv, typbas, pulsa2, masgen,&
                  descmm, riggen, descmr, lamor, descma,&
                  work1, temps, tol, depli, vitei,&
                  kde, kvi, fonca, foncv, istep,&
                  rigy, amgy, nbconv, nbmxcv, vitvar,&
                  gyogen, rgygen, amogen, errt)
!
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
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/amgene.h"
#include "asterfort/fointe.h"
#include "asterfort/mdacce.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfnli.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/rigene.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
!   taille de la boucle en fonction du schema
    integer :: netag
    parameter (netag=6)
!
    character(len=16) :: typbas
    integer :: ee, ss, nbconv, nbmxcv, descmm, descmr, descma, palmax, im
    integer :: nbrede, nbrevi, nbrfis, saredi(*), sarevi(*), neqgen, nbexci
    integer :: idescf(*), liad(*), inumor(*), nbchoc, logcho(nbchoc, *), nbpal
    integer :: istep, ier, ind, jm
    parameter (palmax=20)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax), nomfon(*), noecho(nbchoc, *), fonred(*)
    character(len=8) :: fonrev(*), nofdep(*), nofvit(*), nofacc(*), monmot
    character(len=8) :: fk(2), dfk(2), foncp, fonca, foncv, vitvar
!
    logical(kind=1) :: lamor, prdeff
    real(kind=8) :: zero, teval, dteval, dt, coefm(*), dplmod(nbchoc, neqgen, *)
    real(kind=8) :: pulsa2(*), masgen(*), riggen(*), parcho(*), dplred(*)
    real(kind=8) :: dplrev(*), angini, dtsto, vrotat, errt, errd
    real(kind=8) :: errv, r8bid
    real(kind=8) :: psidel(*), conv, skd, skv, tol, temps, arot
    real(kind=8) :: cdp(7), adp(6, 6) , b4b5(7), errde, errvi
    real(kind=8) :: fsauv(palmax, 3), vrot, gyogen(*), rgygen(*), amogen(*)
    real(kind=8) :: saucho(nbchoc,*), saured(*), saurev(*), work1(*), amgy(*), rigy(*), depl(*)
    real(kind=8) :: vite(*), acce(*), fext(*), depli(*), vitei(*)
    real(kind=8) :: kde(*), kvi(*)
!
!   ------------------------------------------------------------------------------------
!   Definition of statement functions giving the appropriate (i,j) term in the mass,
!   rigidity and damping matrices
#define rgen(row,col) rigene(row, col, riggen, neqgen, typbas, 'RUNGE_KUTTA_54')
#define agen(row,col) amgene(row, col, amogen, neqgen, typbas, 'RUNGE_KUTTA_54', lamor)
!   ------------------------------------------------------------------------------------
    zero = 0.d0
!   on peut utiliser atol pour eventuellement donner une tolerance absolue. Ici on la force a zero
!    atol=0.d0
    r8bid = zero
!
!   COEFFICIENTS DE DORMAND PRINCE
    cdp(1)=0.0d0
    cdp(2)=0.2d0
    cdp(3)=0.3d0
    cdp(4)=0.8d0
    cdp(5)=8.d0/9.d0
    cdp(6)=1.d0
    cdp(7)=1.d0
!
    adp(1,1)=  0.2d0
    adp(2,1)=  3.d0/40.d0
    adp(2,2)=  9.d0/40.d0
!
    adp(3,1)=  44.d0/45.d0
    adp(3,2)= -56.d0/15.d0
    adp(3,3)=  32.d0/9.d0
!
    adp(4,1)=  19372.d0/6561.d0
    adp(4,2)= -25360.d0/2187.d0
    adp(4,3)=  64448.d0/6561.d0
    adp(4,4)= -212.d0/729.d0
!
    adp(5,1)=  9017.d0/3168.d0
    adp(5,2)= -355.d0/33.d0
    adp(5,3)=  46732.d0/5247.d0
    adp(5,4)=  49.d0/176.d0
    adp(5,5)= -5103.d0/18656.d0
!
    adp(6,1)=  35.d0/384.d0
    adp(6,2)=  0.d0
    adp(6,3)=  500.d0/1113.d0
    adp(6,4)=  125.d0/192.d0
    adp(6,5)= -2187.d0/6784.d0
    adp(6,6)=  11.d0/84.d0
!
!    b5(1) = 5179.0d0/57600.0d0
!    b5(2) = 0.0d0
!    b5(3) = 7571.0d0/16695.0d0
!    b5(4) = 393.0d0/640.0d0
!    b5(5) = -92097.0d0/339200.0d0
!    b5(6) = 187.0d0/2100.0d0
!    b5(7) = 1.0d0/40.0d0
!
!    b4(1) = 35.0d0/384.0d0
!    b4(2) = 0.0d0
!    b4(3) = 500.0d0/1113.0d0
!    b4(4) = 125.0d0/192.0d0
!    b4(5) = -2187.0d0/6784.0d0
!    b4(6) = 11.0d0/84.0d0
!    b4(7) = 0.0d0
!
!   Calcul de b4-b5 pour gagner du temps et de la précision (peut-être)
    b4b5(1) =  71.0d0/57600.0d0
    b4b5(2) =  0.0d0
    b4b5(3) = -71.0d0/16695.0d0
    b4b5(4) =  71.0d0/1920.0d0
    b4b5(5) = -17253.0d0/339200.0d0
    b4b5(6) =  22.0d0/525.0d0
    b4b5(7) = -1.0d0/40.0d0

!   Boucle sur les estimations de ki
    do ee = 1, netag
!       Initialisation des forces extérieures
        fext(1:neqgen) = zero
!       Calcul classique forces non-linéaires à "teval + dteval"
        teval  = temps + dt*cdp(ee)
        dteval = dt*( cdp(ee+1) - cdp(ee) )
        call mdfnli(neqgen, depl, vite, acce, fext,&
                    nbchoc, logcho, dplmod, parcho, noecho,&
                    saucho, nbrede, dplred, fonred, saured,&
                    saredi, nbrevi, dplrev, fonrev, saurev,&
                    sarevi, teval, nofdep, nofvit, nofacc,&
                    nbexci, psidel, monmot, nbrfis, fk,&
                    dfk, angini, foncp, (istep+1), nbpal,&
                    dteval, dtsto, vrotat, typal, finpal,&
                    cnpal, prdeff, conv, fsauv)
!
        if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
            call utmess('F', 'EDYOS_46')
        else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
            nbconv = nbconv + 1
        endif
!
!       Estimation des dérivées (euler explicite)
        call dcopy(neqgen, vite, 1, kde((ee-1)*neqgen+1), 1)
        call dcopy(neqgen, acce, 1, kvi((ee-1)*neqgen+1), 1)
!       calcul de l'état à chaque étage pour estimer l'accel
        do im = 1, neqgen
            depl(im) = depli(im)
            vite(im) = vitei(im)
            do ss = 1, ee
                depl(im)=depl(im)+dt*adp(ee,ss)*kde((ss-1)*neqgen+im)
                vite(im)=vite(im)+dt*adp(ee,ss)*kvi((ss-1)*neqgen+im)
            enddo
        enddo
!
        teval=temps+dt*cdp(ee+1)
!       la partie suivante est nécessaire au calcul de l'accélération
        vrot = 0.d0
        arot = 0.d0
        if (vitvar(1:3) .eq. 'OUI') then
            call fointe('F ', foncv, 1, ['INST'], [teval], &
                        vrot, ier)
            call fointe('F ', fonca, 1, ['INST'], [teval], &
                        arot, ier)
            do im = 1, neqgen
                do jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    amgy(ind) = agen(im,jm) + vrot * gyogen(ind)
                    rigy(ind) = rgen(im,jm) + arot * rgygen(ind)
                enddo
            enddo
        else
            do im = 1, neqgen
                do jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    amgy(ind) = agen(im,jm)
                    rigy(ind) = rgen(im,jm)
                enddo
            enddo
        endif
!
        if (nbexci .ne. 0) then
            call mdfext(teval, r8bid, neqgen, nbexci, idescf, &
                        nomfon, coefm, liad, inumor, 1, &
                        fext)
        endif
!
!       Accélérations généralisées
        call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                    riggen, descmr, fext, lamor, amgy,&
                    descma, work1, depl, vite, acce)
!
    enddo
!   Mémorisation du dernier étage, pas à faire pour des raisons CPU. !! dimensions de kde, kvi
!    call dcopy(neqgen, vite, 1, kde(6*neqgen+1), 1)
!    call dcopy(neqgen, acce, 1, kvi(6*neqgen+1), 1)
!
!   estimation erreur
    errd=0.d0
    errv=0.d0
    do im = 1, neqgen
!       Erreur sur les déplacements, sur les vitesses. On peut gagner du CPU car b4b5(2) = 0
!       Multiplication par 'dt' quelques lignes plus bas
!       ee=1
        errde = b4b5(1)*kde(im)
        errvi = b4b5(1)*kvi(im)
        do ee = 3, netag
            errde = errde + b4b5(ee)*kde((ee-1)*neqgen+im)
            errvi = errvi + b4b5(ee)*kvi((ee-1)*neqgen+im)
        enddo
        errde = errde + b4b5(netag+1)*vite(im)
        errvi = errvi + b4b5(netag+1)*acce(im)
!       déplacements. Ajouter atol pour tenir compte d'une tolérance relative.
        skd=max(tol*abs(depli(im)),100.0d0*r8prem())
        errd = errd+(errde/skd)**2
!       vitesses. Ajouter atol pour tenir compte d'une tolérance relative.
        skv=max(tol*abs(vitei(im)),100.0d0*r8prem())
        errv = errv+(errvi/skv)**2
    enddo
!   Calcul de la vitesse et du déplacement
!       On peut utiliser soit b5 soit b4, car l'erreur est controlée et le pas adpaté.
!           b5(ss)*kde(ss-1) ou b4(ss)*kde(ss-1)
!       On remarque que b4(ss) = adp(6,ss). Le 'depl' et la 'vite' sont donc déjà calculé.
!    do im = 1, neqgen
!        depl(im) = depli(im)
!        vite(im) = vitei(im)
!        do ss = 1, 7
!            depl(im)=depl(im)+dt*b5(ss)*kde((ss-1)*neqgen+im)
!            vite(im)=vite(im)+dt*b5(ss)*kvi((ss-1)*neqgen+im)
!        enddo
!    enddo
!
!   Pour éviter des problèmes numériques on compare à la tol machine
    errt = max( dt*sqrt((errd+errv)/(2*neqgen)), 100.0d0*r8prem() )
!
end subroutine

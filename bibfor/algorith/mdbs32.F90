subroutine mdbs32(neqgen, depl, vite, acce, fext,&
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
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/amgene.h"
#include "asterfort/fointe.h"
#include "asterfort/mdacce.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfnli.h"
#include "asterfort/rigene.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
!   taille de la boucle en fonction du schema
    integer :: netag
    parameter (netag=3)
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
    logical :: lamor, prdeff
    real(kind=8) :: cdp(7), zero, teval, dt, coefm(*), dplmod(nbchoc, neqgen, *)
    real(kind=8) :: pulsa2(*), masgen(*), riggen(*), parcho(*), dplred(*)
    real(kind=8) :: dplrev(*), angini, dtsto, vrotat, errt, errd, dteval
    real(kind=8) :: errv, edp(4), r8bid, psidel(*), conv, skd
    real(kind=8) :: skv, tol, temps, adp(6, 6), arot, fsauv(palmax, 3), vrot
    real(kind=8) :: gyogen(*), rgygen(*), amogen(*), saucho(*), saured(*), saurev(*)
    real(kind=8) :: work1(*), amgy(*), rigy(*), depl(*), vite(*), acce(*)
    real(kind=8) :: fext(*), depli(*), vitei(*), kde(*)
    real(kind=8) :: kvi(*),errde,errvi
!   ------------------------------------------------------------------------------------
!   Definition of statement functions giving the appropriate (i,j) term in the mass,
!   rigidity and damping matrices
#define rgen(row,col) rigene(row, col, riggen, neqgen, typbas, 'RUNGE_KUTTA_32')
#define agen(row,col) amgene(row, col, amogen, neqgen, typbas, 'RUNGE_KUTTA_32', lamor)
!   ------------------------------------------------------------------------------------
    zero = 0.d0
!   on utilise atol pour eventuellement donner une tolerance absolue. Ici on la force a zero
!    atol=0.d0
    r8bid = zero
!
!   COEFFICIENTS DE BOGACKI–SHAMPINE
    cdp(1)=0.00d0
    cdp(2)=0.50d0
    cdp(3)=0.75d0
    cdp(4)=1.d0
!
    adp(1,1)=0.50d0
    adp(2,1)=0.00d0
    adp(2,2)=0.75d0
!
    adp(3,1)=2.d0/9.d0
    adp(3,2)=1.d0/3.d0
    adp(3,3)=4.d0/9.d0
!
    edp(1)=  5.0d0/72.0d0
    edp(2)= -1.d0/12.0d0
    edp(3)= -1.d0/9.d0
    edp(4)=  1.d0/8.d0
!
!   BOUCLE SUR LES ESTIMATIONS DE Ki
    do ee = 1, netag
!       Initialisation des forces exterieures
        fext(1:neqgen) = zero
!       Calcul classique forces non-lineaires à "teval + dteval"
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
!       la partie suivante est necessaire au calcul de l'accélération
        vrot = 0.d0
        arot = 0.d0
        if (vitvar(1:3) .eq. 'OUI') then
            call fointe('F ', foncv, 1, ['INST'], [teval],&
                        vrot, ier)
            call fointe('F ', fonca, 1, ['INST'], [teval],&
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
            call mdfext(teval, r8bid, neqgen, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, 1,&
                        fext)
        endif
!
!       Accélérations généralisées
        call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                    riggen, descmr, fext, lamor, amgy,&
                    descma, work1, depl, vite, acce)
!
    enddo
!   Mémorisation du dernier étage. Pas à faire pour des raisons CPU. !! dimensions de kde, kvi
!
!   estimation erreur
    errd=0.d0
    errv=0.d0
    do im = 1, neqgen
!       Erreur sur les déplacements, sur les vitesses.
!           ee=[1,netag+1] erreur+= dt*edp(ee)*kde((ee-1)*neqgen+im)
!       Multiplication par 'dt' quelques lignes plus bas
        errde=edp(1)*kde(im)+edp(2)*kde(neqgen+im)+edp(3)*kde(neqgen*2+im)+edp(4)*vite(im)
        errvi=edp(1)*kvi(im)+edp(2)*kvi(neqgen+im)+edp(3)*kvi(neqgen*2+im)+edp(4)*acce(im)
!       déplacements. Ajouter atol pour tenir compte d'une tolérance relative.
        skd=max(tol*abs(depli(im)),100.d0*r8prem())
        errd = errd+(errde/skd)**2
!       vitesses. Ajouter atol pour tenir compte d'une tolérance relative.
        skv=max(tol*abs(vitei(im)),100.d0*r8prem())
        errv = errv+(errvi/skv)**2
    enddo
!
!   Pour éviter des problèmes numériques on compare à la tol machine
    errt = max(dt*sqrt((errd+errv)/(2*neqgen)),100.d0*r8prem())
!
end subroutine

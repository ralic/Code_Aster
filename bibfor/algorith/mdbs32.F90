subroutine mdbs32(neqgen, depl, vite, acce, fext,&
                  dt, dtsto, lflu, nbexci, idescf,&
                  nomfon, coefm, liad, inumor, nbchoc,&
                  logcho, dplmod, parcho, noecho, saucho,&
                  nbrede, dplred, fonred, saured, saredi,&
                  nbrevi, dplrev, fonrev, saurev, sarevi,&
                  nofdep, nofvit, nofacc, psidel, monmot,&
                  nbrfis, fk, dfk, angini, foncp,&
                  nbpal, vrotat, typal, finpal, cnpal,&
                  prdeff, conv, fsauv, typbas, pulsa2,&
                  masgen, descmm, riggen, descmr, lamor,&
                  descma, work1, temps, tol, depli,&
                  vitei, erde, ervi, kde, kvi,&
                  fonca, foncv, istep, rigy, amgy,&
                  nbconv, nbmxcv, vitvar, gyogen, rgygen,&
                  amogen, errt)
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
#include "asterfort/fointe.h"
#include "asterfort/mdacce.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfnli.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
    character(len=16) :: typbas
    integer :: ee, ss, nbconv, nbmxcv, descmm, descmr, descma, palmax, im, iff
    integer :: nbrede, nbrevi, nbrfis, saredi(*), sarevi(*), netag, neqgen, nbexci
    integer :: idescf(*), liad(*), inumor(*), nbchoc, logcho(nbchoc, *), nbpal
    integer :: istep, ier, ind, jm
    parameter (palmax=20)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax), nomfon(*), noecho(nbchoc, *), fonred(*)
    character(len=8) :: fonrev(*), nofdep(*), nofvit(*), nofacc(*), monmot
    character(len=8) :: fk(2), dfk(2), foncp, fonca, foncv, vitvar
!
    logical :: lamor, lflu, prdeff
    real(kind=8) :: cdp(7), zero, teval, dt, coefm(*), dplmod(nbchoc, neqgen, *)
    real(kind=8) :: pulsa2(*), masgen(*), riggen(*), parcho(*), dplred(*)
    real(kind=8) :: dplrev(*), angini, dtsto, vrotat, errt, errd
    real(kind=8) :: errv, edp1, edp2, edp3, edp4, r8bid, psidel(*), conv, skd, r8b(1)
    real(kind=8) :: skv, tol, temps, adp(6, 6), arot, fsauv(palmax, 3), vrot
    real(kind=8) :: gyogen(*), rgygen(*), amogen(*), saucho(*), saured(*), saurev(*)
    real(kind=8) :: work1(*), amgy(*), rigy(*), depl(*), vite(*), acce(*)
    real(kind=8) :: fext(*), depli(*), vitei(*), erde(*), ervi(*), kde(*)
    real(kind=8) :: kvi(*), atol
! ======================================================================
    zero = 0.d0
!     ON UTILISE ATOL POUR EVENTUELLEMENT DONNER UNE TOLERANCE ABSOLUE
!     DONNEE PAR L'UTILISATEUR. ICI ON LA FORCE A ZERO
    atol=0.d0
    r8bid = zero
!
!     --- COEFICIENTS DE DORMAND PRINCE
!
    cdp(1)=0.00d0
    cdp(2)=0.50d0
    cdp(3)=0.75d0
    cdp(4)=1.d0
!
    adp(1,1)=0.50d0
    adp(2,1)=0.00d0
    adp(2,2)=0.75d0
    adp(3,1)=2.d0/9.d0
    adp(3,2)=1.d0/3.d0
    adp(3,3)=4.d0/9.d0
!
    edp1=7.d0/24.d0 - adp(3,1)
    edp2=1.d0/4.d0 - adp(3,2)
    edp3=1.d0/3.d0 - adp(3,3)
    edp4=1.d0/8.d0 - 0.00d0
!
!   TAILLE DE LA BOUCLE EN FONCTION DU SCHEMA
    netag=3
! BOUCLE SUR LES ESTIMATIONS DE Ki
    do 10 ee = 1, netag
!C        --- ESTIMATION DE LA DERIVEE (EULER EXPLICITE)---
        call dcopy(neqgen, vite, 1, kde((ee-1)*neqgen+1), 1)
        call dcopy(neqgen, acce, 1, kvi((ee-1)*neqgen+1), 1)
!       --- CALCUL DE L ETAT A CHAQUE ETAGE POUR ESTIMER L ACCEL
        do 21 im = 1, neqgen
            depl(im) = depli(im)
            vite(im) = vitei(im)
            do 30 ss = 1, ee
                depl(im)=depl(im)+dt*adp(ee,ss)*kde((ss-1)*neqgen+im)
                vite(im)=vite(im)+dt*adp(ee,ss)*kvi((ss-1)*neqgen+im)
30          continue
21      continue
!
        teval=temps+dt*cdp(ee+1)
!
!        LA PARTIE SUIVATE EST NECESSAIRE AU CALCUL DE L'ACCELERATION
!
        vrot = 0.d0
        arot = 0.d0
        if (vitvar(1:3) .eq. 'OUI') then
            call fointe('F ', foncv, 1, ['INST'], [teval],&
                        vrot, ier)
            call fointe('F ', fonca, 1, ['INST'], [teval],&
                        arot, ier)
            do 115 im = 1, neqgen
                do 116 jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    amgy(ind) = amogen(ind) + vrot * gyogen(ind)
                    rigy(ind) = riggen(ind) + arot * rgygen(ind)
116              continue
115          continue
        else
            do 119 im = 1, neqgen
                do 120 jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    amgy(ind) = amogen(ind)
                    rigy(ind) = riggen(ind)
120              continue
119          continue
        endif
!
!        --- FORCES EXTERIEURES ---
!
        do 40 iff = 1, neqgen
            fext(iff) = zero
40      continue
!
        if (nbexci .ne. 0) then
            call mdfext(teval, r8bid, neqgen, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, 1,&
                        fext)
        endif
!
        if (lflu) then
            call utmess('F', 'ALGORITH5_21')
        else
!
!        CALCUL CLASSIQUE FORCES NON-LINEAIRES ET ACCELERATIONS
!        --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!
            call mdfnli(neqgen, depl, vite, acce, fext,&
                        r8b, r8b, r8b, r8b, nbchoc,&
                        logcho, dplmod, parcho, noecho, saucho,&
                        nbrede, dplred, fonred, saured, saredi,&
                        nbrevi, dplrev, fonrev, saurev, sarevi,&
                        teval, nofdep, nofvit, nofacc, nbexci,&
                        psidel, monmot, nbrfis, fk, dfk,&
                        angini, foncp, (istep+1), nbpal, dt,&
                        dtsto, vrotat, typal, finpal, cnpal,&
                        prdeff, conv, fsauv)
            if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
                call utmess('F', 'EDYOS_46')
            else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
                nbconv = nbconv + 1
            endif
!
!        --- ACCELERATIONS GENERALISEES ---
!
            call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                        riggen, descmr, fext, lamor, amgy,&
                        descma, work1, depl, vite, acce)
!
        endif
! FIN DE LA BOUCLE SUR LES ETAGES DE RUNGE-KUTTA
10  end do
!
!      --- ESTIMATION ERREUR ---
!
    errd=0.d0
    errv=0.d0
    errt=0.d0
!
    do 50 im = 1, neqgen
!         POUR LES DEPLACEMENTS
!         NOTER QUE k7 EST DONNEE PAR
!         LA VALEUR DE LA VITESSE AU QUATRIEME ETAGE DE LA METHODE
        erde(im)= (edp1*kde(im)+edp2*kde(neqgen+im) +edp3*kde(2*&
        neqgen+im)+edp4*vite(im))*dt
!
        skd=atol+tol*max(abs(depl(im)),abs(depli(im)),1.d2*r8prem())
        errd = errd+(erde(im)/skd)**2
!         POUR LES VITESSES
!         NOTER QUE k7 EST DONNEE PAR
!         LA VALEUR DE L'ACCELERATION AU QUATRIEME ETAGE DE LA METHODE
        ervi(im)= (edp1*kvi(im)+edp2*kvi(neqgen+im) +edp3*kvi(2*&
        neqgen+im)+edp4*acce(im))*dt
!
        skv=atol+tol*max(abs(vite(im)),abs(vitei(im)),1.d2*r8prem())
        errv = errv+(ervi(im)/skv)**2
50  end do
!
!     POUR EVITER DES PROBLEMES NUMERIQUES ON COMPARE A LA TOL MACHINE
    errt = max(sqrt((errd+errv)/(2*neqgen)),1.d2*r8prem())
!
end subroutine

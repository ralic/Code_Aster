subroutine lchbr2(typmod, option, imate, crit, sigm,&
                  epsm, td, tf, tr, depsm,&
                  vim, vip, dspdp1, dspdp2, sipp,&
                  sigp, dsidep, dsidp1, dsidp2, iret)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/calcvh.h"
#include "asterfort/hbcalc.h"
#include "asterfort/hbcrel.h"
#include "asterfort/hbderi.h"
#include "asterfort/hbdsdp.h"
#include "asterfort/hbmajs.h"
#include "asterfort/hbmata.h"
#include "asterfort/hbrcvx.h"
#include "asterfort/hbrmat.h"
#include "asterfort/hbvaec.h"
#include "asterfort/lcdedi.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lceqma.h"
#include "asterfort/lchbvp.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcsove.h"
#include "asterfort/trace.h"
#include "asterfort/utmess.h"
    integer :: imate, iret
    real(kind=8) :: depsm(6), vim(*), vip(*), sigp(6), dsidep(6, 6)
    real(kind=8) :: sigm(6), td, tf, tr, dsidp1(6), dsidp2(6), epsm(6)
    real(kind=8) :: crit(*), sipp, dspdp1, dspdp2
    character(len=8) :: typmod(*)
    character(len=16) :: option
! ======================================================================
! --- LOI DE COMPORTEMENT DE TYPE HOEK BROWN EN CONTRAINTES TOTALES ----
! --- ELASTICITE ISOTROPE ----------------------------------------------
! --- CRITERE DE PLASTICITE DE HEOK BROWN ------------------------------
! --- ECOULEMENT PLASTIQUE DE DRUCKER PRAGER ---------------------------
! ======================================================================
! IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
! IN  IMATE   NATURE DU MATERIAU
! IN  CRIT    CRITERES LOCAUX
!               CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                         (ITER_INTE_MAXI == ITECREL)
!               CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                         (RESI_INTE_RELA == RESCREL)
! IN  SIGM    CHAMP DE CONTRAINTES EFFECTIVES A T-
! IN  EPSM    CHAMP DE DEFORMATIONS A T-
! IN  TD,TF,TR TEMPERATURES A T-, T+ ET DE REFERENCE
! IN  DEPSM   INCREMENT DU CHAMP DE DEFORMATION
! IN  VIM     VARIABLES INTERNES EN T-
!               1   : PARAMETRE D ECROUISSAGE
!               2   : DEFORMATION PLASTIQUE VOLUMIQUE CUMULEE
!               3   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
! VAR VIP     VARIABLES INTERNES EN T+
!              IN  ESTIMATION (ITERATION PRECEDENTE)
!              OUT CALCULEES
! IN  DSPDP1  DERIVEE DE SIP PAR RAPPORT A PRE1
! IN  DSPDP2  DERIVEE DE SIP PAR RAPPORT A PRE2
! IN  SIPM    TERME DE PRESSION A T-
! IN  SIPP    TERME DE PRESSION A T+
! OUT SIGP    CONTRAINTES EFFECTIVES A T+
! OUT DSIDEP  MATRICE TANGENTE (DSIGPDEPS)
! OUT IRET    CODE RETOUR (0 = OK)
! OUT DSIDP1  DERIVEE DE SIG EFFECTIVES PAR RAPPORT A PRE1
! OUT DSIDP2  DERIVEE DE SIG EFFECTIVES PAR RAPPORT A PRE2
! ======================================================================
!     INFO   MATERF   COEFFICIENTS MATERIAUX A T+DT
!                     MATERF(*,1) = CARACTERISTIQUES ELASTIQUES
!                     MATERF(*,2) = CARACTERISTIQUES PLASTIQUES
!            NDT      NOMBRE DE COMPOSANTES TOTALES DES TENSEURS
!            NDI      NOMBRE DE COMPOSANTES DIRECTES DES TENSEURS
!            NVI      NOMBRE DE VARIABLES INTERNES
!            VP       VALEURS PROPRES DE LA MATRICE SE
!            VECP     VECTEURS PROPRES ASSOCIES A SE
!            TOLER    VALEUR TOLERANCE DE CONVERGENCE
!            ITMAX    NB ITERATIONS MAX A CONVERGENCE
! ======================================================================
    integer :: ndt, ndi, itmax, ii, iteri, iter, nr, nbmat
    real(kind=8) :: gm, gp, etam, etap, aux, sig3, mu, k, neuf
    real(kind=8) :: materf(16, 2), materd(16, 2)
    real(kind=8) :: deps(6), epsp(6), sige(6), se(6), sigeb(6)
    real(kind=8) :: toler, seq, i1e, seuil, plas, dg, sigeqe, un
    real(kind=8) :: hookf(6, 6), deux, trois, vi
    real(kind=8) :: vp(3), vecp(3, 3), detadg, dgdl, dsdsip(6), eta, gam
    real(kind=8) :: incrg, gnp, dgnp, etanp, vh, vg, zero, grup, gres
    real(kind=8) :: parame(4), derive(5), pi, fmoins, pphi0, pphi1, pphi2
    character(len=3) :: matcst
    character(len=8) :: mod
    aster_logical :: resi, rigi
    integer :: nvi
! ======================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( neuf   =  9.0d0  )
    parameter       ( zero   =  0.0d0  )
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
! --- INITIALISATION DES PARAMETRES DE CONVERGENCE ---------------------
! ======================================================================
    mod = typmod(1)
    itmax = int(crit(1))
    toler = crit(3)
    pi = r8pi()
    pi = pi/180.d0
    nbmat = 16
! ======================================================================
! --- RECUPERATION DES PARAMETRES DE LA LOI ----------------------------
! ======================================================================
    call hbrmat(mod, imate, nbmat, zero, materd,&
                materf, matcst, ndt, ndi, nr,&
                nvi)
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    gm = vim(1)
    if (gm .lt. 0.0d0) then
        call utmess('F', 'ALGORITH3_88')
    endif
    iret = 0
    iteri = 0
    call lcinma(0.0d0, hookf)
    call lcinma(0.0d0, dsidep)
! =====================================================================
! --- CALCUL DES PARAMETRES D ECROUISSAGE -----------------------------
! =====================================================================
    call hbvaec(gm, nbmat, materf, parame)
    etam = deux*sin(parame(4)*pi)/(trois+sin(parame(4)*pi))
    resi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    ASSERT(resi .or. rigi)
! =====================================================================
! --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
! =====================================================================
    call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
! ======================================================================
! --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ----------
! ======================================================================
    call lcdedi('RIGI', 1, 1, nbmat, materd,&
                materf, td, tf, tr, depsm,&
                epsm, deps, epsp)
! =====================================================================
! --- INTEGRATION ELASTIQUE : SIGE = HOOKF EPSP + SIP -----------------
! =====================================================================
    call lcprmv(hookf, deps, sigeb)
    call lcsove(sigeb, sigm, sige)
    do ii = 1, ndi
        sige(ii) = sige(ii)+sipp
    end do
    call lcdevi(sige, se)
!      CALL PSCAL(NDT,SE,SE,SEQ)
    call lcprsc(se, se, seq)
    sigeqe = sqrt(trois*seq/deux)
    i1e = trace(ndi,sige)
! ======================================================================
! --- CALCUL DES CONTRAINTES -------------------------------------------
! ======================================================================
    if (resi) then
! ======================================================================
! --- CALCUL DU CRITERE ELASTIQUE --------------------------------------
! ======================================================================
        call hbrcvx(sige, vim, nbmat, materf, seuil,&
                    vp, vecp)
! ======================================================================
! --- CALCUL DE DELTA GAMMA --------------------------------------------
! ======================================================================
        if (seuil .gt. toler) then
            plas = 1.0d0
            dg = 0.0d0
            call hbcrel(vp, gm, dg, nbmat, materf,&
                        sigeqe, i1e, etam, parame, seuil)
            fmoins = seuil
! ======================================================================
! --------- CALCUL DE L INCREMENT DE GAMMA PAR METHODE DE NEWTON -------
! ======================================================================
! --------- INITIALISATION DES VARIABLES -------------------------------
! ======================================================================
            iter = 0
            incrg = 0.d0
            dgnp = dg
            gnp = gm
            etanp = etam
            call calcvh(nbmat, materf, etanp, vp, sigeqe,&
                        vh, vg)
            call hbderi(gnp, nbmat, materf, vg, etanp,&
                        parame, derive)
! ======================================================================
! --------- PREMIERE ITERATION -----------------------------------------
! ======================================================================
            call hbcalc(seuil, gnp, dgnp, nbmat, materf,&
                        i1e, sigeqe, vp, etanp, vh,&
                        vg, parame, derive, incrg)
  2         continue
            gnp = gnp + incrg
            dgnp = dgnp + incrg
! ======================================================================
! -- ON OBTIENT DGAMMA_P NEGATIF : ON ESSAIE DE DECOUPER LE PAS DE TEMPS
! ======================================================================
            if (dgnp .lt. 0.d0) then
                call utmess('I', 'ALGORITH4_57')
                iteri = 1
                goto 100
            endif
            call hbvaec(gnp, nbmat, materf, parame)
            etanp = deux*sin(parame(4)*pi)/(trois+sin(parame(4)*pi))
            call hbcrel(vp, gnp, dgnp, nbmat, materf,&
                        sigeqe, i1e, etanp, parame, seuil)
! ======================================================================
! ---------- IL Y A CONVERGENCE ----------------------------------------
! ======================================================================
            if ((abs(seuil).lt.toler) .or. (abs(seuil/fmoins).lt.toler)) then
! ======================================================================
! --------- ON DETECTE LES SOLUTIONS NON ADMISSIBLES -------------------
! ======================================================================
                aux = sigeqe*(etanp+un)/(trois*materf(4,1))
                if (dgnp .gt. aux) then
                    call utmess('I', 'ALGORITH4_58')
                    iteri = 1
                    goto 100
                endif
                dg = dgnp
                iteri = 0
! ======================================================================
! --------- LE NOMBRE MAX D ITERATIONS N A PAS ETE ATTEINT -------------
! ======================================================================
            else if (iter.lt.itmax) then
                iter = iter + 1
                iteri = 0
                call calcvh(nbmat, materf, etanp, vp, sigeqe,&
                            vh, vg)
                call hbderi(gnp, nbmat, materf, vg, etanp,&
                            parame, derive)
                call hbcalc(seuil, gnp, dgnp, nbmat, materf,&
                            i1e, sigeqe, vp, etanp, vh,&
                            vg, parame, derive, incrg)
                goto 2
! ======================================================================
! --------- LE NOMBRE MAX D ITERATIONS A ETE ATTEINT -------------------
! ======================================================================
            else
! ======================================================================
! --------- ON ESSAIE DE DECOUPER LE PAS DE TEMPS ----------------------
! ======================================================================
                call utmess('I', 'ALGORITH4_59')
                iteri = 1
                goto 100
            endif
100         continue
            if (iteri .eq. 1) goto 1
! ======================================================================
            gp = gnp
            etap = etanp
            call hbmajs(dg, nbmat, materf, se, i1e,&
                        sigeqe, etap, sigp)
! ---------- IL FAUT RENVOYER LES CONTRAINTES EFFECTIVES ---------------
            do ii = 1, ndi
                sigp(ii) = sigp(ii)-sipp
            end do
            vip(1) = vim(1) + dg
            vip(2) = vim(2) + trois*etap*dg/(etap+un)
            vip(3) = plas
            if (rigi) then
                mu = materf(4,1)
                k = materf(5,1)
                sig3 = vp(3)*(un - trois*mu*dg/(sigeqe*(etap+un))) + (i1e - neuf*k*etap*dg/(etap+&
                       &un))/trois
            endif
        else
            plas = 0.0d0
            do ii = 1, ndt
                sigp(ii) = sige(ii)
            end do
! ---------- IL FAUT RENVOYER LES CONTRAINTES EFFECTIVES ---------------
            do ii = 1, ndi
                sigp(ii) = sigp(ii) - sipp
            end do
            gp = gm
            etap = etam
            vip(1) = vim(1)
            vip(2) = vim(2)
            vip(3) = plas
        endif
    endif
! ======================================================================
! --- CALCUL DE LA MATRICE TANGENTE ------------------------------------
! ======================================================================
    if (rigi) then
        grup = materf(1,2)
        gres = materf(2,2)
        pphi1 = materf(9,2)
        pphi2 = materf(15,2)
        pphi0 = materf(16,2)
        if (option(1:9) .eq. 'RIGI_MECA') then
            vi = vim(3)
            dg = 0.0d0
            eta = etam
            gam = gm
            call lchbvp(sige, vp, vecp)
            sig3 = vp(3)+i1e/3.0d0
        else
            vi = vip(3)
            eta = etap
            gam = gp
        endif
        if (vi .eq. 0) then
            call lceqma(hookf, dsidep)
            do ii = 1, ndi
                dsdsip(ii) = 1.0d0
            end do
            do ii = ndi+1, 6
                dsdsip(ii) = 0.0d0
            end do
        else
            if (gam .lt. grup) then
                detadg = 6.0d0*(pphi1-pphi0)*pi*cos(parame(4)*pi) / (grup*(trois+sin(parame(4)*pi&
                         &))**2)
            else if (gam.lt.gres) then
                detadg = 6.0d0*(pphi2-pphi1)*pi*cos(parame(4)*pi) / ((gres-grup)*(trois+sin(param&
                         &e(4)*pi))**2)
            else
                detadg = 0.d0
            endif
            dgdl = eta+un
            call hbderi(gam, nbmat, materf, zero, eta,&
                        parame, derive)
            call hbmata(se, dg, eta, i1e, sigeqe,&
                        vp, vecp, parame, derive, sig3,&
                        detadg, dgdl, nbmat, materf, dsidep)
            call hbdsdp(se, dg, eta, sigeqe, vp,&
                        parame, derive, nbmat, materf, sig3,&
                        detadg, dgdl, dsdsip)
        endif
! ======================================================================
! --- ON A CALCULE LA DERIVEE DES CONTRAINTES TOTALES, ET ON RENVOIE ---
! --- CELLE DES CONTRRAINTES EFFECTIVES --------------------------------
! ======================================================================
        do ii = 1, ndi
            dsdsip(ii) = dsdsip(ii)-1.0d0
        end do
! ======================================================================
! --- CALCUL DE LA DERIVEE DES CONTRAINTES TOTALES PAR RAPPORT A P1,P2 -
! ======================================================================
        do ii = 1, ndt
            dsidp1(ii) = dsdsip(ii)*dspdp1
            dsidp2(ii) = dsdsip(ii)*dspdp2
        end do
    endif
! ======================================================================
    iret = 0
    goto 999
  1 continue
    iret = 1
999 continue
! ======================================================================
end subroutine

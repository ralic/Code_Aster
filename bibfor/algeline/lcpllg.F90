subroutine lcpllg(toler, itmax, mod, nbmat, mater,&
                  nr, nvi, deps, sigd, vind,&
                  seuil, icomp, sigf, vinf, devg,&
                  devgii, irtet)
!
    implicit      none
#include "asterfort/calcpj.h"
#include "asterfort/codent.h"
#include "asterfort/codree.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcsovn.h"
#include "asterfort/lglcov.h"
#include "asterfort/lgldom.h"
#include "asterfort/lglini.h"
#include "asterfort/lglite.h"
#include "asterfort/prjsom.h"
#include "asterfort/trace.h"
#include "asterfort/u2mesk.h"
#include "blas/ddot.h"
    integer :: itmax, nbmat, nr, nvi, icomp, irtet
    real(kind=8) :: toler, mater(nbmat, 2), deps(6), sigd(6)
    real(kind=8) :: vind(*), sigf(6), vinf(*), seuil, devg(6), devgii
    character(len=8) :: mod
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : LOI DE COMPORTEMENT PLASTIQUE POUR LA MECANIQUE DES ROCHES -
! ------- : D'APRES LA LOI DE LAIGLE -----------------------------------
! ======================================================================
! IN  : TOLER  : VALEUR DE LA TOLERANCE DE CONVERGENCE -----------------
! --- :        : (RESI_INTE_RELA) --------------------------------------
! --- : ITMAX  : NOMBRE D'ITERATIONS MAXIMUM A CONVERGENCE -------------
! --- :        : (ITER_INTE_MAXI) --------------------------------------
! --- : MOD    : TYPE DE MODELISATION ----------------------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : NR     : NOMBRE DE RELATIONS NON LINEAIRES ---------------------
! --- : NVI    : NOMBRE DE VARIABLES INTERNES --------------------------
! --- : DEPS   : ACCROISSEMENTS DE DEFORMATIONS A L'ITERATION COURANTE -
! --- : SIGD   : CONTRAINTES A L'INSTANT PRECEDENT ---------------------
! --- : VIND   : VARIABLES INTERNES A L'INSTANT PRECEDENT --------------
! --- : SEUIL  : VARIABLE SEUIL ELASTIQUE ------------------------------
! --- : ICOMP  : COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS ----------
! OUT : SIGF   : CONTRAINTES A L'INSTANT COURANT -----------------------
! --- : VINF   : VARIABLES INTERNES A L'INSTANT COURANT ----------------
! --- : DEVG   : DEVIATEUR DU TENSEUR G, DIRECTION D'ECOULEMENT --------
! --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
! --- : IRTET  : CONTROLE DU REDECOUPAGE DU PAS DE TEMPS ---------------
! ======================================================================
    integer :: ii, ndt, ndi, iter, irteti, codret
    real(kind=8) :: sige(6), lgleps, gamp, se(6), siie, invare, yd(10)
    real(kind=8) :: gamps, invars, b, s(6), delta, dy(10), yf(10)
    real(kind=8) :: fiter, dkooh(6, 6), epsf(6), i1, traceg, trois
    real(kind=8) :: evp, evps
    character(len=10) :: ctol, citer
    character(len=24) :: valk(2)
! ======================================================================
! --- INITIALISATION DE PARAMETRE --------------------------------------
! ======================================================================
    parameter       ( trois   =  3.0d0   )
    parameter       ( lgleps  =  1.0d-8  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- INITIALISATION DES VARIABLES -------------------------------------
! ======================================================================
    irteti = 0
    delta = 0.0d0
    gamp = vind ( 1 )
    evp = vind ( 2 )
    call lceqvn(ndt, sigf, sige)
    call lcdevi(sige, se)
    siie=ddot(ndt,se,1,se,1)
    siie = sqrt (siie)
    invare = trace (ndi,sige)
! ======================================================================
! --- INITIALISATION YD = (SIG, INVAR, GAMP, EVP, DELTA) ---------------
! ======================================================================
    call lceqvn(ndt, se, yd)
    yd(ndt+1)=invare
    yd(ndt+2)=gamp
    yd(ndt+3)=evp
    yd(ndt+4)=delta
! ======================================================================
! --- CALCUL A PRIORI DE LA PROJECTION AU SOMMET -----------------------
! ======================================================================
    call calcpj(nbmat, mater, gamp, evp, sigd,&
                sige, lgleps, invare, gamps, evps,&
                invars, b)
! ======================================================================
! --- FAUT-IL FAIRE UNE PROJECTION AU SOMMET DU DOMAINE ? --------------
! ======================================================================
    if (prjsom(nbmat, mater, invare, invars, b, siie, 'SUPERIEUR')) then
! ======================================================================
! --- LA PROJECTION AU SOMMET DU DOMAINE EST RETENUE -------------------
! ======================================================================
        do 10 ii = 1, ndt
            sigf(ii) = 0.0d0
10      continue
        do 20 ii = 1, ndi
            sigf(ii) = invars / trois
20      continue
        call lcopil('ISOTROPE', mod, mater(1, 1), dkooh)
        call lcprmv(dkooh, sigf, epsf)
        if (mod .eq. 'C_PLAN') then
            sigf(3) = 0.0d0
            epsf(3) = dkooh(3,1) * sigf(1) + dkooh(3,2) * sigf(2) + dkooh(3,4) * sigf(4)
        endif
        vinf(1)=gamps
        vinf(2)=evps
        vinf(nvi) = 1.0d0
        irteti = 0
    else
! ======================================================================
! --- LA PROJECTION AU SOMMET DU DOMAINE N'EST PAS RETENUE -------------
! ======================================================================
! --- CALCUL INITIAL (ITERATION 0) -------------------------------------
! ======================================================================
        call lglini(yd, nbmat, mater, seuil, sigd,&
                    deps, devg, devgii, traceg, dy,&
                    codret)
        if (codret .ne. 0) goto 100
        iter = 0
 1      continue
! ======================================================================
! --- ITERATION ITER ---------------------------------------------------
! ======================================================================
! --- INCREMENTATION DES VARIABLES -------------------------------------
! ======================================================================
        call lcsovn(nr-1, yd, dy, yf)
! ======================================================================
! --- VERIFICATION DE LA COHERENCE DE GAMP -----------------------------
! ======================================================================
        if (yf(ndt+2) .lt. 0.0d0) then
! ======================================================================
! --- GAMP < 0 ---------------------------------------------------------
! --- PEUT-ON FAIRE UN DECOUPAGE DE L'INCREMENT DE DEPLACEMENT ? -------
! ======================================================================
            if (icomp .eq. 0 .or. icomp .eq. 1) then
                call codent(iter, 'G', citer)
                call codree(toler, 'E', ctol)
                valk(1) = citer
                valk(2) = ctol
                call u2mesk('I', 'ALGORITH2_57', 2, valk)
                irteti = 3
                goto 100
            else
                call u2mesk('I', 'ALGELINE5_52', 0, ' ')
!               CALL UTEXCM(23,'ALGELINE5_52',0,' ',1,VALI,1,VALR)
                codret = 2
            endif
        endif
! ======================================================================
        delta = delta + dy(nr)
        yf(nr)=delta
! ======================================================================
! --- CALCUL DE F A L'ITERATION ITER + 1 -------------------------------
! ======================================================================
        call lgldom(nbmat, mater, yf, fiter)
! ======================================================================
! --- A-T-ON CONVERGE ? ------------------------------------------------
! ======================================================================
        if (lglcov(fiter,toler)) then
! ======================================================================
! --- IL Y A CONVERGENCE -----------------------------------------------
! ======================================================================
! --- MISE A JOUR DES VARIABLES INTERNES -------------------------------
! ======================================================================
            call lceqvn(ndt, yf(1), s(1))
            i1  =yf(ndt+1)
            gamp=yf(ndt+2)
            evp =yf(ndt+3)
            do 30 ii = 1, ndt
                sigf(ii) = s(ii)
30          continue
            do 40 ii = 1, ndi
                sigf(ii) = sigf(ii) + i1/trois
40          continue
            call lcopil('ISOTROPE', mod, mater(1, 1), dkooh)
            call lcprmv(dkooh, sigf, epsf)
            if (mod .eq. 'C_PLAN') then
                sigf(3) = 0.0d0
                epsf(3) = dkooh(3,1) * sigf(1) + dkooh(3,2) * sigf(2) + dkooh(3,4) * sigf(4)
            endif
            vinf(1)=gamp
            vinf(2)=evp
            vinf(nvi) = 1.0d0
            irteti = 0
        else
! ======================================================================
! --- IL N'Y A PAS CONVERGENCE -----------------------------------------
! ======================================================================
            if (iter .lt. itmax) then
! ======================================================================
! --- LE NOMBRE D'ITERATION MAXIMAL N'A PAS ETE ATTEINT ----------------
! ======================================================================
                iter = iter + 1
! ======================================================================
! --- NOUVEAU CALCUL PLASTIQUE -----------------------------------------
! ======================================================================
                call lglite(yf, nbmat, mater, fiter, devg,&
                            devgii, traceg, dy, codret)
                irteti = 1
                if (codret .ne. 0) goto 100
            else
! ======================================================================
! --- ON NE CONVERGE VRAIMENT PAS ! ------------------------------------
! ======================================================================
! --- FAUT-IL PROJETER AU SOMMET DU DOMAINE ? --------------------------
! ======================================================================
                if (prjsom( nbmat, mater, invare, invars, b, siie, 'INFERIEUR')) then
! ======================================================================
! --- DECOUPAGE
! ======================================================================
                    if (icomp .eq. 0 .or. icomp .eq. 1) then
                        call codent(iter, 'G', citer)
                        call codree(toler, 'E', ctol)
                        valk(1) = citer
                        valk(2) = ctol
                        call u2mesk('I', 'ALGORITH2_57', 2, valk)
                        irteti = 3
                        goto 100
                    else
                        call u2mesk('I', 'ALGELINE5_52', 0, ' ')
!                     CALL UTEXCM(23,'ALGELINE5_52',0,' ',1,VALI,1,VALR)
                        codret = 2
                    endif
! ======================================================================
! --- ON PROJETE AU SOMMET DU DOMAINE ----------------------------------
! ======================================================================
! --- MISE A JOUR DES VARIABLES INTERNES -------------------------------
! ======================================================================
                    do 50 ii = 1, ndt
                        sigf(ii) = s(ii)
50                  continue
                    do 60 ii = 1, ndi
                        sigf(ii) = invars/trois
60                  continue
                    call lcopil('ISOTROPE', mod, mater(1, 1), dkooh)
                    call lcprmv(dkooh, sigf, epsf)
                    if (mod .eq. 'C_PLAN') then
                        sigf(3) = 0.0d0
                        epsf(3) = dkooh(3,1) * sigf(1) + dkooh(3,2) * sigf(2) + dkooh(3,4) * sigf&
                                  &(4)
                    endif
                    vinf(1)=gamps
                    vinf(2)=evp
                    vinf(nvi) = 1.0d0
                    irteti = 0
                else
! ======================================================================
! --- IL N'Y A PAS CONVERGENCE -----------------------------------------
! --- PEUT-ON FAIRE UN DECOUPAGE DE L'INCREMENT DE DEPLACEMENT ? -------
! ======================================================================
                    if (icomp .eq. 0 .or. icomp .eq. 1) then
                        call codent(iter, 'G', citer)
                        call codree(toler, 'E', ctol)
                        valk(1) = citer
                        valk(2) = ctol
                        call u2mesk('I', 'ALGORITH2_57', 2, valk)
                        irteti = 3
                        goto 100
                    else
                        call u2mesk('I', 'ALGELINE5_52', 0, ' ')
!                     CALL UTEXCM(23,'ALGELINE5_52',0,' ',1,VALI,1,VALR)
                        codret = 2
                    endif
                endif
            endif
        endif
        if (irteti .eq. 1) goto 1
    endif
100  continue
    if (irteti .eq. 3) then
        irtet = 1
    else
        irtet = 0
    endif
    if (codret .eq. 2) irtet = 2
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

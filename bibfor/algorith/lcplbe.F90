subroutine lcplbe(toler, itmax, nmat, materf, nvi,&
                  vind, sigf, vinf, elgeom, nseuil,&
                  irteti)
! aslint: disable=W1501
    implicit none
!       ================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       INTEGRATION ELASTO-PLASTIQUE DE LOIS DE COMPORTEMENT BETON SUR
!       DT  ( Y = ( DPC , DPT ))
!       BETON_DOUBLE_DP: LOI ELASTO PLASTIQUE AVEC DOUBLE CRITERE DE
!       PLASTICITE AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
!       LE SYSTEME A RESOUDRE SE REDUIT A UN SYSTEME NON LINEAIRE D'UNE
!       OU DEUX EQUATIONS A DEUX INCONNUES (LES DEUX MULTIPLICATEURS
!       PLASTIQUES, EN COMPRESSION ET EN TRACTION)
!       ON RESOUD : Y = ( DPC , DPT )
!
!       ON RESOUD DONC                  R(DY) = 0
!       PAR UNE METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
!                                       DYI+1 = DYI + DDYI  (DYO DEBUT)
!       ET ON REACTUALISE               YF = YD + DY
!       ----------------------------------------------------------------
!
!       IN  TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
!           ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
!           NMAT   :  DIMENSION MATER
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           TEMPD  :  TEMPERATURE A T
!           TEMPF  :  TEMPERATURE A T+DT
!           TIMED  :  INSTANT  T
!           TIMEF  :  INSTANT T+DT
!           EPSD   :  DEFORMATION A T
!           VIND   :  VARIABLES INTERNES A T
!           NVI    :  NB VARIABLES INTERNES
!           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                     AUX LOIS DE COMPORTEMENT
!       VAR NSEUIL :  INDICE DE CRITERE ACTIVE
!       VAR DEPS   :  INCREMENT DE DEFORMATION
!       VAR SIGF   :  IN  : PREDICTION ELASTIQUE DE LA CONTRAINTE A T+DT
!                  :  OUT : CONTRAINTE ELASTOPLASTIQUE A T+DT
!       OUT VINF   :  VARIABLES INTERNES A T+DT
!           IRTETI = 1:  CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
!       ----------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/betfpp.h"
#include "asterfort/betinc.h"
#include "asterfort/betini.h"
#include "asterfort/codent.h"
#include "asterfort/codree.h"
#include "asterfort/lceqvn.h"
#include "asterfort/utmess.h"
    integer :: nmat, nseuil
!
    integer :: itmax, nprojs, nessai, osci
    integer :: ndt, ndi, nvi, iter1, iter2, iter3, iter4
!
    real(kind=8) :: toler, zero, precm
    parameter       ( zero =  0.d0   )
    real(kind=8) :: sigf(6)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: materf(nmat, 2), elgeom(*)
!
!
    real(kind=8) :: fcomp, fcomp3, ftrac, ftrac3, sige(6)
    real(kind=8) :: sigeq, sigh, dfcdlc, dftdlt, csec
    real(kind=8) :: coefa(2, 2), coefb(2), coefar(2, 2), coefbr(2)
    real(kind=8) :: dpc, dpt, jac(2, 2), epsi, epsi2, delta, mdelta
    real(kind=8) :: ddpc, ddpt, err, err2, pc, pt, fc, ft, e
    real(kind=8) :: kuc, kut, ke, coneco, conetr, permut, ftrael
    real(kind=8) :: verifc, verift, fc0, ft0, mepsi, fcp, ftp
    real(kind=8) :: ddpt0, ddpc0, ftrac2, ftrac1, fcomp1, fcomp2
    character(len=10) :: ctol, citer, cerr
    character(len=24) :: valk(3)
    integer :: irteti
    aster_logical :: conver
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
!
    precm = 100.d0 * r8prem()
    conver = .false.
    fcp = materf(1,2)
    ftp = materf(2,2)
    ddpc = 0.d0
    ddpt = 0.d0
!
! --  CONTRAINTE ISSUE DE LA PREDICTION ELASTIQUE (DEJA CALCULEE)
!
    call lceqvn(ndt, sigf, sige)
!
! --  CALCUL DES COEFFICIENTS CONSTANTS DU SYSTEME NON LINEAIRE
!
    call betini(materf, nmat, sige, sigeq, sigh,&
                coefa, coefb, coefar, coefbr, coneco,&
                conetr)
!
    epsi = 1.d-6
    epsi2 = toler * sigeq / fcp
    if (epsi2 .lt. toler) epsi2 = toler
    mepsi = -1.d-6
! --  COEFFICIENT DE SECURITE POUR LA CONVERGENCE
    csec = 1.d-1
!
! --  CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
!     A L'INSTANT MOINS
!
    pc = vind(1)
    pt = vind(2)
    call betfpp(materf, nmat, elgeom, pc, pt,&
                3, fc0, ft0, dfcdlc, dftdlt,&
                kuc, kut, ke)
!
! --  TEST DE PERMUTATION DES SOMMETS DES CONES DE TRACTION ET
!     COMPRESSION - ACTIVATION DU CRITERE DE TRACTION
!
    permut = (coefbr(1) * ft0) / (coefbr(2) * fc0) - 1.d0
    ftrael = coefb(2) - ft0
    nprojs = 0
!
    if ((permut .gt. mepsi .and. coneco .gt. fc0) .or. (conetr .gt. ft0) .or. (nseuil.gt.4)) then
!
        if ((&
            (&
            (permut.gt.mepsi .and. coneco.gt.fc0 .and. ftrael.gt. zero) .or.&
            (conetr.gt.ft0)&
            )&
            .and. nseuil .ne. 11 .and. nseuil .ne. 33&
            )&
            .or. (nseuil.eq.22)) then
!-----------------------------------------------------------------------
! --        RESOLUTION AVEC PROJECTION AU SOMMET DU CONE DE TRACTION
!-----------------------------------------------------------------------
! --        ON EFFECTUE LA RESOLUTION AVEC PROJECTION AU SOMMET DU
! --        CONE DE TRACTION SI ELLE EST DEMANDEE (NSEUIL=22) OU SI
! --        LA CONDITION SUR FT0 EST REALISEE
!
            osci = 0
            ftrac1 = zero
            conver = .false.
            nessai = 22
!
! --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
!           D'ADOUCISSEMENT
!
            pc = vind(1)
            pt = vind(2)
            call betfpp(materf, nmat, elgeom, pc, pt,&
                        nessai, fc, ft, dfcdlc, dftdlt,&
                        kuc, kut, ke)
!
! --        CALCUL DES VALEURS DES CRITERES
!
            dpc = zero
            dpt = zero
            ftrac = coefbr(2) + coefar(2,2) * dpt - ft
            fcomp = zero
!
! --        DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
!
            iter1 = 0
  1         continue
            iter1 = iter1 + 1
            ddpt0 = ddpt
            if (osci .eq. 0) then
!
! --           CALCUL DU JACOBIEN
!
                jac(2,2) = coefar(2,2) - dftdlt
!
! --           MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
! --           (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
!
                e = materf(1,1)
                delta = jac(2,2)
                if (abs(delta/e) .lt. epsi) then
                    call utmess('F', 'ALGORITH4_72')
                else
                    mdelta = -1.d0 / delta
                endif
                ddpt = mdelta * ftrac
!
! --           EN CAS D'OSCILLATIONS : RESO PAR DICHOTOMIE
                if (iter1 .gt. 7) then
                    if (abs(ddpt/dpt) .lt. 1.d-3 .and. abs(ddpt) .gt. precm) then
                        if ((ftrac2*ftrac1) .lt. zero) then
                            osci = 20
                            ddpt = -0.5d0 * ddpt0
                        endif
                    endif
                endif
!
            else
                if (ftrac*ftrac1 .lt. zero) then
                    ddpt = -0.5d0 * ddpt0
                else
                    ftrac1 = ftrac2
                    ddpt = 0.5d0 * ddpt0
                endif
            endif
!
            dpt = dpt + ddpt
!
! --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
! --        D'ADOUCISSEMENT
!
            pc = vind(1) + dpc
            pt = vind(2) + dpt
            call betfpp(materf, nmat, elgeom, pc, pt,&
                        nessai, fc, ft, dfcdlc, dftdlt,&
                        kuc, kut, ke)
!
! --        CALCUL DES VALEURS DES CRITERES
!
            ftrac2 = ftrac1
            ftrac1 = ftrac
            ftrac = coefbr(2) + coefar(2,2) * dpt - ft
!
! --        TEST DE CONVERGENCE
!
            err = abs(ftrac/ftp)
            if (abs(ddpt/dpt) .lt. precm) iter1 = itmax+osci
!
            if (err .le. (toler * csec)) then
!              -->>  CONVERGENCE   -->> FIN
                conver = .true.
            else
                if (iter1 .ge. itmax+osci) then
                    err2 = abs(ddpt/dpt)
                    if (err2 .le. toler) then
!     -->>           NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
!                    INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
!                    MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                        conver = .true.
                        if (err2 .gt. (toler*100)) then
                            call codent(iter1, 'G', citer)
                            call codree(toler, 'E', ctol)
                            call codree((err/csec), 'E', cerr)
                            valk(1) = citer
                            valk(2) = cerr
                            valk(3) = ctol
                            call utmess('A', 'ALGORITH4_73', nk=3, valk=valk)
                        endif
                    else
!     -->>           NB MAX D'ITERATIONS DEPASSE  -->> FIN
!                    MESSAGE D'ALARME - ON POURSUIT AVEC RESO STANDARD
                        call codent(iter1, 'G', citer)
                        call codree(toler, 'E', ctol)
                        call codree((err/csec), 'E', cerr)
                        valk(1) = citer
                        valk(2) = cerr
                        valk(3) = ctol
                        call utmess('A', 'ALGORITH4_74', nk=3, valk=valk)
                    endif
                else
!     -->>        NOUVELLE ITERATION -->> RETOUR
                    goto 1
                endif
            endif
!
! --        VERIFICATION DE L'ETAT FINAL OBTENU
!
            fcomp3 = coefbr(1) + coefar(1,2) * dpt - fc0
            ftrac3 = coefbr(2) + coefar(2,2) * dpt - ft
!
! --        VERIFICATION DE LA SOLUTION
!
            if ((ftrac3/ftp) .gt. epsi2 .and. dpt .gt. zero) then
                call utmess('A', 'ALGORITH4_75')
                conver = .false.
            endif
            if ((fcomp3/fcp) .gt. epsi2) then
                call utmess('A', 'ALGORITH4_76')
                conver = .false.
            endif
!
! --        CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
!           CONTRAINTES
!
            if (conver .and. dpt .gt. zero) then
                vinf(1) = vind(1) + dpc
                vinf(2) = vind(2) + dpt
                vinf (nvi) = 1.d0 * nessai
                call betinc(materf, nmat, sige, nessai, dpc,&
                            dpt, sigf, verifc, verift)
                if (verift .gt. zero .or. nseuil .gt. 4) then
                    nprojs = nessai
                endif
            endif
!
        endif
!
!
!
        if (nprojs .eq. 0 .and.&
            (&
            (&
            ( ( permut .gt. mepsi .and. coneco .gt. fc0 .and. ftrael .gt. zero ) ) .and.&
            nseuil .ne. 11&
            )&
            .or. nseuil .eq. 22 .or. nseuil .eq. 33&
            )) then
!-----------------------------------------------------------------------
! --        RESOLUTION AVEC PROJECTION AU SOMMET DES CONES DE
! --        COMPRESSION ET DE TRACTION
!-----------------------------------------------------------------------
! --        ON EFFECTUE LA RESOLUTION AVEC PROJECTION AU SOMMET DES
! --        CONES DE COMPRESSION ET DE TRACTION SI ELLE EST DEMANDEE
! --        (NSEUIL=33) OU SI LA CONDITION SUR FT0 ET FC0 EST REALISEE
!
            osci = 0
            conver = .false.
            nessai = 33
!
! --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
!           D'ADOUCISSEMENT
!
            pc = vind(1)
            pt = vind(2)
            call betfpp(materf, nmat, elgeom, pc, pt,&
                        nessai, fc, ft, dfcdlc, dftdlt,&
                        kuc, kut, ke)
!
! --        CALCUL DES VALEURS DES CRITERES
!
            dpc = zero
            dpt = zero
            fcomp = coefbr(1) + coefar(1,1)*dpc + coefar(1,2)*dpt - fc
            ftrac = coefbr(2) + coefar(2,1)*dpc + coefar(2,2)*dpt - ft
!
! --        DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
!
            iter2 = 0
  2         continue
            iter2 = iter2 + 1
!
! --        CALCUL DU JACOBIEN
!
            jac(1,1) = coefar(1,1) - dfcdlc
            jac(1,2) = coefar(1,2)
            jac(2,1) = coefar(2,1)
            jac(2,2) = coefar(2,2) - dftdlt
!
! --        MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
! --        (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
!
            e = materf(1,1)
            delta = (jac(1,1) * jac(2,2) - jac(1,2) * jac(2,1))
            if (abs(delta/e) .lt. epsi) then
!               SI DFCDLC=DFTDLT=0 LES DEUX EQUATIONS DU SYSTEME A
!               RESOUDRE SONT IDENTIQUES --> ON PASSE DIRECTEMENT A LA
!               PROJECTION AU SOMMET DU CONE DE COMPRESSION SEUL.
                if ((vind(1) + dpc) .lt. kuc .or. (vind(2) + dpt) .lt. kut) then
                    call utmess('F', 'ALGORITH4_77')
                else
                    ddpc = zero
                    ddpt = zero
                    iter2 = itmax
                endif
            else
                mdelta = -1.d0 / delta
                ddpc = (jac(2,2) * fcomp - jac(1,2) * ftrac) * mdelta
                ddpt = (jac(1,1) * ftrac - jac(2,1) * fcomp) * mdelta
            endif
!
            dpc = dpc + ddpc
            dpt = dpt + ddpt
!
!
! --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
! --        D'ADOUCISSEMENT
!
            pc = vind(1) + dpc
            pt = vind(2) + dpt
            call betfpp(materf, nmat, elgeom, pc, pt,&
                        nessai, fc, ft, dfcdlc, dftdlt,&
                        kuc, kut, ke)
!
! --        CALCUL DES VALEURS DES CRITERES
!
            fcomp = coefbr(1) + coefar(1,1)*dpc + coefar(1,2)*dpt - fc
            ftrac = coefbr(2) + coefar(2,1)*dpc + coefar(2,2)*dpt - ft
!
! --        TEST DE CONVERGENCE
!
            err = abs(ftrac/ftp) + abs(fcomp/fcp)
            if (abs(ddpc/dpc) .lt. precm .and. abs(ddpt/dpt) .lt. precm) iter2 = itmax
!
            if (err .le. (toler * csec)) then
!              -->>  CONVERGENCE   -->> FIN
                conver = .true.
            else
                if (iter2 .ge. itmax) then
                    err2 = abs(ddpc/dpc) + abs(ddpt/dpt)
                    if (err2 .le. toler) then
!     -->>           NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
!                    INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
!                    MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                        conver = .true.
                        if (err2 .gt. (toler*100)) then
                            call codent(iter2, 'G', citer)
                            call codree(toler, 'E', ctol)
                            call codree((err/csec), 'E', cerr)
                            valk(1) = citer
                            valk(2) = cerr
                            valk(3) = ctol
                            call utmess('A', 'ALGORITH4_73', nk=3, valk=valk)
                        endif
                    else
!     -->>           NB MAX D'ITERATIONS DEPASSE  -->> FIN
!                    MESSAGE D'ALARME - ON POURSUIT AVEC RESO STANDARD
                        call codent(iter2, 'G', citer)
                        call codree(toler, 'E', ctol)
                        call codree((err/csec), 'E', cerr)
                        valk(1) = citer
                        valk(2) = cerr
                        valk(3) = ctol
                        call utmess('A', 'ALGORITH4_74', nk=3, valk=valk)
                    endif
                else
!     -->>        NOUVELLE ITERATION -->> RETOUR
                    goto 2
                endif
            endif
!
! --        VERIFICATION DE L'ETAT FINAL OBTENU
!
            fcomp3 = coefbr(1) + coefar(1,1)*dpc + coefar(1,2)*dpt - fc
            ftrac3 = coefbr(2) + coefar(2,1)*dpc + coefar(2,2)*dpt - ft
!
! --        VERIFICATION DE LA SOLUTION
!
            if ((ftrac3/ftp) .gt. epsi2 .and. dpt .gt. zero .and. conver) then
                call utmess('A', 'ALGORITH4_78')
                conver = .false.
            endif
            if ((fcomp3/fcp) .gt. epsi2 .and. dpc .gt. zero .and. conver) then
                call utmess('A', 'ALGORITH4_79')
                conver = .false.
            endif
!
! --        CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
!           CONTRAINTES
!
            if (conver .and. dpc .gt. zero .and. dpt .gt. zero) then
                vinf(1) = vind(1) + dpc
                vinf(2) = vind(2) + dpt
                vinf (nvi) = 1.d0 * nessai
                call betinc(materf, nmat, sige, nessai, dpc,&
                            dpt, sigf, verifc, verift)
                if (verifc .gt. zero .or. nseuil .gt. 4) then
                    nprojs = nessai
                endif
            endif
!
        endif
!
!
!
        if (nprojs .eq. 0 .and.&
            (&
            (permut .gt. mepsi .and. coneco .gt. fc0) .or. nseuil .eq. 11 .or. nseuil .eq.&
            22 .or. nseuil .eq. 33&
            )) then
!-----------------------------------------------------------------------
! --        RESOLUTION AVEC PROJECTION AU SOMMET DU CONE DE COMPRESSION
!-----------------------------------------------------------------------
!
! --        ON EFFECTUE LA RESOLUTION AVEC PROJECTION AU SOMMET DU
! --        CONE DE COMPRESSION SI ELLE EST DEMANDEE (NSEUIL=11) OU SI
! --        LA CONDITION SUR FC0 EST REALISEE
!
            osci = 0
            fcomp1 = zero
            conver = .false.
            nessai = 11
!
! --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
!           D'ADOUCISSEMENT
!
            pc = vind(1)
            pt = vind(2)
            call betfpp(materf, nmat, elgeom, pc, pt,&
                        nessai, fc, ft, dfcdlc, dftdlt,&
                        kuc, kut, ke)
!
! --        CALCUL DES VALEURS DES CRITERES
!
            dpc = zero
            dpt = zero
            ftrac = zero
            fcomp = coefbr(1) + coefar(1,1) * dpc - fc
!
! --        DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
!
            iter3 = 0
  3         continue
            iter3 = iter3 + 1
            ddpc0 = ddpc
            if (osci .eq. 0) then
!
! --           CALCUL DU JACOBIEN
!
                jac(1,1) = coefar(1,1) - dfcdlc
!
! --           MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
! --           (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
!
                e = materf(1,1)
                delta = jac(1,1)
                if (abs(delta/e) .lt. epsi) then
                    call utmess('F', 'ALGORITH4_80')
                else
                    mdelta = -1.d0 / delta
                endif
                ddpc = mdelta * fcomp
!
! --           EN CAS D'OSCILLATIONS : RESO PAR DICHOTOMIE
                if (iter3 .gt. 7) then
                    if (abs(ddpc/dpc) .lt. 1.d-3 .and. abs(ddpc) .gt. precm) then
                        if ((fcomp2*fcomp1) .lt. zero) then
                            osci = 20
                            ddpc = -0.5d0 * ddpc0
                        endif
                    endif
                endif
!
            else
                if (fcomp*fcomp1 .lt. zero) then
                    ddpc = -0.5d0 * ddpc0
                else
                    fcomp1 = fcomp2
                    ddpc = 0.5d0 * ddpc0
                endif
            endif
!
            dpc = dpc + ddpc
!
!
! --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
! --        D'ADOUCISSEMENT
!
            pc = vind(1) + dpc
            pt = vind(2) + dpt
            call betfpp(materf, nmat, elgeom, pc, pt,&
                        nessai, fc, ft, dfcdlc, dftdlt,&
                        kuc, kut, ke)
!
! --        CALCUL DES VALEURS DES CRITERES
!
            fcomp2 = fcomp1
            fcomp1 = fcomp
            fcomp = coefbr(1) + coefar(1,1) * dpc - fc
!
! --        TEST DE CONVERGENCE
!
            err = abs(fcomp/fcp)
            if (abs(ddpc/dpc) .lt. precm) iter3 = itmax+osci
!
            if (err .le. (toler * csec)) then
!              -->>  CONVERGENCE   -->> FIN
                conver = .true.
            else
                if (iter3 .ge. itmax+osci) then
                    err2 = abs(ddpc/dpc)
                    if (err2 .le. toler) then
!     -->>           NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
!                    INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
!                    MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                        conver = .true.
                        if (err2 .gt. (toler*100)) then
                            call codent(iter3, 'G', citer)
                            call codree(toler, 'E', ctol)
                            call codree((err/csec), 'E', cerr)
                            valk(1) = citer
                            valk(2) = cerr
                            valk(3) = ctol
                            call utmess('A', 'ALGORITH4_73', nk=3, valk=valk)
                        endif
                    else
!     -->>           NB MAX D'ITERATIONS DEPASSE  -->> FIN
!                    MESSAGE D'ALARME - ON POURSUIT AVEC RESO STANDARD
                        call codent(iter3, 'G', citer)
                        call codree(toler, 'E', ctol)
                        call codree((err/csec), 'E', cerr)
                        valk(1) = citer
                        valk(2) = cerr
                        valk(3) = ctol
                        call utmess('A', 'ALGORITH4_74', nk=3, valk=valk)
                    endif
                else
!     -->>        NOUVELLE ITERATION -->> RETOUR
                    goto 3
                endif
            endif
!
! --        VERIFICATION DE L'ETAT FINAL OBTENU
!
            fcomp3 = coefbr(1) + coefar(1,1) * dpc - fc
            ftrac3 = coefbr(2) + coefar(2,1) * dpc - ft0
!
! --        VERIFICATION DE LA SOLUTION
!
            if ((fcomp3/fcp) .gt. epsi2 .and. dpc .gt. zero) then
                call utmess('A', 'ALGORITH4_81')
                conver = .false.
            endif
!
            if ((ftrac3/ftp) .gt. epsi2) then
                call utmess('A', 'ALGORITH4_82')
                conver = .false.
            endif
!
! --        CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
!           CONTRAINTES
!
            if (conver .and. dpc .gt. zero) then
                vinf(1) = vind(1) + dpc
                vinf(2) = vind(2) + dpt
                vinf (nvi) = 1.d0 * nessai
                call betinc(materf, nmat, sige, nessai, dpc,&
                            dpt, sigf, verifc, verift)
                if (verifc .gt. zero .or. nseuil .gt. 4) then
                    nprojs = nessai
                endif
            endif
!
        endif
!
        if (nprojs .gt. 0) then
            nseuil = nprojs
        else
            nseuil = 44
        endif
    else
!-----------------------------------------------------------------------
! --     RESOLUTION STANDARD : INTEGRATION ELASTO-PLASTIQUE DE LA LOI
! --     DE COMPORTEMENT BETON DANS LES CAS OU NSEUIL = 1, 2 OU 3
!-----------------------------------------------------------------------
!
! --     CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
!
        osci = 0
        fcomp1 = zero
        ftrac1 = zero
        pc = vind(1)
        pt = vind(2)
        call betfpp(materf, nmat, elgeom, pc, pt,&
                    nseuil, fc, ft, dfcdlc, dftdlt,&
                    kuc, kut, ke)
!
! --     CALCUL DES VALEURS DES CRITERES
!
        dpc = zero
        dpt = zero
        fcomp = coefb(1) + coefa(1,1) * dpc + coefa(1,2) * dpt - fc
        ftrac = coefb(2) + coefa(2,1) * dpc + coefa(2,2) * dpt - ft
!
! --     DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
!
        iter4 = 0
  4     continue
        iter4 = iter4 + 1
        ddpc0 = ddpc
        ddpt0 = ddpt
!
        if (osci .eq. 0) then
!
! --        CALCUL DU JACOBIEN
!
            jac(1,1) = coefa(1,1) - dfcdlc
            jac(1,2) = coefa(1,2)
            jac(2,1) = coefa(2,1)
            jac(2,2) = coefa(2,2) - dftdlt
!
! --        MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
! --        (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
!
            e = materf(1,1)
            if (nseuil .eq. 1) then
                delta = jac(1,1)
                if (abs(delta/e) .lt. epsi) then
                    call utmess('F', 'ALGORITH4_83')
                else
                    mdelta = -1.d0 / delta
                endif
                ddpc = mdelta * fcomp
                ddpt = zero
            else if (nseuil.eq.2) then
                delta = jac(2,2)
                if (abs(delta/e) .lt. epsi) then
                    call utmess('F', 'ALGORITH4_83')
                else
                    mdelta = -1.d0 / delta
                endif
                ddpc = zero
                ddpt = mdelta * ftrac
            else if (nseuil.eq.3) then
                delta = (jac(1,1) * jac(2,2) - jac(1,2) * jac(2,1))
                if (abs(delta/e) .lt. epsi) then
                    call utmess('F', 'ALGORITH4_83')
                else
                    mdelta = -1.d0 / delta
                endif
                ddpc = (jac(2,2) * fcomp - jac(1,2) * ftrac) * mdelta
                ddpt = (jac(1,1) * ftrac - jac(2,1) * fcomp) * mdelta
            else
                call utmess('A', 'ALGORITH4_84')
                goto 5
            endif
!
! --        EN CAS D'OSCILLATIONS : RESO PAR DICHOTOMIE
            if (iter4 .gt. 7 .and. nseuil .eq. 2) then
                if (abs(ddpt/dpt) .lt. 1.d-3 .and. abs(ddpt) .gt. precm) then
                    if ((ftrac2*ftrac1) .lt. zero) then
                        osci = 20
                        ddpt = -0.5d0 * ddpt0
                    endif
                endif
            endif
            if (iter4 .gt. 7 .and. nseuil .eq. 1) then
                if (abs(ddpc/dpc) .lt. 1.d-3 .and. abs(ddpc) .gt. precm) then
                    if ((fcomp2*fcomp1) .lt. zero) then
                        osci = 20
                        ddpc = -0.5d0 * ddpc0
                    endif
                endif
            endif
!
        else
            if (nseuil .eq. 2) then
                if (ftrac*ftrac1 .lt. zero) then
                    ddpt = -0.5d0 * ddpt0
                else
                    ftrac1 = ftrac2
                    ddpt = 0.5d0 * ddpt0
                endif
            endif
            if (nseuil .eq. 1) then
                if (fcomp*fcomp1 .lt. zero) then
                    ddpc = -0.5d0 * ddpc0
                else
                    fcomp1 = fcomp2
                    ddpc = 0.5d0 * ddpc0
                endif
            endif
        endif
!
!
        dpc = dpc + ddpc
        dpt = dpt + ddpt
!
! --     CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
!
        pc = vind(1) + dpc
        pt = vind(2) + dpt
        call betfpp(materf, nmat, elgeom, pc, pt,&
                    nseuil, fc, ft, dfcdlc, dftdlt,&
                    kuc, kut, ke)
!
! --     CALCUL DES VALEURS DES CRITERES
!
        fcomp2 = fcomp1
        fcomp1 = fcomp
        ftrac2 = ftrac1
        ftrac1 = ftrac
        fcomp = coefb(1) + coefa(1,1) * dpc + coefa(1,2) * dpt - fc
        ftrac = coefb(2) + coefa(2,1) * dpc + coefa(2,2) * dpt - ft
!
! --     TEST DE CONVERGENCE
!
        if (nseuil .eq. 3) then
            err = abs(fcomp/fcp) + abs(ftrac/ftp)
            if (abs(ddpc/dpc) .lt. precm .and. abs(ddpt/dpt) .lt. precm) iter4 = itmax+osci
        else if (nseuil.eq.2) then
            err = abs(ftrac/ftp)
            if (abs(ddpt/dpt) .lt. precm) iter4 = itmax+osci
        else if (nseuil.eq.1) then
            err = abs(fcomp/fcp)
            if (abs(ddpc/dpc) .lt. precm) iter4 = itmax+osci
        else
            call utmess('F', 'ALGORITH4_85')
        endif
!
!
        if (err .le. (toler * csec)) then
!     -->>     CONVERGENCE   -->> FIN
        else
            if (iter4 .ge. itmax+osci) then
                if (nseuil .eq. 3) then
                    err2 = abs(ddpc/dpc) + abs(ddpt/dpt)
                else if (nseuil.eq.2) then
                    err2 = abs(ddpt/dpt)
                else if (nseuil.eq.1) then
                    err2 = abs(ddpc/dpc)
                endif
!              SI L'UN DES INCREMENTS DE DEFORMATION EST NEGATIF, ON
!              FAIT UN NOUVEL ESSAI AVEC UNE AUTRE VALEUR DE NSEUIL,
!              APRES PASSAGE DANS BETCVC. IL N'EST DONC PAS NECESSAIRE
!              DE CONVERGER ! ON FORCE LA CONVERGENCE.
!              DANS LE CAS CONTRAIRE :
                if (dpt .ge. zero .and. dpc .ge. zero) then
                    if (err2 .le. toler) then
!        -->>      NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
!                  INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
!                  MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                        if (err2 .gt. (toler*100)) then
                            call codent(iter4, 'G', citer)
                            call codree(toler, 'E', ctol)
                            call codree((err/csec), 'E', cerr)
                            valk(1) = citer
                            valk(2) = cerr
                            valk(3) = ctol
                            call utmess('A', 'ALGORITH4_73', nk=3, valk=valk)
                        endif
                    else
!     -->>         NB MAX D'ITERATIONS DEPASSE  -->> FIN
                        call codent(iter4, 'G', citer)
                        call codree(toler, 'E', ctol)
                        call codree((err/csec), 'E', cerr)
                        valk(1) = citer
                        valk(2) = cerr
                        valk(3) = ctol
                        call utmess('A', 'ALGORITH4_74', nk=3, valk=valk)
                        nseuil = 4
                        goto 5
                    endif
                endif
            else
!     -->>     NOUVELLE ITERATION -->> RETOUR
                goto 4
            endif
        endif
!
! --     VERIFICATION DE L'ETAT FINAL OBTENU
!
        fcomp3 = coefb(1) + coefa(1,1) * dpc + coefa(1,2) * dpt - fc
        ftrac3 = coefb(2) + coefa(2,1) * dpc + coefa(2,2) * dpt - ft
!
! --     VERIFICATION DE LA SOLUTION
!
        if (nseuil .eq. 3) then
            if ((fcomp3/fcp) .gt. epsi2 .and. (ftrac3/ftp) .gt. epsi2) then
                if (dpt .gt. zero .and. dpc .gt. zero) then
                    call utmess('A', 'ALGORITH4_86')
                    goto 5
                endif
            else
                if ((fcomp3/fcp) .gt. epsi2) then
                    if (dpt .gt. zero .and. dpc .gt. zero) then
                        call utmess('A', 'ALGORITH4_87')
                        goto 5
                    endif
                else if ((ftrac3/ftp).gt.epsi2) then
                    if (dpt .gt. zero .and. dpc .gt. zero) then
                        call utmess('A', 'ALGORITH4_88')
                        goto 5
                    endif
                endif
            endif
!
        else if (nseuil.eq.2) then
            if ((ftrac3/ftp) .gt. epsi2 .and. dpt .gt. zero) then
                call utmess('A', 'ALGORITH4_89')
                goto 5
            endif
        else if (nseuil.eq.1) then
            if ((fcomp3/fcp) .gt. epsi2 .and. dpc .gt. zero) then
                call utmess('A', 'ALGORITH4_90')
                goto 5
            endif
        endif
!
! --     CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
!        CONTRAINTES
!
        if (nseuil .ne. 4 .and. nseuil .ne. 44) then
            vinf(1) = vind(1) + dpc
            vinf(2) = vind(2) + dpt
            vinf (nvi) = 1.d0 * nseuil
            call betinc(materf, nmat, sige, nseuil, dpc,&
                        dpt, sigf, verifc, verift)
        endif
!
    endif
!
!
!
    irteti = 0
    goto 9999
  5 continue
    irteti = 1
    goto 9999
!
9999 continue
end subroutine

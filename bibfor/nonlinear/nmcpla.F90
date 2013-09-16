subroutine nmcpla(fami, kpg, ksp, ndim, typmod,&
                  imat, comp, crit, timed, timef,&
                  neps, epsdt, depst, nsig, sigd,&
                  vind, opt, nwkin, wkin, sigf,&
                  vinf, ndsde, dsde, nwkout, wkout,&
                  iret)
! aslint: disable=W1504
    implicit none
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ================================================================
!       INTEGRATION DU COUPLAGE FLUAGE/FISSURATION, C'EST A DIRE LE
!       COUPLAGE D'UNE LOI DE COMPORTEMENT DE TYPE FLUAGE GRANGER
!       ET D'UNE LOI DE  COMPORTEMENT ELASTO PLASTIQUE
!               AVEC    . N VARIABLES INTERNES
!                       . UNE FONCTION SEUIL ELASTIQUE
!
!       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
!       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT) (CUMUL DES
!              VARIABLES INTERNES DES DEUX LOIS)
!       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
!
!       ================================================================
!       ARGUMENTS
!
!       IN      KPG,KSP  NUMERO DU (SOUS)POINT DE GAUSS
!               NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!               TYPMOD  TYPE DE MODELISATION
!               IMAT    ADRESSE DU MATERIAU CODE
!               COMP    COMPORTEMENT DE L ELEMENT
!                       COMP(1) = RELATION DE COMPORTEMENT
!                       COMP(2) = NB DE VARIABLES INTERNES
!                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!               OPT     OPTION DE CALCUL A FAIRE
!                               'RIGI_MECA_TANG'> DSDE(T)
!                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
!                               'RAPH_MECA'     > SIG(T+DT)
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (ITER_INTE_PAS == ITEDEC)
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               WKIN  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!               TIMED   INSTANT T
!               TIMEF   INSTANT T+DT
!               EPSDT   DEFORMATION TOTALE A T
!               DEPST   INCREMENT DE DEFORMATION TOTALE
!               SIGD    CONTRAINTE A T
!               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!       OUT     SIGF    CONTRAINTE A T+DT
!               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               IRET    CODE RETOUR DE L'INTEGRATION INTEGRATION DU
!                       COUPLAGE FLUAGE/FISSURATION
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ABSENCE DE CONVERGENCE
!       ----------------------------------------------------------------
#include "jeveux.h"
#include "asterc/iisnan.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/r8vide.h"
#include "asterfort/betnvi.h"
#include "asterfort/granvi.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcprmv.h"
#include "asterfort/nmgran.h"
#include "asterfort/nmisot.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/redece.h"
#include "asterfort/rslnvi.h"
#include "asterfort/utmess.h"
    integer :: imat, ndim, kpg, ksp, iret
    integer :: neps, nsig, nwkin, nwkout, ndsde
!
    real(kind=8) :: crit(*)
    real(kind=8) :: timed, timef, tempd, tempf, tref
    real(kind=8) :: wkin(*), wkout(*)
    real(kind=8) :: epsdt(6), depst(6)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*)
!
    real(kind=8) :: dsde(ndsde)
!
    character(len=16) :: comp(*), opt
    character(len=*) :: fami
    character(len=8) :: typmod(*)
!       ----------------------------------------------------------------
!       VARIABLES LOCALES
    integer :: ndt, ndi, nvi1, ibid, ibid2, ibid3, ire2
    integer :: nvi2, nn, i, retcom
    integer :: cerr(5)
    character(len=8) :: mod, mod3d, nomc(5), nompar
    character(len=16) :: optflu, cmp1(3), cmp2(3), cmp3(3), cveri, comcod
    real(kind=8) :: rbid, nu, angmas(3)
    real(kind=8) :: epsfl(6), epsfld(6), epsflf(6), depsfl(6)
    real(kind=8) :: deps(6), kooh(6, 6), valpad, valpaf
    real(kind=8) :: materd(5), materf(5), depst2(6), depsel(6)
    real(kind=8) :: epsicv, toler, ndsig, nsigf
    real(kind=8) :: dsigf(6), hydrd, hydrf, sechd, sechf, sref
    real(kind=8) :: epseld(6), epself(6), epsthe
!
    integer :: k, nbvar2
    integer :: iter, itemax, iret1, iret2, iret3, numlc2
    real(kind=8) :: sigf2(6), r8bid
    real(kind=8) :: tmpdmx, tmpfmx, epsth
    real(kind=8) :: alphad, alphaf, bendod, bendof, kdessd, kdessf
!
    logical :: cp
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
!
! --- DECODAGE DES COMPORTEMENTS ET VERIFICATIONS
!
    r8bid=r8vide()
!
    mod3d = '3D'
    cmp1(1) = comp(8)
    cmp2(1) = comp(9)
    cmp3(1) = comp(10)
    mod = typmod(1)
!
    if (cmp3(1)(1:8) .ne. '        ') then
        call utmess('F', 'ALGORITH7_1')
    endif
!
    if (cmp1(1)(1:10) .ne. 'GRANGER_FP') then
        call utmess('F', 'ALGORITH7_11')
    endif
!
!     TABLEAU DES VARIABLES INTERNES DIMENSIONNE AUX MAX I.E. 3D
    call granvi(mod3d, ibid, ibid2, nvi1)
    write (cmp1(2),'(I16)') nvi1
    cmp1(3) = comp(3)
!
!     DIMENSION DES TENSEURS
    call granvi(mod, ndt, ndi, ibid)
!
    nn = nvi1 + 1
    if (cmp2(1)(1:5) .eq. 'ELAS ' .or. cmp2(1)(1:9) .eq. 'VMIS_ISOT' .or. cmp2(1)(1:14)&
        .eq. 'VMIS_ISOT_LINE') then
        if (cmp2(1)(1:5) .eq. 'ELAS ') nvi2 = 1
        if (cmp2(1)(1:9) .eq. 'VMIS_ISOT') nvi2 = 2
        if (cmp2(1)(1:14) .eq. 'VMIS_ISOT_LINE') nvi2 = 2
!
        elseif (cmp2(1)(1:8).eq. 'ROUSS_PR' .or. cmp2(1)(1:15).eq.&
    'BETON_DOUBLE_DP') then
!
        if (cmp2(1)(1:8) .eq. 'ROUSS_PR') call rslnvi(mod3d, ibid, ibid2, ibid3, nvi2)
        if (cmp2(1)(1:15) .eq. 'BETON_DOUBLE_DP') call betnvi(mod3d, ibid, ibid2, ibid3, nvi2)
!
    else
        call utmess('F', 'ALGORITH7_3')
    endif
!
    write (cmp2(2),'(I16)') nvi2
    cmp2(3) = comp(3)
    write (cveri,'(I16)') (nvi1 + nvi2)
!
    if (cveri(1:16) .ne. comp(2)(1:16)) then
        call utmess('F', 'ALGORITH7_12')
    endif
!
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tempd, iret1)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tempf, iret2)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret3)
!
! --- TEMPERATURE MAXIMALE AU COURS DE L'HISTORIQUE DE CHARGEMENT
!
    tmpdmx = tempd
    tmpfmx = tempf
    if (((iret1+iret2).eq.0) .and. (iisnan(vind(nvi1+3)).eq.0) .and.&
        (cmp2(1)(1:15).eq. 'BETON_DOUBLE_DP' )) then
        if (tmpdmx .lt. vind(nvi1+3)) tmpdmx = vind(nvi1+3)
        if (tmpfmx .lt. vind(nvi1+3)) tmpfmx = vind(nvi1+3)
    endif
!
! --- CRITERE DE CONVERGENCE
!
    itemax = int(crit(1))
    toler = crit(3)
!
! --- OPTION DE CALCUL POUR LA LOI DE FLUAGE
!
    if (opt .eq. 'RAPH_MECA' .or. opt .eq. 'FULL_MECA') then
        optflu = 'RAPH_MECA'
    else
        optflu = '                '
    endif
!
!     --------------------------------
! --- DEBUT DES ITERATIONS DU COUPLAGE
!     --------------------------------
    iter = 1
    do 10 k = 1, 6
        depst2(k) = depst(k)
10  end do
!
20  continue
!
! --- RESOLUTION LOI DE FLUAGE
!
    if (optflu .eq. 'RAPH_MECA') then
        call nmgran(fami, kpg, ksp, typmod, imat,&
                    cmp1, timed, timef, tmpdmx, tmpfmx,&
                    depst2, sigd, vind(1), opt, sigf2,&
                    vinf(1), dsde)
!
! ---    CALCUL DE L'INCREMENT DE LA DEFORMATION DE FLUAGE
!
        nomc(1) = 'E       '
        nomc(2) = 'NU      '
        nomc(3) = 'ALPHA   '
        nomc(4) = 'B_ENDOGE'
        nomc(5) = 'K_DESSIC'
        nompar = 'TEMP'
        valpad = tmpdmx
        valpaf = tmpfmx
!
! -      RECUPERATION MATERIAU A TEMPD (T)
!
        call rcvalb(fami, 1, 1, '+', imat,&
                    ' ', 'ELAS', 1, nompar, [valpad],&
                    1, nomc(2), materd(2), cerr(1), 2)
!
! -      RECUPERATION MATERIAU A TEMPF (T+DT)
!
        call rcvalb(fami, 1, 1, '+', imat,&
                    ' ', 'ELAS', 1, nompar, [valpaf],&
                    1, nomc(2), materf(2), cerr(1), 2)
!
        materd(1) = 1.d0
        materf(1) = 1.d0
!
        do 40 k = 1, ndt
            epsfl(k) = vind(8*ndt+k)
            do 30 i = 1, 8
                epsfl(k) = epsfl(k) - vind((i-1) * ndt+k)
30          continue
40      continue
!
        call lcopil('ISOTROPE', mod, materd, kooh)
        call lcprmv(kooh, epsfl, epsfld)
!
        do 60 k = 1, ndt
            epsfl(k) = vinf(8*ndt+k)
            do 50 i = 1, 8
                epsfl(k) = epsfl(k) - vinf((i-1) * ndt+k)
50          continue
60      continue
!
        call lcopil('ISOTROPE', mod, materf, kooh)
        call lcprmv(kooh, epsfl, epsflf)
!
        do 70 k = 1, ndt
            depsfl(k) = epsflf(k) - epsfld(k)
70      continue
    endif
!
! --- RETRAIT DE LA DEFORMATION DE FLUAGE A LA DEFORMATION TOTALE
!
    if (optflu .eq. 'RAPH_MECA') then
        do 80 k = 1, ndt
            deps(k) = depst(k) - depsfl(k)
80      continue
    else
        do 90 k = 1, ndt
            deps(k) = depst(k)
90      continue
    endif
!
!
! --- RESOLUTION LOI DE PLASTICITE FISSURATION
!
    if (cmp2(1)(1:9) .eq. 'VMIS_ISOT' .or. cmp2(1)(1:14) .eq. 'VMIS_ISOT_LINE') then
!
        call nmisot(fami, kpg, ksp, ndim, typmod,&
                    imat, cmp2, crit, deps, sigd,&
                    vind(nn), opt, sigf, vinf(nn), dsde,&
                    rbid, rbid, iret)
!
        elseif (cmp2(1)(1:8).eq. 'ROUSS_PR' .or. cmp2(1)(1:15).eq.&
    'BETON_DOUBLE_DP') then
!
        call lccree(1, cmp2, comcod)
        call lcinfo(comcod, numlc2, nbvar2)
        call redece(fami, kpg, ksp, ndim, typmod,&
                    imat, cmp2, crit, timed, timef,&
                    neps, epsdt, deps, nsig, sigd,&
                    vind(nn), opt, angmas, nwkin, wkin,&
                    cp, numlc2, r8bid, r8bid, r8bid,&
                    sigf, vinf(nn), ndsde, dsde, nwkout,&
                    wkout, retcom)
    else
        call utmess('F', 'ALGORITH7_3')
    endif
!
    if (optflu .eq. 'RAPH_MECA') then
!
! -      RECUPERATION MATERIAU A TEMPD (T)
!
        call rcvalb(fami, kpg, ksp, '-', imat,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    5, nomc(1), materd(1), cerr(1), 2)
!
        if (cerr(3) .ne. 0) materd(3) = 0.d0
        if (cerr(4) .ne. 0) materd(4) = 0.d0
        if (cerr(5) .ne. 0) materd(5) = 0.d0
!
! -      RECUPERATION MATERIAU A TEMPF (T+DT)
!
        call rcvalb(fami, kpg, ksp, '+', imat,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    5, nomc(1), materf(1), cerr(1), 2)
!
        if (cerr(3) .ne. 0) materf(3) = 0.d0
        if (cerr(4) .ne. 0) materf(4) = 0.d0
        if (cerr(5) .ne. 0) materf(5) = 0.d0
!
! --- CALCUL DE L'INCREMENT DE DEFORMATION ELASTIQUE
! --- + RETRAIT ENDOGENNE + RETRAIT DESSICCATION + RETRAIT THERMIQUE
!
        call lcopil('ISOTROPE', mod, materd, kooh)
        call lcprmv(kooh, sigd, epseld)
        call lcopil('ISOTROPE', mod, materf, kooh)
        call lcprmv(kooh, sigf, epself)
!
        do 100 k = 1, ndt
            depsel(k) = epself(k) - epseld(k)
100      continue
!
        alphad = materd(3)
        alphaf = materf(3)
        bendod = materd(4)
        bendof = materf(4)
        kdessd = materd(5)
        kdessf = materf(5)
!
!        RECUPERATION DE L HYDRATATION ET DU SECCHAGE
        call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                    ksp, hydrd, ire2)
        if (ire2 .ne. 0) hydrd=0.d0
        call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                    ksp, hydrf, ire2)
        if (ire2 .ne. 0) hydrf=0.d0
        call rcvarc(' ', 'SECH', '-', fami, kpg,&
                    ksp, sechd, ire2)
        if (ire2 .ne. 0) sechd=0.d0
        call rcvarc(' ', 'SECH', '+', fami, kpg,&
                    ksp, sechf, ire2)
        if (ire2 .ne. 0) sechf=0.d0
        call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                    ksp, sref, ire2)
        if (ire2 .ne. 0) sref=0.d0
!
        if ((iret1+iret2+iret3) .ne. 0) then
            epsthe = 0.d0
        else
            epsthe = alphaf*(tempf-tref) - alphad*(tempd-tref)
        endif
        epsth = epsthe - bendof*hydrf + bendod*hydrd - kdessf*(sref- sechf) + kdessd*(sref-sechd)
        do 110 k = 1, 3
            depsel(k) = depsel(k) + epsth
110      continue
        if (mod(1:6) .eq. 'C_PLAN') then
            nu = materf(2)
            depsel(3)=-nu / (1.d0-nu) * (depsel(1)+depsel(2)) +(1.d0+&
            nu) / (1.d0-nu) * epsth
        endif
!
!
! ---    CALCUL DE L'INCREMENT DE DEFORMATION EN ENTREE DU CALCUL
! ---    DE FLUAGE POR L'ITERATION SUIVANTE
!
        do 120 k = 1, ndt
            depst2(k) = depst2(k) + depsel(k) + depsfl(k) - depst2(k)
120      continue
!
! ---    CRITERE DE CONVERGENCE - NORME DE SIGF2 - SIGF
!
        do 150 k = 1, ndt
            dsigf(k) = sigf2(k) - sigf(k)
150      continue
!
        ndsig = 0.d0
        nsigf = 0.d0
        do 160 k = 1, ndt
            ndsig = ndsig + dsigf(k) * dsigf(k)
            nsigf = nsigf + sigf(k) * sigf(k)
160      continue
!
        if (nsigf .gt. toler*toler) then
            epsicv = (ndsig/nsigf) ** 0.5d0
        else
            epsicv = ndsig ** 0.5d0
        endif
!
        if (epsicv .gt. toler) then
            if (iter .lt. itemax) then
                iter = iter + 1
                goto 20
            else
!               CALL TECAEL ( IADZI, IAZK24 )
!               NOMAIL = ZK24(IAZK24-1+3)(1:8)
!               CALL CODREE(ABS(EPSICV),'E',DCV)
!               CALL CODENT(ITER,'G',CITER)
                iret = 1
                goto 9999
            endif
        endif
    endif
!     ------------------
! --- FIN DES ITERATIONS
!     ------------------
!
9999  continue
end subroutine

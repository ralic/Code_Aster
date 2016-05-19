subroutine nmcpla(fami, kpg   , ksp  , ndim  , typmod,&
                  imat, compor, crit , timed , timef ,&
                  neps, epsdt , depst, nsig  , sigd  ,&
                  vind, option, nwkin, wkin  , sigf  ,&
                  vinf, ndsde , dsde , nwkout, wkout ,&
                  iret)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
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
!
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
! aslint: disable=W1504
!
    integer :: imat, ndim, kpg, ksp, iret
    integer :: neps, nsig, nwkin, nwkout, ndsde
    real(kind=8) :: crit(*)
    real(kind=8) :: timed, timef, tempd, tempf, tref
    real(kind=8) :: wkin(*), wkout(*)
    real(kind=8) :: epsdt(6), depst(6)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: dsde(ndsde)
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    character(len=8) :: typmod(*)
!
! --------------------------------------------------------------------------------------------------
!
! Comportment management
!
! KIT_DDI with GRANGER
!
! --------------------------------------------------------------------------------------------------
!
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
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ndt, ndi, nvi_flua, ire2, nvi_tot
    integer :: nvi_plas, idx_vi_plas, i, retcom
    integer :: cerr(5)
    character(len=8) :: elem_model, nomc(5)
    character(len=16) :: rela_flua, rela_plas
    character(len=16) :: compor_creep(3), compor_plas(3)
    real(kind=8) :: nu, angmas(3)
    real(kind=8) :: espi_creep(6), epsfld(6), epsflf(6), depsfl(6)
    real(kind=8) :: deps(6), kooh(6, 6)
    real(kind=8) :: materd(5), materf(5), depst2(6), depsel(6)
    real(kind=8) :: epsicv, toler, ndsig, nsigf
    real(kind=8) :: dsigf(6), hydrd, hydrf, sechd, sechf, sref
    real(kind=8) :: epseld(6), epself(6), epsthe
    integer :: k, iter, itemax, iret1, iret2, iret3, nume_plas
    real(kind=8) :: sigf2(6), r8bid
    real(kind=8) :: tmpdmx, tmpfmx, epsth
    real(kind=8) :: alphad, alphaf, bendod, bendof, kdessd, kdessf
    aster_logical :: cp, l_inte_forc
!
! --------------------------------------------------------------------------------------------------
!
    common /tdim/   ndt  , ndi
!
! --------------------------------------------------------------------------------------------------
!
    l_inte_forc = option .eq. 'RAPH_MECA' .or. option .eq. 'FULL_MECA'
    r8bid       = r8vide()
    read (compor(2),'(I16)') nvi_tot
    rela_flua   = compor(8)
    rela_plas   = compor(9)
    elem_model  = typmod(1)
    read (compor(16),'(I16)') nvi_plas
    read (compor(15),'(I16)') nume_plas
!
! - Number of internal variables in Granger (take maximum from 3D)
!
    call granvi('3D', nvi_ = nvi_flua)
!
! - Prepare COMPOR <CARTE> for Granger
!
    compor_creep(1) = rela_flua
    write (compor_creep(2),'(I16)') nvi_flua
    compor_creep(3) = compor(3)
    ASSERT(compor_creep(1)(1:10) .eq. 'GRANGER_FP')

!
! - Number of internal variables
!
    call granvi(elem_model, ndt, ndi)
    idx_vi_plas = nvi_flua + 1
    ASSERT(nvi_tot .eq. (nvi_flua + nvi_plas))
!
! - Prepare COMPOR <CARTE> for plasticity
!
    compor_plas(1) = rela_plas
    write (compor_plas(2),'(I16)') nvi_plas
    compor_plas(3) = compor(3)
!
! - Get temperatures
!
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tempd, iret1)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tempf, iret2)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret3)
!
! - Get maximum temperature during load (for BETON_DOUBLE_DP)
!
    tmpdmx = tempd
    tmpfmx = tempf
    if (rela_plas.eq. 'BETON_DOUBLE_DP' ) then
        if (((iret1+iret2).eq.0) .and. (.not.isnan(vind(nvi_flua+3)))) then
            if (tmpdmx .lt. vind(nvi_flua+3)) then
                tmpdmx = vind(nvi_flua+3)
            endif
            if (tmpfmx .lt. vind(nvi_flua+3)) then
                tmpfmx = vind(nvi_flua+3)
            endif
        endif
    endif
!
! - Get convergence criteria
!
    itemax = int(crit(1))
    toler  = crit(3)
!
! = COUPLING = BEGIN
!
    iter = 1
    depst2(1:6) = depst(1:6)
!
 20 continue
!
! - Solve creep law
!
    if (l_inte_forc) then
!
! ----- Solve creep law
!
        call nmgran(fami        , kpg  , ksp    , typmod, imat  ,&
                    compor_creep, timed, timef  , tmpdmx, tmpfmx,&
                    depst2      , sigd , vind(1), option, sigf2 ,&
                    vinf(1)     , dsde)
!
! ----- Get material parameters
!
        nomc(1) = 'E       '
        nomc(2) = 'NU      '
        nomc(3) = 'ALPHA   '
        nomc(4) = 'B_ENDOGE'
        nomc(5) = 'K_DESSIC'
        call rcvalb(fami, 1, 1, '+', imat,&
                    ' ', 'ELAS', 1, 'TEMP', [tmpdmx],&
                    1, nomc(2), materd(2), cerr(1), 2)
        call rcvalb(fami, 1, 1, '+', imat,&
                    ' ', 'ELAS', 1, 'TEMP', [tmpfmx],&
                    1, nomc(2), materf(2), cerr(1), 2)
        materd(1) = 1.d0
        materf(1) = 1.d0
!
! ----- Creep strains - At beginning of step
!
        do k = 1, ndt
            espi_creep(k) = vind(8*ndt+k)
            do i = 1, 8
                espi_creep(k) = espi_creep(k) - vind((i-1) * ndt+k)
            enddo
        enddo
        call lcopil('ISOTROPE', elem_model, materd, kooh)
        call lcprmv(kooh, espi_creep, epsfld)
!
! ----- Creep strains - At end of step
!
        do k = 1, ndt
            espi_creep(k) = vinf(8*ndt+k)
            do i = 1, 8
                espi_creep(k) = espi_creep(k) - vinf((i-1) * ndt+k)
            enddo
        enddo
        call lcopil('ISOTROPE', elem_model, materf, kooh)
        call lcprmv(kooh, espi_creep, epsflf)
!
! ----- Creep strain increment
!
        do k = 1, ndt
            depsfl(k) = epsflf(k) - epsfld(k)
        enddo
    endif
!
! - Total strains
!
    if (l_inte_forc) then
        do k = 1, ndt
            deps(k) = depst(k) - depsfl(k)
        enddo
    else
        do k = 1, ndt
            deps(k) = depst(k)
        enddo
    endif
!
! - Solve plasticity law
!
    if (rela_plas(1:9) .eq. 'VMIS_ISOT' .or. rela_plas(1:14) .eq. 'VMIS_ISOT_LINE') then
        call nmisot(fami             , kpg      , ksp , ndim             , typmod,&
                    imat             , rela_plas, crit, deps             , sigd  ,&
                    vind(idx_vi_plas), option   , sigf, vinf(idx_vi_plas), dsde  ,&
                    iret)
    else if (rela_plas(1:8).eq. 'ROUSS_PR' .or. rela_plas(1:15).eq.'BETON_DOUBLE_DP') then
        call redece(fami             , kpg              , ksp   , ndim , typmod,&
                    imat             , compor_plas      , crit  , timed, timef ,&
                    neps             , epsdt            , deps  , nsig , sigd  ,&
                    vind(idx_vi_plas), option           , angmas, nwkin, wkin  ,&
                    cp               , nume_plas        , r8bid , r8bid, r8bid ,&
                    sigf             , vinf(idx_vi_plas), ndsde , dsde , nwkout,&
                    wkout            , retcom)
    else
        ASSERT(.false.)
    endif
!
! - Coupling algorithm
!
    if (l_inte_forc) then
!
! ----- Get material parameters
!
        call rcvalb(fami, kpg, ksp, '-', imat,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    5, nomc(1), materd(1), cerr(1), 2)
        if (cerr(3) .ne. 0) materd(3) = 0.d0
        if (cerr(4) .ne. 0) materd(4) = 0.d0
        if (cerr(5) .ne. 0) materd(5) = 0.d0
        call rcvalb(fami, kpg, ksp, '+', imat,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    5, nomc(1), materf(1), cerr(1), 2)
        if (cerr(3) .ne. 0) materf(3) = 0.d0
        if (cerr(4) .ne. 0) materf(4) = 0.d0
        if (cerr(5) .ne. 0) materf(5) = 0.d0
!
! ----- Elastic strain increment
!
        call lcopil('ISOTROPE', elem_model, materd, kooh)
        call lcprmv(kooh, sigd, epseld)
        call lcopil('ISOTROPE', elem_model, materf, kooh)
        call lcprmv(kooh, sigf, epself)
        do k = 1, ndt
            depsel(k) = epself(k) - epseld(k)
        enddo
!
! --- CALCUL DE L'INCREMENT DE DEFORMATION ELASTIQUE
! --- + RETRAIT ENDOGENNE + RETRAIT DESSICCATION + RETRAIT THERMIQUE
!
        alphad = materd(3)
        alphaf = materf(3)
        bendod = materd(4)
        bendof = materf(4)
        kdessd = materd(5)
        kdessf = materf(5)
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
        do k = 1, 3
            depsel(k) = depsel(k) + epsth
        enddo
!
! ----- For plane stress
!
        if (elem_model(1:6) .eq. 'C_PLAN') then
            nu = materf(2)
            depsel(3)=-nu / (1.d0-nu) * (depsel(1)+depsel(2)) +(1.d0+nu) / (1.d0-nu) * epsth
        endif
!
!
! ---    CALCUL DE L'INCREMENT DE DEFORMATION EN ENTREE DU CALCUL
! ---    DE FLUAGE POR L'ITERATION SUIVANTE
!
        do k = 1, ndt
            depst2(k) = depst2(k) + depsel(k) + depsfl(k) - depst2(k)
        enddo
!
! ---    CRITERE DE CONVERGENCE - NORME DE SIGF2 - SIGF
!
        do k = 1, ndt
            dsigf(k) = sigf2(k) - sigf(k)
        enddo
!
        ndsig = 0.d0
        nsigf = 0.d0
        do k = 1, ndt
            ndsig = ndsig + dsigf(k) * dsigf(k)
            nsigf = nsigf + sigf(k) * sigf(k)
        enddo
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
                iret = 1
                goto 999
            endif
        endif
    endif
!
! = COUPLING = END
!
999 continue
end subroutine

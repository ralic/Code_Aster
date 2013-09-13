subroutine nmcjs(typmod, imat, comp, crit, instam,&
                 instap, tempm, tempf, tref, epsd,&
                 deps, sigd, vind, opt, sigf,&
                 vinf, dsde, iret)
    implicit none
!       ================================================================
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
!       INTEGRATION DE LA LOI DE COMPORTEMENT ELASTO PLASTIQUE CJS
!               AVEC    . N VARIABLES INTERNES
!                       . 2 FONCTIONS SEUIL ELASTIQUE
!
!       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
!       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
!       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
!       ================================================================
!       IN      TYPMOD  TYPE DE MODELISATION
!               IMAT    ADRESSE DU MATERIAU CODE
!               COMP    COMPORTEMENT DE L ELEMENT
!                       COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
!                       COMP(2) = NB DE VARIABLES INTERNES
!                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
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
!                                 (RESI_INTE_PAS == ITEDEC )
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               INSTAM  INSTANT T
!               INSTAP  INSTANT T+DT
!               TEMPM   TEMPERATURE A T
!               TEMPF   TEMPERATURE A T+DT
!               TREF    TEMPERATURE DE REFERENCE
!               EPSD    DEFORMATION TOTALE A T
!               DEPS    INCREMENT DE DEFORMATION TOTALE
!               SIGD    CONTRAINTE A T
!               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!               OPT     OPTION DE CALCUL A FAIRE
!                               'RIGI_MECA_TANG'> DSDE(T)
!                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
!                               'RAPH_MECA'     > SIG(T+DT)
!       OUT     SIGF    CONTRAINTE A T+DT
!               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               IRET    CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!       ----------------------------------------------------------------
!       INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
!                             (*,2) = CARACTERISTIQUES PLASTIQUES A T
!               MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
!                             (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
!               NDT             NB DE COMPOSANTE TOTALES DES TENSEURS
!                                       = 6  3D
!                                       = 4  AXIS  C_PLAN  D_PLAN
!                                       = 1  1D
!               NDI             NB DE COMPOSANTE DIRECTES DES TENSEURS
!               NVI             NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
!       ROUTINE LC....UTILITAIRES POUR INTEGRATION LOI DE COMPORTEMENT
!       ----------------------------------------------------------------
!       ORDRE DES TENSEURS      3D      XX YY ZZ XY XZ YZ
!                               DP      XX YY ZZ XY
!                               AX      RR ZZ TT RZ
!                               1D      XX YY ZZ
!       ----------------------------------------------------------------
!       ATTENTION
!       SI OPT = 'RIGI_MECA_TANG' NE PAS TOUCHER AUX VARIABLES SIGF,VINF
!       QUI N ONT PAS DE PLACE MEMOIRE ALLOUEE
!
!       SIG EPS DEPS  ONT DEJA LEURS COMPOSANTES DE CISAILLEMENT
!       MULTIPLIES PAR RACINE DE 2 > PRISE EN COMPTE DES DOUBLES
!       PRODUITS TENSORIELS ET CONSERVATION DE LA SYMETRIE
!
!       ----------------------------------------------------------------
#include "asterc/iisnan.h"
#include "asterfort/cjsela.h"
#include "asterfort/cjsinp.h"
#include "asterfort/cjsmat.h"
#include "asterfort/cjspla.h"
#include "asterfort/cjssmd.h"
#include "asterfort/cjssmi.h"
#include "asterfort/cjstde.h"
#include "asterfort/cjstel.h"
#include "asterfort/cjstid.h"
#include "asterfort/cjstis.h"
#include "asterfort/iunifi.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcsove.h"
#include "asterfort/utmess.h"
    integer :: imat, ndt, ndi, nvi, iret
!
    real(kind=8) :: crit(*)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: instam, instap, tempm, tempf, tref
    real(kind=8) :: epsd(6), deps(6), epsf(6)
    real(kind=8) :: sigd(6), sigf(6)
!
    real(kind=8) :: seuili, seuild, q0, rinit, pa, qinit
!
    real(kind=8) :: dsde(6, 6)
!
    real(kind=8) :: materf(14, 2)
!
!
    character(len=4) :: nivcjs
    character(len=6) :: mecand, mecanf
    character(len=7) :: etatd, etatf
    character(len=8) :: mod, typmod(*)
    character(len=16) :: comp(*), opt
    integer :: niter, i, ndec
    real(kind=8) :: epscon
    real(kind=8) :: epsthe, epsthm
    real(kind=8) :: depsth(6), epsdth(6), alphaf, alpham
    logical :: trac
!
    real(kind=8) :: i1d
!
    integer :: umess
!
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
!
!
!
    umess = iunifi('MESSAGE')
    mod = typmod(1)
    trac = .false.
!
! --    RECUPERATION COEF DE LA LOI CJS (INDEPENDANTS DE LA TEMPERATURE)
!                    NB DE CMP DIRECTES/CISAILLEMENT
!                    NB VARIABLES INTERNES
!                    NIVEAU DE LA LOI CJS: CJS1, CJS2 OU CJS3
    call cjsmat(mod, imat, tempf, materf, ndt,&
                ndi, nvi, nivcjs)
    pa = materf(12,2)
    qinit = materf(13,2)
!
!  COEF DE DILATATION LE MEME A TPLUS ET TMOINS
!
    alphaf = materf(3,1)
    alpham = materf(3,1)
!
    niter = 0
    ndec = 0
    epscon = 0
!
    i1d = 0.d0
    do 10 i = 1, ndi
        i1d = i1d + sigd(i)
10  continue
!
!     --  CALCUL DE DEPSTH ET EPSDTH
!     --------------------------------
!
    if (((iisnan(tempm).gt.0).or.(iisnan(tref).gt.0)) .and. (materf(3,1).ne.0.d0)) then
        call utmess('F', 'CALCULEL_15')
    else if (materf(3,1).eq.0.d0) then
        epsthe = 0.d0
        epsthm = 0.d0
    else
        epsthe = alphaf*(tempf-tref) - alpham*(tempm-tref)
        epsthm = alpham*(tempm-tref)
    endif
    do 20 i = 1, ndi
        depsth(i) = deps(i) - epsthe
        epsdth(i) = epsd(i) - epsthm
20  continue
    do 21 i = ndi+1, ndt
        depsth(i) = deps(i)
        epsdth(i) = epsd(i)
21  continue
    if (ndt .lt. 6) then
        do 22 i = ndt+1, 6
            depsth(i) = 0.d0
            epsdth(i) = 0.d0
22      continue
    endif
!
!  TESTER QUE VIND DE NVI EST 1 2 OU 3
!
    if ((vind(nvi).ne.0.d0) .and. (vind(nvi).ne.1.d0) .and. (vind(nvi).ne.2.d0) .and.&
        (vind(nvi).ne.3.d0)) then
        write(umess,*) ' INDICATEUR DE PLASTICITE ERRONE : ',vind(nvi)
        call utmess('F', 'ALGORITH6_80')
    endif
!
!
! --    BLOCAGE DES VARIABLES INTERNES EN FONCTION DU NIVEAU DE LA LOI
!       CJS CHOISI, ET ON PREND DES VALEURS INITIALES DE Q ET R PETITES
!       MAIS NON NULLES
!
    if (nivcjs .eq. 'CJS1') then
        vind(1) = 1.d25 * materf(12,2)
        vind(2) = materf(2,2)
    endif
!
    if (nivcjs .eq. 'CJS3') then
        vind(2) = materf(2,2)
    endif
!
!  SI SEUIL ISOTROPE = 0 SEUIL ISOTROPE = PRESSION HYDRO
!
!
!
    if (vind(1) .eq. 0.d0) then
        if (i1d .lt. 0.d0) then
            q0= (1.d-3*pa+i1d+qinit)/3.d0
        else
            q0= (1.d-3*pa+qinit)/3.d0
        endif
!
        vind(1)= q0
        if (opt(1:14) .ne. 'RIGI_MECA_TANG') then
            vinf(1) = q0
        endif
    endif
!
! INITIALISATION SEUIL DEVIATOIRE SI NUL
!
    if (vind(2) .eq. 0.d0) then
        if (materf(14,2) .eq. 0.d0) then
            rinit=1.d-3
        else
            rinit = materf(14,2)
        endif
        vind(2)= rinit
        if (opt(1:14) .ne. 'RIGI_MECA_TANG') then
            vinf(2) = rinit
        endif
    endif
!
!
! --    ETAT ELASTIC OU PLASTIC A T
!
    if (vind(nvi) .eq. 0.d0) then
        etatd = 'ELASTIC'
    endif
!
    if (vind(nvi) .eq. 1.d0) then
        etatd = 'PLASTIC'
        mecand = 'ISOTRO'
    endif
!
    if (vind(nvi) .eq. 2.d0) then
        etatd = 'PLASTIC'
        mecand = 'DEVIAT'
    endif
!
    if (vind(nvi) .eq. 3.d0) then
        etatd = 'PLASTIC'
        mecand = 'ISODEV'
    endif
!
!
!       ----------------------------------------------------------------
!       OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
!       ----------------------------------------------------------------
!
    if (opt .eq. 'RAPH_MECA' .or. opt .eq. 'FULL_MECA') then
!
!
!
!
! --    INTEGRATION ELASTIQUE SUR DT
!
!
!
        call cjsela(mod, crit, materf, depsth, sigd,&
                    sigf, nvi, vind, vinf, iret)
        if (iret .eq. 1) goto 9999
!
!
! --    PREDICTION ETAT ELASTIQUE A T+DT :
!       FI(SIG(T+DT),VIN(T)) = 0 ?     SEUIL MECANISME ISOTROPE
!       FD(SIG(T+DT),VIN(T)) = 0 ?     SEUIL MECANISME DEVIATOIRE
!
!
        call cjssmi(materf, sigf, vind, seuili)
        call cjssmd(materf, sigf, vind, seuild)
!
!
!
        if ((seuili .gt. 0.d0) .or. (seuild .gt. 0.d0)) then
!
! ecriture des contraintes
!
!
! --      PREDICTION INCORRECTE > INTEGRATION ELASTO-PLASTIQUE SUR DT
!
            etatf = 'PLASTIC'
!
            call cjspla(mod, crit, materf, seuili, seuild,&
                        nvi, epsdth, depsth, sigd, vind,&
                        sigf, vinf, mecanf, nivcjs, niter,&
                        ndec, epscon, iret, trac)
            if (iret .eq. 1) goto 9999
            if ((trac)) then
                etatf = 'ELASTIC'
            endif
!
        else
!
! --      PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
!
            etatf = 'ELASTIC'
        endif
!
    endif
!
!
!       ----------------------------------------------------------------
!       OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
!       ----------------------------------------------------------------
!       CALCUL ELASTIQUE ET EVALUATION DE DSDE A (T)
!       POUR 'RIGI_MECA_TANG' ET POUR 'FULL_MECA'
!       ----------------------------------------------------------------
!
    if (opt .eq. 'RIGI_MECA_TANG') then
        call lcinma(0.d0, dsde)
!
!
!
! REMARQUE: CALCUL DE DSDE A T AVEC MATERF CAR PARAMETRES CJS
!           INDEPENDANTS DE LA TEMPERATURE
!
!
!
! --      CALCUL MATRICE DE RIGIDITE ELASTIQUE
        if (etatd .eq. 'ELASTIC') then
            call cjstel(mod, materf, sigd, dsde)
        endif
!
! --      CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
        if (etatd .eq. 'PLASTIC') then
!
!          MECANISME ISOTROPE SEUL
            if (mecand .eq. 'ISOTRO') then
                call cjstis(mod, materf, sigd, vind, dsde)
            endif
!
!          MECANISME DEVIATOIRE SEUL
            if ((mecand .eq. 'DEVIAT')) then
                call cjstde(mod, materf, nvi, epsdth, sigd,&
                            vind, dsde)
            endif
!
!          MECANISMES ISOTROPE ET DEVIATOIRE
            if (mecand .eq. 'ISODEV') then
                call cjstid(mod, materf, nvi, epsdth, sigd,&
                            vind, dsde)
            endif
!
!
        endif
!
    endif
!
!
    if (opt .eq. 'FULL_MECA') then
!
        call lcinma(0.d0, dsde)
!
!
!
! --      CALCUL MATRICE DE RIGIDITE ELASTIQUE
        if (etatf .eq. 'ELASTIC') then
            call cjstel(mod, materf, sigf, dsde)
        endif
!
!
! --      CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
        if (etatf .eq. 'PLASTIC') then
!
!          MECANISME ISOTROPE SEUL
            if (mecanf .eq. 'ISOTRO') then
                call cjstis(mod, materf, sigf, vinf, dsde)
            endif
!
!          MECANISME DEVIATOIRE SEUL
            if ((mecanf .eq. 'DEVIAT')) then
                call lcsove(epsdth, depsth, epsf)
                call cjstde(mod, materf, nvi, epsf, sigf,&
                            vinf, dsde)
            endif
!
!          MECANISMES ISOTROPE ET DEVIATOIRE
            if (mecanf .eq. 'ISODEV') then
                call lcsove(epsdth, depsth, epsf)
                call cjstid(mod, materf, nvi, epsf, sigf,&
                            vinf, dsde)
            endif
!
!
        endif
!
    endif
!
!  VARIABLES INTERNES POUR SORTIES
!
    if ((opt .eq. 'FULL_MECA') .or. (opt .eq. 'RAPH_MECA')) then
        call cjsinp(materf, epsdth, depsth, sigf, vinf,&
                    niter, nvi, nivcjs, ndec, epscon)
    endif
!
9999  continue
end subroutine

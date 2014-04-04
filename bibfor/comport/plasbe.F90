subroutine plasbe(fami, kpg, ksp, typmod, imat,&
                  crit, epsdt, depst, sigd, vind,&
                  opt, elgeom, sigf, vinf, dsde,&
                  icomp, nvi, irteti)
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
!
!       ================================================================
!       INTEGRATION DE LOIS DE COMPORTEMENT ELASTO PLASTIQUE ET VISCO
!       PLASTIQUE
!               AVEC    . N VARIABLES INTERNES
!                       . UNE FONCTION SEUIL ELASTIQUE
!
!       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
!       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
!       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
!
!       EN CAS DE NON-CONVERGENCE LOCALE ON EFFECTUE UN REDECOUPAGE DU
!       PAS DE TEMPS, L ORDRE D EXECUTION ETANT REMONTE EN ARGUMENT
!       DANS REDECE, APPELE PAR NMCOMP AVANT PLASBE
!       ================================================================
!       ROUTINE CONSTRUITE SUIVANT LE MODELE ET L'ARCHITECTURE DE
!                                PLASTI
!       ================================================================
!       ROUTINES UTILITAIRES DE CALCUL MATRICIEL(6,6) - VECTORIEL (6)
!       COMMON /TDIM/ NDT,NDI    A INCLURE OBLIGATOIREMENT
!       () = DEBUG
!
!       ( LCIMMA  IMPRESSION MATRICE )
!       ( LCIMVE  IMPRESSION VECTEUR )
!       ( LCIMSC  IMPRESSION SCALAIRE )
!       LCPRTE  PRODUIT TENSORIEL DE VECTEURS
!       LCPRSC  PRODUIT SCALAIRE  DE VECTEURS
!       LCPRSM  PRODUIT SCALAIRE * MATRICE
!       LCPTMV  PRODUIT MATRICE TRANSPOSEE * VECTEUR
!       LCPRMV  PRODUIT MATRICE  * VECTEUR
!       LCPRMM  PRODUIT MATRICE  * MATRICE
!       LCDIMA  DIFFERENCE DE MATRICES
!       LCSOMA  SOMME DE MATRICES
!       LCTRMA  TRANSPOSEE DE MATRICE
!       LCEQMA  EGALITE DE MATRICES
!       LCINMA  INITIALISATION DE MATRICE A UNE VALEUR
!       LCPRSV  PRODUIT SCALAIRE * VECTEUR
!       LCSOVE  SOMME DE VECTEUR
!       LCDIVE  DIFFERENCE DE VECTEURS
!       LCINVE  INITIALISATION DE VECTEUR
!       LCEQVE  EGALITE DE VECTEURS
!
!       ----------------------------------------------------------------
!       ROUTINES UTILITAIRES D INTEGRATION D UN MODELE DE COMPORTEMENT
!
!       LCDEVI  PARTIE DEVIATORIQUE D UN TENSEUR
!       LCHYDR  PARTIE SPHERIQUE    D UN TENSEUR
!       LCVS    DERIVEE DE LA CONTRAINTE VON MISES / CONTRAINTE
!       LCVVSS  DERIVEE SECONDE DE LA CONTRAINTE VON MISES / CONTRAINTE
!       LCIV2S  SECOND INVARIANT DU TENSEUR CONTRAINTE
!       LCIV2E  SECOND INVARIANT DU TENSEUR DEFORMATION
!       LCNRTS  'NORME' DU TENSEUR CONTRAINTE
!       LCNRTE  'NORME' DU TENSEUR DEFORMATION
!       LCOPLI  OPERATEUR ELASTIQUE LINEAIRE
!       LCELIN  INTEGRATION  ELASTIQUE LINEAIRE ISOTROPE
!       LCVERR  CALCUL DU VECTEUR D'ERREUR RELATIVE, ABSOLU, NORMEE...
!
!       ================================================================
!       ARGUMENTS
!
!       IN      TYPMOD  TYPE DE MODELISATION
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
!                                 (RESI_INTE_PAS == ITEDEC )
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               ELGEOM  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!               EPSDT   DEFORMATION TOTALE A T
!               DEPST   INCREMENT DE DEFORMATION TOTALE
!               SIGD    CONTRAINTE A T
!               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!               ICOMP   COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
!       OUT     SIGF    CONTRAINTE A T+DT
!               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               IRTETI = 1 CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
!       ----------------------------------------------------------------
!       INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
!                             (*,2) = CARACTERISTIQUES PLASTIQUES A T
!               MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
!                             (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
!               MATCST          'OUI' SI MATERIAU CST ENTRE T ET T+DT
!                               'NON' SINON
!               NDT             NB DE COMPOSANTE TOTALES DES TENSEURS
!                                       = 6  3D
!                                       = 4  AXIS  C_PLAN  D_PLAN
!                                       = 1  1D
!               NDI             NB DE COMPOSANTE DIRECTES DES TENSEURS
!               NVI             NB DE VARIABLES INTERNES
!               NR              NB EQUATION SYSTEME INTEGRE A RESOUDRE
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
#include "jeveux.h"
#include "asterc/iisnan.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/betcvx.h"
#include "asterfort/betimp.h"
#include "asterfort/betjpl.h"
#include "asterfort/betmat.h"
#include "asterfort/codent.h"
#include "asterfort/lcdedi.h"
#include "asterfort/lcdehy.h"
#include "asterfort/lcelin.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcplbe.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: imat, ndt, ndi, nr, nvi
    integer :: itmax, icomp
    integer :: nmat, irtet, irteti, nseui4
    integer :: nseuil, nseui1, nseui2, nseui3
    integer :: iadzi, iazk24
    real(kind=8) :: toler
    real(kind=8) :: epsi
!
!-----------------------------------------------------------------------
    integer :: iret, kpg, ksp
    real(kind=8) :: tneg, tref
!-----------------------------------------------------------------------
    parameter       ( epsi = 1.d-15 )
    parameter       ( nmat = 90     )
    parameter       ( tneg = -1.d3  )
!
    real(kind=8) :: crit(*)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: tempd, tempf
    real(kind=8) :: elgeom(*)
    real(kind=8) :: epsd(6), deps(6)
    real(kind=8) :: epsdt(6), depst(6)
    real(kind=8) :: sigd(6), sigf(6), sige(6)
!
    real(kind=8) :: dsde(6, 6)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), tmpmx
    character(len=7) :: etatd, etatf
    character(len=8) :: mod, typma, typmod(*)
    character(len=16) :: opt
    character(len=3) :: matcst, cnseui
    character(len=8) :: nomail
    character(len=*) :: fami
    logical :: rigi, resi, istemp
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
    common /ecri/   nomail
!       ----------------------------------------------------------------
!
! --    INITIALISATION DES PARAMETRES DE CONVERGENCE ET ITERATIONS
!
    irteti = 0
    itmax = int(crit(1))
    toler = crit(3)
!        LOI      = COMP(1)
    mod = typmod(1)
    nseuil = 0
    nseui1 = 0
    nseui2 = 0
    nseui3 = 0
    nseui4 = 0
    nomail = ' '
!
    resi = opt(1:9).eq.'FULL_MECA' .or. opt .eq.'RAPH_MECA'
    rigi = opt(1:9).eq.'FULL_MECA' .or. opt(1:9).eq.'RIGI_MECA'
    ASSERT((opt(1:9).eq.'RIGI_MECA') .or. (opt(1:9).eq.'FULL_MECA') .or. (opt .eq.'RAPH_MECA'))
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tempd, iret)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tempf, iret)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret)
!
! --    C'EST INTERDIT DE MELANGER DEUX MODELISATIONS AVEC OU SANS
! --      DEPENDENCE DES PARAMETRES DE LA TEMPERATURE
    if ((iisnan(tempd).eq.0) .and. (iisnan(tempf).eq.1)) then
        call utmess('F', 'ALGORITH9_100')
    else if ((iisnan(tempd).eq.1).and.(iisnan(tempf).eq.0)) then
        call utmess('F', 'ALGORITH9_100')
    else if ((vind(3).eq.tneg).and.(iisnan(tempf).eq.0)) then
        call utmess('F', 'ALGORITH9_100')
    else
        istemp = iisnan(tempd).eq.0 .and. iisnan(tempf).eq.0
    endif
!
! --    OPTION SUPPRIMEE CAR TYPMA EST IMPOSE SUIVANT QUE L'ON EST EN
! --    PLASTCITE OU VISCOPLASTICITE. TYPMA EST DEFINI DANS LCMATE
! --    POUR LES MODELE VISCO-PLASTIQUES A LA VALEUR 'COHERENT'.
!          IF ( INT(CRIT(2)) .EQ. 0 ) THEN
!          TYPMA = 'VITESSE '
!          ELSE
!          TYPMA = 'COHERENT'
!          ENDIF
!
    typma = 'VITESSE '
!
    if (itmax .le. 0) itmax = -itmax
!
! --    LES PARAMETRES SONT FONCTIONS DE LA TEMPERATURE MAXIMALE
! --    VIND(3) EST LE MAX DES TEMPERATURES DANS L'HISTORIQUE DES TEMP.
!
    if (istemp) then
        tmpmx = vind(3)
        if (tempd .gt. tmpmx) tmpmx = tempd
    else
        tmpmx=r8nnem()
    endif
!
! --    RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
!                    NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
!
!
    call betmat(fami, kpg, ksp, mod, imat,&
                nmat, tmpmx, tempf, materd, materf,&
                matcst, ndt, ndi, nr, nvi)
!
! --    RETRAIT INCREMENT DE DEFORMATION DUE A LA DILATATION THERMIQUE
!
    call lcdedi(fami, kpg, ksp, nmat, materd,&
                materf, tempd, tempf, tref, depst,&
                epsdt, deps, epsd)
!
! --    RETRAIT ENDOGENNE ET RETRAIT DE DESSICCATION
!
    call lcdehy(fami, kpg, ksp, nmat, materd,&
                materf, deps, epsd)
!
! --    SEUIL A T > ETAT ELASTIQUE OU PLASTIQUE A T
!
    if (abs(vind (nvi)) .le. epsi) then
        etatd = 'ELASTIC'
    else
        etatd = 'PLASTIC'
    endif
!
!  -->  REDECOUPAGE IMPOSE
    if (icomp .eq. -1 .and. opt .ne. 'RIGI_MECA_TANG') then
        irteti = 0
        goto 9999
    endif
!
!       ----------------------------------------------------------------
!       OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
!       ----------------------------------------------------------------
!
!        IF ( OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA' ) THEN
    if (resi) then
!
! --    INTEGRATION ELASTIQUE SUR DT
!
!       CALL LCELAS ( LOI  ,MOD ,  IMAT,  NMAT, MATERD, MATERF, MATCST,
!    1                NVI,  TEMPD, TEMPF, TIMED,TIMEF,  DEPS,   EPSD,
!    2                SIGD ,VIND,  SIGE,  VINF )
        call lcelin(mod, nmat, materd, materf, deps,&
                    sigd, sige)
        call lceqvn(nvi-1, vind, vinf)
        vinf(nvi) = 0.d0
!
        if (istemp) then
            if (tmpmx .lt. tempf) tmpmx = tempf
            vinf(3) = tmpmx
        else
            vinf(3) = tneg
        endif
!
        call lceqvn(ndt, sige, sigf)
!
! --    PREDICTION ETAT ELASTIQUE A T+DT : F(SIG(T+DT),VIN(T)) = 0 ?
!
        call betcvx(nmat, materf, sigf, vind, vinf,&
                    elgeom, nvi, nseuil)
!
        if (nseuil .ge. 0) then
!
! --       PREDICTION INCORRECTE > INTEGRATION ELASTO-PLASTIQUE SUR DT
!
            etatf = 'PLASTIC'
!
            nseui1 = nseuil
            call lcplbe(toler, itmax, nmat, materf, nvi,&
                        vind, sigf, vinf, elgeom, nseuil,&
                        irtet)
!           GOTO (1), IRTET
!
            call betcvx(nmat, materf, sigf, vind, vinf,&
                        elgeom, nvi, nseuil)
            nseui2 = nseuil
!
            if (nseui2 .gt. 0) then
                if (nseui2 .eq. 44) then
                    call utmess('A', 'ALGORITH9_93')
                    goto 1
                endif
                if (nseui2 .eq. 4) then
                    call codent(nseui1, 'G', cnseui)
                    call utmess('A', 'ALGORITH9_94', sk=cnseui)
                    goto 1
                endif
                if (nseui2 .eq. nseui1) then
                    if (nseui2 .ne. 3) then
                        nseui2 = 3
                    else
                        nseui2 = 2
                    endif
                    nseuil = nseui2
                endif
                call lceqvn(ndt, sige, sigf)
                call lcplbe(toler, itmax, nmat, materf, nvi,&
                            vind, sigf, vinf, elgeom, nseuil,&
                            irtet)
!              GOTO (1), IRTET
!
                call betcvx(nmat, materf, sigf, vind, vinf,&
                            elgeom, nvi, nseuil)
                nseui3 = nseuil
            endif
!
            if (nseui3 .gt. 0) then
                if (nseui3 .eq. 44) then
                    call utmess('A', 'ALGORITH9_93')
                    goto 1
                endif
                if (nseui3 .eq. 4) then
                    call codent(nseui2, 'G', cnseui)
                    call utmess('A', 'ALGORITH9_94', sk=cnseui)
                    goto 1
                endif
                if (nseui3 .eq. nseui1 .or. nseui3 .eq. nseui2) then
                    nseui3 = 6 - nseui1 - nseui2
                    nseuil = nseui3
                endif
                call lceqvn(ndt, sige, sigf)
                call lcplbe(toler, itmax, nmat, materf, nvi,&
                            vind, sigf, vinf, elgeom, nseuil,&
                            irtet)
!              GOTO (1), IRTET
!
                call betcvx(nmat, materf, sigf, vind, vinf,&
                            elgeom, nvi, nseuil)
                nseui4 = nseuil
            endif
!
            if (nseui4 .gt. 0) then
                if (nseui4 .eq. 44) then
                    call utmess('A', 'ALGORITH9_93')
                    goto 1
                endif
                if (nseui4 .eq. 4) then
                    call codent(nseui3, 'G', cnseui)
                    call utmess('A', 'ALGORITH9_94', sk=cnseui)
                    goto 1
                endif
                nseuil = 22
                nseui4 = nseuil
                call lceqvn(ndt, sige, sigf)
                call lcplbe(toler, itmax, nmat, materf, nvi,&
                            vind, sigf, vinf, elgeom, nseuil,&
                            irtet)
!             GOTO (1), IRTET
!
                call betcvx(nmat, materf, sigf, vind, vinf,&
                            elgeom, nvi, nseuil)
            endif
!
            if (nseuil .ge. 0) then
                call codent(nseui4, 'G', cnseui)
                call utmess('A', 'ALGORITH9_94', sk=cnseui)
                goto 1
            endif
!
        else
!
! --       PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
!
            etatf = 'ELASTIC'
        endif
!
    endif
!
!       ----------------------------------------------------------------
!       OPTIONS 'FULL_MECA', 'RIGI_MECA_TANG', 'RIGI_MECA_ELAS' :
!          CALCUL DE DSDE
!       ----------------------------------------------------------------
!       EVALUATION DU JACOBIEN DSDE A (T+DT) POUR 'FULL_MECA'
!       ET CALCUL ELASTIQUE    ET   A (T)    POUR 'RIGI_MECA_TANG'
!       ----------------------------------------------------------------
!
!        IF ( OPT .EQ. 'RIGI_MECA_TANG' .OR. OPT .EQ. 'FULL_MECA' ) THEN
    if (rigi) then
        if (opt(1:9) .eq. 'RIGI_MECA') then
            if (( etatd.eq.'ELASTIC' ) .or. ( opt(10:14).eq.'_ELAS' )) then
!
!           CALL LCJELA ( LOI  , MOD ,  IMAT,  NMAT, MATERD, NVI,
!    1                    TEMPD, TIMED, DEPS,  EPSD, SIGD ,  VIND, DSDE)
                call lcopli('ISOTROPE', mod, materd(1, 1), dsde)
            else if (etatd .eq. 'PLASTIC') then
!   ------> ELASTOPLASTICITE ==> TYPMA = 'VITESSE '
!   ------> VISCOPLASTICITE  ==> TYPMA = 'COHERENT '==> CALCUL ELASTIQUE
                if (typma .eq. 'COHERENT') then
! PAS UTILISE ICI  CALL LCJELA ( LOI  , MOD ,  NMAT, MATERD,VIND, DSDE)
                else if (typma .eq. 'VITESSE ') then
                    call betjpl(mod, nmat, materd, sigd, vind,&
                                elgeom, dsde)
                endif
            endif
!
        else if (opt (1:9) .eq. 'FULL_MECA') then
            if (( etatf .eq. 'ELASTIC' ) .or. ( opt(10:14) .eq. '_ELAS' )) then
!           CALL LCJELA ( LOI  , MOD ,  IMAT,  NMAT, MATERF, NVI,
!    1                    TEMPF, TIMEF, DEPS,  EPSD, SIGF ,  VINF, DSDE)
                call lcopli('ISOTROPE', mod, materf(1, 1), dsde)
            else if (etatf .eq. 'PLASTIC') then
!   ------> ELASTOPLASTICITE ==>  TYPMA = 'VITESSE '
!   ------> VISCOPLASTICITE  ==>  TYPMA = 'COHERENT '
                if (typma .eq. 'COHERENT') then
! PAS UTILISE ICI  CALL LCJPLC ( LOI  , MOD ,  NMAT, MATERD, DSDE)
                else if (typma .eq. 'VITESSE ') then
                    call betjpl(mod, nmat, materd, sigf, vinf,&
                                elgeom, dsde)
                endif
            endif
        endif
    endif
!
!       ----------------------------------------------------------------
!
    irteti = 0
    goto 9999
 1  continue
    irteti = 1
    call betimp(nmat, materf, sigf, vind, vinf,&
                elgeom, nseui1, nseui2, nseui3, nseui4,&
                sige, sigd)
!
    call tecael(iadzi, iazk24)
    nomail = zk24(iazk24-1+3)(1:8)
    call utmess('A', 'ALGORITH9_95', sk=nomail)
!
    goto 9999
!
9999  continue
!
end subroutine

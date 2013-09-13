subroutine nmhuj(typmod, imat, comp, crit, instam,&
                 instap, tempm, tempf, tref, angmas,&
                 epsd, deps, sigd, vind, opt,&
                 sigf, vinf, dsde, iret)
! aslint: disable=W1501
    implicit none
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
!  ================================================================
!  INTEGRATION DE LA LOI DE COMPORTEMENT ELASTO PLASTIQUE DE HUJEUX
!  AVEC    . 50 VARIABLES INTERNES
!          . 4 FONCTIONS SEUIL ELASTIQUE DEDOUBLEES AVEC CYCLIQUE
!
!  INTEGRATION DES CONTRAINTES           = SIG(T+DT)
!  INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
!  ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
!  ================================================================
!  IN      TYPMOD  TYPE DE MODELISATION
!          IMAT    ADRESSE DU MATERIAU CODE
!          COMP    COMPORTEMENT DE L ELEMENT
!                  COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
!                  COMP(2) = NB DE VARIABLES INTERNES
!                  COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!          CRIT    CRITERES  LOCAUX
!                  CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                            (ITER_INTE_MAXI == ITECREL)
!                  CRIT(2) = TYPE DE JACOBIEN A T+DT
!                            (TYPE_MATR_COMP == MACOMP)
!                            0 = EN VITESSE     > SYMETRIQUE
!                            1 = EN INCREMENTAL > NON-SYMETRIQUE
!                  CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                            (RESI_INTE_RELA == RESCREL)
!                  CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                            REDECOUPAGE LOCAL DU PAS DE TEMPS
!                            (RESI_INTE_PAS == ITEDEC )
!                            0 = PAS DE REDECOUPAGE
!                            N = NOMBRE DE PALIERS
!          INSTAM  INSTANT T
!          INSTAP  INSTANT T+DT
!          TEMPM   TEMPERATURE A T
!          TEMPF   TEMPERATURE A T+DT
!          TREF    TEMPERATURE DE REFERENCE
!          ANGMAS  LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM),
!                    + UN REEL QUI VAUT 0 SI NAUTIQUIES OU 2 SI EULER
!                    + LES 3 ANGLES D'EULER
!          EPSD    DEFORMATION TOTALE A T
!          DEPS    INCREMENT DE DEFORMATION TOTALE
!          SIGD    CONTRAINTE A T
!          VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!          OPT     OPTION DE CALCUL A FAIRE
!                          'RIGI_MECA_TANG'> DSDE(T)
!                          'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
!                          'RAPH_MECA'     > SIG(T+DT)
!  OUT     SIGF    CONTRAINTE A T+DT
!          VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!          DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!          IRET    CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
!                         IRET=0 => PAS DE PROBLEME
!                         IRET=1 => ECHEC
!  ----------------------------------------------------------------
!  INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
!                        (*,2) = CARACTERISTIQUES PLASTIQUES A T
!          MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
!                        (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
!          NDT             NB DE COMPOSANTE TOTALES DES TENSEURS
!                                  = 6  3D
!                                  = 4  AXIS  C_PLAN  D_PLAN
!                                  = 1  1D
!          NDI             NB DE COMPOSANTE DIRECTES DES TENSEURS
!          NVI             NB DE VARIABLES INTERNES
!  ----------------------------------------------------------------
!  ROUTINE LC....UTILITAIRES POUR INTEGRATION LOI DE COMPORTEMENT
!  ----------------------------------------------------------------
!  ORDRE DES TENSEURS      3D      XX YY ZZ XY XZ YZ
!                          DP      XX YY ZZ XY
!                          AX      RR ZZ TT RZ
!                          1D      XX YY ZZ
!  ----------------------------------------------------------------
!  ATTENTION
!  SI OPT = 'RIGI_MECA_TANG' NE PAS TOUCHER AUX VARIABLES SIGF,VINF
!  QUI N ONT PAS DE PLACE MEMOIRE ALLOUEE
!
!  SIG EPS DEPS  ONT DEJA LEURS COMPOSANTES DE CISAILLEMENT
!  MULTIPLIES PAR RACINE DE 2 > PRISE EN COMPTE DES DOUBLES
!  PRODUITS TENSORIELS ET CONSERVATION DE LA SYMETRIE
!
!  ----------------------------------------------------------------
#include "asterc/iisnan.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/hujcrd.h"
#include "asterfort/hujcri.h"
#include "asterfort/hujdp.h"
#include "asterfort/hujmat.h"
#include "asterfort/hujori.h"
#include "asterfort/hujpre.h"
#include "asterfort/hujprj.h"
#include "asterfort/hujres.h"
#include "asterfort/hujtel.h"
#include "asterfort/hujtid.h"
#include "asterfort/lceqve.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcinma.h"
#include "asterfort/mgauss.h"
#include "asterfort/utmess.h"
    integer :: imat, ndt, ndi, nvi, iret, iret1
    integer :: i, inc, incmax, ndtt, limsup
    real(kind=8) :: crit(*), vind(50), vinf(50), vind0(50)
    real(kind=8) :: instam, instap, tempm, tempf, tref
    real(kind=8) :: epsd(6), deps(6), deps0(6)
    real(kind=8) :: sigd(6), sigf(6), dsde(6, 6), seuil
    real(kind=8) :: piso, depsr(6), depsq(6), tin(3)
    real(kind=8) :: d, q, m, phi, b, degr, angmas(3)
    real(kind=8) :: pc0, sigd0(6), hill, dsig(6)
    character(len=7) :: etatd, etatf
    character(len=8) :: mod, typmod(*)
    character(len=16) :: comp(*), opt
    real(kind=8) :: depsth(6), alpha(3)
    real(kind=8) :: det, bid16(6), bid66(6, 6)
    real(kind=8) :: materf(22, 2), zero, un, deux, dix
    real(kind=8) :: neps, nsig, ptrac, rtrac
    logical :: debug, conv, reorie, tract
!
    parameter     ( degr  = 0.0174532925199d0 )
!
!     ----------------------------------------------------------------
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!     ----------------------------------------------------------------
    data       zero / 0.0d0 /
    data       un   / 1.0d0 /
    data       deux / 2.0d0 /
    data       dix  / 10.d0 /
!
    iret = 0
    debug = .false.
    tract = .false.
! --- DEBUG = .TRUE. : MODE AFFICHAGE ENRICHI
!
    if (debug) write(6,'(A)')'HHHHHHHHHHHHHHHHHHHHHH'
    mod = typmod(1)
!
! ---> RECUPERATION COEF DE LA LOI HUJEUX
!      (INDEPENDANTS DE LA TEMPERATURE)
!      NB DE CMP DIRECTES/CISAILLEMENT
!      NB VARIABLES INTERNES
    call hujmat(mod, imat, tempf, materf, ndt,&
                ndi, nvi)
!
    ptrac = materf(21,2)
    rtrac = abs(1.d-6*materf(8,2))
!
! --- REORIENTATION DES PLANS DE GLISSEMENT SUR LES AXES DU
!     REPERE LOCAL DONNE PAR LES ANGLES NAUTIQUES (ANGMAS)
    if (angmas(1) .eq. r8vide()) then
        call utmess('F', 'ALGORITH8_20')
    endif
    reorie =(angmas(1).ne.zero) .or. (angmas(2).ne.zero)&
     &         .or. (angmas(3).ne.zero)
    call hujori('LOCAL', 1, reorie, angmas, sigd,&
                bid66)
    call hujori('LOCAL', 1, reorie, angmas, epsd,&
                bid66)
    call hujori('LOCAL', 1, reorie, angmas, deps,&
                bid66)
!
! --- ON TRAVAILLE TOUJOURS AVEC UN TENSEUR CONTRAINTES
!     DEFINI EN 3D
!
    ndtt = 6
    if (ndt .lt. 6) then
        ndtt = 4
        ndt = 6
    endif
!
!
!     CALCUL DE DEPSTH ET EPSDTH
!     --------------------------
! ---> COEF DE DILATATION LE MEME A TPLUS ET TMOINS
    if (materf(17,1) .eq. un) then
!
        if (((iisnan(tempm).gt.0) .or. (iisnan(tref).gt.0)) .and. ( materf(3,1).ne.zero)) then
            call utmess('F', 'CALCULEL_15')
        endif
!
        alpha(1) = materf(3,1)
        alpha(2) = materf(3,1)
        alpha(3) = materf(3,1)
!
    else if (materf(17,1).eq.deux) then
!
        alpha(1) = materf(10,1)
        alpha(2) = materf(11,1)
        alpha(3) = materf(12,1)
        if (((iisnan(tempm).gt.0) .or. (iisnan(tref).gt.0)) .and.&
            ( (alpha(1).ne.zero) .or. (alpha(2).ne.zero) .or. (alpha(3) .ne.zero) )) then
            call utmess('F', 'CALCULEL_15')
        endif
!
    else
        call utmess('F', 'COMPOR1_33')
    endif
!
    if ((iisnan(tempm).gt.0) .or. (iisnan(tempf).gt.0) .or. (iisnan(tref) .gt.0)) then
!
        do 20 i = 1, ndi
            depsth(i) = deps(i)
20      continue
!
    else
!
        do 25 i = 1, ndi
            depsth(i) = deps(i) - alpha(i)*(tempf-tref) + alpha(i)*( tempm-tref)
25      continue
!
    endif
!
    do 21 i = ndi+1, ndt
        depsth(i) = deps(i)
21  continue
!
    if (ndtt .lt. 6) then
        do 22 i = ndtt+1, 6
            depsth(i) = zero
            sigd(i) = zero
22      continue
    endif
!
! ---> INITIALISATION SEUIL DEVIATOIRE SI NUL
!
    do 30 i = 1, ndi
        if (vind(i) .eq. zero) then
!
            if (materf(13, 2) .eq. zero) then
                vind(i) = 1.d-3
            else
                vind(i) = materf(13,2)
            endif
!
            call hujcrd(i, materf, sigd, vind, seuil)
!
! --- SI LE SEUIL EST DESEQUILIBRE A L'ETAT INITIAL
!     ON EQUILIBRE LE SEUIL EN CALCULANT LA VALEUR DE R
!     APPROPRIEE
!
            if (seuil .gt. zero) then
                call hujprj(i, sigd, tin, piso, q)
                piso = piso - ptrac
                b = materf(4,2)
                phi = materf(5,2)
                m = sin(degr*phi)
                pc0 = materf(7,2)
                vind(i) = -q/(m*piso*(un-b*log(piso/pc0)))
                vind(23+i) = un
            endif
!
        endif
30  continue
!
! ---> INITIALISATION SEUIL ISOTROPE SI NUL
    if (vind(4) .eq. zero) then
        if (materf(14, 2) .eq. zero) then
            vind(4) = 1.d-3
        else
            vind(4) = materf(14,2)
        endif
!
        call hujcri(materf, sigd, vind, seuil)
!
! --- SI LE SEUIL EST DESEQUILIBRE A L'ETAT INITIAL
!     ON EQUILIBRE LE SEUIL EN CALCULANT LA VALEUR DE R
!     APPROPRIEE
!
        if (seuil .gt. zero) then
            piso = (sigd(1)+sigd(2)+sigd(3))/3
            d = materf(3,2)
            pc0 = materf(7,2)
            vind(4) = piso/(d*pc0)
            if (vind(4) .gt. 1.d0) then
                call utmess('F', 'COMPOR1_83')
            endif
            vind(27)= un
        endif
!
    endif
!
! ---> INITIALISATION SEUIL CYCLIQUE SI NUL
    do 40 i = 1, ndi
        if (vind(4+i) .eq. zero) then
            if (materf(18, 2) .eq. zero) then
                vind(4+i) = 1.d-3
            else
                vind(4+i) = materf(18,2)
            endif
        endif
40  continue
!
    if (vind(8) .eq. zero) then
        if (materf(19, 2) .eq. zero) then
            vind(8) = 1.d-3
        else
            vind(8) = materf(19,2)
        endif
    endif
!
!ONTROLE DES INDICATEURS DE PLASTICITE
    do 39 i = 1, 4
        if (abs(vind(27+i)-un) .lt. r8prem()) vind(23+i)=-un
39  continue
!
    if (opt(1:9) .ne. 'RIGI_MECA') call lceqvn(50, vind, vinf)
!
! ---> ETAT ELASTIQUE OU PLASTIQUE A T
    if (( (vind(24) .eq. zero) .or. (vind(24) .eq. -un .and. vind(28) .eq. zero) ) .and.&
        ( (vind(25) .eq. zero) .or. (vind(25) .eq. -un .and. vind(29) .eq. zero) ) .and.&
        ( (vind(26) .eq. zero) .or. (vind(26) .eq. -un .and. vind(30) .eq. zero) ) .and.&
        ( (vind(27) .eq. zero) .or. (vind(27) .eq. -un .and. vind(31) .eq. zero) )) then
        etatd = 'ELASTIC'
    else
        etatd = 'PLASTIC'
    endif
!
!     -------------------------------------------------------------
!     OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
!     -------------------------------------------------------------
    if (opt(1:9) .eq. 'RAPH_MECA' .or. opt(1:9) .eq. 'FULL_MECA') then
!
        if (debug) write(6,*)'DEPS =',(depsth(i),i=1,3)
!
        do 44 i = 1, 3
            call hujprj(i, sigd, tin, piso, q)
            if (abs(piso+deux*rtrac-ptrac) .lt. r8prem()) tract = .true.
44      continue
!
! ---> INTEGRATION ELASTIQUE SUR DT
        do 45 i = 1, ndt
            depsq(i) = zero
45      continue
!
! -----------------------------------------------
! ---> INCREMENT TOTAL DE DEFORMATION A APPLIQUER
! -----------------------------------------------
! - ENREGISTREMENT DE L'ETAT DE CONTRAINTES A T
        call lceqve(sigd, sigd0)
! - ENREGISTREMENT DE L'INCREMENT TOTAL DEPS0
        call lceqve(depsth, deps0)
! - INITIALISATION DES DEFORMATIONS RESTANTES
        call lceqve(depsth, depsq)
        call lceqvn(nvi, vind, vind0)
! - INITIALISATION DU COMPTEUR D'ITERATIONS LOCALES
        vind(35) = zero
!
! -----------------------------------------------------
! ---> PREDICTION VIA TENSEUR ELASTIQUE DES CONTRAINTES
! -----------------------------------------------------
        inc = 0
        incmax = 1
100      continue
!
        inc = inc + 1
        call lceqve(depsq, depsr)
        call hujpre(etatd, mod, crit, imat, materf,&
                    depsr, sigd, sigf, vind0, iret)
        if (iret .eq. 1) goto 9999
!
! ----------------------------------------------------
! ---> CONTROLE DE L EVOLUTION DE LA PRESSION ISOTROPE
! ----------------------------------------------------
        iret1 =0
        call hujdp(mod, depsr, sigd, sigf, materf,&
                   vind, incmax, iret1)
        if (debug .and. iret1 .eq. 1) write (6, '(A)' ) 'NMHUJ :: HUJDP :: PAS DE RESUBDIVISON'
!
! --- ON LIMITE LE REDECOUPAGE LOCAL A 20 POUR HUJDP
        limsup = 20
        if (abs(crit(5)) .gt. limsup) limsup = int(abs(crit(5)))
        if (incmax .ge. limsup) then
            incmax = limsup
        else if (incmax.le.1) then
            incmax =1
        endif
!
        if (inc .eq. 1 .and. incmax .gt. 1) then
            do 48 i = 1, ndt
                depsq(i)=deps0(i) /incmax
                depsr(i)=deps0(i) /incmax
48          continue
            call hujpre(etatd, mod, crit, imat, materf,&
                        depsr, sigd, sigf, vind0, iret)
        endif
!
! ---------------------------------------------
! CALCUL DE L'ETAT DE CONTRAINTES CORRESPONDANT
! ---------------------------------------------
        if (debug) write(6,*)'NMHUJ -- VINF =',(vinf(i),i=24,31)
        call hujres(mod, crit, materf, imat, nvi,&
                    depsr, sigd, vind, sigf, vinf,&
                    iret, etatf)
        if (iret .eq. 1) goto 9999
!
! -------------------------------------------
! - CONTROLE DES DEFORMATIONS DEJA APPLIQUEES
! -------------------------------------------
        if (inc .lt. incmax) then
            conv =.false.
        else
            conv =.true.
        endif
!
        if (.not.conv) then
            call lceqve(sigf, sigd)
            call lceqvn(nvi, vinf, vind)
            goto 100
        endif
!
! --- CALCUL DU CRITERE DE HILL: DSIG*DEPS
        hill = zero
        nsig = zero
        neps = zero
        do 57 i = 1, ndt
            dsig(i) = sigf(i) - sigd0(i)
            hill = hill + dsig(i)*deps0(i)
            nsig = nsig + dsig(i)**2.d0
            neps = neps + deps0(i)**2.d0
57      continue
!
! --- NORMALISATION DU CRITERE : VARIE ENTRE -1 ET 1
        if ((neps.gt.r8prem()) .and. (nsig.gt.r8prem())) then
            vinf(32) = hill/sqrt(neps*nsig)
        else
            vinf(32) = zero
        endif
!
    endif
!af 07/05/07 fin <IF RAPH_MECA et FULL_MECA>
!
!       ----------------------------------------------------------------
!       OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
!       ----------------------------------------------------------------
!       CALCUL ELASTIQUE ET EVALUATION DE DSDE A (T)
!       POUR 'RIGI_MECA_TANG' ET POUR 'FULL_MECA'
!       ----------------------------------------------------------------
    if (opt .eq. 'RIGI_MECA_TANG') then
!
        call lcinma(zero, dsde)
!
! REMARQUE: CALCUL DE DSDE A T AVEC MATERF CAR PARAMETRES HUJEUX
! --------  INDEPENDANTS DE LA TEMPERATURE
!
! ---> CALCUL MATRICE DE RIGIDITE ELASTIQUE
        if (etatd .eq. 'ELASTIC') then
            call hujtel(mod, materf, sigd, dsde)
        endif
!
! ---> CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
        if (etatd .eq. 'PLASTIC') then
            call hujtid(mod, imat, sigd, vind, dsde,&
                        iret)
            if (iret .eq. 1) goto 9999
        endif
!
        call hujori('GLOBA', 2, reorie, angmas, bid16,&
                    dsde)
!
    else if (opt .eq. 'FULL_MECA') then
!
        call lcinma(zero, dsde)
!
! ---> CALCUL MATRICE DE RIGIDITE ELASTIQUE
        if (etatf .eq. 'ELASTIC') then
            call hujtel(mod, materf, sigf, dsde)
        endif
!
! ---> CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
        if (etatf .eq. 'PLASTIC') then
            call hujtid(mod, imat, sigf, vinf, dsde,&
                        iret)
            if (iret .eq. 1) goto 9999
        endif
!
    else if (opt .eq. 'FULL_MECA_ELAS') then
!
        call lcinma(zero, dsde)
        call hujtel(mod, materf, sigf, dsde)
!
    else if (opt .eq. 'RIGI_MECA_ELAS') then
!
        call lcinma(zero, dsde)
        call hujtel(mod, materf, sigd, dsde)
        call hujori('GLOBA', 2, reorie, angmas, bid16,&
                    dsde)
!
    endif
! fin <IF RIGI_MECA_TANG>
!
! ---> CALCUL DETERMINANT DE LA MATRICE TANGENTE + INDICATEUR
! --- RELIE AUX MECANISMES ACTIFS
    if (opt(1:9) .ne. 'RIGI_MECA') then
!
        call hujori('GLOBA', 2, reorie, angmas, bid16,&
                    dsde)
!
        if (opt .eq. 'FULL_MECA') then
            call mgauss('NCSD', dsde, sigd, 6, 6,&
                        1, det, iret)
            if (iret .eq. 1) then
                vinf(33) = un
                iret = 0
            else
                vinf(33) = det
            endif
        endif
!
        vinf(34) = zero
!
        do 60 i = 1, 8
            if (abs(vinf(23+i)-un) .lt. r8prem()) then
                if (i .eq. 1) vinf(34)=vinf(34)+dix**zero
                if (i .eq. 2) vinf(34)=vinf(34)+dix**un
                if (i .eq. 3) vinf(34)=vinf(34)+dix**deux
                if (i .eq. 4) vinf(34)=vinf(34)+dix**3.d0
                if (i .eq. 5) vinf(34)=vinf(34)+dix**4.d0
                if (i .eq. 6) vinf(34)=vinf(34)+dix**5.d0
                if (i .eq. 7) vinf(34)=vinf(34)+dix**6.d0
                if (i .eq. 8) vinf(34)=vinf(34)+dix**7.d0
            endif
60      continue
!
!
    endif
! --- ON RENVOIE LA VALEUR ADEQUATE DE NDT
!     POUR MODELISATION D_PLAN
    if (ndtt .eq. 4) ndt = 4
!
    if (opt .eq. 'RAPH_MECA' .or. opt(1:9) .eq. 'FULL_MECA') call hujori('GLOBA', 1, reorie,&
                                                                         angmas, sigf, bid66)
!
9999  continue
!
    if (opt(1:9) .eq. 'RAPH_MECA' .or. opt(1:9) .eq. 'FULL_MECA') then
        if (iret .eq. 1) then
            if (.not.tract) then
                call lcinma(zero, dsde)
                call hujtel(mod, materf, sigd, dsde)
                call lceqve(sigd0, sigf)
                call lceqve(sigd0, sigd)
                call lceqvn(50, vind0, vinf)
                call lceqvn(50, vind0, vind)
!
                if (debug) then
                    write(6,*)'************************************'
                    write(6,*)'DEPS =',(deps0(i),i=1,ndt)
                    write(6,*)'SIGD =',(sigd0(i),i=1,ndt)
                    write(6,*)'VIND =',(vind0(i),i=1,50)
                    write(6,*)
                endif
            else
!
                call lcinma(zero, dsde)
                call hujtel(mod, materf, sigd, dsde)
                do 61 i = 1, 3
                    sigf(i) = -deux*rtrac+ptrac
                    sigf(i+3) = zero
61              continue
                call lceqvn(50, vind0, vinf)
                iret = 0
            endif
        endif
        call lceqve(sigd0, sigd)
        call lceqve(deps0, deps)
        call lceqvn(50, vind0, vind)
    endif
!
end subroutine

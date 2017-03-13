subroutine plasti(fami, kpg, ksp, typmod, imate,&
                  compor, carcri, instam, instap, &
                  epsdt, depst, sigm,&
                  vim, option, angmas, sigp, vip,&
                  dsidep, icomp, nvi, codret, mult_compor_, wkin_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/lccnvx.h"
#include "asterfort/lcdedi.h"
#include "asterfort/lcdehy.h"
#include "asterfort/lcelas.h"
#include "asterfort/lcelpl.h"
#include "asterfort/lcmate.h"
#include "asterfort/lcotan.h"
#include "asterfort/lcplas.h"
#include "asterfort/lcpopl.h"
#include "asterfort/get_varc.h"
#include "blas/dcopy.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: imate
    character(len=16), intent(in) :: compor(*)
    real(kind=8), intent(in) :: carcri(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(in) :: epsdt(9)
    real(kind=8), intent(in) :: depst(9)
    real(kind=8), intent(in) :: sigm(6)
    real(kind=8), intent(in) :: vim(*)
    character(len=16), intent(in) :: option
    real(kind=8), intent(in) :: angmas(3)
    real(kind=8), intent(out) :: sigp(6)
    real(kind=8), intent(out) :: vip(*)

    character(len=8), intent(in) :: typmod(*)
    integer, intent(in) :: icomp
    integer, intent(in) :: nvi
    real(kind=8), intent(out) :: dsidep(6, *)
    integer, intent(out) :: codret
    character(len=16), optional, intent(in) :: mult_compor_
    real(kind=8), optional, intent(in) :: wkin_(9)
!
!     INTEGRATION DE LOIS DE COMPORTEMENT ELASTO PLASTIQUE ET VISCO
!     PLASTIQUE PAR UNE MATHODE DE NEWTON (DISCRETISATION IMPLICITE)
!
!     CALCUL DES CONTRAINTES           = SIGF(T+DT)
!     CALCUL DES VARIABLES INTERNES    = VINF(T+DT)
!     CALCUL DU JACOBIEN ASSOCIE       = DS/DE(T+DT) OU DS/DE(T)
!     CONVENTION :
!                 SUFFIXE D : DEBUT DU PAS DE TEMPS
!                 SUFFIXE F : FIN DU PAS DE TEMPS
!     ==================================================================
!     ARGUMENTS
!
!     IN FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!        KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!        TYPMOD  TYPE DE MODELISATION
!        IMAT    ADRESSE DU MATERIAU CODE
!        COMP    COMPORTEMENT DE L ELEMENT
!                COMP(1) = RELATION DE COMPORTEMENT (ROUSSELIER.)
!                COMP(2) = NB DE VARIABLES INTERNES
!                COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!        CRIT    CRITERES  LOCAUX
!                CRIT(1) = NOMBRE D ITERATIONS MAXI (ITER_INTE_MAXI)
!                CRIT(3) = TOLERANCE DE CONVERGENCE(RESI_INTE_RELA)
!                CRIT(4) = THETA
!                CRIT(5) = ITER_INTE_PAS (UTILISE PAR REDECE EN AMONT)
!                CRIT(6) = ALGO_INTE(NEWTON, NEWTON_PERT, NEWTON_RELI)
!        TIMED   INSTANT T
!        TIMEF   INSTANT T+DT
!        CES PARAMETRES DE TEMPERATURE NE SONT PAS PRIS EN COMPTE EN
!        MECANIQUE PURE (ON UTILISE LES VARIABLES DE COMMANDES)
!
!        EPSDT   DEFORMATION TOTALE A T
!        DEPST   INCREMENT DE DEFORMATION TOTALE
!        SIGD    CONTRAINTE A T
!        VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!        OPT     OPTION DE CALCUL
!                        'RIGI_MECA_TANG'> DSDE(T)
!                        'FULL_MECA'     > DSDE(T+DT), SIGF, VINF
!                        'RAPH_MECA'     > SIGF, VINF
!        ANGMAS  ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!                +  0 SI NAUTIQUIES OU 2 SI EULER
!                + LES 3 ANGLES D'EULER
!     OUT
!        SIGF    CONTRAINTE A T+DT
!        VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!        DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!        ICOMP   COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
!        NVI     NB DE VARIABLES INTERNES
!        IRTETI  CODE RETOUR =0 OK, =1 => REDECOUPAGE DU PAS DE TEMPS
!     ------------------------------------------------------------------
!     INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
!                           (*,2) = CARACTERISTIQUES PLASTIQUES A T
!             MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
!                           (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
!             MATCST          'OUI' SI MATERIAU CST ENTRE T ET T+DT
!                             'NON' SINON
!             NDT             NB DE COMPOSANTE TOTALES DES TENSEURS
!                                     = 6  3D
!                                     = 4  AXIS  C_PLAN  D_PLAN
!             NDI             NB DE COMPOSANTE DIRECTES DES TENSEURS
!             NR              NB EQUATION SYSTEME INTEGRE A RESOUDRE
!     ------------------------------------------------------------------
!     ATTENTION
!     SI OPT = 'RIGI_MECA_TANG' NE PAS TOUCHER AUX VARIABLES SIGF,VINF
!     QUI N ONT PAS DE PLACE MEMOIRE ALLOUEE
!
!     SIG EPS DEPS  ONT DEJA LEURS COMPOSANTES DE CISAILLEMENT
!     MULTIPLIES PAR RACINE DE 2 > PRISE EN COMPTE DES DOUBLES
!     PRODUITS TENSORIELS ET CONSERVATION DE LA SYMETRIE
!
!     ------------------------------------------------------------------
!
!     NMAT = NOMBRE MAXI DE PARMETRES MATERIAU
!     POUR LE MONOCRISTAL, DIMENSIONS MAX
!     NSG=NOMBRE DE SYSTEMES DE GLISSEMENT MAXIMUM
!     NFS=NOMBRE DE FAMILLES DE SYSTEMES DE GLISSEMENT MAXIMUM

    integer :: nmat, nsg, nfs, nrm, iret
    parameter  ( nsg=30)
    parameter  ( nfs=5)
    parameter  ( nrm=nfs*nsg+6)
    parameter  ( nmat=90)
!
    character(len=3) :: matcst
    character(len=7) :: etatd, etatf
    character(len=8) :: mod, typma
    character(len=16) :: rela_compor, defo_compor, mult_compor
    character(len=24) :: cpmono(5*nmat+1)
    aster_logical :: l_temp
!
    integer :: ndt, ndi, nr, itmax, irtet
    integer :: nbcomm(nmat, 3), numhsr(1), irr, decirr, nbsyst, decal, gdef
    real(kind=8) :: toler, epsi, materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: epsd(9), deps(9)
    real(kind=8) :: seuil, theta, dt, devg(6), devgii
    real(kind=8) :: vp(3), vecp(3, 3), pgl(3, 3), wkin(9)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg), drdy(nrm*nrm)

    real(kind=8) :: tempd, tempf, tref
!     POUR BETON_BURGER - ATTENTION DIMENSION MAXI POUR CE MODELE
    real(kind=8) :: yd(21), yf(21)
    parameter  ( epsi = 1.d-15 )
    aster_logical :: resi, rigi
!     ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
!
! --  INITIALISATION DES PARAMETRES DE CONVERGENCE ET ITERATIONS
!
    codret = 0
    itmax = int(carcri(1))
    toler = carcri(3)
    theta = carcri(4)
    rela_compor = compor(1)
    defo_compor = compor(3) 
    mult_compor = ' '
    if (present(mult_compor_)) then
        mult_compor = mult_compor_
    endif
    wkin(:) = 0.d0
    if (present(wkin_)) then
        wkin = wkin_
    endif
    mod = typmod(1)
    dt = instap - instam
    resi = option(1:9).eq.'RAPH_MECA' .or. option(1:9).eq.'FULL_MECA'
    rigi = option(1:9).eq.'RIGI_MECA' .or. option(1:9).eq.'FULL_MECA'
    gdef = 0
    if (defo_compor .eq. 'SIMO_MIEHE') gdef=1
    numhsr(1)=1
!
    typma = 'VITESSE '
!
! - Get temperatures
!
    call get_varc(fami , kpg  , ksp , 'T',&
                  tempd, tempf, tref, l_temp)
!
! - Glute pour LKR
!
    if (.not.l_temp .and. compor(1).eq.'LKR') then
        tempd = 0.d0
        tempf = 0.d0
        tref  = 0.d0
    endif
!
! --  RECUPERATION COEF MATERIAU A T ET/OU T+DT
!
    call lcmate(fami, kpg, ksp, compor, mod,&
                imate, nmat, tempd, tempf, tref, 0,&
                typma, hsr, materd, materf, matcst,&
                nbcomm, cpmono, angmas, pgl, itmax,&
                toler, ndt, ndi, nr, carcri,&
                nvi, vim, nfs, nsg, toutms,&
                1, numhsr, sigm, mult_compor)
!
!
    if (gdef .eq. 1) then
!        GDEF_MONO : PAS DE DEFORM. THERMIQUE
        call dcopy(9, depst, 1, deps, 1)
        call dcopy(9, epsdt, 1, epsd, 1)
    else
! --     RETRAIT INCREMENT DE DEFORMATION DUE A LA DILATATION THERMIQUE
        call lcdedi(fami, kpg, ksp, nmat, materd,&
                    materf, tempd, tempf, tref, depst,&
                    epsdt, deps, epsd)
! --     RETRAIT ENDOGENNE ET RETRAIT DE DESSICCATION
        call lcdehy(fami, kpg, ksp, nmat, materd,&
                    materf, deps, epsd)
    endif
!
! --    SEUIL A T > ETAT ELASTIQUE OU PLASTIQUE A T
    if (abs(vim(nvi)) .le. epsi) then
        etatd = 'ELASTIC'
    else
        etatd = 'PLASTIC'
    endif
!
! --> REDECOUPAGE IMPOSE
    if (icomp .eq. -1 .and. option .ne. 'RIGI_MECA_TANG') then
        codret = 0
        goto 999
    endif
!
!     ----------------------------------------------------------------
!     OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
!     ----------------------------------------------------------------
!
    if (resi) then
!
        if (gdef .eq. 1) then
!           GDEF_MONO : PAS DE SEUIL CAR C'EST PLUS COMPLIQUE
            seuil=1.d0
        else
! --        INTEGRATION ELASTIQUE SUR DT
            call lcelas(fami, kpg, ksp, rela_compor, mod,&
                        imate, nmat, materd, materf, matcst,&
                        nvi, angmas, deps, sigm, vim,&
                        sigp, vip, theta, etatd, carcri,&
                        iret)
            if (iret .ne. 0) goto 1
!
! --        PREDICTION ETAT ELASTIQUE A T+DT : F(SIG(T+DT),VIN(T)) = 0 ?
            seuil=1.d0
            call lccnvx(fami, kpg, ksp, rela_compor, mod,&
                        imate, nmat, materd, materf, sigm,&
                        sigp, deps, vim, vip, nbcomm,&
                        cpmono, pgl, nvi, vp, vecp,&
                        hsr, nfs, nsg, toutms, instam,&
                        instap, nr, yd, yf, toler,&
                        seuil, iret)
!
            if (iret .ne. 0) goto 1
        endif
!
        if (seuil .ge. 0.d0) then
! --        PREDICTION INCORRECTE > INTEGRATION ELASTO-PLASTIQUE SUR DT
            etatf = 'PLASTIC'
!
            call lcplas(fami, kpg, ksp, rela_compor, toler,&
                        itmax, mod, imate, nmat, materd,&
                        materf, nr, nvi, instam, instap,&
                        deps, epsd, sigm, vim, sigp,&
                        vip, compor, nbcomm, cpmono, pgl,&
                        nfs, nsg, toutms, hsr, icomp,&
                        irtet, theta, vp, vecp, seuil,&
                        devg, devgii, drdy, wkin, carcri)
!

            if (irtet .eq. 1) then
                goto 1
            else if (irtet .eq. 2) then
                goto 2
            endif
        else
! --        PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
            etatf = 'ELASTIC'
! ---       MISE A JOUR DE VINF EN FONCTION DE LA LOI
!           ET POST-TRAITEMENTS POUR DES LOIS PARTICULIERES
            call lcelpl(mod, rela_compor, nmat, materd, materf,&
                        instam, instap, deps, nvi, vim,&
                        vip, nr, yd, yf, sigm,&
                        sigp, drdy)
        endif
!
!        POST-TRAITEMENTS PARTICULIERS
        call lcpopl(rela_compor, angmas, nmat, materd, materf,&
                    mod, deps, sigm, sigp, vim,&
                    vip)
!
    endif
!
!     ----------------------------------------------------------------
!     OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
!     ----------------------------------------------------------------
!     EVALUATION DU JACOBIEN DSDE A (T+DT) POUR 'FULL_MECA'
!     ET CALCUL ELASTIQUE    ET   A (T)    POUR 'RIGI_MECA_TANG'
!     ----------------------------------------------------------------
!
    if (rigi) then
        call lcotan(option, angmas, etatd, etatf, fami,&
                    kpg, ksp, rela_compor, mod, imate,&
                    nmat, materd, materf, epsd, deps,&
                    sigm, sigp, nvi, vim, vip,&
                    drdy, vp, vecp, theta, dt,&
                    devg, devgii, instam, instap, compor,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, itmax, toler,&
                    typma, dsidep, irtet)
        if (irtet .ne. 0) goto 1
!
    endif
!
!       ----------------------------------------------------------------
!
    codret = 0
    goto 999
  1 continue
    codret = 1
    goto 999
!
  2 continue
    codret = 2
    goto 999
!
999 continue
!
end subroutine

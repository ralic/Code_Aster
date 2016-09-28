subroutine nmgr3d(nno  , npg   , ipoids, ivf   , idfde ,&
                  geomi, typmod, option, imate , compor, mult_comp,&
                  lgpg , carcri  , instam, instap, deplm ,&
                  deplp, angmas, sigm  , vim   , matsym,&
                  dfdi , pff   , def   , sigp  , vip   ,&
                  matuu, vectu , codret)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/r8inir.h"
#include "asterc/r8nnem.h"
#include "asterfort/codere.h"
#include "asterfort/lcdetf.h"
#include "asterfort/lcegeo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
#include "asterfort/nmgrtg.h"
#include "asterfort/pk2sig.h"
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
    integer, intent(in) :: nno
    integer, intent(in) :: npg
    integer, intent(in) :: ipoids
    integer, intent(in) :: ivf
    integer, intent(in) :: idfde
    real(kind=8), intent(in) :: geomi(3, nno)
    character(len=8), intent(in) :: typmod(*)
    character(len=16), intent(in) :: option
    integer, intent(in) :: imate
    character(len=16), intent(in) :: compor(*)
    character(len=16), intent(in) :: mult_comp
    real(kind=8), intent(in) :: carcri(*)
    integer, intent(in) :: lgpg
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(inout) :: deplm(1:3, 1:nno)
    real(kind=8), intent(inout) :: deplp(1:3, 1:nno)
    real(kind=8), intent(in) :: angmas(*)
    real(kind=8), intent(inout) :: sigm(6, npg)
    real(kind=8), intent(inout) :: vim(lgpg, npg)
    aster_logical, intent(in) :: matsym
    real(kind=8), intent(inout) :: dfdi(nno, 3)
    real(kind=8), intent(inout) :: pff(6, nno, nno)
    real(kind=8), intent(inout) :: def(6, nno, 3)
    real(kind=8), intent(inout) :: sigp(6, npg)
    real(kind=8), intent(inout) :: vip(lgpg, npg)
    real(kind=8), intent(inout) :: matuu(*)
    real(kind=8), intent(inout) :: vectu(3, nno)
    integer, intent(inout) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: 3D
! Options: RIGI_MECA_TANG, RAPH_MECA and FULL_MECA - Large displacements/rotations (GROT_GDEP)
!
! --------------------------------------------------------------------------------------------------
!
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOMI   : COORDONEES DES NOEUDS SUR CONFIG INITIALE
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DEPLP   : DEPLACEMENT A L'INSTANT COURANT
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! IN  MATSYM  : VRAI SI LA MATRICE DE RIGIDITE EST SYMETRIQUE
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: grand, axi
    integer :: kpg, j, cod(27)
    real(kind=8) :: dsidep(6, 6), f(3, 3), fm(3, 3), epsm(6), epsp(6), deps(6)
    real(kind=8) :: r, sigma(6), sigm_norm(6), detf, poids, vff(1)
    real(kind=8) :: elgeom(10, 27), rac2, maxeps, detfm
!
! --------------------------------------------------------------------------------------------------
!
    rac2  = sqrt(2.d0)
    grand = .true._1
    axi   = .false._1
    do kpg = 1, npg
        cod(kpg) = 0
    end do
!
! - Specific geometric parameters for some behaviours
!
    call lcegeo(nno  , npg   , ipoids, ivf, idfde,&
                geomi, typmod, compor, 3  , dfdi ,&
                deplm, deplp , elgeom)
!
! - Only isotropic material !
!
    call r8inir(3, r8nnem(), angmas, 1)
!
! - Loop on Gauss points
!
    do kpg = 1, npg
        epsm(1:6)=0.d0
        epsp(1:6)=0.d0
!
! ----- Kinematic - Previous strains
!
        call nmgeom(3       , nno   , .false._1, grand, geomi,&
                    kpg     , ipoids, ivf      , idfde, deplm,&
                    .true._1, poids , dfdi     , fm   , epsm,&
                    r)
!
! ----- Kinematic - Increment of strains
!
        call nmgeom(3        , nno   , .false._1, grand, geomi,&
                    kpg      , ipoids, ivf      , idfde, deplp,&
                    .false._1, poids , dfdi     , f    , epsp ,&
                    r)
        maxeps = 0.d0
        do j = 1, 6
            deps (j)=epsp(j)-epsm(j)
            maxeps=max(maxeps,abs(epsp(j)))
        end do
!
! ----- Check "small strains"
!
        if (maxeps .gt. 0.05d0) then
            if (compor(1)(1:4) .ne. 'ELAS') then
                call utmess('A', 'COMPOR2_9', sr=maxeps)
            endif
        endif
!
! ----- Stresses: convert Cauche to PK2
!
        call lcdetf(3, fm, detfm)
        call pk2sig(3, fm, detfm, sigm_norm, sigm(1, kpg),&
                    -1)
        sigm_norm(4) = sigm_norm(4)*rac2
        sigm_norm(5) = sigm_norm(5)*rac2
        sigm_norm(6) = sigm_norm(6)*rac2
!
! ----- Compute behaviour
!
        call nmcomp('RIGI'     , kpg        , 1        , 3     , typmod        ,&
                    imate      , compor     , carcri     , instam, instap        ,&
                    6          , epsm       , deps     , 6     , sigm_norm     ,&
                    vim(1, kpg), option     , angmas   , 10    , elgeom(1, kpg),&
                    sigma      , vip(1, kpg), 36       , dsidep, 1             ,&
                    vff        , cod(kpg)   , mult_comp)
        if (cod(kpg) .eq. 1) then
            goto 999
        endif
!
! ----- Cauchy stresses and rigidity matrix
!
        call nmgrtg(3    , nno   , poids, kpg   , vff      ,&
                    dfdi , def   , pff  , option, axi      ,&
                    r    , fm    , f    , dsidep, sigm_norm,&
                    sigma, matsym, matuu, vectu)
!
! ----- Stresses: convert PK2 to Cauchy
!
        if (option(1:4) .eq. 'RAPH' .or. option(1:4) .eq. 'FULL') then
            call lcdetf(3, f, detf)
            call pk2sig(3, f, detf, sigma, sigp(1, kpg),&
                        1)
        endif
    end do
!
999 continue
!
! - Return code summary
!
    call codere(cod, npg, codret)
!
end subroutine

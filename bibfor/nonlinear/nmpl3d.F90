subroutine nmpl3d(fami  , nno  , npg   , ipoids, ivf   ,&
                  idfde , geom , typmod, option, imate ,&
                  compor, mult_comp, lgpg , carcri  , instam, instap,&
                  deplm , deplp, angmas, sigm  , vim   ,&
                  matsym, dfdi , def   , sigp  , vip   ,&
                  matuu , vectu, codret)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/codere.h"
#include "asterfort/crirup.h"
#include "asterfort/lcegeo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
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
    character(len=*), intent(in) :: fami
    integer, intent(in) :: nno
    integer, intent(in) :: npg
    integer, intent(in) :: ipoids
    integer, intent(in) :: ivf
    integer, intent(in) :: idfde
    real(kind=8), intent(in) :: geom(3, nno)
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
! Options: RIGI_MECA_TANG, RAPH_MECA and FULL_MECA - Hypoelasticity (PETIT/PETIT_REAC)
!
! --------------------------------------------------------------------------------------------------
!
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS  : POIDS DES POINTS DE GAUSS
! IN  IVF     : VALEUR  DES FONCTIONS DE FORME
! IN  IDFDE   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DEPLP   : INCREMENT DE DEPLACEMENT
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  VARDEP  : VARIABLE DELOCALISEE EN T+
! IN  DELOCA  : VRAI SI VARIABLES DELOCALISEES PRESENTES
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: grand
    integer :: kpg, kk, i_node, i_dim, m, j, j1, kl, kkd, i_tens
    integer :: cod(27), ndim
    real(kind=8) :: dsidep(6, 6), f(3, 3), eps(6), deps(6), r, sigma(6), sigm_norm(6)
    real(kind=8) :: rbid(1), sig(6)
    real(kind=8) :: poids, tmp, rac2
    real(kind=8) :: elgeom(10, 27)
!
! --------------------------------------------------------------------------------------------------
!
    rac2  = sqrt(2.d0)
    grand = .false._1
    do kpg = 1, npg
        cod(kpg) = 0
    end do
!
! - Specific geometric parameters for some behaviours
!
    call lcegeo(nno  , npg   , ipoids, ivf, idfde,&
                geom , typmod, compor, 3  , dfdi,&
                deplm, deplp , elgeom)
!
! - Loop on Gauss points
!
    do kpg = 1, npg
        eps (1:6)=0.d0
        deps(1:6)=0.d0
!
! ----- Kinematic - Previous strains
!
        call nmgeom(3       , nno   , .false._1, grand, geom ,&
                    kpg     , ipoids, ivf      , idfde, deplm,&
                    .true._1, poids , dfdi     , f    , eps  ,&
                    r)
!
! ----- Kinematic - Increment of strains
!
        call nmgeom(3        , nno   , .false._1, grand, geom ,&
                    kpg      , ipoids, ivf      , idfde, deplp,&
                    .false._1, poids , dfdi     , f    , deps ,&
                    r)
!
! ----- Kinematic - Product [F].[B]
!
        do i_node = 1, nno
            do i_dim = 1, 3
                def(1,i_node,i_dim) = f(i_dim,1)*dfdi(i_node,1)
                def(2,i_node,i_dim) = f(i_dim,2)*dfdi(i_node,2)
                def(3,i_node,i_dim) = f(i_dim,3)*dfdi(i_node,3)
                def(4,i_node,i_dim) = (f(i_dim,1)*dfdi(i_node,2) +&
                                       f(i_dim,2)*dfdi(i_node,1))/ rac2
                def(5,i_node,i_dim) = (f(i_dim,1)*dfdi(i_node,3) +&
                                       f(i_dim,3)*dfdi(i_node,1))/ rac2
                def(6,i_node,i_dim) = (f(i_dim,2)*dfdi(i_node,3) +&
                                       f(i_dim,3)*dfdi(i_node,2))/ rac2
            end do
        end do
        do i_tens = 1, 3
            sigm_norm(i_tens) = sigm(i_tens,kpg)
        end do
        do i_tens = 4, 6
            sigm_norm(i_tens) = sigm(i_tens,kpg)*rac2
        end do
!
! ----- Compute behaviour
!
        call nmcomp(fami       , kpg        , 1        , 3     , typmod        ,&
                    imate      , compor     , carcri     , instam, instap        ,&
                    6          , eps        , deps     , 6     , sigm_norm     ,&
                    vim(1, kpg), option     , angmas   , 10    , elgeom(1, kpg),&
                    sigma      , vip(1, kpg), 36       , dsidep, 1             ,&
                    rbid       , cod(kpg)   , mult_comp)
        if (cod(kpg) .eq. 1) then
            goto 999
        endif
!
! ----- Rigidity matrix
!
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA') then
            if (matsym) then
                do i_node = 1, nno
                    do i_dim = 1, 3
                        kkd = (3*(i_node-1)+i_dim-1) * (3*(i_node-1)+i_dim) /2
                        do kl = 1, 6
                            sig(kl)=0.d0
                            sig(kl)=sig(kl)+def(1,i_node,i_dim)*dsidep(1,kl)
                            sig(kl)=sig(kl)+def(2,i_node,i_dim)*dsidep(2,kl)
                            sig(kl)=sig(kl)+def(3,i_node,i_dim)*dsidep(3,kl)
                            sig(kl)=sig(kl)+def(4,i_node,i_dim)*dsidep(4,kl)
                            sig(kl)=sig(kl)+def(5,i_node,i_dim)*dsidep(5,kl)
                            sig(kl)=sig(kl)+def(6,i_node,i_dim)*dsidep(6,kl)
                        end do
                        do j = 1, 3
                            do m = 1, i_node
                                if (m .eq. i_node) then
                                    j1 = i_dim
                                else
                                    j1 = 3
                                endif
                                tmp=0.d0
                                tmp=tmp+sig(1)*def(1,m,j)
                                tmp=tmp+sig(2)*def(2,m,j)
                                tmp=tmp+sig(3)*def(3,m,j)
                                tmp=tmp+sig(4)*def(4,m,j)
                                tmp=tmp+sig(5)*def(5,m,j)
                                tmp=tmp+sig(6)*def(6,m,j)
                                if (j .le. j1) then
                                    kk = kkd + 3*(m-1)+j
                                    matuu(kk) = matuu(kk) + tmp*poids
                                endif
                            end do
                        end do
                    end do
                end do
            else
                do i_node = 1, nno
                    do i_dim = 1, 3
                        do kl = 1, 6
                            sig(kl)=0.d0
                            sig(kl)=sig(kl)+def(1,i_node,i_dim)*dsidep(1,kl)
                            sig(kl)=sig(kl)+def(2,i_node,i_dim)*dsidep(2,kl)
                            sig(kl)=sig(kl)+def(3,i_node,i_dim)*dsidep(3,kl)
                            sig(kl)=sig(kl)+def(4,i_node,i_dim)*dsidep(4,kl)
                            sig(kl)=sig(kl)+def(5,i_node,i_dim)*dsidep(5,kl)
                            sig(kl)=sig(kl)+def(6,i_node,i_dim)*dsidep(6,kl)
                        end do
                        do j = 1, 3
                            do m = 1, nno
                                tmp=0.d0
                                tmp=tmp+sig(1)*def(1,m,j)
                                tmp=tmp+sig(2)*def(2,m,j)
                                tmp=tmp+sig(3)*def(3,m,j)
                                tmp=tmp+sig(4)*def(4,m,j)
                                tmp=tmp+sig(5)*def(5,m,j)
                                tmp=tmp+sig(6)*def(6,m,j)
                                kk = 3*nno*(3*(i_node-1)+i_dim-1) + 3*(m-1)+j
                                matuu(kk) = matuu(kk) + tmp*poids
                            end do
                        end do
                    end do
                end do
            endif
        endif
!
! ----- Cauchy stresses and internal forces
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
            do i_node = 1, nno
                vectu(1,i_node) = vectu(1,i_node)+ poids* (def(1,i_node,1)*sigma(1)+&
                                  def(4,i_node,1)*sigma(4)+ def(5,i_node,1)*sigma(5))
                vectu(2,i_node) = vectu(2,i_node)+ poids* (def(2,i_node,2)*sigma(2)+&
                                  def(4,i_node,2)*sigma(4)+ def(6,i_node,2)*sigma(6))
                vectu(3,i_node) = vectu(3,i_node)+ poids* (def(3,i_node,3)*sigma(3)+&
                                  def(5,i_node,3)*sigma(5)+ def(6,i_node,3)*sigma(6))
            end do
            do kl = 1, 3
                sigp(kl,kpg) = sigma(kl)
            end do
            do kl = 4, 6
                sigp(kl,kpg) = sigma(kl)/rac2
            end do
        endif
!
! ----- Cauchy stresses for IMPLEX
!
        if (option .eq. 'RIGI_MECA_IMPLEX') then
            do kl = 1, 3
                sigp(kl,kpg) = sigma(kl)
            end do
            do kl = 4, 6
                sigp(kl,kpg) = sigma(kl)/rac2
            end do
        endif
    end do
!
! - For POST_ITER='CRIT_RUPT'
!
    if (carcri(13) .gt. 0.d0) then
        ndim = 3
        call crirup(fami, imate, ndim, npg, lgpg,&
                    option, compor, sigp, vip, vim,&
                    instam, instap)
    endif
!
999 continue
!
! - Return code summary
!
    call codere(cod, npg, codret)
!
end subroutine

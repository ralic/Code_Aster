subroutine nmsspl( hexa,  shb6,   shb8, icoopg,&
                   fami,   nno,    npg, ipoids,    ivf,&
                  idfde,  geom, typmod, option,  imate,&
                 compor,  lgpg,   crit, instam, instap,&
                  deplm, deplp, angmas,   sigm,    vim,&
                 sigp,    vip,  matuu,  vectu, codret)
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
    implicit none
!
! Non-linear Solid-Shell Petit (small) Linear deformation
!
!
! Evaluation of tangent stiffness matrix and internal forces (B^T.sigma)
! for PETIT deformation model.
!
!
! IN  hexa     true if element is a hexahedral one
! IN  shb6     true if element is a SHB6
! IN  shb8     true if element is a SHB8
! IN  icoopg   pointer to Gauss point coordinates
! IN  fami     behaviour type (here, 'RIGI')
! IN  nno      number of nodes in element
! IN  npg      number of Gauss point in element
! IN  ipoids   element gauss point weight index
! IN  ivf      pointer to shape function values (currently un-used)
! IN  idfde    index of the element shape function derivatives in parametric space
! IN  geom     element node coordinates
! IN  typmod   model type (for SHB elements, here, set to C_PLAN)
! IN  option   element option to be evaluated
! IN  imate    pointer to material data
! IN  compor   deformation model: here, PETIT
! IN  lgpg     maximal number of internal variable for a given Gauss point
! IN  crit     local convergence criteria
! IN  instam   previous time
! IN  instap   current time
! IN  deplm    displacement at previous time
! IN  deplp    displacement at current time
! IN  angmas   3 angles for MASSIF keyword
! IN  sigm     stress at previous time
! IN  vim      internal variables at previous time
! OUT sigp     stress at current time
! OUT vip      internal variables at current time
! OUT matuu    tangent stiffness matrix
! OUT vectu    nodal forces B^T . sigma
! OUT codret   return code
!
#include "asterf_types.h"
#include "asterfort/r8inir.h"
!
#include "asterfort/bmatmc.h"
#include "asterfort/btdbpr.h"
#include "asterfort/codere.h"
#include "asterfort/hgfsca.h"
#include "asterfort/lcdetf.h"
#include "asterfort/mhomss.h"
#include "asterfort/mpsoqo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
#include "asterfort/pk2sig.h"
#include "asterfort/rcvalb.h"
#include "asterfort/ss6bgl.h"
#include "asterfort/ss6eps.h"
#include "asterfort/ss8hsm.h"
#include "asterfort/tmassf.h"
#include "asterfort/tpsivp_shb.h"
#include "asterfort/utbtab.h"
!
! aslint: disable=W1504
!
    aster_logical, intent(in) :: hexa
    aster_logical, intent(in) :: shb6
    aster_logical, intent(in) :: shb8
    integer, intent(in) ::  icoopg
    character(len=*), intent(in) :: fami
    integer, intent(in) :: nno
    integer, intent(in) :: npg
    integer, intent(in) :: ipoids
    integer, intent(in) :: ivf
    integer, intent(in) :: idfde
    real(kind=8), intent(in) :: geom(3,nno)
    character(len=8), intent(in) :: typmod(*)
    character(len=16), intent(in) :: option
    integer, intent(in) :: imate
    character(len=16), intent(in) :: compor(*)
    integer, intent(in) :: lgpg
    real(kind=8), intent(in) :: crit(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(in) :: deplm(1:3, 1:nno)
    real(kind=8), intent(in) :: deplp(1:3, 1:nno)
    real(kind=8), intent(in) :: angmas(*)
    real(kind=8), intent(in) :: sigm(18,npg)
    real(kind=8), intent(in) :: vim(lgpg,npg)
    real(kind=8), intent(out) :: sigp(18,npg)
    real(kind=8), intent(out) :: vip(lgpg,npg)
    real(kind=8), intent(out) :: matuu(*)
    real(kind=8), intent(out) :: vectu(3,nno)
    integer, intent(out) :: codret
!
    integer :: i, i_tens, ino, j, k, kpg, nbinco, nbv
    integer :: cod(20), icodre(2)
    aster_logical :: grand, resi, rigi
    character(len=8) :: nomres(2)
    real(kind=8) :: epsp3i, mu, nu, young
    real(kind=8) :: detfpl, nharm, poids
    real(kind=8) :: r, rac2, unsrac2, tmp
    real(kind=8) :: eps(6), epsi(6), deps(6), sigma(6), sigmag(6), sigm_norm(6)
    real(kind=8) :: rbid(1), para(2)
    real(kind=8) :: dfdi(1:nno, 1:3)
    real(kind=8) :: fglo(1:3, 1:nno)
    real(kind=8) :: bg(6,81)
    real(kind=8), dimension(3,3) :: f, fp, fpl, fplt, pgl, pglt, work
    real(kind=8), dimension(6,6) :: cmatgl, cmatlo, dsidep, pglqo, work66
    real(kind=8), dimension(81,81) :: btdb
!
! ......................................................................
!
! - Initializations
!
    parameter(rac2 = sqrt(2.d0))
    parameter(unsrac2 = 1.0d0 / sqrt(2.d0))
    parameter(grand = .false._1)
!
    parameter(nbv = 2)
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
    do kpg = 1, npg
       cod(kpg) = 0
    end do
!
    nbinco = nno*3
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
    if (rigi) then
       do 13 i = 1, nbinco
          do 14 j = 1, nbinco
             btdb(i,j) = 0.0d0
14        continue
13     continue
    endif
!
    if (resi) then
       call r8inir(3*nno, 0.d0, fglo, 1)
    endif
!
! - Loop over Gauss points
!
    do 65 kpg = 1, npg
       eps (1:6)=0.d0
       deps(1:6)=0.d0
!
!      Evaluate pgl(3,3) transformation matrix from global coordinate system
!      to local 'shell' coordinate system
       call tmassf(geom, icoopg, kpg, hexa, pgl)
!
! ----- Kinematic - Previous strains & increment of strains
!
       if (shb6) then
!
!         SHB6 \ Start
!
          call ss6eps(  geom,   pgl,   kpg,&
                      ipoids, idfde, poids, dfdi,&
                       deplm,   eps, deplp, deps)
!
!         Transformation matrix fpl at T+ in local coordinate system
!         With this method to evaluate fpl, we obtain a symmetric matrix
!         which is not the case for the other SHB elements or standard 3D ISO
!         elements.
!         Indeed, it is expressed as F = 1/2 * (gradU + gradU^T) + Id
!         For other SHB or 3D ISO elements, it is expressed as F = gradU + Id
!
!         Glut \ Start
          tmp     = deps(5)
          deps(5) = deps(6)
          deps(6) = tmp
!         Glut \ End
!
          fpl(1,1) = deps(1) + 1.d0
          fpl(1,2) = deps(4) * 2.0d0
          fpl(1,3) = deps(5) * 2.0d0
          fpl(2,1) = deps(4) * 2.0d0
          fpl(2,2) = deps(2) + 1.d0
          fpl(2,3) = deps(6) * 2.0d0
          fpl(3,1) = deps(5) * 2.0d0
          fpl(3,2) = deps(6) * 2.0d0
          fpl(3,3) = deps(3) + 1.d0
!
!         Glut \ Start
          tmp     = deps(5)
          deps(5) = deps(6)
          deps(6) = tmp
!         Glut \ End
!
!         SHB6 \ End
!
       else
!
!         SHB8, SHB15 & SHB20 \ Start
!
!         pglt needed by tpsivp_shb
          do 91 i = 1, 3
             do 81 j = 1, 3
                pglt(j,i) = pgl(i,j)
81           continue
91        continue
!
! ------- First call to nmgeom to evaluate eps (strain at T-) in global frame
          call nmgeom(3       , nno   , .false._1, grand, geom ,&
                      kpg     , ipoids, ivf      , idfde, deplm,&
                      .true._1, poids , dfdi     , f    , eps  ,&
                      r)
!         Glut \ 1/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         eps are required
          eps(4) = eps(4)*rac2
          eps(5) = eps(5)*rac2
          eps(6) = eps(6)*rac2
!         Glut \ 1/2 \ End
!
!         Expressing strain tensor at T- from global to local frame
          call tpsivp_shb(pglt, eps, .true._1)
!
!         Glut \ 2/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         eps are required
          tmp    = eps(5) 
          eps(5) = eps(6)
          eps(6) = tmp
!         Glut \ 2/2 \ End
!
! ------- Second call to nmgeom to evaluate deps (strain at T+) in global frame
          call nmgeom(3        , nno   , .false._1, grand, geom ,&
                      kpg      , ipoids, ivf      , idfde, deplp,&
                      .false._1, poids , dfdi     , f    , deps ,&
                      r)
!
!         Glut \ 1/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         deps are required
          deps(4) = deps(4)*rac2
          deps(5) = deps(5)*rac2
          deps(6) = deps(6)*rac2
!         Glut \ 1/2 \ End
!
!         Expressing strain tensor at T+ from global to local frame
          call tpsivp_shb(pglt, deps, .true._1)
!
!         Glut \ 2/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         deps are required
          tmp     = deps(5) 
          deps(5) = deps(6)
          deps(6) = tmp
!         Glut \ 2/2 \ End
!
! ------- Third call to nmgeom to evaluate fp (grad(U) at T+) in global frame
!         It is possible to have it with nmgeom through 'fp' if 'grand' is set
!         to 'true'
          call nmgeom(3        , nno   , .false._1, .true._1, geom ,&
                      kpg      , ipoids, ivf      , idfde   , deplp,&
                      .false._1, poids , dfdi     , fp      , epsi ,&
                      r)
!
!         Transforming fp from global to local frame
          call utbtab('ZERO', 3, 3, fp, pglt, work, fplt)
!
          do 201 i = 1, 3
             do 110 j = 1, 3
                fpl(i,j) = fplt(j,i)
110          continue
201       continue
!
!         SHB8, SHB15 & SHB20 \ End
!
       endif
!
!------------------------------------------------------
!
!      Because of 'C_PLAN' hypothesis in nmcomp, deps(3) will be modified
!      It is here stored before so as to be re-used according SHB approach
       epsp3i      = deps(3)
!
       eps(4)      = eps(4) *unsrac2
       deps(4)     = deps(4)*unsrac2
!
!      Getting stress from previous increment (local frame)
       do i_tens = 1, 3
          sigm_norm(i_tens) = sigm(i_tens,kpg)
       end do
       do i_tens = 4, 6
          sigm_norm(i_tens) = sigm(i_tens,kpg)*rac2
       end do
!
!      Glut / Start
!      To be noticed, in nmpl3d ('PETIT' for 3D ISO elements, all shear terms
!      are multiplied with rac(2)
!      Is restriction to 4th term really relevant?
       sigm_norm(5) = sigm_norm(5)*unsrac2
       sigm_norm(6) = sigm_norm(6)*unsrac2
!      Glut / End
!
! ---- Compute behaviour
!
!      'ndim' is set to 2 because of 'typmod' = C_PLAN hypothesis
!       To be noticed for 3D ISO elements:
!        - 'nwkin' is set to 10 instead of 1 here
       call nmcomp(fami       , kpg        , 1     , 2     , typmod   ,&
                   imate      , compor     , crit  , instam, instap   ,&
                   6          , eps        , deps  , 6     , sigm_norm,&
                   vim(1, kpg), option     , angmas, 1     , [0.d0]   ,&
                   sigma      , vip(1, kpg), 36    , dsidep, 1        ,&
                   rbid       , cod(kpg))
!
       if (cod(kpg) .eq. 1) then
          goto 999
       endif
!
!      Retrieving matrial data
!
       call rcvalb(fami,    kpg,    1,    '+',  imate,&
                    ' ', 'ELAS',    1, 'INST', [0.d0],&
                    nbv, nomres, para, icodre, 1)
!
!
!      sigma(3), sigma(5) & sigma(6) are re-evaluated considering
!      'elastic' deformation because of SHB formulation
!
       young = para(1)
       nu    = para(2)
       mu    = 0.5d0*young/(1.d0+nu)
!
       sigma(3) = sigm_norm(3) + young*epsp3i
       sigma(4) = sigma(4)*unsrac2
       sigma(5) = sigm_norm(5) + mu*deps(5)
       sigma(6) = sigm_norm(6) + mu*deps(6)
!
!      Evaluating det(fpl)
       call lcdetf(3, fpl, detfpl)
!
!      Glut \ Start
!      Adjusting and inverting shear stress components to comply with initial
!      SHB code
       sigma(4) = sigma(4)*rac2
       sigma(5) = sigma(5)*rac2
       sigma(6) = sigma(6)*rac2
       tmp      = sigma(5)
       sigma(5) = sigma(6)
       sigma(6) = tmp
!      Glut \ End
!
!      Transforming Piola-Kirchhoff II stress to Cauchy stress
!      Storing it as stress at end of increment (local frame)
       call pk2sig(3, fpl, detfpl, sigma, sigp(1,kpg), 1)
!
!      Glut \ Start
!      Sigma terms have to be adjusted so as to comply with initial SHB code.
!      When evaluated in the original routine, input stress vector was expanded
!      in tensor but lower diagonal stress components were set to 0.
!      Only upper diagonal terms were set to shear stress components.
!      In pk2sig, such an expansion is not used...
!      We thus remove now the additionnal terms
       sigp(1, kpg) = sigp(1, kpg) - (sigma(4)*fpl(1,2)*fpl(1,1)&
                                     +sigma(5)*fpl(1,3)*fpl(1,1)&
                                     +sigma(6)*fpl(1,3)*fpl(1,2)) * unsrac2 / detfpl
!
       sigp(2, kpg) = sigp(2, kpg) - (sigma(4)*fpl(2,2)*fpl(2,1)&
                                     +sigma(5)*fpl(2,3)*fpl(2,1)&
                                     +sigma(6)*fpl(2,3)*fpl(2,2)) * unsrac2 / detfpl
!
       sigp(3, kpg) = sigp(3, kpg) - (sigma(4)*fpl(3,2)*fpl(3,1)&
                                     +sigma(5)*fpl(3,3)*fpl(3,1)&
                                     +sigma(6)*fpl(3,3)*fpl(3,2)) * unsrac2 / detfpl
!
       sigp(4, kpg) = sigp(4, kpg) - (sigma(4)*fpl(1,2)*fpl(2,1)&
                                     +sigma(5)*fpl(1,3)*fpl(2,1)&
                                     +sigma(6)*fpl(1,3)*fpl(2,2)) * unsrac2 / detfpl
!
!      Glut \ Start
       tmp          = sigp(5, kpg)
       sigp(5, kpg) = sigp(6, kpg)
       sigp(6, kpg) = tmp
!
       sigp(5, kpg) = sigp(5, kpg) - (sigma(4)*fpl(2,2)*fpl(3,1)&
                                     +sigma(5)*fpl(2,3)*fpl(3,1)&
                                     +sigma(6)*fpl(2,3)*fpl(3,2)) * unsrac2 / detfpl
!
       sigp(6, kpg) = sigp(6, kpg) - (sigma(4)*fpl(1,2)*fpl(3,1)&
                                     +sigma(5)*fpl(1,3)*fpl(3,1)&
                                     +sigma(6)*fpl(1,3)*fpl(3,2)) * unsrac2 / detfpl
!
!       sigp(5, kpg) = sigp(5, kpg) - (sigma(4)*fpl(1,2)*fpl(3,1)&
!                                     +sigma(6)*fpl(1,3)*fpl(3,1)&
!                                     +sigma(5)*fpl(1,3)*fpl(3,2)) * unsrac2 / detfpl
!
!       sigp(6, kpg) = sigp(6, kpg) - (sigma(4)*fpl(2,2)*fpl(3,1)&
!                                     +sigma(6)*fpl(2,3)*fpl(3,1)&
!                                     +sigma(5)*fpl(2,3)*fpl(3,2)) * unsrac2 / detfpl
!      Glut \ End
!
!
!      Tangent stiffness matrix & internal forces B^T.sigma evaluations
!
!
! ---- Compute matrix [B]: displacement -> strain (first order)
!
       if (shb6) then
!
!         [B] SHB6 \ Start
!         Evaluation of SHB6 B matrix in global coordinates
!         This code being re-used for linear (RIGI_MECA) &
!         non-linear behaviors (PETIT, GROT_GDEP),
!         it is encapsulated in a dedicated routine
!
          call ss6bgl(.false._1, kpg, geom, ipoids, idfde, icoopg, pgl, poids, bg)
!
!         [B] SHB6 \ End
!
       else
!
!         [B] SHB8, SHB15 & SHB20 \ Start
!
          call bmatmc(kpg, 6, geom, ipoids, ivf, idfde, nno, nharm, poids, bg)
!
!         [B] SHB8, SHB15 & SHB20 \ End
!
       endif
!
! ---- Stiffness matrix
!
       if (rigi) then
!

!         Modified Hooke matrix
          call mhomss(young, nu, cmatlo, dsidep)
!
!         pgl(3,3) transformation matrix modified into fourth order transformation matrix pglqo(6,6)
          call mpsoqo(pgl, pglqo)
!
!         Transforming modified Hooke matrix in global coordinate system
!
          call utbtab('ZERO', 6, 6, cmatlo, pglqo, work66, cmatgl)
!
!         Evaluate elementary stiffness matrix
!
          call btdbpr(bg, cmatgl, poids, 6, nbinco, btdb)
!
       endif
!
! ---- Internal forces B^T.sigma
!
       if (resi) then
!
! Glut / Start
          if (.not. shb6) then
             do 132 ino = 1, 3*nno
                tmp = bg(5,ino)
                bg(5,ino) = bg(6,ino)
                bg(6,ino) = tmp
132          continue
          endif
! Glut / End
!
!         Expressing sigma in global coordinates
          do 481 i = 1, 6
             sigmag(i) = sigp(i, kpg)
481       continue
!
!         Transforming stress from local to global coordinate system
!         Glut \ 1/2 \ Start
          tmp       = sigmag(5) 
          sigmag(5) = sigmag(6)
          sigmag(6) = tmp
!         Glut \ 1/2 \ End
!         Expressing stress tensor at T- from global to local frame
          call tpsivp_shb(pgl, sigmag, .false._1)
!         Glut \ 2/2 \ Start
          tmp       = sigmag(5) 
          sigmag(5) = sigmag(6)
          sigmag(6) = tmp
!         Glut \ 2/2 \ End
!
!         Internal forces are not evaluated the same way than for 3D ISO elements
          do 141 j = 1, nno
             k = 3*(j-1) + 1
             do 133 i = 1, 6
                fglo(1,j)=fglo(1,j)+bg(i,k  )*sigmag(i)*poids
                fglo(2,j)=fglo(2,j)+bg(i,k+1)*sigmag(i)*poids
                fglo(3,j)=fglo(3,j)+bg(i,k+2)*sigmag(i)*poids
133          continue
141       continue
!
       endif
!
65  continue
!  ===============================================================
!      End over Gauss point loop
!  ===============================================================
!
    if (rigi) then
!      Storing stiffness matrix for SHB elements
! ---- Symmetric matrix
       k = 0
       do 172 i = 1, 3*nno
          do 162 j = 1, i
             k = k + 1
             matuu(k) = btdb(i,j)
162       continue
172    continue
    endif
!
    if (resi) then
!      Storing internal forces for SHB elements
       do 911 i = 1, 3
          do 901 j = 1, nno
             vectu(i,j) = fglo(i,j)
901       continue
911    continue
    endif
!
    if (shb8) then
!      SHB8 stabilization
       if (rigi) then
!         Adding stabilization term to stiffness matrix for SHB8 element
          call ss8hsm(geom, para, matuu)
       endif
!
       if (resi) then
!         Dealing with stabilization force for SHB8 element
!         zr(icontp+6): Stabilization stresses at T+ stored as an output
!         One evaluates stabilization forces for SHB8 element in a
!         dedicated routine
!         sigm is 18 x npg (and not 6 x npg) because 12 last components
!         are used for SHB8 stabilization matrix (but only 12 additionnal
!         components of 1st Gauss point are used)
          call hgfsca(geom, para, deplp, sigm(7,1), vectu, sigp(7,1))
       endif
    endif
!
999 continue
!
! - Return code summary
!
    call codere(cod, npg, codret)
!
end subroutine

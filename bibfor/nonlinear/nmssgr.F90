subroutine nmssgr(  hexa,   shb6,   shb8, icoopg,&
                    fami,    nno,    npg, ipoids,    ivf,&
                   idfde,   geom, typmod, option,  imate,&
                  compor,   lgpg,   crit, instam, instap,&
                   deplm,  deplp, angmas,   sigm,    vim,&
                  matsym,   sigp,    vip,  matuu,  vectu,&
                  codret)
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
! Non-linear M Solid-Shell Grande Rotation
!
!
! Evaluation of tangent stiffness matrix and internal forces (B^T.sigma)
! for GROT_GDEP deformation model.
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
! IN  compor   deformation model: here, GROT_GDEP
! IN  lgpg     maximal number of internal variable for a given Gauss point
! IN  crit     local convergence criteria
! IN  instam   previous time
! IN  instap   current time
! IN  deplm    displacement at previous time
! IN  deplp    displacement at current time
! IN  angmas   3 angles for MASSIF keyword
! IN  sigm     stress at previous time
! IN  vim      internal variables at previous time
! IN  matsym   true if tangent stiffness matrix is symmetric
! OUT sigp     stress at current time
! OUT vip      internal variables at current time
! OUT matuu    tangent stiffness matrix
! OUT vectu    nodal forces B^T . sigma
! OUT codret   return code
!
#include "asterf_types.h"
#include "asterfort/r8inir.h"
!
#include "asterfort/codere.h"
#include "asterfort/hgfsca.h"
#include "asterfort/lcdetf.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
#include "asterfort/nmsstg.h"
#include "asterfort/nmssfi.h"
#include "asterfort/pk2sig.h"
#include "asterfort/rcvalb.h"
#include "asterfort/ss6eps.h"
#include "asterfort/ss8hsm.h"
#include "asterfort/tmassf.h"
#include "asterfort/tpsivp_shb.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
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
    aster_logical, intent(in) :: matsym
    real(kind=8), intent(out) :: sigp(18,npg)
    real(kind=8), intent(out) :: vip(lgpg,npg)
    real(kind=8), intent(out) :: matuu(*)
    real(kind=8), intent(out) :: vectu(3,nno)
    integer, intent(out) :: codret
!
    integer :: i, i_tens, j, kpg, ndim, nbv
    integer :: cod(20), icodre(2)
    aster_logical :: grand, resi, rigi
    character(len=8) :: nomres(2)
    real(kind=8) :: epsp3i, mu, nu, young, maxeps
    real(kind=8) :: detfpl, poids
    real(kind=8) :: r, rac2, unsrac2, tmp
    real(kind=8) :: dfdi(1:nno, 1:3)
    real(kind=8) :: eps(6), epsm(6), epsp(6), sigma(6), sigm_norm(6)
    real(kind=8) :: rbid(1), para(2)
    real(kind=8) :: geom_up(3,nno)
    real(kind=8), dimension(3,3) :: f, fp, fpl, fplt, pgl, pglt, pgl_up, work
    real(kind=8), dimension(6,6) :: dsidep
!
! ......................................................................
!
! - Initializations
!
    parameter (rac2 = sqrt(2.d0))
    parameter (unsrac2 = 1.0d0 / sqrt(2.d0))
    parameter (ndim = 3)
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nbv = 2
!
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
!
    do kpg = 1, npg
       cod(kpg) = 0
    end do
!
!   Geometrical update (only used for B^T . sigma evaluation)
!   geom_up = geometry at start of current iteration + zr(ideplp)
!   zr(ideplp) is the cumulative displacement during the Newton iterations since T-
    if (resi) then
       do 360 i = 1, nno
          do 362 j = 1, 3
             geom_up(j,i) = geom(j,i) + deplp(j,i)
362       continue
360     continue
    endif
!
! - Loop over Gauss points
!
    do 65 kpg = 1, npg
        epsm(1:6)=0.d0
        epsp(1:6)=0.d0
!
!      Evaluate pgl(3,3) transformation matrix from global coordinate system
!      to local 'shell' coordinate system
       call tmassf(geom, icoopg, kpg, hexa, pgl)
       if (resi) then
          call tmassf(geom_up, icoopg, kpg, hexa, pgl_up)
       endif
!
! ----- Kinematic - Previous strains & increment of strains
!
       if (shb6) then
!
!         SHB6 \ Start
!
!         epsm & epsp are expressed in local coordinate system
          call ss6eps(  geom,   pgl,   kpg,&
                      ipoids, idfde, poids, dfdi,&
                       deplm,  epsm, deplp, epsp)
!
!         Transformation matrix fpl at T+ in local coordinate system
!         With this method to evaluate fpl, we obtain a symmetric matrix
!         which is not the case for the other SHB elements or standard 3D ISO
!         elements
!         Indeed, it is expressed as F = 1/2 * (gradU + gradU^T) + Id
!         For other SHB or ISO elements, it is expressed as F = gradU +Id
!
!         Glut \ Start
          tmp     = epsp(5)
          epsp(5) = epsp(6)
          epsp(6) = tmp
!         Glut \ End
!
          fpl(1,1) = epsp(1) + 1.d0
          fpl(1,2) = epsp(4) * 2.0d0
          fpl(1,3) = epsp(5) * 2.0d0
          fpl(2,1) = epsp(4) * 2.0d0
          fpl(2,2) = epsp(2) + 1.d0
          fpl(2,3) = epsp(6) * 2.0d0
          fpl(3,1) = epsp(5) * 2.0d0
          fpl(3,2) = epsp(6) * 2.0d0
          fpl(3,3) = epsp(3) + 1.d0
!
!         Glut \ Start
          tmp     = epsp(5)
          epsp(5) = epsp(6)
          epsp(6) = tmp
!         Glut \ End
!
!         SHB6 \ End
!
       else
!
!         SHB8, SHB15 & SHB20 \ Start
!
!         pglt needed by tpsivp_shb to express epsm & epsp in local frame
          do 91 i = 1, 3
             do 81 j = 1, 3
                pglt(j,i) = pgl(i,j)
81           continue
91        continue
!
! ------- First call to nmgeom to evaluate epsm (strain at T-) in global frame
!         To have equivalent code to original SHB code, grand is set to false
!         To be noticed, in 3D ISO element, it is set to true.
          grand = .false._1
          call nmgeom(       3,    nno, .false._1, grand,  geom,&
                           kpg, ipoids,       ivf, idfde, deplm,&
                      .true._1,  poids,      dfdi,     f,  epsm,&
                              r)
!
!         Glut \ 1/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         epsm are required
          epsm(4) = epsm(4)*rac2
          epsm(5) = epsm(5)*rac2
          epsm(6) = epsm(6)*rac2
!         Glut \ 1/2 \ End
!
!         Expressing strain tensor at T- from global to local frame
          call tpsivp_shb(pglt, epsm, .true._1)
!
!         Glut \ 2/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         epsm are required
          tmp     = epsm(5) 
          epsm(5) = epsm(6)
          epsm(6) = tmp
!         Glut \ 2/2 \ End
!
! ------- Second call to nmgeom to evaluate epsp (strain at T+) in global frame
!         To have equivalent code to original SHB code, 'grand' is set to false
!         To be noticed, in 3D ISO element, it is set to true.
          grand = .false._1
          call nmgeom(        3,    nno, .false._1, grand,  geom,&
                            kpg, ipoids,       ivf, idfde, deplp,&
                      .false._1,  poids,      dfdi,     f,  epsp,&
                              r)
!         Glut \ 1/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         epsp are required
          epsp(4) = epsp(4)*rac2
          epsp(5) = epsp(5)*rac2
          epsp(6) = epsp(6)*rac2
!         Glut \ 1/2 \ End
!
!         Expressing strain tensor at T+ from global to local frame
          call tpsivp_shb(pglt, epsp, .true._1)
!
!         Glut \ 2/2 \ Start
!         To comply with the initial SHB code, the following modifications to
!         epsp are required
          tmp     = epsp(5) 
          epsp(5) = epsp(6)
          epsp(6) = tmp
!         Glut \ 2/2 \ End
!
! ------- Third call to nmgeom to evaluate fp (grad(U) at T+) in global frame
!         It is possible to have it with nmgeom through 'fp' if 'grand' is set
!         to true
          grand = .true._1
          call nmgeom(        3,    nno, .false._1, grand,  geom,&
                            kpg, ipoids,       ivf, idfde, deplp,&
                      .false._1,  poids,      dfdi,    fp,   eps,&
                              r)
!
!         Expressing fp matrix from global to local frame
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
! ---- Check "small strains"
!
       maxeps = 0.d0
       do j = 1, 6
!         To be noticed, at this step, for 3D ISO elements, deps variable
!         is evaluated to be re-used in nmcomp
!         This is not the case for SHB elements
!         Glut / Start
!         deps (j)=epsp(j)-epsm(j)
          maxeps=max(maxeps,abs(epsp(j)))
!         Glut / End
       end do
!
       if (maxeps .gt. 0.05d0) then
          if (compor(1)(1:4) .ne. 'ELAS') then
             call utmess('A', 'COMPOR2_9', sr=maxeps)
          endif
       endif
!
!------------------------------------------------------
!
!      Because of 'C_PLAN' hypothesis in nmcomp, epsp(3) will be modified
!      It is here stored before so as to be re-used according SHB approach
       epsp3i = epsp(3)
!
       epsm(4)  = epsm(4)*unsrac2
       epsp(4)  = epsp(4)*unsrac2
!

!      Getting stress from previous increment (local frame)
       do i_tens = 1, 3
          sigm_norm(i_tens) = sigm(i_tens,kpg)
       end do
       do i_tens = 4, 6
          sigm_norm(i_tens) = sigm(i_tens,kpg)*rac2
       end do

!      Glut / Start
!      In original SHB code, only the 4th term is multiplied with rac2
!      Is this restriction to 4th term really relevant?
       sigm_norm(5) = sigm_norm(5)*unsrac2
       sigm_norm(6) = sigm_norm(6)*unsrac2
!      Glut / End
!
!      'ndim' is set to 2 because of 'typmod' = C_PLAN hypothesis 
!      To be noticed for 3D ISO elements:
!        - deps is used instead of epsp with deps = epsp - epsm
!        - 'nwkin' is set to 10 instead of 1 here
       call nmcomp(      fami,        kpg,      1,      2,    typmod,&
                        imate,     compor,   crit, instam,    instap,&
                            6,       epsm,   epsp,      6, sigm_norm,&
                   vim(1,kpg),     option, angmas,      1,    [0.d0],&
                        sigma, vip(1,kpg),     36, dsidep,         1,&
                         rbid,   cod(kpg))
!
       if (cod(kpg) .eq. 1) then
          goto 999
       endif
!
!      One re-initializes sigm_norm(4) because re-used in nmsstg & nmssfi
       sigm_norm(4) = sigm_norm(4)*unsrac2
!
!      Retrieving matrial data
!
       call rcvalb(fami,    kpg,    1,    '+',  imate,&
                    ' ', 'ELAS',    1, 'INST', [0.d0],&
                    nbv, nomres, para, icodre, 1)
!
!
!      sig(3), sig(5) & sig(6) are re-evaluated considering
!      'elastic' deformation because of SHB formulation
!
       young = para(1)
       nu    = para(2)
       mu    = 0.5d0*young/(1.d0+nu)
!
       sigma(3) = sigm_norm(3) + young*epsp3i
       sigma(4) = sigma(4)*unsrac2
       sigma(5) = sigm_norm(5) + mu*epsp(5)
       sigma(6) = sigm_norm(6) + mu*epsp(6)
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
! ---- Tangent stiffness matrix
!
       if (rigi) then
!
          call nmsstg(       shb6,      geom, idfde, ipoids, icoopg, pgl, para,&
                             ndim,       nno, poids,    kpg,&
                             dfdi,    option,&
                           dsidep, sigm_norm,&
                      sigp(1,kpg),    matsym, matuu)
!
       endif
!
! ---- Internal forces B^T.sigma
!
       if (resi) then
!
          call nmssfi(       shb6, geom_up, idfde, ipoids, icoopg, pgl_up,&
                             ndim,     nno,   kpg,&
                             rigi,&
                      sigp(1,kpg),  matsym, vectu)
!
         endif
!
65  continue
!  ===============================================================
!      End over Gauss point loop
!  ===============================================================
!
    if (shb8) then
!      SHB8 stabilization
!
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
          call hgfsca(geom_up, para, deplp, sigm(7,1), vectu, sigp(7,1))
       endif
!
    endif
!
999 continue
!
! - Return code summary
!
    call codere(cod, npg, codret)
!
end subroutine

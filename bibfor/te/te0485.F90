subroutine te0485(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
! 'SIEF_ELGA' option for SHB elements
!
!
! 'SIEF_ELGA' option for solid-shell elements SHB6, SHB8, SHB15 & SHB20.
! Computation of stress at Gauss points
!
!
! IN  option   'SIEF_ELGA'
! IN  nomte    elment type name
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/r8inir.h"
#include "blas/dsymv.h"
!
#include "asterfort/dfdmshb.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mhomss.h"
#include "asterfort/nmgeog.h"
#include "asterfort/rcvalb.h"
#include "asterfort/sshini.h"
#include "asterfort/ss6eps.h"
#include "asterfort/tmassf.h"
#include "asterfort/tpsivp_shb.h"
!
    character(len=16) :: option
    character(len=16) :: nomte
!
    aster_logical :: hexa, grand, shb6, shb8
    integer :: i, icont, icoopg, idepl, idfde, igeom, imate, ipoids, ivf
    integer :: j
    integer :: kpg
    integer :: nbv, ndim, nno, nnos, npg
    integer :: icodre(2)
    character(len=8) :: nomres(2)
    real(kind=8) :: para(2)
    real(kind=8) :: poids
    real(kind=8) :: r, rac2, tmp
    real(kind=8) :: dfdi(60)
    real(kind=8), dimension(6) :: eps, sigloc
    real(kind=8), dimension(3,3) :: f, pgl, pglt, invjac
    real(kind=8), dimension(6,6) :: cmatlo
!
!-----------------------------------------------------------------------
!
    parameter(ndim = 3)
    parameter(rac2 = sqrt(2.d0))
    parameter(grand = .false._1)
!
    parameter(nbv = 2)
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
! - Finite element informations
!
    call elrefe_info(fami='RIGI', nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfde)
!
! - Geometry
!
    call jevech('PGEOMER', 'L', igeom)
!
! - Material parameters
!
    call jevech('PMATERC', 'L', imate)
!
!   Initialization of specific SHB variables
!
    call sshini(nno, nnos, hexa, shb6, shb8)
!
! - Displacement field
!
    call jevech('PDEPLAR', 'L', idepl)
!
! - Output stress fied
    call jevech('PCONTRR', 'E', icont)
!
!
! - Loop over Gauss points \ Start
!  
    do 450 kpg = 1, npg
!
!      Retrieving matrial data
!
       call rcvalb('RIGI', kpg, 1, '+', zi(imate),&
                   ' ', 'ELAS', 1, 'INST', [0.d0],&
                   nbv, nomres, para, icodre, 1)
!
!      Assemble modified Hooke matrix
!
       call mhomss(para(1), para(2), cmatlo)
!
!      Evaluate pgl(3,3) transformation matrix from global coordinate system
!      to local 'shell' coordinate system
!
       call tmassf(zr(igeom), icoopg, kpg, hexa, pgl)
!
       if (shb6) then
!
!         [eps] SHB6 \ Start
!         Evaluation of strain from displacement field for SHB6 element
!         in local SHB6 coordinate system
!
          call ss6eps(zr(igeom),       pgl, kpg, ipoids, idfde, poids,&
                           dfdi, zr(idepl), eps)
!
!         [eps] SHB6 \ End
!
       else
!
!         [eps] SHB8, SHB15 & SHB20 \ Start
!
          call dfdmshb(    nno,         kpg,        ipoids,&
                        idfde,   zr(igeom),        invjac, poids,&
                      dfdi(1), dfdi(nno+1), dfdi(2*nno+1))
!
!         Evaluation of strain from displacement field for SHB8, SHB15 & SHB20
!         in global coordinate system
          call nmgeog(     ndim,   nno, .false._1,     grand,&
                      zr(igeom),   kpg,       ivf, zr(idepl),&
                      .false._1, poids,      dfdi,         f,&
                            eps,     r)
!
!         To comply with the initial SHB code, the following modifications to
!         eps are required
          eps(4) = eps(4)*rac2
          eps(5) = eps(5)*rac2
          eps(6) = eps(6)*rac2
!
          do 91 i = 1, 3
             do 81 j = 1, 3
                pglt(j,i) = pgl(i,j)
81           continue
91        continue
!
!         Expressing strain tensor from global to local frame
          call tpsivp_shb(pglt, eps, .true._1)
!
!         Glut \ Start
!         To comply with the initial SHB code, the following modifications to
!         eps are required
          tmp    = eps(5) 
          eps(5) = eps(6)
          eps(6) = tmp
!         Glut \ End
!
!         [eps] SHB8, SHB15 & SHB20 \ End
!
       endif
!
!      Evaluating mechanical stress in local coordinate system
       call dsymv('U', 6, 1.0d0, cmatlo, 6, eps, 1, 0.d0, sigloc, 1)
!
!      Storing stress vector at Gauss point in local coordinate system
!      (for re-use in case of simulation with non-linear plastic material)
       do 440 i = 1, 6
          zr(icont+18*(kpg-1)+i-1)=sigloc(i)
440    continue
!
450 continue
!
! - Loop over Gauss points \ End
!
end subroutine

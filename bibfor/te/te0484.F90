subroutine te0484(option, nomte)
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
! 'FORC_NODA' option for SHB elements
!
!
! 'FORC_NODA' option for solid-shell elements SHB6, SHB8, SHB15 & SHB20.
! Computation of 3D elementary force vector.
!
!
! IN  option   'FORC_NODA'
! IN  nomte    elment type name
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/moytem.h"
#include "asterfort/rcvalb.h"
#include "asterfort/ss6bgl.h"
#include "asterfort/tecach.h"
#include "asterfort/dfdmshb.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/tpsivp_shb.h"
#include "asterfort/hgfsca.h"
#include "asterfort/bmatmc.h"
#include "asterfort/btsig.h"
#include "asterfort/sshini.h"
#include "asterfort/tmassf.h"
!
    character(len=16) :: option
    character(len=16) :: nomte
!
    aster_logical :: hexa, shb6, shb8
    integer :: i, icompo, icontm, icoopg, ideplm, idfde, igeom, imate
    integer :: ipoids, iret, ivectu, ivf, j, jgano, kpg
    integer :: nbinco, nbsig, nbv, ndim, nno, nnos, npg
    integer :: icodre(2)
    character(len=4) :: fami
    character(len=8) :: nomres(2)
    real(kind=8) :: poids, nharm, tmp
    real(kind=8) :: para(2), sigmag(6), xidepm(24)
    real(kind=8) :: pgl(3,3), fstp(3,4), bg(6,81)
!
! ......................................................................
!
    parameter(fami='RIGI')
    parameter(nbsig=6)
    parameter(ndim=3)
!
! - Finite element informations
!
    call elrefe_info(fami=fami, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids,jcoopg=icoopg,jvf=ivf,jdfde=idfde,&
                     jgano=jgano)
!
!   Initialization of specific SHB variables
!
    call sshini(nno, nnos, hexa, shb6, shb8)
!
! - Geometry
!
    call jevech('PGEOMER', 'L', igeom)
!
! - Material parameters
!
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nbv = 2
!
! - Initializations
!
    nbinco = ndim*nno
!
!   Input parameters
!
! - Stress at Gauss points
    call jevech('PCONTMR', 'L', icontm)
!
!   Displacement at last converged increment
    call jevech('PDEPLMR', 'L', ideplm)
!
    call tecach('ONO', 'PCOMPOR', 'L', iret, iad=icompo)
    if (icompo .ne. 0) then
       call jevech('PCOMPOR', 'L', icompo)
    endif
!
! - Geometrical update
!      Initial geometry + displacement at last converged increment
!
    if ((zk16(icompo+2).eq.'GROT_GDEP')) then
       do 150 i = 1, ndim*nno
          zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1)
150    continue
    endif
!
!
!   Output parameter
!
! - Internal force vector (B^T.sigma) & stresses for stabilization
    call jevech('PVECTUR', 'E', ivectu)
!
!
! - Loop over Gauss points \ Start
!
    do 65 kpg = 1, npg
!
!      Retrieving matrial data
!
       call rcvalb('RIGI', kpg, 1, '+', zi(imate),&
                   ' ', 'ELAS', 1, 'INST', [0.d0],&
                   nbv, nomres, para, icodre, 1)
!
!      Evaluate pgl(3,3) transformation matrix from global coordinate system
!      to local 'shell' coordinate system
       call tmassf(zr(igeom), icoopg, kpg, hexa, pgl)
!
!      Internal forces B^T.sigma
!
       if (shb6) then
!
!         [B] SHB6 \ Start
!         Evaluation of SHB6 B matrix in global coordinates
!
          call ss6bgl(.true._1, kpg, zr(igeom), ipoids, idfde, icoopg, pgl, poids, bg)
!
!         [B] SHB6 \ End
!
       else
!
!         [B] SHB8, SHB15 & SHB20 \ Start
!         ([B] SHB8 will be 'completed' later with a stabilization matrix)
!
          call bmatmc(kpg, nbsig, zr(igeom), ipoids, ivf,&
                      idfde, nno, nharm, poids, bg)
!
! Glut / Start
          do 132 j = 1, ndim*nno
             tmp = bg(5,j)
             bg(5,j) = bg(6,j)
             bg(6,j) = tmp
132       continue
! Glut / End
!
!         [B] SHB8, SHB15 & SHB20 \ End
!
       endif
!
!      Expressing sigma in global coordinates
       do 481 i = 1, 6
          sigmag(i)=zr(icontm+18*(kpg-1)+i-1)
481    continue
!
!      Transforming stress from local to global coordinate system
!      Glut \ 1/2 \ Start
       tmp       = sigmag(5) 
       sigmag(5) = sigmag(6)
       sigmag(6) = tmp
!      Glut \ 1/2 \ End
!      Expressing stress tensor at T- from global to local frame
       call tpsivp_shb(pgl, sigmag, .false._1)
!      Glut \ 2/2 \ Start
       tmp       = sigmag(5) 
       sigmag(5) = sigmag(6)
       sigmag(6) = tmp
!      Glut \ 2/2 \ End
!
!      Evaluating B^T.sigma product
       call btsig(nbinco, nbsig, poids, bg, sigmag, zr(ivectu))
!
65  continue
!
! - Loop over Gauss points \ End
!
!
! - SHB8 hourglass forces
!
    if (shb8) then
!
       call r8inir(24, 0.d0, xidepm, 1)
!
       if ((zk16(icompo+2).eq.'GROT_GDEP')) then
          do 151 i = 1, 24
             xidepm(i) = zr(ideplm+i-1)
151       continue
       else if ((zk16(icompo+2) (1:5).eq.'PETIT')) then
          call r8inir(24, 0.d0, xidepm, 1)
       else
          do 153 i = 1, 24
             xidepm(i) = zr(ideplm+i-1)
153       continue
       endif
!
       call hgfsca(zr(igeom), para, xidepm, zr(icontm+6), zr(ivectu), fstp)
!
    endif
!
end subroutine

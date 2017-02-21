subroutine te0473(option, nomte)
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
! 'RIGI_MECA' option for SHB elements
!
!
! 'RIGI_MECA' option for solid-shell elements SHB6, SHB8, SHB15 & SHB20.
! Computation of 3D elementary matrix.
!
!
! IN  option   'RIGI_MECA'
! IN  nomte    elment type name
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
!
#include "asterfort/bmatmc.h"
#include "asterfort/btdbpr.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/mhomss.h"
#include "asterfort/mpsoqo.h"
#include "asterfort/rcvalb.h"
#include "asterfort/sshini.h"
#include "asterfort/ss6bgl.h"
#include "asterfort/ss8hsm.h"
#include "asterfort/tmassf.h"
#include "asterfort/utbtab.h"
!
    character(len=16) :: option
    character(len=16) :: nomte
!
    aster_logical :: hexa, shb6, shb8
    integer :: i, icoopg, idfde, igeom, imate, imatuu, ipoids, ivf
    integer :: j
    integer :: k, kpg
    integer :: nbinco, nbv, ndim, nno, nnos, npg
    integer :: icodre(2)
    character(len=8) :: nomres(2)
    real(kind=8) :: para(2)
    real(kind=8) :: jac, nharm
    real(kind=8), dimension(3,3) :: pgl
    real(kind=8), dimension(6,6) :: pglqo, work66, cmatlo, cmatgl
    real(kind=8), dimension(6,81) :: bg
    real(kind=8), dimension(81,81) :: btdb
!
! ......................................................................
!
    parameter(ndim = 3)
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
! - Initializations
!
    nbinco = nno*ndim
!
!   Initialization of btdb which is a (81,81) matrix not fully used.
!   At most (60,60) is used for SHB20. matini or r8inir routines are thus not used.
!   To save time, its initialization is managed in a dedicated loop.
    do 10 i = 1, nbinco
       do 11 j = 1, nbinco
          btdb(i,j) = 0.0d0
11     continue
10  continue
!
!   Initialization of specific SHB variables
!
    call sshini(nno, nnos, hexa, shb6, shb8)
!
!
! - Loop over Gauss points \ Start
!  
    do 100 kpg = 1, npg
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
! ---- Compute matrix [B]: displacement -> strain (first order)
!
       if (shb6) then
!
!         [B] SHB6 \ Start
!         Evaluation of SHB6 B matrix in global coordinates
!         This code being re-used for non-linear behaviors (PETIT, GROT_GDEP),
!         it is encapsulated in a dedicated routine
!
          call ss6bgl(.false._1, kpg, zr(igeom), ipoids, idfde, icoopg, pgl, jac, bg)
!
!         [B] SHB6 \ End
!
       else
!
!         [B] SHB8, SHB15 & SHB20 \ Start
!         ([B] SHB8 will be 'completed' later with a stabilization matrix)
!
          call bmatmc(kpg, 6, zr(igeom), ipoids, ivf,&
                      idfde, nno, nharm, jac, bg)
!
!         [B] SHB8, SHB15 & SHB20 \ End
!
       endif
!
!      pgl(3,3) transformation matrix modified into fourth order transformation matrix pglqo(6,6)
!
       call mpsoqo(pgl, pglqo)
!
!      Transforming modified Hooke matrix in global coordinate system
!
       call utbtab('ZERO', 6, 6, cmatlo, pglqo, work66, cmatgl)
!
!      Evaluate elementary stiffness matrix
!
       call btdbpr(bg, cmatgl, jac, 6, nbinco, btdb)
!
100 continue
!
! - Loop over Gauss points \ End
!
!   Storing elementary stiffness matrix (half of this symmetric matrix is stored)
!
    call jevech('PMATUUR', 'E', imatuu)
    k = 0
    do 300 i = 1, nbinco
       do 301 j = 1, i
          k = k + 1
          zr(imatuu+k-1) = btdb(i,j)
301    continue
300 continue
!
! - SHB8 stabilization matrix
!   Matrix evaluated at the center of the element in a corotional frame
!
    if (shb8) then
!
       call ss8hsm(zr(igeom), para, zr(imatuu))
!
    endif
!
end subroutine


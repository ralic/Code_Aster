subroutine te0427(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/voiuti.h"
!
    character(len=16) :: option, nomte
!.......................................................................
!          MODELISATION : VF1
!          OPTION       : 'RIGI_MECA' OU 'CHAR_MECA_PESA_R'
!.......................................................................
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
    character(len=16) :: codvoi
    integer :: nvoima, nscoma, nbvois
    parameter(nvoima=100,nscoma=4)
    integer :: livois(1:nvoima), tyvois(1:nvoima), nbnovo(1:nvoima)
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2)
!
    character(len=4) :: fami
    integer :: ndim, nno, nnos, npg1, ipoids, ivf, idfde, jgano
    integer :: igeom, imate, imatu, iadzi, iazk24, numa, igeom2
    integer :: kvois, numav, nnov, k1, k2
    integer :: ico2, imatu2, iret, ivectu
!
!
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
!
!     -- OPTION CHAR_MECA_PESA_R (BIDON):
!     -----------------------------------
    if (option .eq. 'CHAR_MECA_PESA_R') then
        call jevech('PVECTUR', 'E', ivectu)
        do 21, k1=1,nno
        zr(ivectu-1+k1)=13.d0
21      continue
        goto 9999
    endif
!
!
!     -- OPTION RIGI_MECA (BIDON):
!     ----------------------------
    codvoi='A2'
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUNS', 'E', imatu)
    call tecach('OOO', 'PMATUNS', 'E', iret, iad=imatu2)
    ASSERT(iret.eq.0)
    ASSERT(imatu2.eq.imatu)
!
    call tecael(iadzi, iazk24)
    numa=zi(iadzi-1+1)
!
    ico2=0
    numav=numa
    do 10,k2=1,nno
    do 11, k1=1,nno
    ico2=ico2+1
    zr(imatu-1+ico2)=1000.d0*numa+numav
11  continue
    10 end do
!
    call voiuti(numa, codvoi, nvoima, nscoma, jrepe,&
                jptvoi, jelvoi, nbvois, livois, tyvois,&
                nbnovo, nbsoco, lisoco)
    do 1,kvois=1,nbvois
    nnov=nbnovo(kvois)
    numav=livois(kvois)
    call tecach('OOO', 'PGEOMER', 'L', iret, iad=igeom2, numa=numav)
    ASSERT(iret.eq.0)
    do 2, k2=1,nnov
    do 3, k1=1,nno
    ico2=ico2+1
    zr(imatu-1+ico2)=1000.d0*numa+numav
 3  continue
 2  continue
    1 end do
!
9999  continue
end subroutine

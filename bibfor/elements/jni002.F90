subroutine jni002(elrefa, nmaxob, liobj, nbobj)
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/elraca.h"
#include "asterfort/elraga.h"
#include "asterfort/elrfd2.h"
#include "asterfort/elrfdf.h"
#include "asterfort/elrfvf.h"
#include "asterfort/inmat4.h"
#include "asterfort/jeexin.h"
#include "asterfort/wkvect.h"
    character(len=8) :: elrefa
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! BUT : INITIALISER LES ELREFA
! ======================================================================
!
! ----------------------------------------------------------------------
!
    integer :: nbpgmx, nbnomx, nbfamx
    parameter (nbpgmx=1000,nbnomx=27,nbfamx=20)
!
    integer :: nbpg(nbfamx), iret, ndim, nno, nnos, nbfpg
    integer :: nmaxob, nbobj, lonfam, ifam, lon2, decal, idim
    integer :: ipg, ino, nderiv, jvi, jvr, npg, nno2, jdim
    real(kind=8) :: xno(3*nbnomx), vol, rvide
    real(kind=8) :: xpg(3*nbpgmx), poipg(nbpgmx)
    real(kind=8) :: ff(nbnomx), dff(3, nbnomx), dff2(3, 3, nbnomx)
    character(len=24) :: liobj(nmaxob)
    character(len=8) :: nofpg(nbfamx)
!
!     NBPGMX, NBNOMX, NBFAMX SE REFERER A ELRACA
!
! DEB ------------------------------------------------------------------
!
!
    nbobj = 2
    ASSERT(nmaxob.gt.nbobj)
    liobj(1) = '&INEL.'//elrefa//'.ELRA_I'
    liobj(2) = '&INEL.'//elrefa//'.ELRA_R'
!
!
!
    call jeexin('&INEL.'//elrefa//'.ELRA_I', iret)
    if (iret .gt. 0) goto 150
!
    call elraca(elrefa, ndim, nno, nnos, nbfpg,&
                nofpg, nbpg, xno, vol)
    ASSERT((ndim.ge.0) .and. (ndim.le.3))
    ASSERT((nno.gt.0) .and. (nno.le.nbnomx))
    ASSERT((nbfpg.gt.0) .and. (nbfpg.le.nbfamx))
!
!
    call wkvect(liobj(1), 'V V I', 4+nbfpg, jvi)
    zi(jvi-1+1) = ndim
    zi(jvi-1+2) = nbfpg
    zi(jvi-1+3) = nno
    zi(jvi-1+4) = nnos
    lon2 = 0
    do 10,ifam = 1,nbfpg
    npg = nbpg(ifam)
    ASSERT((npg.gt.0) .and. (npg.le.nbpgmx))
    zi(jvi-1+4+ifam) = npg
!
!       ON VEUT STOCKER : W(IPG),GEOM(IDIM,IPG)
!                         FF(INO,IPG) ET
!                         DFF(IDIM,INO,IPG)
!                         DFF2(IDIM,JDIM,INO,IPG)
!                         MAPGNO(INO,IPG)
    lonfam = npg
    lonfam = lonfam + npg*ndim
    lonfam = lonfam + npg*nno
    lonfam = lonfam + npg*nno*ndim
    lonfam = lonfam + npg*nno*ndim*ndim
    lonfam = lonfam + 2 + npg*nno
!
    lon2 = lon2 + lonfam
    10 end do
!
    call wkvect(liobj(2), 'V V R', lon2, jvr)
!
    decal = 0
    do 140,ifam = 1,nbfpg
!
!       -- COORDONNEES ET POIDS DES POINTS DE GAUSS :
!       ------------------------------------------------
    call elraga(elrefa, nofpg(ifam), ndim, npg, xpg,&
                poipg)
    do 20,ipg = 1,npg
    decal = decal + 1
    zr(jvr-1+decal) = poipg(ipg)
20  continue
    do 40,ipg = 1,npg
    do 30,idim = 1,ndim
    decal = decal + 1
    zr(jvr-1+decal) = xpg(ndim* (ipg-1)+idim)
30  continue
40  continue
!
!
!       -- VALEURS DES FONCTIONS DE FORME :
!       ------------------------------------------------
    do 60,ipg = 1,npg
    call elrfvf(elrefa, xpg(ndim* (ipg-1)+1), nbnomx, ff, nno)
    do 50,ino = 1,nno
    decal = decal + 1
    zr(jvr-1+decal) = ff(ino)
50  continue
60  continue
!
!
!       -- DERIVEES 1ERES DES FONCTIONS DE FORME :
!       ------------------------------------------------
    do 90,ipg = 1,npg
    call elrfdf(elrefa, xpg(ndim* (ipg-1)+1), 3*nbnomx, dff, nno,&
                nderiv)
    ASSERT(nderiv.eq.ndim)
    do 80,ino = 1,nno
    do 70,idim = 1,ndim
    decal = decal + 1
    zr(jvr-1+decal) = dff(idim,ino)
70  continue
80  continue
90  continue
!
!
!       -- DERIVEES 2EMES DES FONCTIONS DE FORME :
!       ------------------------------------------------
    do 130,ipg = 1,npg
    call elrfd2(elrefa, xpg(ndim* (ipg-1)+1), 9*nbnomx, dff2, nno2,&
                nderiv)
    if (nderiv .eq. 0) then
        ASSERT(nno2.eq.0)
        rvide = r8vide()
    else
        ASSERT(nderiv.eq.ndim)
        ASSERT(nno2.eq.nno)
    endif
    do 120,ino = 1,nno
    do 110,jdim = 1,ndim
    do 100,idim = 1,ndim
    decal = decal + 1
    if (nderiv .ne. 0) then
        zr(jvr-1+decal) = dff2(idim,jdim,ino)
    else
        zr(jvr-1+decal) = rvide
    endif
100  continue
110  continue
120  continue
130  continue
!
!
!       -- MATRICE GAUSS -> NOEUDS : MAPGNO
!       ------------------------------------------------
!       ON STOCKE DANS L'ORDRE : NNO,NPG,MAPGNO
    call inmat4(elrefa, nno, nnos, npg, nofpg(ifam),&
                zr(jvr-1+decal+1))
    decal = decal + 2 + npg*nno
!
!
    140 end do
!
150  continue
!
end subroutine

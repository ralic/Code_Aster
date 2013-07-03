subroutine inmat4(elrefa, nno, nnos, npg, nofpg,&
                  mgano)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/inmat5.h"
#include "asterfort/inmat6.h"
    character(len=8) :: elrefa, nofpg
    integer :: nno, nnos, npg
    real(kind=8) :: mgano(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! BUT : CALCULER LA MATRICE DE PASSAGE GAUSS -> NOEUDS
!       POUR UNE FAMILLE D'UN ELREFA
! ======================================================================
!
    integer :: nbpgmx, nbnomx
    parameter (nbpgmx=1000,nbnomx=27)
    integer :: kpg, kno, knos, k
    real(kind=8) :: mganos(nbpgmx, nbnomx), mgano2(nbpgmx, nbnomx)
!
!     NBPGMX, NBNOMX SE REFERER A ELRACA
!
! DEB ------------------------------------------------------------------
!
!
    call assert(npg.le.nbpgmx)
    call assert(nno.le.nbnomx)
    call assert(nnos.le.nbnomx)
!
!
!     -- MISES A ZERO :
!     ----------------------------------------------------------
    do 30,kpg = 1,npg
    do 10,kno = 1,nno
    mgano2(kpg,kno) = 0.d0
10  continue
    do 20,knos = 1,nnos
    mganos(kpg,knos) = 0.d0
20  continue
    30 end do
    do 40,k = 1,2 + npg*nno
    mgano(k) = 0.d0
    40 end do
!
!
!     -- ON TRAITE LE CAS GENERIQUE NPG=1  (INCLUT NOFPG='FPG1')
!     ----------------------------------------------------------
    if (npg .eq. 1) then
        do 50,kno = 1,nno
        mgano2(1,kno) = 1.d0
50      continue
        goto 80
    endif
!
!
!     -- ON TRAITE LE CAS GENERIQUE NOFPG='NOEU'
!     -------------------------------------------------
    if (nofpg .eq. 'NOEU') then
        call assert(nno.eq.npg)
        do 60,k = 1,nno
        mgano2(k,k) = 1.d0
60      continue
        goto 80
    endif
!
!
!     -- ON TRAITE LE CAS GENERIQUE NOFPG='NOEU_S'
!     -------------------------------------------------
    if (nofpg .eq. 'NOEU_S') then
        call assert(nnos.eq.npg)
        do 70,k = 1,nnos
        mganos(k,k) = 1.d0
70      continue
        call inmat5(elrefa, nno, nnos, npg, mganos,&
                    mgano2)
        goto 80
    endif
!
!
!     -- AUTRES CAS : GAUSS -> SOMMETS -> NOEUDS
!     -------------------------------------------
    call inmat6(elrefa, nofpg, mganos)
    call inmat5(elrefa, nno, nnos, npg, mganos,&
                mgano2)
    goto 80
!
!
80  continue
    mgano(1) = nno
    mgano(2) = npg
    do 100,kpg = 1,npg
    do 90,kno = 1,nno
    mgano(2+ (kno-1)*npg+kpg) = mgano2(kpg,kno)
90  continue
    100 end do
    goto 110
!
110  continue
!
end subroutine

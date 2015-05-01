subroutine crsolv(method, renum, solve, bas)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevtbl.h"
#include "asterfort/sdsolv.h"
#include "asterfort/wkvect.h"
    character(len=*) :: method, renum, solve, bas
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CREATION D'UNE STRUCTURE SOLVEUR
!
!-----------------------------------------------------------------------
    integer :: islvi, islvk, islvr, nprec
    real(kind=8) :: resire
!-----------------------------------------------------------------------
!
    integer :: zslvk, zslvr, zslvi
    real(kind=8) :: epsmat
    character(len=1) :: base
    character(len=3) :: syme
    character(len=8) :: preco
    character(len=19) :: solveu
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    call jemarq()
!
    solveu = solve
    base = bas
!
    preco = 'SANS'
    syme = 'NON'
!
    resire = 1.d-6
    nprec = 8
    epsmat=-1.d0
!
! --- CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SOLVEUR
!
    zslvk = sdsolv('ZSLVK')
    zslvr = sdsolv('ZSLVR')
    zslvi = sdsolv('ZSLVI')
    call wkvect(solveu//'.SLVK', base//' V K24', zslvk, islvk)
    call wkvect(solveu//'.SLVR', base//' V R', zslvr, islvr)
    call wkvect(solveu//'.SLVI', base//' V I', zslvi, islvi)
!
! --- REMPLISSAGE DE LA S.D. SOLVEUR
!
    zk24(islvk-1+1) = method
    if ((method.eq.'MULT_FRONT') .or. (method.eq.'LDLT')) then
        zk24(islvk-1+2) = 'XXXX'
    else
        zk24(islvk-1+2) = preco
    endif
    if (method .eq. 'MUMPS') then
        zk24(islvk-1+3) = 'AUTO'
        zk24(islvk-1+6) = 'LAGR2'
    else
        zk24(islvk-1+3) = 'XXXX'
        zk24(islvk-1+6) = 'XXXX'
    endif
    zk24(islvk-1+4) = renum
    zk24(islvk-1+5) = syme
    zk24(islvk-1+7) = 'XXXX'
    zk24(islvk-1+8) = 'XXXX'
    zk24(islvk-1+9) = 'XXXX'
    zk24(islvk-1+10) = 'XXXX'
    zk24(islvk-1+11) = 'XXXX'
    zk24(islvk-1+12) = 'XXXX'
    zk24(islvk-1+13) = 'NON'
!
    zr(islvr-1+1) = epsmat
    zr(islvr-1+2) = resire
    zr(islvr-1+3) = jevtbl('TAILLE_BLOC')
    zr(islvr-1+4) = 0.d0
!
    zi(islvi-1+1) = nprec
    zi(islvi-1+2) =-9999
    zi(islvi-1+3) =-9999
    zi(islvi-1+4) =-9999
    zi(islvi-1+5) =-9999
    zi(islvi-1+6) =-9999
    zi(islvi-1+7) =-9999
    zi(islvi-1+8) = 0
!
    call jedema()
!
end subroutine

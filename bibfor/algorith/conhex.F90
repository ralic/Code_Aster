subroutine conhex(macor, nbcor, macoc, nbcoc, lface,&
                  lomodi, locorr, loreor, ma)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  ROUTINE CONHEX
!    TRAITEMENT DE KTYC = HEXA20, KTYR = HEXA20, PENTA15, PYRAM13
!            ET DE KTYC = HEXA8 , KTYR = HEXA8 , PENTA6 , PYRAM5
!  DECLARATIONS
!    LOMODI : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE MODIFIEE
!    MACOC  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE FISSURE
!    MACOR  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE REFERENCE
!    NBCOC  : NOMBRE DE CONNEX              POUR UNE MAILLE FISSURE
!    NBCOR  : NOMBRE DE CONNEX              POUR UNE MAILLE REFERENCE
!    NBLIC  : NOMBRE DE NOEUD TESTES        POUR UNE MAILLE FISSURE
!    NBLIR  : NOMBRE DE NOEUD TESTES        POUR UNE MAILLE REFERENCE
!    NBNOCO : NOMBRE DE NOEUD COMMUNS
!    NOCOC  : TABLEAU DE RANG DES NOEUDS    POUR UNE MAILLE FISSURE
!
!  MOT_CLEF : ORIE_FISSURE
!
!
    implicit none
!
!     ------------------------------------------------------------------
!
#include "asterfort/assert.h"
#include "asterfort/concom.h"
#include "asterfort/conech.h"
#include "asterfort/conjac.h"
#include "asterfort/conors.h"
#include "asterfort/conper.h"
#include "asterfort/utmess.h"
    integer :: nbnoco
    integer :: nblir, nbcor
    integer :: nblic, nbcoc, nococ(8)
    integer :: vali
!
    character(len=8) :: macor(nbcor+2), macoc(nbcoc+2), ma
!
    logical :: lomodi, locorr, lface, quadra, loreor
    integer :: i1, i2, i3
#define face(i1,i2,i3,i4) nococ(1).eq.i1.and.nococ(2).eq.i2.and. \
    nococ(3).eq.i3.and.nococ(4).eq.i4
!
!     ------------------------------------------------------------------
    quadra=nbcoc.eq.20
    if (quadra) then
        if (nbcor .eq. 20) nblir=8
        if (nbcor .eq. 15) nblir=6
        if (nbcor .eq. 13) nblir=5
    else
        nblir = nbcor
    endif
    nblic = 8
!
!     -----------------------------------------------------------------
!
    call concom(macor, nblir, macoc, nblic, nbnoco,&
                nococ)
!
    if (nbnoco .eq. 4) then
        if (face(1,2,3,4) .or. face(5,6,7,8)) then
            lface=face(1,2,3,4)
            locorr=.true.
        else if (face(1,2,5,6).or.face(3,4,7,8)) then
!     -------------------------------------------------------------
!     SELON LE MODE 1 (ON TOURNE A 90 DEGRES AUTOUR DE AXE 1)
!     -------------------------------------------------------------
            lomodi=.true.
            lface=face(3,4,7,8)
            call conper(macoc, 1, 4, 8, 5)
            call conper(macoc, 2, 3, 7, 6)
            if (quadra) then
                call conper(macoc, 10, 15, 18, 14)
                call conper(macoc, 9, 11, 19, 17)
                call conper(macoc, 12, 16, 20, 13)
            endif
        else if (face(1,4,5,8).or.face(2,3,6,7)) then
!     -------------------------------------------------------------
!     SELON LE MODE 2 (ON TOURNE A 90 DEGRES AUTOUR DE AXE 2)
!     -------------------------------------------------------------
            lomodi=.true.
            lface=face(2,3,6,7)
            call conper(macoc, 1, 2, 6, 5)
            call conper(macoc, 4, 3, 7, 8)
            if (quadra) then
                call conper(macoc, 9, 14, 17, 13)
                call conper(macoc, 12, 10, 18, 20)
                call conper(macoc, 11, 15, 19, 16)
            endif
        else
            ASSERT(.false.)
        endif
        if (lface) then
            i1=1
            i2=2
            i3=3
        else
            i1=8
            i2=7
            i3=6
        endif
        call conors(i1, i2, i3, macoc, nbcoc,&
                    macor, nbcor, loreor, ma)
        if (loreor) then
            call conech(macoc, 1, 5)
            call conech(macoc, 2, 6)
            call conech(macoc, 3, 7)
            call conech(macoc, 4, 8)
            if (quadra) then
                call conech(macoc, 9, 17)
                call conech(macoc, 10, 18)
                call conech(macoc, 11, 19)
                call conech(macoc, 12, 20)
            endif
        endif
        call conjac(1, 2, 4, 5, macoc,&
                    nbcoc, ma)
        call conjac(2, 1, 6, 3, macoc,&
                    nbcoc, ma)
        call conjac(3, 2, 7, 4, macoc,&
                    nbcoc, ma)
        call conjac(4, 1, 3, 8, macoc,&
                    nbcoc, ma)
        call conjac(5, 1, 8, 6, macoc,&
                    nbcoc, ma)
        call conjac(6, 2, 5, 7, macoc,&
                    nbcoc, ma)
        call conjac(7, 3, 6, 8, macoc,&
                    nbcoc, ma)
        call conjac(8, 4, 7, 5, macoc,&
                    nbcoc, ma)
!
    else if (nbnoco.gt.2) then
        vali = nbnoco
        call utmess('E', 'ALGORITH12_59', si=vali)
    endif
!
end subroutine

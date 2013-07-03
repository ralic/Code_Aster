subroutine elrfd2(elrefz, x, dimd, dff2, nno,&
                  ndim)
    implicit none
#include "asterfort/assert.h"
    integer :: dimd, nno, ndim
    real(kind=8) :: x(*), dff2(3, 3, *)
    character(len=*) :: elrefz
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
!
!
! BUT:   CALCUL DES DERIVEES 2EMES DES FONCTIONS DE FORME
!        AU POINT DE COORDONNEES X
!
! ----------------------------------------------------------------------
!   IN   ELREFZ : NOM DE L'ELREFE (K8)
!        X      : COORDONNEES DU POINT DE CALCUL : X(IDIM)
!        DIMD   : DIMENSION DE DFF2
!   OUT  DFF2   : DERIVEES 2EMES DES FONCTIONS DE FORME :
!                 DFF2(IDIM,JDIM,INO)
!        NNO    : NOMBRE DE NOEUDS
!        NDIM   : DIMENSION DE L'ESPACE DE L'ELREFE : 1,2 OU 3
!   -------------------------------------------------------------------
    character(len=8) :: elrefe
    integer :: idim, jdim, ino
    real(kind=8) :: x1, x2, x3, x4, d1, d2, d3, d4, x0, y0
    real(kind=8) :: zero, undemi, un, deux, trois, quatre, six, sept, huit, uns4
!
! DEB ------------------------------------------------------------------
    elrefe = elrefz
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
    quatre = 4.0d0
    six = 6.0d0
    sept = 7.0d0
    huit = 8.0d0
    uns4 = un/quatre
!
!     -- POUR LES ELEMENTS LINEAIRES : C'EST FACILE : 0.
!     ------------------------------------------------------------------
    if ((elrefe.eq.'SE2') .or. (elrefe.eq.'TR3') .or. (elrefe.eq.'TE4')) then
        if (elrefe .eq. 'SE2') then
            nno = 2
            ndim = 1
        else if (elrefe.eq.'TR3') then
            nno = 3
            ndim = 2
        else if (elrefe.eq.'TE4') then
            nno = 4
            ndim = 3
        endif
!
        do 30,idim = 1,ndim
        do 20,jdim = 1,ndim
        do 10,ino = 1,nno
        dff2(idim,jdim,ino) = 0.d0
10      continue
20      continue
30      continue
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR6') then
        nno = 6
        ndim = 2
!
        dff2(1,1,1) = quatre
        dff2(2,1,1) = quatre
        dff2(1,2,1) = quatre
        dff2(2,2,1) = quatre
!
        dff2(1,1,2) = quatre
        dff2(2,1,2) = zero
        dff2(1,2,2) = zero
        dff2(2,2,2) = zero
!
        dff2(1,1,3) = zero
        dff2(2,1,3) = zero
        dff2(1,2,3) = zero
        dff2(2,2,3) = quatre
!
        dff2(1,1,4) = -huit
        dff2(2,1,4) = -quatre
        dff2(1,2,4) = -quatre
        dff2(2,2,4) = zero
!
        dff2(1,1,5) = zero
        dff2(2,1,5) = quatre
        dff2(1,2,5) = quatre
        dff2(2,2,5) = zero
!
        dff2(1,1,6) = zero
        dff2(2,1,6) = -quatre
        dff2(1,2,6) = -quatre
        dff2(2,2,6) = -huit
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TW6') then
        nno = 6
        ndim = 2
!
        dff2(1,1,1) = -un
        dff2(2,1,1) = -un
        dff2(1,2,1) = -un
        dff2(2,2,1) = un
!
        dff2(1,1,2) = zero
        dff2(2,1,2) = deux
        dff2(1,2,2) = deux
        dff2(2,2,2) = zero
!
        dff2(1,1,3) = +un
        dff2(2,1,3) = -un
        dff2(1,2,3) = -un
        dff2(2,2,3) = -un
!
        dff2(1,1,4) = quatre
        dff2(2,1,4) = zero
        dff2(1,2,4) = zero
        dff2(2,2,4) = zero
!
        dff2(1,1,5) = zero
        dff2(2,1,5) = zero
        dff2(1,2,5) = zero
        dff2(2,2,5) = quatre
!
        dff2(1,1,6) = 2.8284271247461901d0
        dff2(2,1,6) = 2.8284271247461901d0
        dff2(1,2,6) = 2.8284271247461901d0
        dff2(2,2,6) = 2.8284271247461901d0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR7') then
        x0 = x(1)
        y0 = x(2)
        nno = 7
        ndim = 2
!
        dff2(1,1,1) = quatre - six * y0
        dff2(2,1,1) = sept - six * ( x0 + y0 )
        dff2(1,2,1) = sept - six * ( x0 + y0 )
        dff2(2,2,1) = quatre - six * x0
!
        dff2(1,1,2) = quatre - six * y0
        dff2(2,1,2) = trois - six * ( x0 + y0 )
        dff2(1,2,2) = trois - six * ( x0 + y0 )
        dff2(2,2,2) = - six * x0
!
        dff2(1,1,3) = - six * y0
        dff2(2,1,3) = trois - six * ( x0 + y0 )
        dff2(1,2,3) = trois - six * ( x0 + y0 )
        dff2(2,2,3) = quatre - six * x0
!
        dff2(1,1,4) = huit * ( -un + trois * y0 )
        dff2(2,1,4) = quatre * ( -quatre + six * ( x0 + y0 ))
        dff2(1,2,4) = quatre * ( -quatre + six * ( x0 + y0 ))
        dff2(2,2,4) = 24.0d0 * x0
!
        dff2(1,1,5) = 24.0d0 * y0
        dff2(2,1,5) = quatre * ( -deux + six * ( x0 + y0 ))
        dff2(1,2,5) = quatre * ( -deux + six * ( x0 + y0 ))
        dff2(2,2,5) = 24.0d0 * x0
!
        dff2(1,1,6) = 24.0d0 * y0
        dff2(2,1,6) = quatre * ( -quatre + six * ( x0 + y0 ))
        dff2(1,2,6) = quatre * ( -quatre + six * ( x0 + y0 ))
        dff2(2,2,6) = huit * ( -un + trois * x0 )
!
        dff2(1,1,7) = -54.0d0 * y0
        dff2(2,1,7) = 27.0d0 * ( un - deux * ( x0 + y0 ))
        dff2(1,2,7) = 27.0d0 * ( un - deux * ( x0 + y0 ))
        dff2(2,2,7) = -54.0d0 * x0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU4') then
        x0 = x(1)
        y0 = x(2)
        nno = 4
        ndim = 2
!
        dff2(1,1,1) = zero
        dff2(2,1,1) = uns4
        dff2(1,2,1) = uns4
        dff2(2,2,1) = zero
!
        dff2(1,1,2) = zero
        dff2(2,1,2) = -uns4
        dff2(1,2,2) = -uns4
        dff2(2,2,2) = zero
!
        dff2(1,1,3) = zero
        dff2(2,1,3) = uns4
        dff2(1,2,3) = uns4
        dff2(2,2,3) = zero
!
        dff2(1,1,4) = zero
        dff2(2,1,4) = -uns4
        dff2(1,2,4) = -uns4
        dff2(2,2,4) = zero
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU6') then
        x0 = x(1)
        y0 = x(2)
        nno = 6
        ndim = 2
!
        dff2(1,1,1) = undemi*(un - y0)
        dff2(2,1,1) = zero
        dff2(1,2,1) = zero
        dff2(2,2,1) = undemi*(un - x0) - 0.25d0
!
        dff2(1,1,2) = undemi*(un - y0)
        dff2(2,1,2) = zero
        dff2(1,2,2) = zero
        dff2(2,2,2) = 0.25d0 - undemi*(un + x0)
!
        dff2(1,1,3) = undemi*(un + y0)
        dff2(2,1,3) = zero
        dff2(1,2,3) = zero
        dff2(2,2,3) = undemi*(un + x0) - 0.25d0
!
        dff2(1,1,4) = undemi*(un + y0)
        dff2(2,1,4) = zero
        dff2(1,2,4) = zero
        dff2(2,2,4) = 0.25d0 - undemi*(un - x0)
!
        dff2(1,1,5) = y0 - un
        dff2(2,1,5) = zero
        dff2(1,2,5) = zero
        dff2(2,2,5) = x0
!
        dff2(1,1,6) = -y0 - un
        dff2(2,1,6) = zero
        dff2(1,2,6) = zero
        dff2(2,2,6) = -x0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU8') then
        x0 = x(1)
        y0 = x(2)
        nno = 8
        ndim = 2
!
        dff2(1,1,1) = (un - y0) * undemi
        dff2(2,1,1) = (un - deux*x0 - deux*y0) * uns4
        dff2(1,2,1) = (un - deux*x0 - deux*y0) * uns4
        dff2(2,2,1) = (un - x0) * undemi
!
        dff2(1,1,2) = (un - y0) * undemi
        dff2(2,1,2) = -(un + deux*x0 - deux*y0) * uns4
        dff2(1,2,2) = -(un + deux*x0 - deux*y0) * uns4
        dff2(2,2,2) = (un + x0) * undemi
!
        dff2(1,1,3) = (un + y0) * undemi
        dff2(2,1,3) = (un + deux*x0 + deux*y0) * uns4
        dff2(1,2,3) = (un + deux*x0 + deux*y0) * uns4
        dff2(2,2,3) = (un + x0) * undemi
!
        dff2(1,1,4) = (un + y0) * undemi
        dff2(2,1,4) = -(un - deux*x0 + deux*y0) * uns4
        dff2(1,2,4) = -(un - deux*x0 + deux*y0) * uns4
        dff2(2,2,4) = (un - x0) * undemi
!
        dff2(1,1,5) = -un + y0
        dff2(2,1,5) = x0
        dff2(1,2,5) = x0
        dff2(2,2,5) = zero
!
        dff2(1,1,6) = zero
        dff2(2,1,6) = -y0
        dff2(1,2,6) = -y0
        dff2(2,2,6) = -un - x0
!
        dff2(1,1,7) = -un - y0
        dff2(2,1,7) = -x0
        dff2(1,2,7) = -x0
        dff2(2,2,7) = zero
!
        dff2(1,1,8) = zero
        dff2(2,1,8) = y0
        dff2(1,2,8) = y0
        dff2(2,2,8) = -un + x0
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU9') then
        x0 = x(1)
        y0 = x(2)
        nno = 9
        ndim = 2
!
        dff2(1,1,1) = y0 * (y0 - un) * undemi
        dff2(2,1,1) = (x0 - undemi) * (y0 - undemi)
        dff2(1,2,1) = (x0 - undemi) * (y0 - undemi)
        dff2(2,2,1) = x0 * (x0 - un) * undemi
!
        dff2(1,1,2) = y0 * (y0 - un) * undemi
        dff2(2,1,2) = (x0 + undemi) * (y0 - undemi)
        dff2(1,2,2) = (x0 + undemi) * (y0 - undemi)
        dff2(2,2,2) = x0 * (x0 + un) * undemi
!
        dff2(1,1,3) = y0 * (y0 + un) * undemi
        dff2(2,1,3) = (x0 + undemi) * (y0 + undemi)
        dff2(1,2,3) = (x0 + undemi) * (y0 + undemi)
        dff2(2,2,3) = x0 * (x0 + un) * undemi
!
        dff2(1,1,4) = y0 * (y0 + un) * undemi
        dff2(2,1,4) = (x0 - undemi) * (y0 + undemi)
        dff2(1,2,4) = (x0 - undemi) * (y0 + undemi)
        dff2(2,2,4) = x0 * (x0 - un) * undemi
!
        dff2(1,1,5) = -y0 * (y0 - un)
        dff2(2,1,5) = -deux * x0 * (y0 - undemi)
        dff2(1,2,5) = -deux * x0 * (y0 - undemi)
        dff2(2,2,5) = -(x0 + un) * (x0 - un)
!
        dff2(1,1,6) = -(y0 + un) * (y0 - un)
        dff2(2,1,6) = -deux * y0 * (x0 + undemi)
        dff2(1,2,6) = -deux * y0 * (x0 + undemi)
        dff2(2,2,6) = -x0 * (x0 + un)
!
        dff2(1,1,7) = -y0 * (y0 + un)
        dff2(2,1,7) = -deux * x0 * (y0 + undemi)
        dff2(1,2,7) = -deux * x0 * (y0 + undemi)
        dff2(2,2,7) = -(x0 + un) * (x0 - un)
!
        dff2(1,1,8) = -(y0 + un) * (y0 - un)
        dff2(2,1,8) = -deux * y0 * (x0 - undemi)
        dff2(1,2,8) = -deux * y0 * (x0 - undemi)
        dff2(2,2,8) = -x0 * (x0 - un)
!
        dff2(1,1,9) = deux * (y0 + un) * (y0 - un)
        dff2(2,1,9) = quatre * x0 * y0
        dff2(1,2,9) = quatre * x0 * y0
        dff2(2,2,9) = deux * (x0 + un) * (x0 - un)
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE3') then
        nno = 3
        ndim = 1
!
        dff2(1,1,1) = un
        dff2(1,1,2) = un
        dff2(1,1,3) = -deux
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE4') then
        nno = 4
        ndim = 1
!
        x1 = -un
        x2 = un
        x3 = -un/3.d0
        x4 = un/3.d0
!
        d1 = (x1-x2)* (x1-x3)* (x1-x4)
        d2 = (x2-x3)* (x2-x4)* (x2-x1)
        d3 = (x3-x4)* (x3-x1)* (x3-x2)
        d4 = (x4-x1)* (x4-x2)* (x4-x3)
!
        dff2(1,1,1) = 2* ((x(1)-x2)+ (x(1)-x3)+ (x(1)-x4))/d1
        dff2(1,1,2) = 2* ((x(1)-x3)+ (x(1)-x4)+ (x(1)-x1))/d2
        dff2(1,1,3) = 2* ((x(1)-x4)+ (x(1)-x1)+ (x(1)-x2))/d3
        dff2(1,1,4) = 2* ((x(1)-x1)+ (x(1)-x2)+ (x(1)-x3))/d4
!
!     ------------------------------------------------------------------
!     -- POUR LES ELEREFE NON ENCORE RENSEIGNES, ON REND NDIM=NNO=0
    else
        nno = 0
        ndim = 0
!
    endif
!
!     ------------------------------------------------------------------
!
    call assert(dimd.ge. (nno*ndim*ndim))
!
end subroutine

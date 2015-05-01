subroutine xdelt4(elp, xini, base, ndim,&
                  tabls, delta)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "blas/ddot.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elrfdf.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/matini.h"
#include "asterfort/matinv.h"
#include "asterfort/matmul.h"
#include "asterfort/vecini.h"
    integer :: ndim
    real(kind=8) :: delta(ndim), xini(ndim), tabls(2*20), base(2*ndim)
    character(len=8) :: elp
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
! person_in_charge: daniele.colombo at ifpen.fr
!              TROUVER LES PTS D'INTERSECTION ENTRE LE FOND DE FISSURE
!                 ET UN PLAN POUR LES ELEMENTS EN FOND DE FISSURE
!
!     ENTREE
!  ELP    : MACRO MAILLE ASSOCIEE
!  BASE   : BASE ORTHONORMEE DE LA FACE
!  XINI   : POINT DE DEPART DU NEWTON (EN CORDS DE REFERENCE)
!  DELTA  : INCREMENT DE DEPLACEMENT
!
!     SORTIE
!  X      : POINT TROUVE COORDONNEES REELES
!  XREF   : POINT TROUVE COORDONNEES DE REFERENCE
!
!     ------------------------------------------------------------------
!
    integer :: i, j, nbnomx, nno
    real(kind=8) :: plan(ndim,2), ls(2), epsi(2,1), dff(3,27)
    real(kind=8) :: gradls(2,ndim), mat(2,2), inv(2,2), det, ff(27)
    parameter   (nbnomx = 27)
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     CALCUL DE MATRCICE DE CHANGEMENT DE BASE ENTRE (U,V) ET LE
!     REPERE DE REFERENCE
    do i = 1, ndim
       plan(i,1) = base(i)
       plan(i,2) = base(ndim+i)
    end do
!
!     CALCUL AU POINT COURANT DE LST, LSN, GRLST, GRLSN
!
!     CALCUL DES FF ET DERVIES DES FF AU POINT COURANT
    call elrfvf(elp, xini, nbnomx, ff, nno)
    call elrfdf(elp, xini, ndim*nbnomx, dff, nno,&
                ndim)
    call matini(2, ndim, 0.d0, gradls)
    call vecini(2, 0.d0, ls)
    do j = 1, nno
       ls(1) = ls(1)+tabls(2*j-1)*ff(j)
       ls(2) = ls(2)+tabls(2*j)*ff(j)
       do i = 1, ndim
          gradls(1,i) = gradls(1,i)+tabls(2*j-1)*dff(i,j)
          gradls(2,i) = gradls(2,i)+tabls(2*j)*dff(i,j)
       end do
    end do
!
!   ASSEMBLAGE ET INVERSION DE LA MATRICE
!
    call matmul(gradls, plan, 2, ndim, 2, mat)
    call matini(2 ,2 , 0.d0, inv)
    call matinv('S', 2, mat, inv, det)
!
    call matini(2 ,1 ,0.d0, epsi)
    call matmul(inv,ls, 2, 2, 1, epsi)
!
!  CALCUL DE L'INCREMENT DE DEPLACEMENT
    do i = 1, ndim
       delta(i) = epsi(1,1)*base(i) + epsi(2,1)*base(ndim+i)
    end do
!
    call jedema()
!
end subroutine

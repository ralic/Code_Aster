subroutine te0562(option, nomte)
!
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
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elrefv.h"
#include "asterfort/jevech.h"
#include "asterfort/massup.h"
#include "asterfort/teattr.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  OPTION MASS_MECA_* POUR GRAD_VARI
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: dlns
    integer :: nno, npg1, imatuu, ndim, nnos, jgano, iret, icodr1(1)
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: nnob, ivfb, idfdeb, jganob
!
    character(len=8) :: typmod(2)
    character(len=16) :: phenom
!
!
    call elrefv(nomte, 'MASS', ndim, nno, nnob,&
                nnos, npg1, ipoids, ivf, ivfb,&
                idfde, idfdeb, jgano, jganob)
!
!    TYPE DE MODELISATION
    call teattr('S', 'TYPMOD', typmod(1), iret)
    typmod(2) = 'GRADVARI'
!
!     PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
!
!    DEPLACEMENT + LAMBDA + VAR_REG
    ASSERT(ndim.eq.2 .or. ndim.eq.3)
    dlns = ndim + 2
!
    call massup(option, ndim, dlns, nno, nnob,&
                zi(imate), phenom, npg1, ipoids, idfde,&
                zr(igeom), zr(ivf), imatuu, icodr1, igeom,&
                ivf)
!
end subroutine

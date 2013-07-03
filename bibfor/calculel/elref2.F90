subroutine elref2(nomte, dim, lielrf, ntrou)
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
!
!
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
    character(len=16) :: nomte
    integer :: dim
    character(len=8) :: lielrf(dim)
! ---------------------------------------------------------------------
! BUT: RECUPERER LA LISTE DES ELREFE D'UN TYPE_ELEM
! ---------------------------------------------------------------------
!     ARGUMENTS:
! NOMTE  IN      K16    : NOM DU TYPE_ELEM
! DIM    IN      I      : DIMENSION DU VECTEUR LIELRF
! LIELRF OUT     L_K8   : LISTE DES ELREFE POUR NOMTE
!        OUT     NTROU  : NOMBRE D'ELREFE POUR NOMTE
!
! REMARQUE :
!   SI NOMTE A PLUS D'ELREFE QUE DIM => ERREUR <F>
!----------------------------------------------------------------------
!
    integer :: nute, jnbelr, jnoelr, ntrou, iad, k
!
    call jenonu(jexnom('&CATA.TE.NOMTE', nomte), nute)
    call assert(nute.gt.0)
!
    call jeveuo('&CATA.TE.NBELREFE', 'L', jnbelr)
    call jeveuo('&CATA.TE.NOELREFE', 'L', jnoelr)
!
    ntrou = zi(jnbelr-1+2* (nute-1)+1)
    iad = zi(jnbelr-1+2* (nute-1)+2)
!
    call assert(ntrou.le.dim)
!
    do 10,k = 1,ntrou
    lielrf(k) = zk8(jnoelr-1+iad-1+k)
    10 end do
!
!
end subroutine

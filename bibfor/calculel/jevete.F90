subroutine jevete(nomobj, code, iad)
    implicit none
!
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
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/indk24.h"
    character(len=*) :: nomobj
    character(len=1) :: code
    character(len=24) :: nomob2
    integer :: iainel, ii, ininel, nbobj, iad
!     -----------------------------------------------------------------
!     ENTREES:
!     NOMOBJ  : NOMBRE DE L'OBJET '&INEL.XXXX' DONT ON VEUT L'ADRESSE
!     CODE    : 'L' (POUR RESSEMBLER A JEVEUO MAIS NE SERT A RIEN !)
!
!     SORTIES:
!     IAD      : ADRESSE DE L'OBJET '&INEL.XXX' VOULU.
!                ( = 0 SI L'OBJET N'EXISTE PAS).
    common /caii09/nbobj,iainel,ininel
!
! DEB-------------------------------------------------------------------
!
    nomob2 = nomobj
    ii = indk24(zk24(ininel),nomob2,1,nbobj)
    ASSERT(ii.ne.0)
    iad = zi(iainel-1+ii)
! FIN ------------------------------------------------------------------
end subroutine

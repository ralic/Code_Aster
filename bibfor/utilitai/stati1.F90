subroutine stati1(nval, serie, moyenn, ectype)
    implicit none
#include "asterfort/assert.h"
    integer :: nval
    real(kind=8) :: serie(nval), moyenn, ectype
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
! ----------------------------------------------------------------------
!  CALCULE LA MOYENNE ET L'ECART-TYPE D'UNE SERIE DE VALEURS REELLES
!  IN : NVAL  : NB DE VALEURS DE LA SERIE
!  IN : SERIE : LISTE DES VALEURS LA SERIE
!  OUT: MOYENN : VALEUR DE LA MOYENNE
!  OUT: ECTYPE : VALEUR DE L'ECART-TYPE
! ----------------------------------------------------------------------
!
    integer :: k
!----------------------------------------------------------------------
    ASSERT(nval.ge.1)
!
!
!     -- MOYENNE :
    moyenn=0.d0
    do 1, k=1,nval
    moyenn=moyenn+serie(k)
    1 end do
    moyenn=moyenn/nval
!
!
!     -- ECART-TYPE :
    ectype=0.d0
    do 2, k=1,nval
    ectype=ectype+(serie(k)-moyenn)**2
    2 end do
    ectype=sqrt(ectype/nval)
!
end subroutine

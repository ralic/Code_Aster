subroutine ssdmu1(dref, crit, prec, geo1, geo2,&
                  iconf)
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
    implicit none
#include "asterfort/utmess.h"
!     ARGUMENTS:
!     ----------
    character(len=*) :: crit
    real(kind=8) :: prec, geo1(3), geo2(3), dref
    character(len=8) :: crit2
    real(kind=8) :: dist, a1, a2, a3
    integer :: iconf
! ----------------------------------------------------------------------
!     BUT:
!        - DETERMINER SI DEUX NOEUDS DE COORDONEES GEO1 ET GEO2
!          SONT CONFONDUS GEOMETRIQUEMENT.
!
!     IN:
!        DREF : DISTANCE DE REFERENCE POUR LE CRITERE RELATIF
!      CRIT : CRITERE : 'RELATIF' OU 'ABSOLU'
!      PREC : PRECISION
!      GEO1 : GEOMETRIE DU 1ER NOEUD
!      GEO2 : GEOMETRIE DU 2EM NOEUD
!
!     OUT:
!       ICONF: 0 --> LES 2 NOEUDS SONT CONFONDUS. (1 SINON)
! ----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    crit2=crit
    a1= geo1(1)-geo2(1)
    a2= geo1(2)-geo2(2)
    a3= geo1(3)-geo2(3)
    dist= sqrt(a1**2+a2**2+a3**2)
!
    iconf=1
    if (crit2(1:6) .eq. 'ABSOLU') then
        if (dist .le. prec) iconf=0
    else if (crit2(1:7).eq.'RELATIF') then
        if (dist .le. prec*dref) iconf=0
    else
        call utmess('F', 'ALGORITH3_42', sk=crit2)
    endif
!
end subroutine

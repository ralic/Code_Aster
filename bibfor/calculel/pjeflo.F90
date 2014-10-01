subroutine pjeflo(elrefa, ndim, ipb, xr2, disprj)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
!
    integer :: ipb, ndim
    real(kind=8) :: xr2(ndim), disprj
    character(len=*) :: elrefa
! ----------------------------------------------------------------------
! BUT :
!   * calculer disprj : distance de projection d'un point
!                       (normee par le "diametre" de la maille)
!  disprj =   0. => le point est interieur a la maille
!  disprj = 999. => la routine reereg n'a pas converge
!  disprj = a>0  => le point est exterieur a la maille.
!   La distance du point a la maille est de l'ordre de a*diametre_reel(maille)
! ----------------------------------------------------------------------
!
! in  elrefa   : elrefa de l'element
! in  ndim     : dimension de l'espace
! in  xr2      : coordonnees du point dans l'element de reference
!                (calcule par reereg)
! in  ipbd     : code retour de reereg
! out disprj   : distance de projection (en relatif)
! ----------------------------------------------------------------------
!
    real(kind=8) :: x, y, z, diam
! --------------------------------------------------------------------------------------------------
    disprj = 0.0d0
!   SI REEREG N'A PAS CONVERGE, ON N'A PAS CONFIANCE DANS XR2 :
    if (ipb .ne. 0) then
        disprj=dble(999)
        goto 80
    endif
!
    if (ndim .ge. 1) x=xr2(1)
    if (ndim .ge. 2) y=xr2(2)
    if (ndim .ge. 3) z=xr2(3)
!
! --------------------------------------------------------------------------------------------------
!   POUR LES HEXA : KSI,ETA,DZETA SONT DANS [-1,1]
    if (elrefa .eq. 'HE8' .or. elrefa .eq. 'H20' .or. elrefa .eq. 'H27') then
        ASSERT(ndim.eq.3)
        if (abs(x) .gt. 1.d0) goto 10
        if (abs(y) .gt. 1.d0) goto 10
        if (abs(z) .gt. 1.d0) goto 10
!       ON EST INTERIEUR
        goto 80
!
10      continue
!       ON EST EXTERIEUR. EST-ON LOIN ?
        disprj=0.d0
        disprj=max(disprj,abs(x)-1.d0)
        disprj=max(disprj,abs(y)-1.d0)
        disprj=max(disprj,abs(z)-1.d0)
!       -- diam : "dimension" de l'elrefe :
        diam=2.
        disprj=disprj/diam
! --------------------------------------------------------------------------------------------------
!   POUR LES TETRA :
    elseif (elrefa.eq.'TE4' .or. elrefa.eq.'T10') then
        ASSERT(ndim.eq.3)
        if (x .lt. 0.d0) goto 20
        if (y .lt. 0.d0) goto 20
        if (z .lt. 0.d0) goto 20
        if (x+y+z .gt. 1.d0) goto 20
!
!       ON EST INTERIEUR
        goto 80
!
20      continue
!       ON EST EXTERIEUR. EST-ON LOIN ?
        disprj=0.d0
        disprj=max(disprj,-x)
        disprj=max(disprj,-y)
        disprj=max(disprj,-z)
        disprj=max(disprj,x+y+z-1.d0)
!       -- diam : "dimension" de l'elrefe :
        diam=1.
        disprj=disprj/diam
!
! --------------------------------------------------------------------------------------------------
!   POUR LES PYRAM :
    elseif (elrefa.eq.'PY5' .or. elrefa.eq.'P13') then
        ASSERT(ndim.eq.3)
        if (z .lt. 0.d0) goto 30
        if (x+y+z .gt. 1.d0) goto 30
        if (x-y+z .gt. 1.d0) goto 30
        if (-x+y+z .gt. 1.d0) goto 30
        if (-x-y+z .gt. 1.d0) goto 30
!
!       ON EST INTERIEUR
        goto 80
!
30      continue
!       ON EST EXTERIEUR. EST-ON LOIN ?
        disprj=0.d0
        disprj=max(disprj,-z)
        disprj=max(disprj,x+y+z-1.d0)
        disprj=max(disprj,x-y+z-1.d0)
        disprj=max(disprj,-x+y+z-1.d0)
        disprj=max(disprj,-x-y+z-1.d0)
!       -- diam : "dimension" de l'elrefe :
        diam=2.
        disprj=disprj/diam
!
! --------------------------------------------------------------------------------------------------
!   POUR LES PENTA :
    elseif (elrefa.eq.'PE6' .or. elrefa.eq.'P15' .or. elrefa.eq.'P18') then
        ASSERT(ndim.eq.3)
        if (x .lt. -1.d0) goto 40
        if (x .gt. +1.d0) goto 40
        if (y .lt. 0.d0) goto 40
        if (z .lt. 0.d0) goto 40
        if (y+z .gt. 1.d0) goto 40
!
!       ON EST INTERIEUR
        goto 80
!
40      continue
!       ON EST EXTERIEUR. EST-ON LOIN ?
        disprj=0.d0
        disprj=max(disprj,abs(x)-1.d0)
        disprj=max(disprj,-y)
        disprj=max(disprj,-z)
        disprj=max(disprj,+y+z-1.d0)
!       -- diam : "dimension" de l'elrefe :
        diam=2.
        disprj=disprj/diam
!
! --------------------------------------------------------------------------------------------------
!   POUR LES TRIA :
    elseif (elrefa.eq.'TR3' .or. elrefa.eq.'TR6' .or. elrefa.eq.'TR7') then
        ASSERT(ndim.eq.2)
        if (x .lt. 0.d0) goto 50
        if (y .lt. 0.d0) goto 50
        if (x+y .gt. 1.d0) goto 50
!
!       ON EST INTERIEUR
        goto 80
!
50      continue
!       ON EST EXTERIEUR. EST-ON LOIN ?
        disprj=0.d0
        disprj=max(disprj,-x)
        disprj=max(disprj,-y)
        disprj=max(disprj,+x+y-1.d0)
!       -- diam : "dimension" de l'elrefe :
        diam=1.
        disprj=disprj/diam
!
! --------------------------------------------------------------------------------------------------
!   POUR LES QUAD :
    elseif (elrefa.eq.'QU4' .or. elrefa.eq.'QU8' .or. elrefa.eq.'QU9') then
        ASSERT(ndim.eq.2)
        if (x .lt. -1.d0) goto 60
        if (y .lt. -1.d0) goto 60
        if (x .gt. +1.d0) goto 60
        if (y .gt. +1.d0) goto 60
!
!       ON EST INTERIEUR
        goto 80
!
60      continue
!       ON EST EXTERIEUR. EST-ON LOIN ?
        disprj=0.d0
        disprj=max(disprj,-1.d0-x)
        disprj=max(disprj,-1.d0-y)
        disprj=max(disprj,x-1.d0)
        disprj=max(disprj,y-1.d0)
!       -- diam : "dimension" de l'elrefe :
        diam=2.
        disprj=disprj/diam
    else
        ASSERT(.false.)
    endif
!
80  continue
end subroutine

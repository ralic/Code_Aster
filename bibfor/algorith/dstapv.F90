subroutine dstapv(nbpt, d, t, dmin, dmax,&
                  dmoy, detyp, drms, sd, sde,&
                  sd2)
!***********************************************************************
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
!       MOYENNAGE STATISTIQUE DES DEPLACEMENTS
!       ALGORITHME CALCUL TEMPOREL A PAS DE TEMPS VARIABLE
!
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/trapez.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: d(*), t(*), dmoy, detyp, drms, dmax, dmin, sd, sde, sd2
!
!
!-----------------------------------------------------------------------
    integer :: i, ift, nbpt
!-----------------------------------------------------------------------
    call jemarq()
!
!       ARGUMENTS:
!       ----------------------------------------
!       IN:
!            NBPT         NB DE POINTS DU TABLEAU A ANALYSER
!            D            TABLEAU A ANALYSER
!            T            TABLEAU DES INSTANTS CORRESPONDANTS
!
!       OUT:
!            DMOY        VALEUR MOYENNE ( COMPTAGE AU DESSUS DU SEUIL )
!            DETYP       ECART TYPE
!            DRMS        SQRT DE LA MOYENNE DES CARRES ( RMS )
!            DMAX        VALEUR MAXIMUM ABSOLU DU TABLEAU
!            DMIN        VALEUR MINIMUM ABSOLU DE LA FONCTION
!
!
!
!       VARIABLES UTILISEES
!       ----------------------------------------
!       SD SOMME DES VALEURS
!       SD2 SOMME DES CARRES DES VALEURS
!       SDD SOMME DES CARRES DES DIFFERENCES A LA MOYENNE
!
    call wkvect('&&DSTAPV.TEMP.FCNT', 'V V R', nbpt, ift)
!
    sd=0.d0
    sd2=0.d0
    dmoy = 0.d0
    drms = 0.d0
    detyp = 0.d0
    dmax=-10.d20
    dmin=-dmax
!
! --- RECHERCHE DES EXTREMAS ABSOLUS
!
    do 10 i = 1, nbpt
        if (d(i) .gt. dmax) dmax=d(i)
        if (d(i) .lt. dmin) dmin=d(i)
10  continue
!
! --- DEPLACEMENT MOYEN
!
    do 20 i = 1, nbpt
        zr(ift + i-1) = d(i)
20  continue
    call trapez(t, zr(ift), nbpt, sd)
    dmoy = sd / (t(nbpt) - t(1))
!
! --- DEPLACEMENT QUADRATIQUE MOYEN
!
    do 30 i = 1, nbpt
        zr(ift + i-1) = d(i)*d(i)
30  continue
    call trapez(t, zr(ift), nbpt, sd2)
    drms = sqrt(sd2 / (t(nbpt) - t(1)))
!
! --- DEPLACEMENT QUADRATIQUE MOYEN (MOYENNE NULLE)
!
    do 40 i = 1, nbpt
        zr(ift + i-1) = (d(i)-dmoy)*(d(i)-dmoy)
40  continue
    call trapez(t, zr(ift), nbpt, sde)
    detyp = sqrt(sde / (t(nbpt) - t(1)))
!
!
    call jedetr('&&DSTAPV.TEMP.FCNT')
    call jedema()
end subroutine

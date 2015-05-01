subroutine dstat0(nbpt, d, dmoy, detyp, drms,&
                  dmax, dmin)
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
! CETTE ROUTINE EST EN FAIT L'ANCIENNE DSTAT RENOMMEE DSTAT0
!
!       MOYENNAGE STATISTIQUE DES DEPLACEMENTS AMV
!
!
!
    implicit none
    real(kind=8) :: d(*), dmoy, detyp, drms, dmax, dmin, sd2, sd, sdd
!
!
!       ARGUMENTS:
!       ----------------------------------------
!       IN:
!            NBPT         NB DE POINTS DU TABLEAU A ANALYSER
!            D            TABLEAU A ANALYSER
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
!-----------------------------------------------------------------------
    integer :: i, nbpt
!-----------------------------------------------------------------------
    sd = 0.d0
    sd2 = 0.d0
    sdd = 0.d0
    dmax = -10.d20
    dmin = -dmax
!
    do 10 i = 1, nbpt
!
        sd = sd + d(i)
        sd2 = sd2 + d(i)**2
!
!           RECHERCHE DES EXTREMAS ABSOLUS
!
        if (d(i) .gt. dmax) dmax = d(i)
        if (d(i) .lt. dmin) dmin = d(i)
!
!
10  end do
    dmoy = sd/nbpt
!
    do 20 i = 1, nbpt
        sdd = sdd + (d(i)-dmoy)**2
20  end do
!
    drms = sqrt(sd2/nbpt)
    detyp = sqrt(sdd/nbpt)
!
end subroutine

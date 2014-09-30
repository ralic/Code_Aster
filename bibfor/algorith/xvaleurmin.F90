subroutine xvaleurmin(jcalculs, jvtemp, jnodto, nbno, minlo)

    implicit none

#include "jeveux.h"
#include "asterfort/assert.h"

    integer :: jcalculs, jnodto, jvtemp
    integer :: nbno, minlo

! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr

! XVALEURMIN : RECHERCHE LE MINIMUM DANS UN VECTEUR

!ENTREE
!      CALCULS = VECTEUR CONTENANT LES VALEURS DES LEVEL SETS DANS LA NARROWBAND
!      JVTEMP  = VECTEUR LOGIQUE INDIQUANT SI LE NOEUD EST CALCULE
!      JNODTO  = LISTE DES NOEUDS DEFINISSANTS LE DOMAINE DE CALCUL
!      NBNO    = NOMBRE DE NOEUD DU TORE DE CALCUL
!      MINLO   = INDICE DU NOEUD OU LE MINIMUM EST LOCALISE

!SORTIE
!      MINLO   = INDICE DU NOEUD OU LE MINIMUM EST LOCALISE
!------------------------------------------------------------------------

    integer :: inod, node
    integer :: ideb, nodeb
    real(kind=8) :: minimum

    !   tolerances --- absolue et relative --- pour determiner si deux valeurs sont egales
    real(kind=8), parameter :: atol=1.e-12
    real(kind=8), parameter :: rtol=1.e-12
    aster_logical :: near

!----------------DEBUT---------------------------------------------------

    ! recherche du premier noeud dans la narrow band
    ideb  = 0
    nodeb = 0

    do inod = 1 , nbno
        node = zi(jnodto-1+inod)
        if (zl(jvtemp-1+node)) then
            ideb=inod
            nodeb=node
            exit
        endif
    end do

    ! assertion : la narrow band n'est pas vide
    ASSERT(ideb.ne.0)

    ! initialisation du minimum : valeur du premier noeud de la narrow band
    minlo = ideb
    minimum = zr(jcalculs-1+nodeb)

    ! recherche du minimum sur les autres noeuds de la narrow band
    do inod = ideb + 1 , nbno
        node = zi(jnodto-1+inod)

        ! la valeur courante est-elle egale au minimum ?
        near = abs(zr(jcalculs-1+node) - minimum) .le. (atol + minimum*rtol)

        if (zr(jcalculs-1+node) .lt. minimum .and. .not.near .and. zl(jvtemp-1+node))  then
            minimum = zr(jcalculs-1+node)
            minlo = inod
        endif
    end do
end subroutine

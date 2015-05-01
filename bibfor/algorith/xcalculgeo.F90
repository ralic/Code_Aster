subroutine xcalculgeo(ndim, vale, jvp, jbl, deltat, jbeta, &
                      jlistp, node, newlst, newlsn)
    implicit none
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"

    integer             :: jbl, jvp, jbeta, jlistp
    integer             :: node, ndim
    real(kind=8)        :: newlsn, newlst, deltat
    real(kind=8)        :: vale(:)
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
! person_in_charge: patrick.massin at edf.fr
!
!
!     ------------------------------------------------------------------
!
!   xcaclulgeo   : calcul géométrique pour les points problematiques

!    ENTREE
!        NDIM    : DIMENSION DE L'ESPACE
!        JCOOR   : COORDONNE DES NOEUDS
!        JVP     : VECTEUR DES VITESSES DE PROPAGATION EN CHAQUE POINT
!                  DU DOMAINE DE CALCUL (MODULE DE LA VITESSE DU POINT
!                  PROJETE SUR LE FOND DE LA FISSURE)
!        JBL     : CHAM_NO_S DES VECTEURS NORMALE ET TANGENTIELLE DE LA
!                  BASE LOCALE IN CHAQUE NODE DU MAILLAGE
!        DELTAT  : TEMPS TOTAL DU PAS DE PROPAGATION
!        JNODTO  : LISTE DES NOEUDS DEFINISSANT LE DOMAINE DE CALCUL
!        NBNO    : NOMBRE DE NOEUD DU TORE DE CALCUL
!        JBETA   : VECTEUR DES ANGLES DE BIFURCATION DE LA FISSURE
!                  EN CHAQUE POINT DU DOMAINE DE CALCUL (ANGLE AU POINT
!                  PROJETE SUR LE FOND DE LA FISSURE)
!        JLISTP  : VECTEUR (A 3 COMPOSANTES) OU LES CORDONNEES DU
!                  PROJETE DE CHAQUE POINT DU DOMAINE DE CALCUL SUR LE
!                  FOND DE LA FISSURE SONT STOCKEES
!        NODE    : NOEUD PROBLEMATIQUE TROUVE ET CALCULE PAR METHODE GEOMETRIQUE
!        NEWLSN  : VALEUR DU NOEUD A MODIFIE
!        NEWLSN  : VALEUR DU NOEUD A MODIFIE
!
!    SORTIE
!
!        NEWLSN  : VALEUR DU NOEUD MODIFIE PAR METHODE GEOMETRIQUE
!        NEWLSN  : VALEUR DU NOEUD MODIFIE PAR METHODE GEOMETRIQUE
!
!     ------------------------------------------------------------------

    integer                      :: k, pos, pos1
    real(kind=8),dimension(ndim) :: t1, n1, p1
    real(kind=8)                 :: deltaa, cbeta, sbeta
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------

!     PROPAGATION VECTOR DELTA_A
    deltaa=zr(jvp-1+node)*deltat
!
!     STORE THE COS AND SIN OF THE PROPAGATION ANGLE
    cbeta = cos(zr(jbeta-1+node))
    sbeta = sin(zr(jbeta-1+node))
!
!     POINTERS INSIDE THE JEVEUX OBJECTS
    pos = 2*ndim*(node-1)
    pos1 = 3*(node-1)
!
!     RESET THE NEW VALUE OF THE TWO LEVEL SETS
    newlsn = 0.d0
    newlst = 0.d0
!
    do k = 1, ndim
!        NEW T-AXIS BY A RIGID ROTATION AT THE NEW CRACK TIP
        t1(k) = cbeta*zr(jbl-1+pos+ndim+k)+sbeta*zr(jbl-1+pos+k)
!        NEW N-AXIS BY A RIGID ROTATION AT THE NEW CRACK TIP
        n1(k) = cbeta*zr(jbl-1+pos+k)-sbeta*zr(jbl-1+pos+ndim+k)
!        NEW CRACK TIP POSITION
        p1(k) = zr(jlistp-1+pos1+k)+deltaa*t1(k)
!        NEW VALUES OF THE TWO LEVEL SETS
        newlsn = newlsn+(vale(pos1+k)-p1(k))*n1(k)
        newlst = newlst+(vale(pos1+k)-p1(k))*t1(k)
    end do

end subroutine

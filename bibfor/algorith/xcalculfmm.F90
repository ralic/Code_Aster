subroutine xcalculfmm(nbno, jvtemp, jcalculs, jcopiels, jnodto, jzero, ndim, &
                      nodvois, jltno, jvcn, jgrlr, jbl, jbeta, jlistp , jvp, &
                      jcoor, deltat, levset, signls)

   implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/xcalculgeo.h"


 integer           :: jvtemp, jcalculs, jcopiels, jzero, jnodto
 integer           :: jbl, jbeta, jlistp, jvp
 integer           :: jvcn, jgrlr, jltno, jcoor
 integer           :: ndim, nbno, nodvois
 real(kind=8)      :: deltat
 character(len=2)  :: levset
 character(len=3)  :: signls

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
! person_in_charge: patrick.massin at edf.fr
!
!       XCALCULFMM  : CALCUL EXPLICITE DES LEVEL SETS PAR FMM

!    ENTREE
!    ------
!      NBNO    = NOMBRE DE NOEUD DU TORE DE CALCUL
!      jVTEMP  = VECTEUR LOGIQUE INDIQUANT SI LE NOEUD EST CALCULE
!      JCALCULS= VECTEUR CONTENANT LES VALEURS DES LEVEL SETS A MODIFIER
!      JCOPIELS= VECTEUR CONTENANT LES ANCIENNES VALEURS DES LEVELS SETS
!      JNODTO  = LISTE DES NOEUDS DEFINISSANTS LE DOMAINE DE CALCUL
!      JZERO   = VECTEUR LOGIQUE INDIQUANT SI LA "VRAIE" LEVEL SET
!               (DISTANCE SIGNEE) A ETE CALCULEE POUR LE NOEUD
!      NDIM    = DIMENSION DE L'ESPACE
!      NODVOIS = NOEUD VOISIN DU MINIMUM TROUVER DAND LA NARROWBAND
!      JLTNO   = VALEUR DE LST POUR MODIFIER LSN PAR METHODE GEOMETRIQUE
!      JVCN    = VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      JGRLR   = VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      JBL     = CHAM_NO_S DES VECTEURS NORMALE ET TANGENTIELLE DE LA
!                BASE LOCALE IN CHAQUE NODE DU MAILLAGE
!      JBETA   = VECTEUR DES ANGLES DE BIFURCATION DE LA FISSURE
!                EN CHAQUE POINT DU DOMAINE DE CALCUL (ANGLE AU POINT
!                PROJETE SUR LE FOND DE LA FISSURE)
!      JLISTP  = VECTEUR (A 3 COMPOSANTES) OU LES CORDONNEES DU
!                PROJETE DE CHAQUE POINT DU DOMAINE DE CALCUL SUR LE
!                FOND DE LA FISSURE SONT STOCKEES
!      JVP     = VECTEUR DES VITESSES DE PROPAGATION EN CHAQUE POINT
!                DU DOMAINE DE CALCUL (MODULE DE LA VITESSE DU POINT
!                PROJETE SUR LE FOND DE LA FISSURE)
!      JCOOR   = COORDONNE DES NOEUDS
!      DELTAT  = TEMPS TOTAL DU PAS DE PROPAGATION
!      LEVSET  = INFORMATION SUR LA LEVEL SET : LSN OU LST
!      SIGNLS  = INFORMATION SUR LA CALCUL DE LA LEVEL AU-DESSUS DE L'ISOZERO OU EN-DESSOUS
!
!    SORTIE
!    ------
!     CALCULS = VECTEUR CONTENANT LES VALEURS DES LEVEL SETS MODIFIEES
!
!------------------------------------------------------------------------

    integer :: search, jvcnd, i
    real(kind=8) :: phi(3), h(3), signe, newlsn, newlst
! variables utilisees pour recuperer le minimum
    integer :: vmin(1), imin
    real(kind=8) :: v(2)

   !! initialisation variables
    jvcnd = jgrlr+10
    phi(1:3) = 0.d0
    h(1:3)   = 0.d0

!----------------DEBUT---------------------------------------------------

    !! recherche du noeud voisin à calculer!!
    search=0
    do i=1, nbno
        if (zi(jnodto-1+i) .eq. zi(jvcn-1+nodvois)) then
            search=i
            exit
        endif
    end do

    ! assertion : le numero de noeud voisin est dans le tore de calcul
    ASSERT(search.ne.0)

    !! min entre i-1 et i+1 dans les trois directions sur le noeud à calculer!!
    do  i= 1, ndim

        if (zi(jvcn-1+6*(search-1)+2*(i-1)+1) .eq. 0  ) then
            ! si i-1 n'est pas defini, on prend la valeur en i+1

            phi(i) = zr(jcalculs-1+zi(jvcn-1+6*(search-1)+2*(i-1)+2))
            h(i) = zr(jvcnd-1+ 6*(search-1)+2*(i-1)+2)
        elseif (zi(jvcn-1+6*(search-1)+2*(i-1)+2) .eq. 0 ) then
            ! si i+1 n'est pas defini, on prend la valeur en i-1

            phi(i) = zr(jcalculs-1+zi(jvcn-1+6*(search-1)+2*(i-1)+1))
            h(i) = zr(jvcnd-1+ 6*(search-1) +2*(i-1)+1)
        else
            ! si i-1 et i+1 ont definis, on prend la plus petite des deux

            ! recuperation des valeurs correspondant a i-1 et i+1
            v(1) = zr(jcalculs-1+zi(jvcn-1+6*(search-1)+2*(i-1)+1))
            v(2) = zr(jcalculs-1+zi(jvcn-1+6*(search-1)+2*(i-1)+2))

            ! calcul de l'indice imin realisant le minimum :
            !   * 1: si l miminimum correspond a la valeur i-1
            !   * 2: si l miminimum correspond a la valeur i+1
            vmin = minloc(v)
            imin = vmin(1)

            phi(i) = zr(jcalculs-1+zi(jvcn-1+6*(search-1)+2*(i-1)+imin))
            h(i) = zr(jvcnd-1+ 6*(search-1) + 2*(i-1)+imin)
        endif

    end do

    !! évalution du terme sous la racine pour verifier son signe
     signe = 0
     if( maxval(phi) .ne. r8gaem()) then
         if (ndim .eq. 3) then
             signe = (-1*2*phi(1)*h(2)**2.d0*h(3)**2.d0 - 2*phi(2)*h(1)**2.d0* &
                     h(3)**2.d0 - 2*phi(3)*h(2)**2.d0*h(1)**2.d0)**2.d0 - &
                     4*(h(2)**2.d0*h(3)**2.d0 + h(1)**2.d0*h(3)**2.d0 + &
                     h(1)**2.d0*h(2)**2.d0) * (phi(1)**2.d0*h(2)**2.d0* &
                     h(3)**2.d0 + phi(2)**2.d0*h(1)**2.d0*h(3)**2.d0 + phi(3)**2d0* &
                     h(1)**2.d0*h(2)**2.d0 - h(2)**2.d0*h(3)**2.d0*h(1)**2.d0)
         else
             signe = h(1)**2.d0*h(2)**2.d0*(h(1)**2.d0+ h(2)**2.d0 &
                     -phi(1)**2.d0+2*phi(1)*phi(2)-phi(2)**2d0)
         endif
     endif

    !!calcul exlicite de la valeur du noeud!!
     if ( maxval(phi) .ne. r8gaem() .and. signe .gt. 0) then
         if ( ndim .eq. 3 ) then
             zr(jcalculs-1+zi(jvcn-1+nodvois)) = ( 2*phi(1)*h(2)**2.d0*h(3)**2.d0 + &
                                                  2*phi(2)*h(1)**2.d0*h(3)**2.d0 + &
                                                  2*phi(3)*h(1)**2.d0*h(2)**2.d0 + &
                                                  sqrt( signe ) ) / ( 2*(h(2)**2.d0* &
                                                  h(3)**2.d0 + h(1)**2.d0*h(3)**2.d0 &
                                                  + h(1)**2.d0*h(2)**2.d0) )
         else
             zr(jcalculs-1+zi(jvcn-1+nodvois)) =  ( phi(2)*h(1)**2.d0+phi(1)* &
                                                  h(2)**2.d0 + sqrt( signe) ) &
                                                  / (h(1)**2.d0+h(2)**2.d0)
         endif

     else
         !calcul geometrique pour les noeuds problématiques
         call xcalculgeo(ndim, jcoor, jvp, jbl, deltat, jnodto, nbno, &
                         jbeta, jlistp, zi(jvcn-1+nodvois), newlst, newlsn)
         if (levset .eq. 'LN') then
             if (zr(jltno-1+zi(jvcn-1+nodvois)) .gt. 0.d0) then
                if (signls .eq. 'inf') then
                    zr(jcalculs-1+zi(jvcn-1+nodvois)) = -newlsn
                else
                    zr(jcalculs-1+zi(jvcn-1+nodvois)) = newlsn
                endif
             else
                 zr(jcalculs-1+zi(jvcn-1+nodvois)) = zr(jcopiels-1+zi(jvcn-1+nodvois))
             endif
         else
             if (signls .eq. 'inf') then
                 zr(jcalculs-1+zi(jvcn-1+nodvois)) = -newlst
             else
                 zr(jcalculs-1+zi(jvcn-1+nodvois)) = newlst
             endif
         endif
     endif
end subroutine

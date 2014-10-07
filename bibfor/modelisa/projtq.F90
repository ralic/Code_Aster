subroutine projtq(nbcnx, xyzma, icnx, x3dp, excent,&
                  itria, inoeu, icote, xbar, iproj)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!  DESCRIPTION : TEST D'APPARTENANCE DU POINT PROJETE X3DP(3)
!  -----------   AU DOMAINE GEOMETRIQUE DEFINI PAR UNE MAILLE
!                TRIANGLE OU QUADRANGLE
!
!                POUR CELA, ON CALCULE LES COORDONNEES BARYCENTRIQUES
!                DU POINT PROJETE
!                  - DANS LE TRIANGLE 1-2-3 POUR UNE MAILLE TRIANGLE
!                  - DANS LE TRIANGLE 1-2-3 PUIS SI NECESSAIRE DANS
!                    LE TRIANGLE 3-4-1 POUR UNE MAILLE QUADRANGLE
!                CETTE METHODE EST EXACTE POUR DES MAILLES A BORDS
!                DROITS SUPPORTANT DES ELEMENTS LINEAIRES, APPROCHEE
!                POUR DES MAILLES A BORDS COURBES SUPPORTANT DES
!                ELEMENTS QUADRATIQUES OU AUTRES
!
!                APPELANT : PROJKM
!
!  IN     : NBCNX  : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS DE LA MAILLE
!  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    CONTIENT LES COORDONNEES DES NOEUDS DE LA MAILLE
!  IN     : ICNX   : INTEGER , SCALAIRE
!                    RANG DU NOEUD BETON LE PLUS PROCHE DU POINT PROJETE
!                    DANS LA TABLE DE CONNECTIVITE DE LA MAILLE
!  IN     : X3DP   : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU POINT PROJETE
!  IN     : EXCENT : REAL*8 (POSITIF), SCALAIRE
!                    EXCENTREMENT (VAL ABS) DU POINT PAR RAPPORT A LA MAILLE
!  OUT    : ITRIA  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : INDICATEUR DU SOUS-DOMAINE
!                    AUQUEL APPARTIENT LE POINT PROJETE X3DP(3) :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  OUT    : INOEU  : INTEGER , SCALAIRE
!                    SI PROJECTION ECHOUE MAIS POSSIBLE PROJECTION
!                    SUR UN NOEUD, NUMERO DU NOEUD (1, 2, 3 ou 4)
!  OUT    : ICOTE  : INTEGER , SCALAIRE
!                    SI PROJECTION ECHOUE MAIS POSSIBLE PROJECTION
!                    SUR UN COTE, NUMERO DU COTE (1, 2, 3 ou 4)
!  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
!                    DU POINT PROJETE (BARYCENTRE DES SOMMETS DU
!                    TRIANGLE 1-2-3 OU 3-4-1)
!  OUT    : IPROJ  : INTEGER , SCALAIRE
!                    INDICE DE PROJECTION
!                    IPROJ = -1  PROJECTION NON REUSSIE
!                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
!                                DE LA MAILLE
!                    IPROJ =  1X LE POINT PROJETE EST SUR UNE FRONTIERE
!                                DE LA MAILLE
!                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
!                                NOEUDS DE LA MAILLE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterf_types.h"
#include "asterc/matfpe.h"
#include "asterc/r8prem.h"
#include "asterfort/analybar.h"
#include "asterfort/assert.h"
#include "asterfort/tstbar.h"
#include "blas/dnrm2.h"
    integer :: nbcnx, icnx, itria, inoeu, icote, iproj
    real(kind=8) :: xyzma(3, *), x3dp(*), excent, xbar(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: ino, nbsom
    real(kind=8) :: d, dx, dy, dz, epsg, nrm2, r8bid3(3)
    aster_logical :: notlin
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call matfpe(-1)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   INITIALISATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    epsg = 1.0d+08 * r8prem()
    r8bid3(1) = 0.d0
    r8bid3(2) = 0.d0
    r8bid3(3) = 0.d0
!
    notlin = (nbcnx.gt.4)
    if ((nbcnx.eq.3) .or. (nbcnx.eq.6)) then
        nbsom = 3
    else
        nbsom = 4
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   TEST POUR UNE MAILLE TRIANGLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (nbsom .eq. 3) then
!
        itria = 1
!
!....... TEST D'APPARTENANCE AU TRIANGLE 1-2-3, PAR DETERMINATION DES
!....... COORDONNEES BARYCENTRIQUES
!
        call tstbar(3, xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), r8bid3,&
                    x3dp, xbar, iproj)
        if (iproj.lt.0)then
            call analybar(xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), x3dp,&
                    xbar, excent, iproj, inoeu, icote)
        endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   TESTS POUR UNE MAILLE QUADRANGLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 3.1 APPARTENANCE PLUS PROBABLE AU TRIANGLE 3-4-1
! ---
    else if ((icnx.eq.4).or.(icnx.eq.7).or.(icnx.eq.8)) then
!
        itria = 2
!
!....... TEST D'APPARTENANCE AU TRIANGLE 3-4-1, PAR DETERMINATION DES
!....... COORDONNEES BARYCENTRIQUES
!
        call tstbar(3, xyzma(1, 3), xyzma(1, 4), xyzma(1, 1), r8bid3,&
                    x3dp(1), xbar(1), iproj)
!
!....... REAJUSTEMENT DE IPROJ SI APPARTENANCE A UN BORD
!
        if (iproj .eq. 11) then
            iproj = 13
        else if (iproj.eq.12) then
            iproj = 14
        else if (iproj.eq.13) then
            iproj = 0
        else if (iproj.lt.0) then
            call analybar(xyzma(1, 3), xyzma(1, 4), xyzma(1, 1), x3dp,&
                    xbar, excent, iproj, inoeu, icote)
            if (iproj.lt.0)then
                if (.not. (xbar(2).lt.0.d0 .and. xbar(1).ge.0.d0 & 
                                           .and. xbar(3).ge.0.d0)) then
                    goto 999
                endif
            elseif (iproj .eq. 20 .and. icote .eq. 3)then
                iproj = -1
            elseif (iproj .eq. 20)then
                if (icote .eq. 1)then
                    icote = 3
                else if (icote .eq. 2)then
                    icote = 4
                else 
!                   on ne doit pas passer par la
                    ASSERT(.false.)
                endif        
            elseif (iproj .eq. 30)then
                if (inoeu .eq. 1)then
                    inoeu = 3
                else if (inoeu .eq. 2)then
                    inoeu = 4
                else if (inoeu .eq. 3)then
                    inoeu = 1
                else 
                    ASSERT(.false.)
                endif    
            else
                ASSERT(.false.)
            endif
        endif
!
!....... EN CAS D'ECHEC TEST D'APPARTENANCE AU TRIANGLE 1-2-3,
!....... PAR DETERMINATION DES COORDONNEES BARYCENTRIQUES
!
        if (iproj.lt.0)then
            itria = 1
            call tstbar(3, xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), r8bid3,&
                        x3dp(1), xbar(1), iproj)
!.......... REAJUSTEMENT DE IPROJ SI PROJECTION SUR LE TROISIEME COTE
!.......... DU TRIANGLE 1-2-3
            if (iproj .eq. 13) then
!           on ne doit normalement pas passer par la ! 
                iproj = 0
            else if (iproj.lt.0) then
                call analybar(xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), x3dp,&
                        xbar, excent, iproj, inoeu, icote)
                if (iproj.lt.0)then
                        goto 999
                elseif (iproj .eq. 20)then
                    ASSERT(icote.eq.1 .or. icote.eq.2)
                elseif (iproj .eq. 30)then
                    ASSERT(inoeu.eq.2)
                else
                    ASSERT(.false.)
                endif
            endif
        endif
!
! 3.2 APPARTENANCE PLUS PROBABLE AU TRIANGLE 1-2-3
! ---
    else
!
        itria = 1
!
!....... TEST D'APPARTENANCE AU TRIANGLE 1-2-3, PAR DETERMINATION DES
!....... COORDONNEES BARYCENTRIQUES
!
        call tstbar(3, xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), r8bid3,&
                    x3dp(1), xbar(1), iproj)
!
!....... REAJUSTEMENT DE IPROJ SI PROJECTION SUR LE TROISIEME COTE
!....... DU TRIANGLE 1-2-3
!
        if (iproj .eq. 13) then
            iproj = 0
        else if (iproj.lt.0) then
            call analybar(xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), x3dp,&
                    xbar, excent, iproj, inoeu, icote)
            if (iproj.lt.0)then
                if (.not.(xbar(2).lt.0.d0 .and. xbar(1).ge.0.d0 & 
                                          .and. xbar(3).ge.0.d0)) then
                    goto 999
                endif
            elseif (iproj .eq. 20 .and. icote .eq. 3)then
                iproj = -1
            endif
        endif
!
!....... EN CAS D'ECHEC TEST D'APPARTENANCE AU TRIANGLE 3-4-1,
!....... PAR DETERMINATION DES COORDONNEES BARYCENTRIQUES
!
        if (iproj.lt.0)then
            itria = 2
            call tstbar(3, xyzma(1, 3), xyzma(1, 4), xyzma(1, 1), r8bid3,&
                        x3dp(1), xbar(1), iproj)
!.......... REAJUSTEMENT DE IPROJ SI APPARTENANCE A UN BORD
            if (iproj .eq. 11) then
                iproj = 13
            else if (iproj.eq.12) then
                iproj = 14
            else if (iproj.eq.13) then
                iproj = 0
            else if (iproj.lt.0) then
                call analybar(xyzma(1, 3), xyzma(1, 4), xyzma(1, 1), x3dp,&
                        xbar, excent, iproj, inoeu, icote)
                if (iproj.lt.0)then
                        goto 999
                elseif (iproj .eq. 20)then
                    if (icote .eq. 1)then
                        icote = 3
                    else if (icote .eq. 2)then
                        icote = 4
                    else 
                        ASSERT(.false.)
                    endif            
                elseif (iproj .eq. 30)then
                    ASSERT(inoeu.eq.2)
                    inoeu = 4
                else
                    ASSERT(.false.)
                endif
            endif
        endif
!
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   TESTS COMPLEMENTAIRES POUR LES MAILLES A BORDS COURBES SUPPORTANT
!     DES ELEMENTS NON LINEAIRES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (notlin) then
!
! 4.1    TEST DE COINCIDENCE AVEC UN NOEUD MILIEU
! ---
        if (iproj .gt. 10) then
            ino = iproj - 10 + nbsom
            nrm2 = dnrm2(3,xyzma(1,ino),1)
            if (nrm2 .eq. 0.0d0) nrm2 = 1.0d0
            dx = xyzma(1,ino) - x3dp(1)
            dy = xyzma(2,ino) - x3dp(2)
            dz = xyzma(3,ino) - x3dp(3)
            d = dble ( sqrt ( dx*dx + dy*dy + dz*dz ) )
            if (d/nrm2 .lt. epsg) iproj = 2
        endif
!
! 4.2    TEST DE COINCIDENCE AVEC LE NOEUD CENTRAL POUR UNE MAILLE QUAD9
! ---
        if ((iproj.eq.0) .and. (nbcnx.eq.9)) then
            nrm2 = dnrm2(3,xyzma(1,9),1)
            if (nrm2 .eq. 0.0d0) nrm2 = 1.0d0
            dx = xyzma(1,9) - x3dp(1)
            dy = xyzma(2,9) - x3dp(2)
            dz = xyzma(3,9) - x3dp(3)
            d = dble ( sqrt ( dx*dx + dy*dy + dz*dz ) )
            if (d/nrm2 .lt. epsg) iproj = 2
        endif
!
    endif
!
! --- FIN DE PROJTQ.
999 continue
    call matfpe(1)
!
end subroutine

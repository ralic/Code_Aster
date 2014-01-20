subroutine projkb(mailla, x3dca, lnuma, licnx, numail,&
                  nbcnx, cxma, xyzma, normal, itria,&
                  xbar, iproj, excent)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION : TENTATIVE DE PROJECTION D'UN NOEUD CABLE SUR LES BORDS
!  -----------   DES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON LE
!                PLUS PROCHE
!                APPELANT : PROJCA
!
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU NOEUD CABLE CONSIDERE
!  IN     : LNUMA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS CONTENANT LES NUMEROS
!                    DES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON
!                    LE PLUS PROCHE DU NOEUD CABLE
!  IN     : LICNX  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS CONTENANT LES RANGS DU
!                    NOEUD BETON LE PLUS PROCHE DANS LES TABLES DE
!                    CONNECTIVITE DES MAILLES AUXQUELLES IL APPARTIENT
!  OUT    : NUMAIL : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : NUMERO DE LA MAILLE
!                    A LAQUELLE APPARTIENT LE BORD SUR LEQUEL EST
!                    REALISEE LA PROJECTION
!  OUT    : NBCNX  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : NOMBRE DE NOEUDS DE LA
!                    MAILLE A LAQUELLE APPARTIENT LE BORD SUR LEQUEL
!                    EST REALISEE LA PROJECTION
!  OUT    : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
!                    SI PROJECTION REUSSIE : NUMEROS DES NOEUDS DE LA
!                    MAILLE A LAQUELLE APPARTIENT LE BORD SUR LEQUEL
!                    EST REALISEE LA PROJECTION
!                    (TABLE DE CONNECTIVITE)
!  OUT    : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    SI PROJECTION REUSSIE : TABLEAU DES COORDONNEES
!                    DES NOEUDS DE LA MAILLE A LAQUELLE APPARTIENT LE
!                    BORD SUR LEQUEL EST REALISEE LA PROJECTION
!  OUT    : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES DANS LE REPERE
!                    GLOBAL DU VECTEUR NORMAL AU BORD SUR LEQUEL EST
!                    REALISEE LA PROJECTION
!  OUT    : ITRIA  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : INDICATEUR DU SOUS-DOMAINE
!                    AUQUEL APPARTIENT LE POINT PROJETE :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
!                    DU POINT PROJETE (BARYCENTRE DES SOMMETS DU
!                    TRIANGLE 1-2-3 OU 3-4-1)
!  OUT    : IPROJ  : INTEGER , SCALAIRE
!                    INDICE DE PROJECTION
!                    IPROJ = -1  PROJECTION NON REUSSIE
!                    IPROJ =  1X NUMERO DU BORD SUR LEQUEL EST REALISEE
!                                LA PROJECTION + 10
!                                (NUMERO D'APPARTENANCE SUR LA MAILLE)
!                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
!                                NOEUDS EXTREMITES DU BORD
!  OUT    : EXCENT : REAL*8 , SCALAIRE
!                    SI PROJECTION REUSSIE : EXCENTRICITE DU NOEUD
!                    CABLE PAR RAPPORT AU BORD SUR LEQUEL EST REALISEE
!                    LA PROJECTION
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
!
#include "asterc/matfpe.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/projsg.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dnrm2.h"
    character(len=8) :: mailla
    character(len=19) :: lnuma, licnx
    integer :: numail, nbcnx, cxma(*), itria, iproj
    real(kind=8) :: x3dca(*), xyzma(3, *), normal(*), xbar(*), excent
!
! VARIABLES LOCALES
! -----------------
    integer :: i1, i2, i3, icnx, ima, imail, inoma, jcoor, jcxma, jlnuma, jlicnx
    integer ::  nbmaok, nbsom, noe, somn12, somn23
    real(kind=8) :: d, dx, dy, dz, epsg, nrm2, x3dp(3), xbw(2)
    character(len=24) :: conxma, coorno
    logical :: dejavu
    integer, pointer :: somno_bord(:) => null()
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call matfpe(-1)
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX OBJETS UTILES - CREATION DES OBJETS DE TRAVAIL
!     INITIALISATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    conxma = mailla//'.CONNEX'
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
!
    call jelira(lnuma, 'LONUTI', nbmaok)
    call jeveuo(lnuma, 'L', jlnuma)
    call jeveuo(licnx, 'L', jlicnx)
!
    AS_ALLOCATE(vi=somno_bord, size=2*nbmaok)
!
    epsg = 1.0d+08 * r8prem()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   TENTATIVE DE PROJECTION DU NOEUD CABLE CONSIDERE SUR LES BORDS DES
!     MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON LE PLUS PROCHE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!.... BOUCLE SUR LES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON LE
!.... PLUS PROCHE, AFIN DE DETERMINER LES BORDS SUR LESQUELS ON TENTE
!.... LA PROJECTION DU NOEUD CABLE
!
    do 10 imail = 1, nbmaok
!
        icnx = zi(jlicnx+imail-1)
!....... SI LE NOEUD BETON LE PLUS PROCHE EST LE NOEUD CENTRAL D'UNE
!....... MAILLE QUAD9, PAS DE TENTATIVE DE PROJECTION SUR UN BORD
        if (icnx .eq. 9) then
            iproj = -1
            goto 10
        endif
        numail = zi(jlnuma+imail-1)
!
! 2.1    RECUPERATION DES INFORMATIONS CARACTERISANT LA TOPOLOGIE
! ---    DE LA MAILLE
!
        call jelira(jexnum(conxma, numail), 'LONMAX', nbcnx)
        call jeveuo(jexnum(conxma, numail), 'L', jcxma)
        if ((nbcnx.eq.3) .or. (nbcnx.eq.6)) then
            nbsom = 3
        else
            nbsom = 4
        endif
!
! 2.2    RECUPERATION DES NUMEROS ET DES COORDONNEES DES NOEUDS
! ---    DE LA MAILLE
!
        do 20 inoma = 1, nbcnx
            noe = zi(jcxma+inoma-1)
            cxma(inoma) = noe
            xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
            xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
            xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
20      continue
!
! 2.3    SI LE NOEUD BETON LE PLUS PROCHE EST UN NOEUD MILIEU D'UN BORD
! ---    DE LA MAILLE, LA PROJECTION NE PEUT ETRE TENTEE QUE SUR LE BORD
!        DONT CE NOEUD EST LE MILIEU
!
        if (icnx .gt. nbsom) then
            i1 = icnx - nbsom
            i2 = i1 + 1
            if (i2 .gt. nbsom) i2 = 1
            somn12 = cxma(i1) + cxma(i2)
            somno_bord(1+2*(imail-1) ) = somn12
            somno_bord(1+2*(imail-1)+1) = 0
!
! 2.3.1     ON VERIFIE QUE LA TENTATIVE DE PROJECTION SUR LE BORD N'A
! .....     PAS DEJA ETE EFFECTUEE (SUR UNE MAILLE LIMITROPHE)
!
            dejavu = .false.
            if (imail .gt. 1) then
                do 30 ima = 1, imail-1
                    if ((somno_bord(1+2*(ima-1) ).eq.somn12) .or.&
                        (somno_bord(1+2*(ima-1)+1).eq.somn12)) dejavu = .true.
                    if (dejavu) goto 10
30              continue
            endif
!
! 2.3.2     TENTATIVE DE PROJECTION SUR LE BORD
! .....
!
! 2.3.2.1   TENTATIVE DE PROJECTION
! .......
            call projsg(x3dca(1), xyzma(1, i1), xyzma(1, i2), normal(1), x3dp(1),&
                        xbw(1), iproj, excent)
!
! 2.3.2.2   REAJUSTEMENT DE IPROJ, AFFECTATION DE ITRIA ET XBAR
! .......   SI PROJECTION REUSSIE
!
            if (iproj .eq. 0) then
                iproj = i1 + 10
!............. TEST DE COINCIDENCE AVEC LE NOEUD MILIEU DU BORD (NOEUD
!............. BETON LE PLUS PROCHE)
                nrm2 = dnrm2(3,xyzma(1,icnx),1)
                if (nrm2 .eq. 0.0d0) nrm2 = 1.0d0
                dx = xyzma(1,icnx) - x3dp(1)
                dy = xyzma(2,icnx) - x3dp(2)
                dz = xyzma(3,icnx) - x3dp(3)
                d = dble ( sqrt ( dx*dx + dy*dy + dz*dz ) )
                if (d/nrm2 .lt. epsg) iproj = 2
!............. AFFECTATION DE ITRIA ET XBAR
                if (nbsom .eq. 3) then
                    itria = 1
                    if (i1 .eq. 1) then
                        xbar(1) = xbw(1)
                        xbar(2) = xbw(2)
                        xbar(3) = 0.0d0
                    else if (i1.eq.2) then
                        xbar(1) = 0.0d0
                        xbar(2) = xbw(1)
                        xbar(3) = xbw(2)
                    else
                        xbar(1) = xbw(2)
                        xbar(2) = 0.0d0
                        xbar(3) = xbw(1)
                    endif
                else
                    if (i1 .lt. 3) then
                        itria = 1
                    else
                        itria = 2
                    endif
                    if (mod(i1,2) .eq. 1) then
                        xbar(1) = xbw(1)
                        xbar(2) = xbw(2)
                        xbar(3) = 0.0d0
                    else
                        xbar(1) = 0.0d0
                        xbar(2) = xbw(1)
                        xbar(3) = xbw(2)
                    endif
                endif
            endif
!
! 2.3.2.3   SORTIE SI PROJECTION REUSSIE
! .......
            if (iproj .gt. 0) goto 9999
!
! 2.4    SI LE NOEUD BETON LE PLUS PROCHE EST UN NOEUD SOMMET DE LA
! ---    MAILLE, LA PROJECTION PEUT ETRE TENTEE SUR DEUX BORDS
!
        else
            i1 = icnx - 1
            if (i1 .lt. 1) i1 = nbsom
            i2 = icnx
            i3 = icnx + 1
            if (i3 .gt. nbsom) i3 = 1
            somn12 = cxma(i1) + cxma(i2)
            somn23 = cxma(i2) + cxma(i3)
            somno_bord(1+2*(imail-1) ) = somn12
            somno_bord(1+2*(imail-1)+1) = somn23
!
! 2.4.1     ON VERIFIE QUE LA TENTATIVE DE PROJECTION SUR LE PREMIER
! .....     BORD N'A PAS DEJA ETE EFFECTUEE (SUR UNE MAILLE LIMITROPHE)
!
            dejavu = .false.
            if (imail .gt. 1) then
                do 40 ima = 1, imail-1
                    if ((somno_bord(1+2*(ima-1) ).eq.somn12) .or.&
                        (somno_bord(1+2*(ima-1)+1).eq.somn12)) dejavu = .true.
                    if (dejavu) goto 41
40              continue
41              continue
            endif
!
! 2.4.2     TENTATIVE DE PROJECTION SUR LE PREMIER BORD
! .....
            if (.not.dejavu) then
!
! 2.4.2.1      TENTATIVE DE PROJECTION
! .......
                call projsg(x3dca(1), xyzma(1, i1), xyzma(1, i2), normal( 1), x3dp(1),&
                            xbw(1), iproj, excent)
!
! 2.4.2.2      REAJUSTEMENT DE IPROJ, AFFECTATION DE ITRIA ET XBAR
! .......      SI PROJECTION REUSSIE
!
                if (iproj .eq. 0) then
                    iproj = i1 + 10
!................ PAS DE TEST DE COINCIDENCE AVEC LE NOEUD MILIEU :
!................ SI TEL ETAIT LE CAS, ON SERAIT PASSE EN 2.3
!................ AFFECTATION DE ITRIA ET XBAR
                    if (nbsom .eq. 3) then
                        itria = 1
                        if (i1 .eq. 1) then
                            xbar(1) = xbw(1)
                            xbar(2) = xbw(2)
                            xbar(3) = 0.0d0
                        else if (i1.eq.2) then
                            xbar(1) = 0.0d0
                            xbar(2) = xbw(1)
                            xbar(3) = xbw(2)
                        else
                            xbar(1) = xbw(2)
                            xbar(2) = 0.0d0
                            xbar(3) = xbw(1)
                        endif
                    else
                        if (i1 .lt. 3) then
                            itria = 1
                        else
                            itria = 2
                        endif
                        if (mod(i1,2) .eq. 1) then
                            xbar(1) = xbw(1)
                            xbar(2) = xbw(2)
                            xbar(3) = 0.0d0
                        else
                            xbar(1) = 0.0d0
                            xbar(2) = xbw(1)
                            xbar(3) = xbw(2)
                        endif
                    endif
                endif
!
! 2.4.2.3      SORTIE SI PROJECTION REUSSIE
! .......
                if (iproj .gt. 0) goto 9999
!
            endif
!
! 2.4.3     ON VERIFIE QUE LA TENTATIVE DE PROJECTION SUR LE DEUXIEME
! .....     BORD N'A PAS DEJA ETE EFFECTUEE (SUR UNE MAILLE LIMITROPHE)
!
            dejavu = .false.
            if (imail .gt. 1) then
                do 50 ima = 1, imail-1
                    if ((somno_bord(1+2*(ima-1) ).eq.somn23) .or.&
                        (somno_bord(1+2*(ima-1)+1).eq.somn23)) dejavu = .true.
                    if (dejavu) goto 10
50              continue
            endif
!
! 2.4.4     TENTATIVE DE PROJECTION SUR LE DEUXIEME BORD
! .....
!
! 2.4.4.1   TENTATIVE DE PROJECTION
! .......
            call projsg(x3dca(1), xyzma(1, i2), xyzma(1, i3), normal(1), x3dp(1),&
                        xbw(1), iproj, excent)
!
! 2.4.4.2   REAJUSTEMENT DE IPROJ, AFFECTATION DE ITRIA ET XBAR
! .......   SI PROJECTION REUSSIE
!
            if (iproj .eq. 0) then
                iproj = i2 + 10
!............. PAS DE TEST DE COINCIDENCE AVEC LE NOEUD MILIEU :
!............. SI TEL ETAIT LE CAS, ON SERAIT PASSE EN 2.3
!............. AFFECTATION DE ITRIA ET XBAR
                if (nbsom .eq. 3) then
                    itria = 1
                    if (i2 .eq. 1) then
                        xbar(1) = xbw(1)
                        xbar(2) = xbw(2)
                        xbar(3) = 0.0d0
                    else if (i2.eq.2) then
                        xbar(1) = 0.0d0
                        xbar(2) = xbw(1)
                        xbar(3) = xbw(2)
                    else
                        xbar(1) = xbw(2)
                        xbar(2) = 0.0d0
                        xbar(3) = xbw(1)
                    endif
                else
                    if (i2 .lt. 3) then
                        itria = 1
                    else
                        itria = 2
                    endif
                    if (mod(i2,2) .eq. 1) then
                        xbar(1) = xbw(1)
                        xbar(2) = xbw(2)
                        xbar(3) = 0.0d0
                    else
                        xbar(1) = 0.0d0
                        xbar(2) = xbw(1)
                        xbar(3) = xbw(2)
                    endif
                endif
            endif
!
! 2.4.4.3   SORTIE SI PROJECTION REUSSIE
! .......
            if (iproj .gt. 0) goto 9999
!
        endif
!
10  end do
!
9999  continue
    AS_DEALLOCATE(vi=somno_bord)
    call jedema()
!
! --- FIN DE PROJKB.
    call matfpe(1)
!
end subroutine

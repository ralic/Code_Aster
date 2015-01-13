subroutine reci2d(lirela, mailla, nnoeca, noebe, nbcnx,&
                  cxma, normal, itria, xbar, iproj,&
                  excent)
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
!  DESCRIPTION : DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES DDLS
!  -----------   D'UN NOEUD DU CABLE ET LES DDLS DES NOEUDS VOISINS DE
!                LA STRUCTURE BETON
!                CAS OU LA STRUCTURE BETON EST MODELISEE PAR DES
!                ELEMENTS 2D
!                APPELANT : PROJCA
!
!  IN     : LIRELA : CHARACTER*19 , SCALAIRE
!                    NOM DE LA SD DE TYPE LISTE_DE_RELATIONS
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : NNOECA : CHARACTER*8 , SCALAIRE
!                    NOM DU NOEUD DU CABLE
!  IN     : NOEBE  : INTEGER , SCALAIRE
!                    NUMERO DU NOEUD VOISIN DE LA STRUCTURE BETON LE
!                    PLUS PROCHE DU NOEUD DU CABLE
!  IN     : NBCNX  : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS DE LA MAILLE VOISINE DE LA
!                    STRUCTURE BETON
!  IN     : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
!                    CONTIENT LES NUMEROS DES NOEUDS DE LA MAILLE
!                    VOISINE DE LA STRUCTURE BETON
!                    (TABLE DE CONNECTIVITE)
!  IN     : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DANS LE REPERE GLOBAL DU VECTEUR NORMAL
!                    AU PLAN MOYEN DE LA MAILLE VOISINE DE LA STRUCTURE
!                    BETON
!  IN     : ITRIA  : INTEGER , SCALAIRE
!                    INDICATEUR DU SOUS-DOMAINE AUQUEL APPARTIENT LE
!                    POINT PROJETE :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI IPROJ.NE.2 : COORDONNEES BARYCENTRIQUES DU POINT
!                    PROJETE (BARYCENTRE DES SOMMETS DU TRIANGLE 1-2-3
!                    OU 3-4-1)
!  IN     : IPROJ  : INTEGER , SCALAIRE
!                    INDICE DE PROJECTION
!                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
!                                DE LA MAILLE VOISINE
!                    IPROJ =  1X LE POINT PROJETE EST SUR UNE FRONTIERE
!                                DE LA MAILLE VOISINE
!                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
!                                NOEUDS DE LA MAILLE VOISINE
!  IN     : EXCENT : REAL*8 , SCALAIRE
!                    EXCENTRICITE DU NOEUD DU CABLE PAR RAPPORT A LA
!                    MAILLE VOISINE DE LA STRUCTURE BETON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/afrela.h"
#include "asterfort/ante2d.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!
! ARGUMENTS
! ---------
    character(len=19) :: lirela
    character(len=8) :: mailla, nnoeca
    integer :: noebe, nbcnx, cxma(*), itria, iproj, nno
    real(kind=8) :: normal(*), xbar(*), excent
!
! VARIABLES LOCALES
! -----------------
    integer :: i1, i2, i3, ibloc, icnx, iterm, ind
    integer :: nbbloc, nbsom, nbterm, nbtmax, nnomax, noeca
    real(kind=8) :: ksi1, ksi2, zero
    complex(kind=8) :: cbid
    character(len=8) :: k8b
    character(len=24) :: nonoma
    aster_logical :: notlin, l_excent
!
    real(kind=8) :: ffel2d, x(2), ff(9)
    real(kind=8), pointer :: coemur(:) => null()
    integer, pointer :: dimens(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    character(len=8), pointer :: nomddl(:) => null()
    character(len=8), pointer :: nomnoe(:) => null()
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   CREATION DES OBJETS DE TRAVAIL - INITIALISATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    nnomax = 9
    nbtmax = 1 + 2*nnomax
    AS_ALLOCATE(vr=coemur, size=nbtmax)
    AS_ALLOCATE(vk8=nomddl, size=nbtmax)
    AS_ALLOCATE(vk8=nomnoe, size=nbtmax)
    AS_ALLOCATE(vi=dimens, size=nbtmax)
    AS_ALLOCATE(vr=direct, size=3*nbtmax)
!
    notlin = (nbcnx.gt.4)
    if ((nbcnx.eq.3) .or. (nbcnx.eq.6)) then
        nbsom = 3
    else
        nbsom = 4
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   DETERMINATION DE L'ANTECEDENT DU POINT PROJETE DANS L'ELEMENT
!     DE REFERENCE ASSOCIE A L'ELEMENT REEL
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (iproj .ne. 2) call ante2d(itria, xbar(1), ksi1, ksi2)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   DETERMINATION DES RELATIONS CINEMATIQUES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    zero = 0.0d0
!
    nonoma = mailla//'.NOMNOE'
!
    nomnoe(1) = nnoeca
    nomddl(1) = 'DEPL'
    coemur(1) = 1.0d0
    
    l_excent = .true.
    if (excent .eq. 0.0d0) l_excent = .false.

!
    if (iproj .eq. 2) then
!
!       pas de liaisons si les noeuds sont les mêmes
        call jenonu(jexnom(nonoma, nnoeca), noeca)
        if (noeca .eq. noebe) goto 110
!
        nbterm = 2
        call jenuno(jexnum(nonoma, noebe), nomnoe(1+1))
        nomddl(1+1) = 'DEPL'
        coemur(1+1) = -1.0d0
!
        if (l_excent)then
            nbterm = 3
            nomnoe(1+2) = nomnoe(1+1)
            nomddl(1+2) = 'ROTA'
            coemur(1+2) = -excent
        endif
!
    else
        if (nbcnx .eq. 3) then
            ff(1) = 0.5d0* (1.0d0+ksi2)
            ff(2) = -0.5d0* (ksi1+ksi2)
            ff(3) = 0.5d0* (1.0d0+ksi1)
        else if (nbcnx.eq.6) then
            ff(1) = 0.5d0* (1.0d0+ksi2)*ksi2
            ff(2) = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+ 1.0d0)
            ff(3) = 0.5d0* (1.0d0+ksi1)*ksi1
            ff(4) = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
            ff(5) = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
            ff(6) = (1.0d0+ksi1)* (1.0d0+ksi2)
        else
            x(1) = ksi1
            x(2) = ksi2
            if (nbcnx.eq.4) then
                call elrfvf('QU4', x, 4, ff, nno)
            else if (nbcnx.eq.8) then
                call elrfvf('QU8', x, 8, ff, nno)
            else if (nbcnx.eq.9) then
                call elrfvf('QU9', x, 9, ff, nno)
            endif
            ffel2d = ff(1)
            ff(1) = ff(4)
            ff(4) = ff(3)
            ff(3) = ff(2)
            ff(2) = ffel2d 
            if (nbcnx.ge.8) then
                ffel2d = ff(5)
                ff(5) = ff(8)
                ff(8) = ff(7)
                ff(7) = ff(6)
                ff(6) = ffel2d
            endif
        endif
!
        if (iproj .gt. 10) then
!
            nbterm = 3
            i1 = iproj - 10
            i2 = i1 + 1
            if (i2 .gt. nbsom) i2 = 1
            if (l_excent) then 
                ind = 3
            else
                ind = 2
            endif
            call jenuno(jexnum(nonoma, cxma(i1)), nomnoe(1+1))
            call jenuno(jexnum(nonoma, cxma(i2)), nomnoe(1+ind))
            nomddl(1+1) = 'DEPL'
            nomddl(1+ind) = 'DEPL'
!
            coemur(1+1) = -ff(i1)
            coemur(1+ind) = -ff(i2)
!
            if (l_excent)then          
                nbterm = 5
                nomnoe(1+2) = nomnoe(1+1)
                nomnoe(1+4) = nomnoe(1+ind)
                nomddl(1+2) = 'ROTA'
                nomddl(1+4) = 'ROTA'
                coemur(1+2) = excent*coemur(1+1)
                coemur(1+4) = excent*coemur(1+ind)
            endif
!
            if (notlin) then
                nbterm = nbterm +1
                i3 = i1 + nbsom
                ind = 3
                if(l_excent) ind = 5
                call jenuno(jexnum(nonoma, cxma(i3)), nomnoe(1+ind))
                nomddl(1+ind) = 'DEPL'
!
                coemur(1+ind) = -ff(i3)
!
                if (l_excent)then
                    nbterm = 7
                    nomnoe(1+6) = nomnoe(1+ind)
                    nomddl(1+6) = 'ROTA'
                    coemur(1+6) = excent*coemur(1+ind)
                endif
            endif
!
        else
!
            nbterm = 1 + nbcnx
            if (l_excent) nbterm = 1 + 2*nbcnx
            do icnx = 1, nbcnx
                ind = icnx
                if (l_excent) ind = 2*icnx - 1
                call jenuno(jexnum(nonoma, cxma(icnx)), nomnoe(1+ ind))
                nomddl(1+ind) = 'DEPL'
                coemur(1+ind) = -ff(icnx)
!
                if (l_excent)then
                    nomnoe(1+ind+1) = nomnoe(1+ind)
                    nomddl(1+ind+1) = 'ROTA'
                    coemur(1+ind+1) = excent*coemur(1+ind)
                endif
            end do
!
        endif
    endif
!
!....... UNE RELATION PAR DDL DE TRANSLATION DU NOEUD DU CABLE
!        .....................................................
!
!....... LE VECTEUR ZI(JDIME) DOIT ETRE REINITIALISE AFIN DE PRENDRE
!....... EN COMPTE LES DIFFERENTS COEFFICIENTS PAR DIRECTION DEFINIS
!....... DANS LE VECTEUR ZR(JDIREC)
!
    do iterm = 1, nbterm
        dimens(iterm) = 3
    end do
!
    nbbloc = nbterm-1
    ind = 3
    if (l_excent) then
        ind = 6
        nbbloc = (nbterm-1)/2
    endif
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA PREMIERE RELATION (DDL DX)
!....... PUIS AFFECTATION
!
    direct(1) = 1.0d0
    direct(1+1) = 0.0d0
    direct(1+2) = 0.0d0
    do ibloc = 1, nbbloc
        direct(1+3+ind* (ibloc-1)) = 1.0d0
        direct(1+3+ind* (ibloc-1)+1) = 0.0d0
        direct(1+3+ind* (ibloc-1)+2) = 0.0d0
        if (l_excent) then
            direct(1+3+6* (ibloc-1)+3) = 0.0d0
            direct(1+3+6* (ibloc-1)+4) = normal(3)
            direct(1+3+6* (ibloc-1)+5) = -normal(2)
        endif
    end do
!
    call afrela(coemur, [cbid], nomddl, nomnoe, dimens,&
                direct, nbterm, zero, cbid, k8b,&
                'REEL', 'REEL', '12', 0.d0, lirela)
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA DEUXIEME RELATION (DDL DY)
!....... PUIS AFFECTATION
!
    direct(1) = 0.0d0
    direct(1+1) = 1.0d0
    direct(1+2) = 0.0d0
    do ibloc = 1, nbbloc
        direct(1+3+ind* (ibloc-1)) = 0.0d0
        direct(1+3+ind* (ibloc-1)+1) = 1.0d0
        direct(1+3+ind* (ibloc-1)+2) = 0.0d0
        if (l_excent)then
            direct(1+3+6* (ibloc-1)+3) = -normal(3)
            direct(1+3+6* (ibloc-1)+4) = 0.0d0
            direct(1+3+6* (ibloc-1)+5) = normal(1)
        endif
    end do
!
    call afrela(coemur, [cbid], nomddl, nomnoe, dimens,&
                direct, nbterm, zero, cbid, k8b,&
                'REEL', 'REEL', '12', 0.d0, lirela)
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA TROISIEME RELATION (DDL DZ)
!....... PUIS AFFECTATION
!
    direct(1) = 0.0d0
    direct(1+1) = 0.0d0
    direct(1+2) = 1.0d0
    do ibloc = 1, nbbloc
        direct(1+3+ind* (ibloc-1)) = 0.0d0
        direct(1+3+ind* (ibloc-1)+1) = 0.0d0
        direct(1+3+ind* (ibloc-1)+2) = 1.0d0
        if (l_excent) then
            direct(1+3+6* (ibloc-1)+3) = normal(2)
            direct(1+3+6* (ibloc-1)+4) = -normal(1)
            direct(1+3+6* (ibloc-1)+5) = 0.0d0
        endif
    end do
!
    call afrela(coemur, [cbid], nomddl, nomnoe, dimens,&
                direct, nbterm, zero, cbid, k8b,&
                'REEL', 'REEL', '12', 0.d0, lirela)
!
110 continue
!
! --- MENAGE
    AS_DEALLOCATE(vr=coemur)
    AS_DEALLOCATE(vk8=nomddl)
    AS_DEALLOCATE(vk8=nomnoe)
    AS_DEALLOCATE(vi=dimens)
    AS_DEALLOCATE(vr=direct)
!
    call jedema()
!
! --- FIN DE RECI2D.
end subroutine

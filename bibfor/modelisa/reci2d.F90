subroutine reci2d(lirela, mailla, nnoeca, noebe, nbcnx,&
                  cxma, normal, itria, xbar, iproj,&
                  excent)
! aslint: disable=W1501
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
    integer :: i1, i2, i3, ibloc, icnx, iterm, jcmur, jddl, jdime, jdirec
    integer :: jnomno, nbbloc, nbsom, nbterm, nbtmax, nnomax, noeca
    real(kind=8) :: ksi1, ksi2, zero
    complex(kind=8) :: cbid
    character(len=8) :: k8b
    character(len=24) :: nonoma
    logical :: notlin
!
    real(kind=8) :: ffel2d, x(2), ff(9)
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
    call wkvect('&&RECI2D.COEMUR', 'V V R', nbtmax, jcmur)
    call wkvect('&&RECI2D.NOMDDL', 'V V K8', nbtmax, jddl)
    call wkvect('&&RECI2D.NOMNOE', 'V V K8', nbtmax, jnomno)
    call wkvect('&&RECI2D.DIMENS', 'V V I', nbtmax, jdime)
    call wkvect('&&RECI2D.DIRECT', 'V V R', 3*nbtmax, jdirec)
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
    zk8(jnomno) = nnoeca
    zk8(jddl) = 'DEPL'
    zr(jcmur) = 1.0d0
!
! 3.1 DETERMINATION DES RELATIONS CINEMATIQUES DANS LE CAS OU
! --- L'EXCENTRICITE EST NULLE
!
    if (excent .eq. 0.0d0) then
!
        if (iproj .eq. 2) then
!
!.......... PAS DE RELATIONS CINEMATIQUES SI LES NOEUDS SONT
!.......... IDENTIQUES TOPOLOGIQUEMENT
!
            call jenonu(jexnom(nonoma, nnoeca), noeca)
            if (noeca .eq. noebe) goto 110
!
            nbterm = 2
            call jenuno(jexnum(nonoma, noebe), zk8(jnomno+1))
            zk8(jddl+1) = 'DEPL'
            zr(jcmur+1) = -1.0d0
!
        else
!
            if (iproj .gt. 10) then
!
                nbterm = 3
                i1 = iproj - 10
                i2 = i1 + 1
                if (i2 .gt. nbsom) i2 = 1
                call jenuno(jexnum(nonoma, cxma(i1)), zk8(jnomno+1))
                call jenuno(jexnum(nonoma, cxma(i2)), zk8(jnomno+2))
                zk8(jddl+1) = 'DEPL'
                zk8(jddl+2) = 'DEPL'
                if (nbcnx .eq. 3) then
                    if (i1 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)
                    else if (i1.eq.2) then
                        ffel2d = -0.5d0* (ksi1+ksi2)
                    else
                        ffel2d = 0.5d0* (1.0d0+ksi1)
                    endif
                else if (nbcnx.eq.6) then
                    if (i1 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                    else if (i1.eq.2) then
                        ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+1.0d0)
                    else if (i1.eq.3) then
                        ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                    else if (i1.eq.4) then
                        ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                    else if (i1.eq.5) then
                        ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                    else
                        ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                    endif
                else if (nbcnx.eq.4) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU4', x, 4, ff, nno)
                    if (i1 .eq. 1) then
                        ffel2d = ff(4)
                    else
                        ffel2d = ff(i1-1)
                    endif
                else if (nbcnx.eq.8) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU8', x, 8, ff, nno)
                    if (i1 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i1.eq.5) then
                        ffel2d = ff(8)
                    else
                        ffel2d = ff(i1-1)
                    endif
                else if (nbcnx.eq.9) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU9', x, 9, ff, nno)
                    if (i1 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i1.eq.5) then
                        ffel2d = ff(8)
                    else if (i1.eq.9) then
                        ffel2d = ff(9)
                    else
                        ffel2d = ff(i1-1)
                    endif
                endif
!
                zr(jcmur+1) = -ffel2d
!
                if (nbcnx .eq. 3) then
                    if (i2 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)
                    else if (i2.eq.2) then
                        ffel2d = -0.5d0* (ksi1+ksi2)
                    else
                        ffel2d = 0.5d0* (1.0d0+ksi1)
                    endif
                else if (nbcnx.eq.6) then
                    if (i2 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                    else if (i2.eq.2) then
                        ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+1.0d0)
                    else if (i2.eq.3) then
                        ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                    else if (i2.eq.4) then
                        ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                    else if (i2.eq.5) then
                        ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                    else
                        ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                    endif
                else if (nbcnx.eq.4) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU4', x, 4, ff, nno)
                    if (i2 .eq. 1) then
                        ffel2d = ff(4)
                    else
                        ffel2d = ff(i2-1)
                    endif
                else if (nbcnx.eq.8) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU8', x, 8, ff, nno)
                    if (i2 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i2.eq.5) then
                        ffel2d = ff(8)
                    else
                        ffel2d = ff(i2-1)
                    endif
                else if (nbcnx.eq.9) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU9', x, 9, ff, nno)
                    if (i2 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i2.eq.5) then
                        ffel2d = ff(8)
                    else if (i2.eq.9) then
                        ffel2d = ff(9)
                    else
                        ffel2d = ff(i2-1)
                    endif
                endif
                zr(jcmur+2) = -ffel2d
!               ZR(JCMUR+1) = -FFEL2D(NBCNX,I1,KSI1,KSI2)
!               ZR(JCMUR+2) = -FFEL2D(NBCNX,I2,KSI1,KSI2)
                if (notlin) then
                    nbterm = 4
                    i3 = i1 + nbsom
                    call jenuno(jexnum(nonoma, cxma(i3)), zk8(jnomno+3))
                    zk8(jddl+3) = 'DEPL'
                    if (nbcnx .eq. 3) then
                        if (i3 .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)
                        else if (i3.eq.2) then
                            ffel2d = -0.5d0* (ksi1+ksi2)
                        else
                            ffel2d = 0.5d0* (1.0d0+ksi1)
                        endif
                    else if (nbcnx.eq.6) then
                        if (i3 .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                        else if (i3.eq.2) then
                            ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+ 1.0d0)
                        else if (i3.eq.3) then
                            ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                        else if (i3.eq.4) then
                            ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                        else if (i3.eq.5) then
                            ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                        else
                            ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                        endif
                    else if (nbcnx.eq.4) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU4', x, 4, ff, nno)
                        if (i3 .eq. 1) then
                            ffel2d = ff(4)
                        else
                            ffel2d = ff(i3-1)
                        endif
                    else if (nbcnx.eq.8) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU8', x, 8, ff, nno)
                        if (i3 .eq. 1) then
                            ffel2d = ff(4)
                        else if (i3.eq.5) then
                            ffel2d = ff(8)
                        else
                            ffel2d = ff(i3-1)
                        endif
                    else if (nbcnx.eq.9) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU9', x, 9, ff, nno)
                        if (i3 .eq. 1) then
                            ffel2d = ff(4)
                        else if (i3.eq.5) then
                            ffel2d = ff(8)
                        else if (i3.eq.9) then
                            ffel2d = ff(9)
                        else
                            ffel2d = ff(i3-1)
                        endif
                    endif
                    zr(jcmur+3) = -ffel2d
!                  ZR(JCMUR+3) = -FFEL2D(NBCNX,I3,KSI1,KSI2)
                endif
!
            else
!
                nbterm = 1 + nbcnx
                do icnx = 1, nbcnx
                    call jenuno(jexnum(nonoma, cxma(icnx)), zk8(jnomno+ icnx))
                    zk8(jddl+icnx) = 'DEPL'
!
                    if (nbcnx .eq. 3) then
                        if (icnx .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)
                        else if (icnx.eq.2) then
                            ffel2d = -0.5d0* (ksi1+ksi2)
                        else
                            ffel2d = 0.5d0* (1.0d0+ksi1)
                        endif
                    else if (nbcnx.eq.6) then
                        if (icnx .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                        else if (icnx.eq.2) then
                            ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+ 1.0d0)
                        else if (icnx.eq.3) then
                            ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                        else if (icnx.eq.4) then
                            ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                        else if (icnx.eq.5) then
                            ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                        else
                            ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                        endif
                    else if (nbcnx.eq.4) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU4', x, 4, ff, nno)
                        if (icnx .eq. 1) then
                            ffel2d = ff(4)
                        else
                            ffel2d = ff(icnx-1)
                        endif
                    else if (nbcnx.eq.8) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU8', x, 8, ff, nno)
                        if (icnx .eq. 1) then
                            ffel2d = ff(4)
                        else if (icnx.eq.5) then
                            ffel2d = ff(8)
                        else
                            ffel2d = ff(icnx-1)
                        endif
                    else if (nbcnx.eq.9) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU9', x, 9, ff, nno)
                        if (icnx .eq. 1) then
                            ffel2d = ff(4)
                        else if (icnx.eq.5) then
                            ffel2d = ff(8)
                        else if (icnx.eq.9) then
                            ffel2d = ff(9)
                        else
                            ffel2d = ff(icnx-1)
                        endif
                    endif
                    zr(jcmur+icnx) = -ffel2d
!                  ZR(JCMUR+ICNX) = -FFEL2D(NBCNX,ICNX,KSI1,KSI2)
                end do
!
            endif
!
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
            zi(jdime+iterm-1) = 3
        end do
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA PREMIERE RELATION (DDL DX)
!....... PUIS AFFECTATION
!
        do iterm = 1, nbterm
            zr(jdirec+3* (iterm-1)) = 1.0d0
            zr(jdirec+3* (iterm-1)+1) = 0.0d0
            zr(jdirec+3* (iterm-1)+2) = 0.0d0
        end do
!
        call afrela(zr(jcmur), [cbid], zk8(jddl), zk8(jnomno), zi(jdime),&
                    zr(jdirec), nbterm, zero, cbid, k8b,&
                    'REEL', 'REEL', '12', 0.d0, lirela)
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA DEUXIEME RELATION (DDL DY)
!....... PUIS AFFECTATION
!
        do iterm = 1, nbterm
            zr(jdirec+3* (iterm-1)) = 0.0d0
            zr(jdirec+3* (iterm-1)+1) = 1.0d0
            zr(jdirec+3* (iterm-1)+2) = 0.0d0
        end do
!
        call afrela(zr(jcmur), [cbid], zk8(jddl), zk8(jnomno), zi(jdime),&
                    zr(jdirec), nbterm, zero, cbid, k8b,&
                    'REEL', 'REEL', '12', 0.d0, lirela)
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA TROISIEME RELATION (DDL DZ)
!....... PUIS AFFECTATION
!
        do iterm = 1, nbterm
            zr(jdirec+3* (iterm-1)) = 0.0d0
            zr(jdirec+3* (iterm-1)+1) = 0.0d0
            zr(jdirec+3* (iterm-1)+2) = 1.0d0
        end do
!
        call afrela(zr(jcmur), [cbid], zk8(jddl), zk8(jnomno), zi(jdime),&
                    zr(jdirec), nbterm, zero, cbid, k8b,&
                    'REEL', 'REEL', '12', 0.d0, lirela)
!
! 3.2 DETERMINATION DES RELATIONS CINEMATIQUES DANS LE CAS GENERAL
! ---
    else
!
        if (iproj .eq. 2) then
!
            nbterm = 3
            call jenuno(jexnum(nonoma, noebe), zk8(jnomno+1))
            zk8(jnomno+2) = zk8(jnomno+1)
            zk8(jddl+1) = 'DEPL'
            zk8(jddl+2) = 'ROTA'
            zr(jcmur+1) = -1.0d0
            zr(jcmur+2) = -excent
!
        else
!
            if (iproj .gt. 10) then
!
                nbterm = 5
                i1 = iproj - 10
                i2 = i1 + 1
                if (i2 .gt. nbsom) i2 = 1
                call jenuno(jexnum(nonoma, cxma(i1)), zk8(jnomno+1))
                zk8(jnomno+2) = zk8(jnomno+1)
                call jenuno(jexnum(nonoma, cxma(i2)), zk8(jnomno+3))
                zk8(jnomno+4) = zk8(jnomno+3)
                zk8(jddl+1) = 'DEPL'
                zk8(jddl+2) = 'ROTA'
                zk8(jddl+3) = 'DEPL'
                zk8(jddl+4) = 'ROTA'
!
                if (nbcnx .eq. 3) then
                    if (i1 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)
                    else if (i1.eq.2) then
                        ffel2d = -0.5d0* (ksi1+ksi2)
                    else
                        ffel2d = 0.5d0* (1.0d0+ksi1)
                    endif
                else if (nbcnx.eq.6) then
                    if (i1 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                    else if (i1.eq.2) then
                        ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+1.0d0)
                    else if (i1.eq.3) then
                        ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                    else if (i1.eq.4) then
                        ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                    else if (i1.eq.5) then
                        ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                    else
                        ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                    endif
                else if (nbcnx.eq.4) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU4', x, 4, ff, nno)
                    if (i1 .eq. 1) then
                        ffel2d = ff(4)
                    else
                        ffel2d = ff(i1-1)
                    endif
                else if (nbcnx.eq.8) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU8', x, 8, ff, nno)
                    if (i1 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i1.eq.5) then
                        ffel2d = ff(8)
                    else
                        ffel2d = ff(i1-1)
                    endif
                else if (nbcnx.eq.9) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU9', x, 9, ff, nno)
                    if (i1 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i1.eq.5) then
                        ffel2d = ff(8)
                    else if (i1.eq.9) then
                        ffel2d = ff(9)
                    else
                        ffel2d = ff(i1-1)
                    endif
                endif
                zr(jcmur+1) = -ffel2d
!               ZR(JCMUR+1) = -FFEL2D(NBCNX,I1,KSI1,KSI2)
                zr(jcmur+2) = excent*zr(jcmur+1)
!
                if (nbcnx .eq. 3) then
                    if (i2 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)
                    else if (i2.eq.2) then
                        ffel2d = -0.5d0* (ksi1+ksi2)
                    else
                        ffel2d = 0.5d0* (1.0d0+ksi1)
                    endif
                else if (nbcnx.eq.6) then
                    if (i2 .eq. 1) then
                        ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                    else if (i2.eq.2) then
                        ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+1.0d0)
                    else if (i2.eq.3) then
                        ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                    else if (i2.eq.4) then
                        ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                    else if (i2.eq.5) then
                        ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                    else
                        ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                    endif
                else if (nbcnx.eq.4) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU4', x, 4, ff, nno)
                    if (i2 .eq. 1) then
                        ffel2d = ff(4)
                    else
                        ffel2d = ff(i2-1)
                    endif
                else if (nbcnx.eq.8) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU8', x, 8, ff, nno)
                    if (i2 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i2.eq.5) then
                        ffel2d = ff(8)
                    else
                        ffel2d = ff(i2-1)
                    endif
                else if (nbcnx.eq.9) then
                    x(1) = ksi1
                    x(2) = ksi2
                    call elrfvf('QU9', x, 9, ff, nno)
                    if (i2 .eq. 1) then
                        ffel2d = ff(4)
                    else if (i2.eq.5) then
                        ffel2d = ff(8)
                    else if (i2.eq.9) then
                        ffel2d = ff(9)
                    else
                        ffel2d = ff(i2-1)
                    endif
                endif
                zr(jcmur+3) = -ffel2d
!               ZR(JCMUR+3) = -FFEL2D(NBCNX,I2,KSI1,KSI2)
                zr(jcmur+4) = excent*zr(jcmur+3)
                if (notlin) then
                    nbterm = 7
                    i3 = i1 + nbsom
                    call jenuno(jexnum(nonoma, cxma(i3)), zk8(jnomno+5))
                    zk8(jnomno+6) = zk8(jnomno+5)
                    zk8(jddl+5) = 'DEPL'
                    zk8(jddl+6) = 'ROTA'
!
                    if (nbcnx .eq. 3) then
                        if (i3 .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)
                        else if (i3.eq.2) then
                            ffel2d = -0.5d0* (ksi1+ksi2)
                        else
                            ffel2d = 0.5d0* (1.0d0+ksi1)
                        endif
                    else if (nbcnx.eq.6) then
                        if (i3 .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                        else if (i3.eq.2) then
                            ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+ 1.0d0)
                        else if (i3.eq.3) then
                            ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                        else if (i3.eq.4) then
                            ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                        else if (i3.eq.5) then
                            ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                        else
                            ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                        endif
                    else if (nbcnx.eq.4) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU4', x, 4, ff, nno)
                        if (i3 .eq. 1) then
                            ffel2d = ff(4)
                        else
                            ffel2d = ff(i3-1)
                        endif
                    else if (nbcnx.eq.8) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU8', x, 8, ff, nno)
                        if (i3 .eq. 1) then
                            ffel2d = ff(4)
                        else if (i3.eq.5) then
                            ffel2d = ff(8)
                        else
                            ffel2d = ff(i3-1)
                        endif
                    else if (nbcnx.eq.9) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU9', x, 9, ff, nno)
                        if (i3 .eq. 1) then
                            ffel2d = ff(4)
                        else if (i3.eq.5) then
                            ffel2d = ff(8)
                        else if (i3.eq.9) then
                            ffel2d = ff(9)
                        else
                            ffel2d = ff(i3-1)
                        endif
                    endif
                    zr(jcmur+5) = -ffel2d
!                  ZR(JCMUR+5) = -FFEL2D(NBCNX,I3,KSI1,KSI2)
                    zr(jcmur+6) = excent*zr(jcmur+5)
                endif
!
            else
!
                nbterm = 1 + 2*nbcnx
                do icnx = 1, nbcnx
                    call jenuno(jexnum(nonoma, cxma(icnx)), zk8(jnomno+ 2* (icnx-1)+1))
                    zk8(jnomno+2*icnx) = zk8(jnomno+2* (icnx-1)+1)
                    zk8(jddl+2* (icnx-1)+1) = 'DEPL'
                    zk8(jddl+2*icnx) = 'ROTA'
!
                    if (nbcnx .eq. 3) then
                        if (icnx .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)
                        else if (icnx.eq.2) then
                            ffel2d = -0.5d0* (ksi1+ksi2)
                        else
                            ffel2d = 0.5d0* (1.0d0+ksi1)
                        endif
                    else if (nbcnx.eq.6) then
                        if (icnx .eq. 1) then
                            ffel2d = 0.5d0* (1.0d0+ksi2)*ksi2
                        else if (icnx.eq.2) then
                            ffel2d = 0.5d0* (ksi1+ksi2)* (ksi1+ksi2+ 1.0d0)
                        else if (icnx.eq.3) then
                            ffel2d = 0.5d0* (1.0d0+ksi1)*ksi1
                        else if (icnx.eq.4) then
                            ffel2d = -1.0d0* (1.0d0+ksi2)* (ksi1+ksi2)
                        else if (icnx.eq.5) then
                            ffel2d = -1.0d0* (1.0d0+ksi1)* (ksi1+ksi2)
                        else
                            ffel2d = (1.0d0+ksi1)* (1.0d0+ksi2)
                        endif
                    else if (nbcnx.eq.4) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU4', x, 4, ff, nno)
                        if (icnx .eq. 1) then
                            ffel2d = ff(4)
                        else
                            ffel2d = ff(icnx-1)
                        endif
                    else if (nbcnx.eq.8) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU8', x, 8, ff, nno)
                        if (icnx .eq. 1) then
                            ffel2d = ff(4)
                        else if (icnx.eq.5) then
                            ffel2d = ff(8)
                        else
                            ffel2d = ff(icnx-1)
                        endif
                    else if (nbcnx.eq.9) then
                        x(1) = ksi1
                        x(2) = ksi2
                        call elrfvf('QU9', x, 9, ff, nno)
                        if (icnx .eq. 1) then
                            ffel2d = ff(4)
                        else if (icnx.eq.5) then
                            ffel2d = ff(8)
                        else if (icnx.eq.9) then
                            ffel2d = ff(9)
                        else
                            ffel2d = ff(icnx-1)
                        endif
                    endif
                    zr(jcmur+2* (icnx-1)+1) = -ffel2d
!            ZR(JCMUR+2*(ICNX-1)+1) = -FFEL2D(NBCNX,ICNX,KSI1,KSI2)
                    zr(jcmur+2*icnx) = excent*zr(jcmur+2* (icnx-1)+1)
                end do
!
            endif
!
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
            zi(jdime+iterm-1) = 3
        end do
!
        nbbloc = (nbterm-1)/2
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA PREMIERE RELATION (DDL DX)
!....... PUIS AFFECTATION
!
        zr(jdirec) = 1.0d0
        zr(jdirec+1) = 0.0d0
        zr(jdirec+2) = 0.0d0
        do ibloc = 1, nbbloc
            zr(jdirec+3+6* (ibloc-1)) = 1.0d0
            zr(jdirec+3+6* (ibloc-1)+1) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+2) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+3) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+4) = normal(3)
            zr(jdirec+3+6* (ibloc-1)+5) = -normal(2)
        end do
!
        call afrela(zr(jcmur), [cbid], zk8(jddl), zk8(jnomno), zi(jdime),&
                    zr(jdirec), nbterm, zero, cbid, k8b,&
                    'REEL', 'REEL', '12', 0.d0, lirela)
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA DEUXIEME RELATION (DDL DY)
!....... PUIS AFFECTATION
!
        zr(jdirec) = 0.0d0
        zr(jdirec+1) = 1.0d0
        zr(jdirec+2) = 0.0d0
        do ibloc = 1, nbbloc
            zr(jdirec+3+6* (ibloc-1)) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+1) = 1.0d0
            zr(jdirec+3+6* (ibloc-1)+2) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+3) = -normal(3)
            zr(jdirec+3+6* (ibloc-1)+4) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+5) = normal(1)
        end do
!
        call afrela(zr(jcmur), [cbid], zk8(jddl), zk8(jnomno), zi(jdime),&
                    zr(jdirec), nbterm, zero, cbid, k8b,&
                    'REEL', 'REEL', '12', 0.d0, lirela)
!
!....... COEFFICIENTS PAR DIRECTIONS POUR LA TROISIEME RELATION (DDL DZ)
!....... PUIS AFFECTATION
!
        zr(jdirec) = 0.0d0
        zr(jdirec+1) = 0.0d0
        zr(jdirec+2) = 1.0d0
        do ibloc = 1, nbbloc
            zr(jdirec+3+6* (ibloc-1)) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+1) = 0.0d0
            zr(jdirec+3+6* (ibloc-1)+2) = 1.0d0
            zr(jdirec+3+6* (ibloc-1)+3) = normal(2)
            zr(jdirec+3+6* (ibloc-1)+4) = -normal(1)
            zr(jdirec+3+6* (ibloc-1)+5) = 0.0d0
        end do
!
        call afrela(zr(jcmur), [cbid], zk8(jddl), zk8(jnomno), zi(jdime),&
                    zr(jdirec), nbterm, zero, cbid, k8b,&
                    'REEL', 'REEL', '12', 0.d0, lirela)
!
    endif
!
110 continue
!
! --- MENAGE
    call jedetr('&&RECI2D.COEMUR')
    call jedetr('&&RECI2D.NOMDDL')
    call jedetr('&&RECI2D.NOMNOE')
    call jedetr('&&RECI2D.DIMENS')
    call jedetr('&&RECI2D.DIRECT')
!
    call jedema()
!
! --- FIN DE RECI2D.
end subroutine

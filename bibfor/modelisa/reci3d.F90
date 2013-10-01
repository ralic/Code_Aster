subroutine reci3d(lirela, mailla, nnoeca, noebe, nbcnx,&
                  cxma, itetra, xbar, immer)
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
!                ELEMENTS 3D
!                APPELANT : IMMECA
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
!  IN     : ITETRA : INTEGER , SCALAIRE
!                    INDICATEUR DU SOUS-DOMAINE TETRAEDRE AUQUEL
!                    APPARTIENT LE NOEUD DU CABLE
!                    ITETRA = 1            SI IMMERSION DANS UNE
!                                          MAILLE TETRAEDRE
!                    ITETRA = 1 OU 2       SI IMMERSION DANS UNE
!                                          MAILLE PYRAMIDE
!                    ITETRA = 1 OU 2 OU 3  SI IMMERSION DANS UNE
!                                          MAILLE PENTAEDRE
!                    ITETRA = 1 OU 2 OU 3  SI IMMERSION DANS UNE
!                          OU 4 OU 5 OU 6  MAILLE HEXAEDRE
!  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 4
!                    COORDONNEES BARYCENTRIQUES DU NOEUD DU CABLE DANS
!                    LE SOUS-DOMAINE TETRAEDRE AUQUEL IL APPARTIENT
!  IN     : IMMER  : INTEGER , SCALAIRE
!                    INDICE D'IMMERSION
!                    IMMER =  0  LE NOEUD DU CABLE EST A L'INTERIEUR
!                                DE LA MAILLE
!                    IMMER = 100 + 10 * NUMERO DE FACE
!                                LE NOEUD DU CABLE EST SUR UNE FACE
!                                DE LA MAILLE
!                    IMMER = 100 + 10 * NUMERO DE FACE + NUMERO D'ARETE
!                                LE NOEUD DU CABLE EST SUR UNE ARETE
!                                DE LA MAILLE
!                    IMMER =  2  LE NOEUD DU CABLE COINCIDE AVEC UN DES
!                                NOEUDS DE LA MAILLE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
#include "jeveux.h"
#include "asterfort/afrela.h"
#include "asterfort/ante3d.h"
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
    integer :: noebe, nbcnx, cxma(*), itetra, immer
    real(kind=8) :: xbar(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: icnx, iterm, jcmur, jddl, jdime, jdirec, jnomno, nbsom, nbterm
    integer :: nbtmax, nnomax, noeca
    real(kind=8) :: ksi1, ksi2, ksi3, zero
    complex(kind=8) :: cbid
    character(len=8) :: k8b
    character(len=24) :: nonoma
    logical :: notlin
    integer :: nno
!
    real(kind=8) :: ffel3d, ff(27), x(3)
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   CREATION DES OBJETS DE TRAVAIL - INITIALISATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    nnomax = 27
    nbtmax = 1 + nnomax
    call wkvect('&&RECI3D.COEMUR', 'V V R', nbtmax, jcmur)
    call wkvect('&&RECI3D.NOMDDL', 'V V K8', nbtmax, jddl)
    call wkvect('&&RECI3D.NOMNOE', 'V V K8', nbtmax, jnomno)
    call wkvect('&&RECI3D.DIMENS', 'V V I', nbtmax, jdime)
    call wkvect('&&RECI3D.DIRECT', 'V V R', 3*nbtmax, jdirec)
!
    notlin = (nbcnx.gt.8)
    if (notlin) then
        if (nbcnx .eq. 10) then
            nbsom = 4
        else if (nbcnx.eq.13) then
            nbsom = 5
        else if (nbcnx.eq.15) then
            nbsom = 6
        else
            nbsom = 8
        endif
    else
        nbsom = nbcnx
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   DETERMINATION DE L'ANTECEDENT DU NOEUD DU CABLE DANS L'ELEMENT DE
!     REFERENCE ASSOCIE A L'ELEMENT REEL
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (immer .ne. 2) call ante3d(nbsom, itetra, xbar(1), ksi1, ksi2,&
                                  ksi3)
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
! 3.1.1 LE NOEUD DU CABLE COINCIDE TOPOLOGIQUEMENT
!       AVEC UN DES NOEUDS DE LA MAILLE
! ---
    call jenonu(jexnom(nonoma, nnoeca), noeca)
    if (noeca .eq. noebe) goto 60
!
! 3.1.2 LE NOEUD DU CABLE COINCIDE GEOGRAPHIQUEMENT AVEC UN DES
!       NOEUDS DE LA MAILLE : PAS DE RELATIONS CINEMATIQUES
! ---
    if (immer .eq. 2) then
!
        nbterm = 2
        call jenuno(jexnum(nonoma, noebe), zk8(jnomno+1))
        zk8(jddl+1) = 'DEPL'
        zr(jcmur+1) = -1.0d0
!
!
! 3.3 LE NOEUD DU CABLE EST A L'INTERIEUR DE LA MAILLE
! ---
    else
!
        nbterm = 1 + nbcnx
        do icnx = 1, nbcnx
            call jenuno(jexnum(nonoma, cxma(icnx)), zk8(jnomno+icnx))
            zk8(jddl+icnx) = 'DEPL'
            x(1) = ksi1
            x(2) = ksi2
            x(3) = ksi3
            if (nbcnx .eq. 4) then
                call elrfvf('TE4', x, 4, ff, nno)
            else if (nbcnx.eq.10) then
                call elrfvf('T10', x, 10, ff, nno)
            else if (nbcnx.eq.5) then
                call elrfvf('PY5', x, 5, ff, nno)
            else if (nbcnx.eq.13) then
                call elrfvf('P13', x, 13, ff, nno)
            else if (nbcnx.eq.6) then
                call elrfvf('PE6', x, 6, ff, nno)
            else if (nbcnx.eq.15) then
                call elrfvf('P15', x, 15, ff, nno)
            else if (nbcnx.eq.8) then
                call elrfvf('HE8', x, 8, ff, nno)
            else if (nbcnx.eq.20) then
                call elrfvf('H20', x, 20, ff, nno)
            else if (nbcnx.eq.27) then
                call elrfvf('H27', x, 27, ff, nno)
            endif
            ffel3d = ff(icnx)
            zr(jcmur+icnx) = -ffel3d
!            ZR(JCMUR+ICNX) = -FFEL3D(NBCNX,ICNX,KSI1,KSI2,KSI3)
        end do
!
    endif
!
! 3.4 UNE RELATION PAR DDL DE TRANSLATION DU NOEUD DU CABLE
! ---
!.... LE VECTEUR ZI(JDIME) DOIT ETRE REINITIALISE AFIN DE PRENDRE
!.... EN COMPTE LES DIFFERENTS COEFFICIENTS PAR DIRECTION DEFINIS
!.... DANS LE VECTEUR ZR(JDIREC)
!
    do iterm = 1, nbterm
        zi(jdime+iterm-1) = 3
    end do
!
!.... COEFFICIENTS PAR DIRECTIONS POUR LA PREMIERE RELATION (DDL DX)
!.... PUIS AFFECTATION
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
!.... COEFFICIENTS PAR DIRECTIONS POUR LA DEUXIEME RELATION (DDL DY)
!.... PUIS AFFECTATION
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
!.... COEFFICIENTS PAR DIRECTIONS POUR LA TROISIEME RELATION (DDL DZ)
!.... PUIS AFFECTATION
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
 60 continue
!
! --- MENAGE
    call jedetr('&&RECI3D.COEMUR')
    call jedetr('&&RECI3D.NOMDDL')
    call jedetr('&&RECI3D.NOMNOE')
    call jedetr('&&RECI3D.DIMENS')
    call jedetr('&&RECI3D.DIRECT')
    call jedema()
!
! --- FIN DE RECI3D.
end subroutine

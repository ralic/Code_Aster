subroutine uthk(nomte, geom, hk, ndim, niv,&
                noe, nsomm, tymvol, ifa)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  UTILITAIRE DE CALCUL DU DIAMETRE D'UN
!                          ELEMENT FINI K.
!      PAR DEFINITION, LE DIAMETRE D'UN ELEMENT EST LA LONGUEUR DU PLUS
!      GRAND SEGMENT QUE L'ON PEUT INSERER DANS LA MAILLE ASSOCIEE.
!
! IN NOMTE  : NOM DU TYPE D'ELEMENT DE K
! IN GEOM   : LA GEOMETRIE
! IN NDIM   : DIMENSION DE L'ELEMENT FINI
!             SI 0, ON EXAMINE UNE FACE D'UN VOLUME
!   SEULEMENT SI NDIM = 0 (EXAMEN DE FACE DE VOLUME) :
! IN   NOE      : LISTE DES NOEUDS PAR FACE (POUR DU 3D) (VOIR TE0003)
!     NOE (IN,IFA,TYMVOL) : IN     : NUMERO DU NOEUD DANS LA FACE
!                           IFA    : NUMERO DE LA FACE
!                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!                                    1 : HEXAEDRE A 8,20 ET 27 NOEUDS
!                                    2 : PENTAEDRE A 6 ET 15 NOEUDS
!                                    3 : TETRAEDRE A 4 ET 10 NOEUDS
!                                    4 : PYRAMIDE A 5 ET 13 NOEUDS
!  ==> POUR LE IN-EME NOEUD DE LA IFA-EME FACE D'UNE MAILLE DE TYPE
!      TYMVOL, NOE (IN,IFA,TYMVOL) EST SON NUMERO LOCAL DANS LA
!      DESCRIPTION DE LA MAILLE VOLUMIQUE.
!      ON RAPPELLE QU'EN FORTRAN L'ORDRE DE RANGEMENT EST LE SUIVANT :
!   (1,1,1) (2,1,1) (3,1,1) ... (9,1,1) (1,2,1) (2,2,1) ... (9,2,1)
!   (1,3,1)  ...    (8,6,4) (9,6,4)
!    POUR UTILISER UN VECTEUR UNIDIMENSIONNEL EQUIVALENT ZI(IVAL)
!    LA POSITION i,j,k  pour noe(n1,n2,n3) EST DONNEE PAR LA FORMULE
!    i + (j-1)*n1 + (k-1)*(n1*n2)
!
!    ON COMMENCE AINSI PAR LES 9 NOEUDS DE LA 1ERE FACE DE L'HEXAEDRE,
!    PUIS LES 9 NOEUDS DE LA 2EME FACE DE L'HEXAEDRE,
!    ETC JUSQU'AUX 9 NOEUDS DE LA 6EME FACE DE L'HEXAEDRE.
!    ENSUITE ON A LES 6 NOEUDS DE LA 1ERE FACE DU PENTAEDRE, ETC
!    ON CHOISIT UNE ORIENTATION ENTRANTE POUR DECRIRE UNE FACE
! IN NSOMM  : NOMBRE DE SOMMETS DE LA FACE
! IN TYMVOL : TYPE DE VOLUME DONT ON EXAMINE UNE FACE
!               1 : HEXAEDRE
!               2 : PENTAEDRE
!               3 : TETRAEDRE
!               4 : PYRAMIDE
! IN IFA    : NUMERO DE FACE
! IN NIV/IFM : PARAMETRES D'IMPRESSION
! OUT HK    : DIAMETRE DE L'ELEMENT K
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       03/07/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!       12/03/07 (SM): CORRECTION CAR CALCUL FAUX DU DIAMETRE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/uttgel.h"
!
    integer, parameter :: l1=9, l2=6, l3=4
    character(len=16), intent(in) :: nomte
    real(kind=8), intent(in) :: geom(*)
    real(kind=8), intent(out) :: hk
    integer, intent(in) :: ndim
    integer, intent(in) :: niv
    integer, intent(in), optional :: noe(l1*l2*l3)
    integer, intent(in), optional :: nsomm
    integer, intent(in), optional :: tymvol
    integer, intent(in), optional :: ifa
!
! DECLARATION VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'UTHK  ' )
!
    integer :: dimtab
    parameter ( dimtab = 28 )
    real(kind=8) :: tabaux(dimtab)
!
    real(kind=8) :: x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, x5, y5, z5
    real(kind=8) :: x6, y6, z6
    real(kind=8) :: x7, y7, z7, x8, y8, z8
    integer :: in, i, iino
    character(len=2) :: typgeo
    character(len=6) :: valk(2)
!
!====
! 1. COORDONNEES DES SOMMETS ET TYPE GEOMETRIQUE DE L'ELEMENT
!====
!
! 1.1. ==> QUADRANGLE/TRIANGLE EN 2D
!
    if (ndim .eq. 2) then
        x1 = geom(1)
        y1 = geom(2)
        x2 = geom(3)
        y2 = geom(4)
        x3 = geom(5)
        y3 = geom(6)
        call uttgel(nomte, typgeo)
!
    else if (ndim.eq.3) then
!
! 1.2. ==> HEXA/TETRA/PENTA/PYRA
!
        x1 = geom(1)
        y1 = geom(2)
        z1 = geom(3)
        x2 = geom(4)
        y2 = geom(5)
        z2 = geom(6)
        x3 = geom(7)
        y3 = geom(8)
        z3 = geom(9)
        x4 = geom(10)
        y4 = geom(11)
        z4 = geom(12)
        call uttgel(nomte, typgeo)
!
    else if (ndim.eq.0) then
!
        ASSERT(present(noe))
        ASSERT(ENSEMBLE4(noe, nsomm, tymvol, ifa))
! 1.3. ==> FACE3/4/6/8
!
        do in = 1, nsomm
!
            iino=noe(in+l1*(ifa-1)+l1*l2*(tymvol-1))
            i = 3*(iino-1)+1
            if (in .eq. 1) then
                x1 = geom(i)
                y1 = geom(i+1)
                z1 = geom(i+2)
            else if (in.eq.2) then
                x2 = geom(i)
                y2 = geom(i+1)
                z2 = geom(i+2)
            else if (in.eq.3) then
                x3 = geom(i)
                y3 = geom(i+1)
                z3 = geom(i+2)
            else if (in.eq.4) then
                x4 = geom(i)
                y4 = geom(i+1)
                z4 = geom(i+2)
            endif
!
        end do
        if ((nsomm.eq.3) .or. (nsomm.eq.6)) then
! FACE_3 OU FACE_6
            typgeo = 'FT'
        else
! FACE_4 OU FACE_8
            typgeo = 'FQ'
        endif
    endif
!
!====
! 2. TRIANGLE : PLUS GRANDE ARETE
!====
!
    if ((typgeo.eq.'TR') .or. (typgeo.eq.'TS') .or. (typgeo.eq.'TL')) then
!
        tabaux(1) = (x2-x1)**2 + (y2-y1)**2
        tabaux(2) = (x3-x2)**2 + (y3-y2)**2
        tabaux(3) = (x1-x3)**2 + (y1-y3)**2
!
        hk = max(tabaux(1),tabaux(2),tabaux(3))
!
!====
! 3. QUADRANGLE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
!====
!
        elseif ( (typgeo.eq.'QU') .or. (typgeo.eq.'QS') .or. (&
    typgeo.eq.'QL') ) then
!
        x4 = geom(7)
        y4 = geom(8)
!
!       LES DIAGONALES
!
        tabaux(1) = (x1-x3)**2 + (y1-y3)**2
        tabaux(2) = (x2-x4)**2 + (y2-y4)**2
!
!       LES ARETES
!
        tabaux(3) = (x1-x2)**2 + (y1-y2)**2
        tabaux(4) = (x1-x4)**2 + (y1-y4)**2
        tabaux(5) = (x2-x3)**2 + (y2-y3)**2
        tabaux(6) = (x3-x4)**2 + (y3-y4)**2
!
        hk = max(tabaux(1),tabaux(2),tabaux(3),tabaux(4),tabaux(5), tabaux(6))
!
!====
! 4. TETRAEDRE : PLUS GRANDE ARETE
!====
!
    else if (typgeo.eq.'TE') then
!
        tabaux(1) = (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
        tabaux(2) = (x3-x1)**2 + (y3-y1)**2 + (z3-z1)**2
        tabaux(3) = (x4-x1)**2 + (y4-y1)**2 + (z4-z1)**2
        tabaux(4) = (x3-x2)**2 + (y3-y2)**2 + (z3-z2)**2
        tabaux(5) = (x4-x2)**2 + (y4-y2)**2 + (z4-z2)**2
        tabaux(6) = (x4-x3)**2 + (y4-y3)**2 + (z4-z3)**2
!
        hk = max(tabaux(1),tabaux(2),tabaux(3),tabaux(4),tabaux(5), tabaux(6))
!
!====
! 5. HEXAEDRE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
!====
!
    else if (typgeo.eq.'HE') then
!
        x5 = geom(13)
        y5 = geom(14)
        z5 = geom(15)
        x6 = geom(16)
        y6 = geom(17)
        z6 = geom(18)
        x7 = geom(19)
        y7 = geom(20)
        z7 = geom(21)
        x8 = geom(22)
        y8 = geom(23)
        z8 = geom(24)
!
! DIAGONALES VOLUMIQUES
!
        tabaux(1) = (x1-x7)**2 + (y1-y7)**2 + (z1-z7)**2
        tabaux(2) = (x2-x8)**2 + (y2-y8)**2 + (z2-z8)**2
        tabaux(3) = (x3-x5)**2 + (y3-y5)**2 + (z3-z5)**2
        tabaux(4) = (x4-x6)**2 + (y4-y6)**2 + (z4-z6)**2
!
! DIAGONALES SURFACIQUES
!
        tabaux(5) = (x1-x3)**2 + (y1-y3)**2 + (z1-z3)**2
        tabaux(6) = (x2-x4)**2 + (y2-y4)**2 + (z2-z4)**2
        tabaux(7) = (x3-x8)**2 + (y3-y8)**2 + (z3-z8)**2
        tabaux(8) = (x7-x4)**2 + (y7-y4)**2 + (z7-z4)**2
        tabaux(9) = (x5-x7)**2 + (y5-y7)**2 + (z5-z7)**2
        tabaux(10) = (x6-x8)**2 + (y6-y8)**2 + (z6-z8)**2
        tabaux(11) = (x5-x2)**2 + (y5-y2)**2 + (z5-z2)**2
        tabaux(12) = (x6-x1)**2 + (y6-y1)**2 + (z6-z1)**2
        tabaux(13) = (x6-x3)**2 + (y6-y3)**2 + (z6-z3)**2
        tabaux(14) = (x7-x2)**2 + (y7-y2)**2 + (z7-z2)**2
        tabaux(15) = (x5-x4)**2 + (y5-y4)**2 + (z5-z4)**2
        tabaux(16) = (x1-x8)**2 + (y1-y8)**2 + (z1-z8)**2
!
! ARETES
!
        tabaux(17) = (x2-x3)**2 + (y2-y3)**2 + (z2-z3)**2
        tabaux(18) = (x2-x6)**2 + (y2-y6)**2 + (z2-z6)**2
        tabaux(19) = (x3-x7)**2 + (y3-y7)**2 + (z3-z7)**2
        tabaux(20) = (x6-x7)**2 + (y6-y7)**2 + (z6-z7)**2
        tabaux(21) = (x5-x6)**2 + (y5-y6)**2 + (z5-z6)**2
        tabaux(22) = (x7-x8)**2 + (y7-y8)**2 + (z7-z8)**2
        tabaux(23) = (x4-x3)**2 + (y4-y3)**2 + (z4-z3)**2
        tabaux(24) = (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
        tabaux(25) = (x1-x4)**2 + (y1-y4)**2 + (z1-z4)**2
        tabaux(26) = (x4-x8)**2 + (y4-y8)**2 + (z4-z8)**2
        tabaux(27) = (x1-x5)**2 + (y1-y5)**2 + (z1-z5)**2
        tabaux(28) = (x5-x8)**2 + (y5-y8)**2 + (z5-z8)**2
!
        hk = max(&
             tabaux(1), tabaux(2), tabaux(3), tabaux(4), tabaux(5), tabaux(6), tabaux(7),&
             tabaux(8), tabaux(9), tabaux(10), tabaux(11), tabaux(12), tabaux(13), tabaux(14),&
             tabaux(15), tabaux(16), tabaux(17), tabaux(18), tabaux(19), tabaux(20), tabaux(21),&
             tabaux(22), tabaux(23), tabaux(24), tabaux(25), tabaux(26), tabaux(27), tabaux(28)&
             )
!
!====
! 5. PENTAEDRE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
!====
!
    else if (typgeo.eq.'PE') then
!
        x5 = geom(13)
        y5 = geom(14)
        z5 = geom(15)
        x6 = geom(16)
        y6 = geom(17)
        z6 = geom(18)
!
! DIAGONALES SURFACIQUES
!
        tabaux(1) = (x1-x6)**2 + (y1-y6)**2 + (z1-z6)**2
        tabaux(2) = (x3-x4)**2 + (y3-y4)**2 + (z3-z4)**2
        tabaux(3) = (x2-x4)**2 + (y2-y4)**2 + (z2-z4)**2
        tabaux(4) = (x1-x5)**2 + (y1-y5)**2 + (z1-z5)**2
        tabaux(5) = (x2-x6)**2 + (y2-y6)**2 + (z2-z6)**2
        tabaux(6) = (x3-x5)**2 + (y3-y5)**2 + (z3-z5)**2
!
! ARETES
!
        tabaux(7) = (x4-x5)**2 + (y4-y5)**2 + (z4-z5)**2
        tabaux(8) = (x5-x6)**2 + (y5-y6)**2 + (z5-z6)**2
        tabaux(9) = (x6-x4)**2 + (y6-y4)**2 + (z6-z4)**2
        tabaux(10) = (x6-x3)**2 + (y6-y3)**2 + (z6-z3)**2
        tabaux(11) = (x4-x1)**2 + (y4-y1)**2 + (z4-z1)**2
        tabaux(12) = (x5-x2)**2 + (y5-y2)**2 + (z5-z2)**2
        tabaux(13) = (x1-x3)**2 + (y1-y3)**2 + (z1-z3)**2
        tabaux(14) = (x3-x2)**2 + (y3-y2)**2 + (z3-z2)**2
        tabaux(15) = (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
!
        hk = max(&
             tabaux(1), tabaux(2), tabaux(3), tabaux(4), tabaux(5), tabaux(6), tabaux(7),&
             tabaux(8), tabaux(9), tabaux(10), tabaux(11), tabaux(12), tabaux(13), tabaux(14),&
             tabaux(15)&
             )
!
!====
! 6. PYRAMIDE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
!====
!
    else if (typgeo.eq.'PY') then
!
        x5 = geom(13)
        y5 = geom(14)
        z5 = geom(15)
!
! DIAGONALES SURFACIQUES
!
        tabaux(1) = (x1-x3)**2 + (y1-y3)**2 + (z1-z3)**2
        tabaux(2) = (x2-x4)**2 + (y2-y4)**2 + (z2-z4)**2
!
! ARETES
!
        tabaux(3) = (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
        tabaux(4) = (x3-x2)**2 + (y3-y2)**2 + (z3-z2)**2
        tabaux(5) = (x4-x3)**2 + (y4-y3)**2 + (z4-z3)**2
        tabaux(6) = (x1-x4)**2 + (y1-y4)**2 + (z1-z4)**2
        tabaux(7) = (x5-x1)**2 + (y5-y1)**2 + (z5-z1)**2
        tabaux(8) = (x5-x2)**2 + (y5-y2)**2 + (z5-z2)**2
        tabaux(9) = (x5-x3)**2 + (y5-y3)**2 + (z5-z3)**2
        tabaux(10) = (x5-x4)**2 + (y5-y4)**2 + (z5-z4)**2
!
        hk = max(&
             tabaux(1), tabaux(2), tabaux(3), tabaux(4), tabaux(5), tabaux(6), tabaux(7),&
             tabaux(8), tabaux(9), tabaux(10)&
             )
!
!====
! 7. FACE QUADRANGULAIRE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
!====
!
    else if (typgeo.eq.'FQ') then
!
        tabaux(1) = (x1-x3)**2 + (y1-y3)**2 + (z1-z3)**2
        tabaux(2) = (x2-x4)**2 + (y2-y4)**2 + (z2-z4)**2
!
        tabaux(3) = (x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2
        tabaux(4) = (x1-x4)**2 + (y1-y4)**2 + (z1-z4)**2
        tabaux(5) = (x2-x3)**2 + (y2-y3)**2 + (z2-z3)**2
        tabaux(6) = (x3-x4)**2 + (y3-y4)**2 + (z3-z4)**2
!
        hk = max( tabaux(1) ,tabaux(2) ,tabaux(3) ,tabaux(4) , tabaux(5) ,tabaux(6))
!
!====
! 8. FACE TRIANGULAIRE : PLUS GRANDE ARETE
!====
!
    else if (typgeo.eq.'FT') then
!
        tabaux(1) = (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
        tabaux(2) = (x3-x2)**2 + (y3-y2)**2 + (z3-z2)**2
        tabaux(3) = (x1-x3)**2 + (y1-y3)**2 + (z1-z3)**2
!
        hk = max(tabaux(1),tabaux(2),tabaux(3))
!
!====
! 9. INCONNU
!====
!
    else
        valk(1) = nompro
        valk(2) = typgeo
        call utmess('F', 'INDICATEUR_32', nk=2, valk=valk)
    endif
!
!====
! 10. LA FIN
!    ON NE MET LA RACINE CARRE QU'A CE MOMENT POUR GAGNER DU TEMPS
!====
!
    hk = sqrt(hk)
!
    if (niv .ge. 2) then
        call utmess('I', 'INDICATEUR_33', sk=typgeo, sr=hk)
    endif
!
end subroutine

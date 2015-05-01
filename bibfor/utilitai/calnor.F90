subroutine calnor(chdim, geom, iare, nnos, nnoa,&
                  orien, nno, npg, noe, ifa,&
                  tymvol, idfde, jac, nx, ny,&
                  nz, tx, ty, hf)
! ----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!         CALCUL DE LA NORMALE SORTANTE D'UN ELEMENT LINEAIRE
!         OU QUADRATIQUE.
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   CHDIM  : DIMENSION DE L'ESPACE : '2D' OU '3D'
! IN   GEOM   : LES COORDONNEES
!
! POUR DU 2D :
! ON CHERCHE LA NORMALE SORTANTE A L'ARETE D'UNE FACE QUAD OU TRIA.
! PAR CONVENTION, L'ARETE NUMERO I EST SITUE ENTRE LES SOMMETS I
! ET I+1 DE LA FACE (MODULO LE DERNIER QUI REPART A 1)
! EXEMPLE POUR UN QUAD :
!                           ARETE 1
!                1 o---------------------o 2
!                  .                     .
!          ARETE 4 .                     . ARETE 2
!                  .                     .
!                4 o---------------------o 3
!                           ARETE 3
!
!
! IN   IARE   : NUMERO DE L'ARETE A EXAMINER
! IN   NNOS   : NOMBRE DE SOMMETS DE LA FACE (3 OU 4)
! IN   NNOA   : NOMBRE DE NOEUDS PAR ARETE (2 OU 3)
! IN   ORIEN  : ORIENTATION DE LA MAILLE PAR RAPPORT A ELREF
!                1 SI IDEM ELEMENT DE REFERENCE
!               -1 SI DIFFERENT
!
! POUR DU 3D :
! ON CHERCHE LA NORMALE SORTANTE A LA FACE QUAD OU TRIA D'UNE
! MAILLE VOLUMIQUE.
! IN   NNO    : NOMBRE DE NOEUDS DE LA FACE (3 OU 4)
! IN   NPG    : NOMBRE DE POINTS DE GAUSS DE L'ELEMENT FACE.
! IN   NOE    : LISTE DES NUMEROS DES NOEUDS PAR FACE (VOIR TE0003)
! IN   IFA    : NUMERO DE LA FACE (POUR DU 3D)
! IN   TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!               1 : HEXAEDRE      2 : PENTAEDRE
!               3 : TETRAEDRE     4 : PYRAMIDE
! IN   IDFDE  : ADRESSE DES DERIVEES DES FONCTIONS DE FORME SUR LA FACE
!
!      SORTIE :
!-------------
! POUR DU 2D :
! OUT  JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
! OUT  NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
! OUT  NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
! OUT  TX     : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
! OUT  TY     : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
! OUT  HF     : EN 2D : LONGUEUR DE L'ARETE
! POUR DU 3D :
! OUT  JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
! OUT  NX     : VECTEUR DES ABSCISSES DES NORMALES AUX POINTS DE GAUSS
! OUT  NY     : VECTEUR DES ORDONNEES DES NORMALES AUX POINTS DE GAUSS
! OUT  NZ     : VECTEUR DES COTES DES NORMALES AUX POINTS DE GAUSS
!
! ......................................................................
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/lteatt.h"
#include "asterfort/utmess.h"
    character(len=2) :: chdim
!
    integer :: iare, nnos, nnoa
    real(kind=8) :: orien, geom(*)
!
    integer :: nno, npg, noe(9, 6, 4)
    integer :: ifa, tymvol, idfde
!
    real(kind=8) :: jac(9), nx(9), ny(9), nz(9)
    real(kind=8) :: tx(3), ty(3), hf
!
!
!
! DECLARATION VARIABLES LOCALES
!
!
    integer :: i, j, ino, jno, mno, ipg, idec, jdec, kdec
    integer :: lenoeu, numno, nnos2
!
    real(kind=8) :: x1, y1, x2, y2, x3, y3, norme
    real(kind=8) :: x(9), y(9), z(9), sx(9, 9), sy(9, 9), sz(9, 9)
    aster_logical :: laxi
!
! ----------------------------------------------------------------------
!
! ----- METHODE :
!
!       M(U,V) SURFACE PARAMETREE
!       NORMALE A LA SURFACE DONNEE PAR
!       N(U,V)=DM(U,V)/DU (VECTORIEL) DM(U,V)/DV
!
!       EN 2D LE PRODUIT VECTORIEL ET LA DERIVEE DES FONCTIONS DE FORME
!       EST CALCULE DE MANIERE IMPLICITE.
!       EN 3D LE PRODUIT VECTORIEL EST CALCULE EXPLICITEMENT ET LA
!       DERIVEE DES FONCTIONS DE FORME EST RECUPERE DANS LA ROUTINE
!       CHAPEAU.
!       LES ELEMENTS DE BARSOUM SUBISSENT UN TRAITEMENT CAR LE JACOBIEN
!       EST NUL EN POINTE DE FISSURE.
!
!              X1          X3          X2
!               O-----------O-----------O
!              IARE      MNO         JNO
!
!         POINTS  1 --> IARE PREMIER POINT DE L'ARETE COURANTE
!                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
!                 3 --> MNO NOEUD MILIEU S'IL EXISTE
!
!====
! 1. --- CAS DU 2D -----------------------------------------------------
!====
!
    if (chdim .eq. '2D') then
!
! 1.1. ==> PREALABLE
!
        if (lteatt('AXIS','OUI')) then
            laxi = .true.
        else
            laxi = .false.
        endif
!
! 1.2. ==> COORDONNEES DES 3 NOEUDS
!
        if (iare .eq. nnos) then
            jno=1
        else
            jno=iare+1
        endif
!
        x1=geom(2*iare-1)
        y1=geom(2*iare)
        x2=geom(2*jno-1)
        y2=geom(2*jno)
!
! 1.3. ==> LONGUEUR DE L'ARETE
!
        hf=sqrt((x1-x2)**2+(y1-y2)**2)
!
        if (nnoa .eq. 3) then
            mno = nnos + iare
            x3 = geom(2*mno-1)
            y3 = geom(2*mno)
!
! 1.2.1. ==> TRAITEMENT ELEMENTS DE BARSOUM
!
! ----- LA NORMALE EST CALCULEE EN UTILISANT LE POINT MILIEU
            norme=sqrt((x3-x1)**2+(y3-y1)**2)/sqrt((x2-x1)**2+(y2-y1)&
            **2)
!
            if ((norme.lt.0.4d0) .or. (norme.gt.0.6d0)) then
!
                x3 = (x1+x2)*0.5d0
                y3 = (y1+y2)*0.5d0
!
            endif
!
! 1.2.2. ==> TRAITEMENT AUTRES ELEMENTS
!
        else
            x3 = (x1+x2)*0.5d0
            y3 = (y1+y2)*0.5d0
        endif
!
! 1.3. ==> CALCUL NORMALE SORTANTE, TANGENTE ET JACOBIEN PREMIER POINT
!
        x(1) = -( (y2-y1)*0.5d0 - (y1+y2-2.d0*y3) )
        y(1) = (x2-x1)*0.5d0 - (x1+x2-2.d0*x3)
!
        jac(1) = sqrt(x(1)**2+y(1)**2)
!
        if (laxi) then
            jac(1)=jac(1)*x1
        endif
!
        nx(1) = -(x(1)*orien)/(sqrt(x(1)**2+y(1)**2))
        ny(1) = -(y(1)*orien)/(sqrt(x(1)**2+y(1)**2))
!
! 1.4. ==> CALCUL NORMALE SORTANTE, TANGENTE ET JACOBIEN DEUXIEME POINT
!
        x(2) = -( (y2-y1)*0.5d0 + (y1+y2-2.d0*y3) )
        y(2) = (x2-x1)*0.5d0 + (x1+x2-2.d0*x3)
!
        jac(2) = sqrt(x(2)**2+y(2)**2)
!
        if (laxi) then
            jac(2) = jac(2)*x2
        endif
!
        nx(2) = -(x(2)*orien)/(sqrt(x(2)**2+y(2)**2))
        ny(2) = -(y(2)*orien)/(sqrt(x(2)**2+y(2)**2))
!
! 1.5. ==> CALCUL NORMALE SORTANTE, TANGENTE ET JACOBIEN TROISIEME POINT
!
        if (nnoa .eq. 3) then
!
            jac(3) = hf*0.5d0
!
            if (laxi) then
                jac(3) = jac(3)*(x2+x1)*0.5d0
            endif
!
            nx(3) = -orien*(y1-y2)/hf
            ny(3) = -orien*(x2-x1)/hf
!
        endif
!
! 1.6. ==> CALCUL DES TANGENTES
!
        do 16 , i = 1 , nnoa
        tx(i) = ny(i)
        ty(i) = -nx(i)
 16     continue
!
!====
! 2. -- CAS DU 3D ------------------------------------------------------
!====
!
    else if (chdim.eq.'3D') then
!
! 2.1. ==> COORDONNEES DES NOEUDS
!
        do 21 , numno=1,nno
        lenoeu=noe(numno,ifa,tymvol)
        x(numno)=geom(3*lenoeu-2)
        y(numno)=geom(3*lenoeu-1)
        z(numno)=geom(3*lenoeu)
 21     continue
!
! 2.2. ==> TRAITEMENT ELEMENTS DE BARSOUM
!
! ----- LA NORMALE EST CALCULEE EN UTILISANT LE POINT MILIEU
        if ((nno.eq.6) .or. (nno.eq.8)) then
!
            nnos2=nno/2
!
            do 22 , ino=1,nnos2
!
            if (ino .eq. nnos2) then
                jno=1
            else
                jno=ino+1
            endif
            mno=nnos2+ino
!
            norme=sqrt( (x(mno) - x(ino) )**2 + (y(mno) - y(ino) )&
                **2 + (z(mno) - z(ino) )**2 ) /sqrt( (x(jno) - x(ino)&
                )**2 + (y(jno) - y(ino) )**2 + (z(jno) - z(ino) )**2 )
!
            if ((norme.lt.0.4d0) .or. (norme.gt.0.6d0)) then
                x(mno)=(x(ino)+x(jno))*0.5d0
                y(mno)=(y(ino)+y(jno))*0.5d0
                z(mno)=(z(ino)+z(jno))*0.5d0
            endif
!
 22         continue
        endif
!
! 2.3. ==> CALCUL DU PRODUIT VECTORIEL OMI OMJ
!
        do 23 , i=1,nno
!
        do 231 , j=1,nno
!
        sx(i,j)=y(i)*z(j)-z(i)*y(j)
        sy(i,j)=z(i)*x(j)-x(i)*z(j)
        sz(i,j)=x(i)*y(j)-y(i)*x(j)
!
231     continue
!
 23     continue
!
! 2.4. ==> SOMMATION DES DERIVEES
!
        do 24 , ipg=1,npg
!
        nx(ipg) = 0.d0
        ny(ipg) = 0.d0
        nz(ipg) = 0.d0
!
        kdec=2*(ipg-1)*nno
!
        do 241 , i=1,nno
        idec=2*(i-1)
!
        do 242 , j=1,nno
        jdec=2*(j-1)
        nx(ipg)=nx(ipg)-zr(idfde+kdec+idec) *zr(idfde+&
                    kdec+jdec+1)*sx(i,j)
        ny(ipg)=ny(ipg)-zr(idfde+kdec+idec) *zr(idfde+&
                    kdec+jdec+1)*sy(i,j)
        nz(ipg)=nz(ipg)-zr(idfde+kdec+idec) *zr(idfde+&
                    kdec+jdec+1)*sz(i,j)
242     continue
!
241     continue
!
!        CALCUL DU JACOBIEN
!
        jac(ipg)=sqrt(nx(ipg)**2+ny(ipg)**2+nz(ipg)**2)
        nx(ipg)=nx(ipg)/jac(ipg)
        ny(ipg)=ny(ipg)/jac(ipg)
        nz(ipg)=nz(ipg)/jac(ipg)
!
 24     continue
!
! ----- PROBLEME -------------------------------------------------------
!
    else
!
        call utmess('F', 'UTILITAI_9', sk=chdim)
!
    endif
!
! ----------------------------------------------------------------------
!
end subroutine

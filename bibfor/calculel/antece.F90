subroutine antece(ino2, mail, tgeom, tailmi, epsi,&
                  nbante, nuno1)
    implicit none
!
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
!
! AUTEUR : G. ROUSSEAU
! BUT: ROUTINE TROUVANT L ANTECEDENT, S IL EXISTE,
!      D UN NOEUD DU MAILLAGE
!      IMAGE  PAR UNE TRANSFORMATION GEOMETRIQUE TGEOM (ROTATION
!      + TRANSLATION) D UN NOEUD D UN
!      CHAMNO DEFINI PAR EXEMPLE SUR LE MODELE THERMIQUE
!      D INTERFACE- APPLICATION AU CALCUL DE MATRICE DE
!      MASSE AJOUTEE AVEC UN MODELE GENERALISE
!
! ARGUMENTS :
!     IN : INTEGER: INO2 : NUMERO DU NOEUD DU MAILLAGE
!     IN : K8 : MAIL : NOM DU MAILLAGE
!     IN : R8 : TGEOM : TABLEAU DE 6 REELS - 3 COMPOSANTES
!        DE TRANSLATION PUIS 3 ANGLES NAUTIQUES DE ROTATION
!     IN : R8 : TAILMI: TAILLE DE MAILLE MIN DANS LE MAILLAGE
!     IN : R8 : EPSI: PRECISION RELATIVE SUR DISTANCE INTER-NOEUDS
!     OUT: INTEGER : NBANTE : NOMBRE D ANTECEDENTS TROUVES
!     OUT: INTEGER : NUNO1 : NUMERO DU NOEUD ANTECEDENT
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: mail
    integer :: nuno1, ino2, nbante
    real(kind=8) :: tgeom(6), tailmi, epsi
!
!
!
!
    integer ::  nbno
    real(kind=8) :: x1, y1, z1, x2, y2, z2, xp1, yp1, zp1, distan
    real(kind=8) :: ca(3), sa(3), rot(3)
!
!-----------------------------------------------------------------------
    integer :: ino1, nbid
    real(kind=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call getvr8(' ', 'DIST_REFE', scal=tailmi, nbret=nbid)
!
    call jeveuo(mail//'.COORDO    .VALE', 'L', vr=vale)
!
    x2 = vale((ino2 -1)*3 +1)
    y2 = vale((ino2 -1)*3 +2)
    z2 = vale((ino2 -1)*3 +3)
!
    call dismoi('NB_NO_MAILLA', mail, 'MAILLAGE', repi=nbno)
!
    ca(1)=cos(tgeom(4))
    sa(1)=sin(tgeom(4))
    ca(2)=cos(tgeom(5))
    sa(2)=sin(tgeom(5))
    ca(3)=cos(tgeom(6))
    sa(3)=sin(tgeom(6))
!
    rot(1)=0.0d0
    rot(2)=0.0d0
    rot(3)=0.0d0
!
    nbante=0
!
    do ino1 = 1, nbno
!
        x1 = vale((ino1 -1)*3 +1)
        y1 = vale((ino1 -1)*3 +2)
        z1 = vale((ino1 -1)*3 +3)
!
!
        rot(1)=ca(2)*ca(1)*x1+y1*(sa(3)*sa(2)*ca(1) -ca(3)*sa(1)) +z1*&
        (ca(3)*sa(2)*ca(1)+sa(3)*sa(1))
!
        rot(2)=sa(1)*ca(2)*x1+y1*(ca(3)*ca(1)+sa(2)*sa(1)*sa(3))&
        +z1*(ca(3)*sa(1)*sa(2)-sa(3)*ca(1))
!
        rot(3)=-x1*sa(2)+y1*sa(3)*ca(2)+z1*ca(3)*ca(2)
!
        xp1 =tgeom(1)+rot(1)
        yp1 =tgeom(2)+rot(2)
        zp1 =tgeom(3)+rot(3)
!
!           IF((X1.EQ.(1.5)).AND.(Y1.EQ.(0.0)).
!     &     AND.(Z1.EQ.(0.0))) THEN
!
!
!
!
!
!
!
!           ENDIF
!
        distan = sqrt((xp1-x2)**2 + (yp1-y2)**2 + (zp1-z2)**2)
!
!
!
!
        if (distan .lt. (epsi*tailmi)) then
            nuno1=ino1
            nbante= nbante+1
!
!
!
!
!
!
!
!
!
!
!               IF (NBANTE.GT.1) THEN
!
!
!
!
!               ENDIF
!
        endif
!
    end do
!
    call jedema()
end subroutine

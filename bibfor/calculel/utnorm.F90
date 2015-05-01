subroutine utnorm(igeom, nsomm, naret, ino, poinc1,&
                  poinc2, jno, mno, zrino2, zrino1,&
                  zrjno2, zrjno1, x3, y3, hf,&
                  xn, yn, jac, laxi, jacob,&
                  ifm, niv)
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
!    - FONCTION REALISEE:  UTILITAIRE DE CALCUL DE LA NORMALE A UN
!                          SEGMENT EN SES NOEUDS. POUR AERER TE0003
!
! IN IGEOM    : ADRESSE JEVEUX DU CHAMP GEOMETRIE
! IN NSOMM    : NOMBRE DE SOMMETS PAR ARETE
! IN NARET    : NOMBRE D'ARETES
! IN INO      : NUMERO DU NOEUD DE DEPART DE L'ARETE
! IN POINC1/2 : POIDS DE NEWTON-COTES
!   COORDONNEES POINTS D'INTERPOLATION
! IN LAXI     : LOGICAL POUR MODELISATION AXISYMETRIQUE
! IN JACOB    : REEL DEFINISSANT ORIENTATION (1 NORMAL, -1 INVERSEE)
! IN IFM/NIV  : PARAMETRES D'IMPRESSION
! OUT JNO     : NUMERO DU NOEUD EXTREME DE L'ARETE
! OUT MNO     : NUMERO DU NOEUD MILIEU DE L'ARETE
! OUT ZRINO2/1 : ABSCISSE/ORDONNEE POINT DE DEPART DE L'ARETE INO
! OUT ZRJNO2/1 : ABSCISSE/ORDONNEE POINT EXTREME DE L'ARETE JNO
! OUT X3/Y3    : ABSCISSE/ORDONNEE POINT MILIEU DE L'ARETE MNO
! OUT HF       : LONGUEUR DE L'ARETE
! OUT XN/YN/JAC : VALEUR DES COORD DE LA NORMALE EXTERNE ET DU
!                 JACOBIEN AUX POINTS CI-DESSUS
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       ENVIMA:R8MIEM.
!
!     FONCTIONS INTRINSEQUES:
!       SQRT.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       03/07/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
    integer :: igeom, nsomm, naret, ino, jno, mno, ifm, niv
    real(kind=8) :: zrino2, zrino1, zrjno2, zrjno1, x3, y3, xn(9), yn(9), jac(9)
    real(kind=8) :: hf, poinc1, poinc2, jacob
    aster_logical :: laxi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: i
    real(kind=8) :: x(3), y(3), aux, ovfl, jacob1
!
! INIT
    ovfl = r8miem()
    if (ino .eq. naret) then
        jno = 1
    else
        jno = ino+1
    endif
    zrjno1 = zr(igeom+2*jno-1)
    zrjno2 = zr(igeom+2*jno-2)
    zrino1 = zr(igeom+2*ino-1)
    zrino2 = zr(igeom+2*ino-2)
!
! CALCUL DU POINT MILIEU
    if (nsomm .eq. 3) then
        mno = naret+ino
        x3 = zr(igeom+2*mno-2)
        y3 = zr(igeom+2*mno-1)
    else
        mno = 0
        x3 = (zrino2+zrjno2)*0.5d0
        y3 = (zrino1+zrjno1)*0.5d0
    endif
!
! CALCUL DE HF
    hf = sqrt((zrino2-zrjno2)**2 +(zrino1-zrjno1)**2)
!
! CALCUL NORMALE, TANGENTE ET JACOBIEN PREMIER POINT D'INTEGRATION
    jacob1 = -1.d0*jacob
    x(1) = 0.5d0*(zrjno1 + 3.d0*zrino1 - 4.d0*y3 )
    y(1) = -0.5d0*(zrjno2 + 3.d0*zrino2 - 4.d0*x3 )
    aux = sqrt(y(1)**2 + x(1)**2)
    ASSERT(aux.gt.ovfl)
    jac(1) = aux*poinc1
    if (laxi) jac(1) = jac(1)*zrino2
    aux = 1.d0/aux
    xn(1) = x(1) * aux * jacob1
    yn(1) = y(1) * aux * jacob1
!
! CALCUL NORMALE, TANGENTE ET JACOBIEN DEUXIEME POINT D'INTEGRATION
    x(2) = -0.5d0*(3.d0*zrjno1 + zrino1 - 4.d0*y3)
    y(2) = 0.5d0*(3.d0*zrjno2 + zrino2 - 4.d0*x3)
    aux = sqrt(y(2)**2 + x(2)**2)
    ASSERT(aux.gt.ovfl)
    jac(2) = aux*poinc1
    if (laxi) jac(2) = jac(2)*zrjno2
    aux = 1.d0/aux
    xn(2) = x(2) * aux * jacob1
    yn(2) = y(2) * aux * jacob1
!
    if (nsomm .eq. 3) then
!
! CALCUL NORMALE, TANGENTE ET JACOBIEN TROISIEME POINT D'INTEGRATION
        ASSERT(hf.gt.ovfl)
        aux = 1.d0/hf
        xn(3) = (zrino1 - zrjno1) * aux * jacob1
        yn(3) = (zrjno2 - zrino2) * aux * jacob1
        jac(3) = hf*0.5d0*poinc2
        if (laxi) jac(3) = jac(3)*x3
    else
        xn(3) = 0.d0
        yn(3) = 0.d0
        jac(3) = 0.d0
    endif
!
    if (niv .eq. 2) then
        write(ifm,*)
        write(ifm,*)'NUMERO D''ARETE/HF ',ino,hf
        write(ifm,*)'NOMBRE DE SOMMETS ',nsomm
        if (nsomm .eq. 3) then
            write(ifm,*)'CONNECTIQUE ',ino,jno,mno
        else
            write(ifm,*)'CONNECTIQUE ',ino,jno
        endif
        write(ifm,*)'XN  ',(xn(i),i=1,nsomm)
        write(ifm,*)'YN  ',(yn(i),i=1,nsomm)
        write(ifm,*)'JAC ',(jac(i),i=1,nsomm)
    endif
end subroutine

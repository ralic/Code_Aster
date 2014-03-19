subroutine te0564(option, nomte)
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
!.......................................................................
!
!     BUT:
!       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
!          CALCUL DU CHAMP ELEMENTAIRE A 6 COMPOSANTES :
!          SOMME/S_ELEMENT(DS,X.DS,Y.DS,X*X.DS,Y*Y.DS,X*Y.DS)
!          SUR LES ELEMENTS DE BORD DES ELEMENTS 2D
!
!          CES 6 QUANTITES GEOMETRIQUES SONT NOTEES :
!          A1 = S,AX,AY,AXX,AYY,AXY
!
!       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
!          CALCUL DES 2 VECTEURS DEFINIS AUX NOEUDS DES ELEMENTS
!          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
!          POUR LE PREMIER VECTEUR
!               SOMME/S_ELEMENT(NI.DS,0,0)
!          POUR LE SECOND VECTEUR
!               SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS)
!          SUR LES ELEMENTS DE BORD DES ELEMENTS 2D
!
!          AVEC X = XM - XG = NJ*XJ - XG
!               Y = YM - YG = NJ*YJ - YG
!               Z = ZM - ZG = NJ*ZJ - ZG
!          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
!                        DU LIGREL DES MAILLES DE BORD (SEG) TRAITEES
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: elrefe
    character(len=16) :: nomte, option
    real(kind=8) :: jac, jacpoi, zero
    real(kind=8) :: xg, yg
    real(kind=8) :: dxdk, dydk, axgau, aygau
    real(kind=8) :: xgau, ygau, axxgau, ayygau
    real(kind=8) :: axygau
    integer :: nno, nnos, jgano, ndim, ipg, npg, idfdk, iopt
    integer :: ldec, isect, i, iorig, ivect1
    integer :: ivect2, ino, ipoids, ivf, igeom
!
!
!
    call jemarq()
!
    call elref1(elrefe)
!
    zero = 0.0d0
    iopt = 0
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
! --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!     ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
    if (option .eq. 'CARA_SECT_POUT3') then
        call jevech('PCASECT', 'E', isect)
        iopt = 3
        do 10 i = 1, 6
            zr(isect+i-1) = zero
10      continue
!
    else if (option.eq.'CARA_SECT_POUT4') then
        call jevech('PORIGIN', 'L', iorig)
        call jevech('PVECTU1', 'E', ivect1)
        call jevech('PVECTU2', 'E', ivect2)
        iopt = 4
        xg = zr(iorig+1-1)
        yg = zr(iorig+2-1)
        do 20 i = 1, 2*nno
            zr(ivect1+i-1) = zero
            zr(ivect2+i-1) = zero
20      continue
!
    endif
!
!     ---------------------------
! --- - OPTION : CARA_SECT_POUT3-
!     ---------------------------
!
    if (iopt .eq. 3) then
!
! --- BOUCLE SUR LES POINTS DE GAUSS :
!     ------------------------------
        do 70 ipg = 1, npg
!
            ldec = (ipg-1)*nno
!
            dxdk = zero
            dydk = zero
!
! ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
!       -------------------------------------------------
            do 40 ino = 1, nno
                i = igeom + 2* (ino-1) - 1
                dxdk = dxdk + zr(i+1)*zr(idfdk+ldec+ino-1)
                dydk = dydk + zr(i+2)*zr(idfdk+ldec+ino-1)
40          continue
!
! ---   JACOBIEN :
!       --------
            jac = sqrt(dxdk*dxdk+dydk*dydk)
            if (jac .le. r8prem()) then
                call utmess('F', 'ELEMENTS4_34')
            endif
            jacpoi = jac*zr(ipoids+ipg-1)
!
! ---   CALCUL DE AX, AY = SOMME(X.DS, Y.DS) :
!       ----------------------------------------------
            axgau = zero
            aygau = zero
!
            do 50 ino = 1, nno
                i = igeom + 2* (ino-1) - 1
                axgau = axgau + zr(ivf+ldec+ino-1)*zr(i+1)
                aygau = aygau + zr(ivf+ldec+ino-1)*zr(i+2)
50          continue
!
! ---   CALCUL DE  AXX, AYY, AXY = SOMME(X*X.DS, Y*Y.DS, X*Y.DS) :
!       -------------------------------------------------------
            xgau = zero
            ygau = zero
!
            do 60 ino = 1, nno
                i = igeom + 2* (ino-1) - 1
                xgau = xgau + zr(ivf+ldec+ino-1)*zr(i+1)
                ygau = ygau + zr(ivf+ldec+ino-1)*zr(i+2)
60          continue
!
            axxgau = xgau*xgau
            ayygau = ygau*ygau
            axygau = xgau*ygau
!
!---  CALCUL DE A1 = S
            zr(isect+1-1) = zr(isect+1-1) + jacpoi
!---  AX
            zr(isect+2-1) = zr(isect+2-1) + axgau*jacpoi
!---  AY
            zr(isect+3-1) = zr(isect+3-1) + aygau*jacpoi
!---  AXX
            zr(isect+4-1) = zr(isect+4-1) + axxgau*jacpoi
!---  AYY
            zr(isect+5-1) = zr(isect+5-1) + ayygau*jacpoi
!---  AXY
            zr(isect+6-1) = zr(isect+6-1) + axygau*jacpoi
!
70      continue
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
! --- ET FIN DE L'OPTION 'CARA_SECT_POUT3'
!
!     ---------------------------
! --- - OPTION : CARA_SECT_POUT4-
!     ---------------------------
!
    else if (iopt.eq.4) then
!
! --- BOUCLE SUR LES POINTS DE GAUSS :
!     ------------------------------
        do 110 ipg = 1, npg
!
            ldec = (ipg-1)*nno
!
            dxdk = zero
            dydk = zero
!
! ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
!       -------------------------------------------------
            do 80 ino = 1, nno
                i = igeom + 2* (ino-1) - 1
                dxdk = dxdk + zr(i+1)*zr(idfdk+ldec+ino-1)
                dydk = dydk + zr(i+2)*zr(idfdk+ldec+ino-1)
80          continue
!
! ---   JACOBIEN :
!       --------
            jac = sqrt(dxdk*dxdk+dydk*dydk)
            if (jac .le. r8prem()) then
                call utmess('F', 'ELEMENTS4_34')
            endif
            jacpoi = jac*zr(ipoids+ipg-1)
!
!---    CALCUL DE VECT1(I) = SOMME(NI.DS, 0)
!       ---------------------------------------
            do 120 ino = 1, nno
                zr(ivect1+2*(ino-1)+1-1) = zr(ivect1+2*(ino-1)+1-1 ) + zr(ivf+ldec+ino-1)*jacpoi
120          continue
!
!---    CALCUL DE VECT2(I) = SOMME(X*NI.DS, Y*NI.DS)
!       --------------------------------------------
            xgau = zero
            ygau = zero
!
            do 130 ino = 1, nno
                i = igeom + 2*(ino-1) -1
                xgau = xgau + zr(ivf+ldec+ino-1) * zr(i+1)
                ygau = ygau + zr(ivf+ldec+ino-1) * zr(i+2)
130          continue
!
            do 140 ino = 1, nno
                zr(ivect2+2*(ino-1)) = zr(ivect2+2*(ino-1)) + zr(ivf+ ldec+ino-1)*(xgau-xg&
                                       )*jacpoi
                zr(ivect2+2*(ino-1)+1) = zr(ivect2+2*(ino-1)+1) + zr(ivf+ldec+ino-1)*(ygau-yg&
                                         )*jacpoi
140          continue
!
!
!
110      continue
!
! ---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
! ---  ET FIN DE L'OPTION 'CARA_SECT_POUT4'
!
    endif
!
    call jedema()
!
end subroutine

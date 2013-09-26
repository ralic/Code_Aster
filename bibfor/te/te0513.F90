subroutine te0513(option, nomte)
    implicit none
! aslint: disable=W0104
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
    character(len=16) :: nomte, option
! ======================================================================
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
!     BUT:
!       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
!          CALCUL DU CHAMP ELEMENTAIRE SUR DES ELEMENTS
!          ISOPARAMETRIQUES DE COQUE
!           10 COMPOSANTES
!              SOMME/S_ELEMENT(DS,  X.DS,  Y.DS,  Z.DS,
!                                   X*X.DS,Y*Y.DS,Z*Z.DS,
!                                   X*Y.DS,X*Z.DS,Y*Z.DS)
!              CES 10 QUANTITES GEOMETRIQUES SONT NOTEES :
!                    S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
!
!            6 COMPOSANTES
!              VECTEUR TANGENT ORTHONORMAL A l'ELEMENT
!                    VT1 VT2
!
!
!       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
!          CALCUL DES 2 VECTEURS DEFINIS AUX NOEUDS DES ELEMENTS
!          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
!          POUR LE PREMIER VECTEUR
!               SOMME/S_ELEMENT(NI.DS,0,0,0,0,0)
!          POUR LE SECOND VECTEUR
!               SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS,Z*NI.DS,0,0,0)
!          SUR DES ELEMENTS ISOPARAMETRIQUES DE COQUE ==> 6DDL/NOEUD
!
!          AVEC X = XM - XG = NJ*XJ - XG
!               Y = YM - YG = NJ*YJ - YG
!               Z = ZM - ZG = NJ*ZJ - ZG
!          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
!                        DU LIGREL DES MAILLES DE SURFACE TRAITE
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    real(kind=8) :: norme, jac, nx, ny, nz, zero
    real(kind=8) :: sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: sigau, axgau, aygau, azgau, xgau, ygau, zgau
    real(kind=8) :: axxgau, ayygau, azzgau, axygau, axzgau, ayzgau
    real(kind=8) :: vt1(3), vt2(3), vnn(3)
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ipg, npg1, nnos, jgano
    integer :: idec, jdec, kdec, ldec
    integer :: ino, jno, nddlno
    integer :: i, j, isect, iorig
    integer :: ivect1, ivect2
!
!
    zero = 0.0d0
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!     COQUE ==> 6 DDL PAR NOEUD
    nddlno=6
!
    call jevech('PGEOMER', 'L', igeom)
!
    if (option .eq. 'CARA_SECT_POUT3') then
        call jevech('PCASECT', 'E', isect)
!
        do i = 1, 10
            zr(isect+i-1) = zero
        end do
!
    else if (option.eq.'CARA_SECT_POUT4') then
        call jevech('PORIGIN', 'L', iorig)
        call jevech('PVECTU1', 'E', ivect1)
        call jevech('PVECTU2', 'E', ivect2)
!
        do i = 1, nddlno*nno
            zr(ivect1+i-1) = zero
            zr(ivect2+i-1) = zero
        end do
!
    endif
!
!     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
    do ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
        end do
    end do
!
!    ---------------------------
!--- - OPTION : CARA_SECT_POUT3-
!    ---------------------------
    if (option .eq. 'CARA_SECT_POUT3') then
!     BOUCLE SUR LES POINTS DE GAUSS
        do ipg = 1, npg1
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!
!           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
            do i = 1, nno
                idec = idfdx+kdec+(i-1)*ndim
                do j = 1, nno
                    jdec = idfdy+kdec+(j-1)*ndim
                    nx = nx + zr(idec)*zr(jdec)*sx(i,j)
                    ny = ny + zr(idec)*zr(jdec)*sy(i,j)
                    nz = nz + zr(idec)*zr(jdec)*sz(i,j)
                end do
            end do
!
!           LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
            jac = sqrt (nx*nx + ny*ny + nz*nz)
            sigau = zr(ipoids+ipg-1)*jac
!           CALCUL DE AX, AY, AZ = SOMME(X.DS, Y.DS, Z.DS)
            axgau = zero
            aygau = zero
            azgau = zero
            do ino = 1, nno
                i = igeom + 3*(ino-1) -1
                axgau = axgau + zr(ivf+ldec+ino-1) * zr(i+1)
                aygau = aygau + zr(ivf+ldec+ino-1) * zr(i+2)
                azgau = azgau + zr(ivf+ldec+ino-1) * zr(i+3)
            end do
!
!---        CALCUL DE  AXX, AYY, AZZ, AXY, AXZ, AYZ
!---        = SOMME(X*X.DS, Y*Y.DS, Z*Z.DS, X*Y.DS, X*Z.DS, Y*Z.DS)
            xgau = zero
            ygau = zero
            zgau = zero
!
            do ino = 1, nno
                i = igeom + 3*(ino-1) -1
                xgau = xgau + zr(ivf+ldec+ino-1) * zr(i+1)
                ygau = ygau + zr(ivf+ldec+ino-1) * zr(i+2)
                zgau = zgau + zr(ivf+ldec+ino-1) * zr(i+3)
            end do
!
            axxgau = xgau * xgau
            ayygau = ygau * ygau
            azzgau = zgau * zgau
            axygau = xgau * ygau
            axzgau = xgau * zgau
            ayzgau = ygau * zgau
!
!           CALCUL DE A1 = S
            zr(isect+1-1) = zr(isect+1-1) + sigau
!           AX
            zr(isect+2-1) = zr(isect+2-1) + axgau* sigau
!           AY
            zr(isect+3-1) = zr(isect+3-1) + aygau* sigau
!           AZ
            zr(isect+4-1) = zr(isect+4-1) + azgau* sigau
!           AXX
            zr(isect+5-1) = zr(isect+5-1) + axxgau*sigau
!           AYY
            zr(isect+6-1) = zr(isect+6-1) + ayygau*sigau
!           AZZ
            zr(isect+7-1) = zr(isect+7-1) + azzgau*sigau
!           AXY
            zr(isect+8-1) = zr(isect+8-1) + axygau*sigau
!           AXZ
            zr(isect+9-1) = zr(isect+9-1) + axzgau*sigau
!           AYZ
            zr(isect+10-1) = zr(isect+10-1) + ayzgau*sigau
        end do
!---     FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
        i = igeom - 1
!        ARETE 1
        vt1(1) = zr(i+4) - zr(i+1)
        vt1(2) = zr(i+5) - zr(i+2)
        vt1(3) = zr(i+6) - zr(i+3)
!        ARETE 2
        vt2(1) = zr(i+7) - zr(i+1)
        vt2(2) = zr(i+8) - zr(i+2)
        vt2(3) = zr(i+9) - zr(i+3)
!        VECTEUR NORMAL VN = VT1^VT2
        call provec(vt1, vt2, vnn)
!        VECTEUR TANGENT 2
        call provec(vnn, vt1, vt2)
!        VECTEUR TANGENT 1 et 2, NORMES
        call normev(vt1, norme)
        call normev(vt2, norme)
!
        zr(isect+11-1) = vt1(1)
        zr(isect+12-1) = vt1(2)
        zr(isect+13-1) = vt1(3)
        zr(isect+14-1) = vt2(1)
        zr(isect+15-1) = vt2(2)
        zr(isect+16-1) = vt2(3)
!---  FIN DE L'OPTION 'CARA_SECT_POUT3'
!
!    ---------------------------
!--- - OPTION : CARA_SECT_POUT4-
!    ---------------------------
    else if (option.eq.'CARA_SECT_POUT4') then
!        BOUCLE SUR LES POINTS DE GAUSS
        do ipg = 1, npg1
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
            do i = 1, nno
                idec = idfdx+kdec+(i-1)*ndim
                do j = 1, nno
                    jdec = idfdy+kdec+(j-1)*ndim
                    nx = nx + zr(idec)*zr(jdec)*sx(i,j)
                    ny = ny + zr(idec)*zr(jdec)*sy(i,j)
                    nz = nz + zr(idec)*zr(jdec)*sz(i,j)
                end do
            end do
!
!           LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
            jac = sqrt (nx*nx + ny*ny + nz*nz)
            sigau = zr(ipoids+ipg-1)*jac
!           VECT1(I) = SOMME(NI.DS, 0, 0, 0, 0, 0)
            do ino = 1, nno
                i = ivect1+nddlno*(ino-1)
                zr(i) = zr(i) + zr(ivf+ldec+ino-1)*sigau
            end do
!
!           VECT2(I) = SOMME(X*NI.DS, Y*NI.DS, Z*NI.DS, 0, 0, 0)
            xgau = zero
            ygau = zero
            zgau = zero
!
            do  ino = 1, nno
                i = igeom + 3*(ino-1) -1
                j = ivf+ldec+ino-1
                xgau = xgau + zr(j) * zr(i+1)
                ygau = ygau + zr(j) * zr(i+2)
                zgau = zgau + zr(j) * zr(i+3)
            end do
!
            do ino = 1, nno
                i = ivect2+nddlno*(ino-1)-1
                j = ivf+ldec+ino-1
                zr(i+1) = zr(i+1) + zr(j)*(xgau-zr(iorig+1-1))*sigau
                zr(i+2) = zr(i+2) + zr(j)*(ygau-zr(iorig+2-1))*sigau
                zr(i+3) = zr(i+3) + zr(j)*(zgau-zr(iorig+3-1))*sigau
            end do
        end do
!---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!---   ET FIN DE L'OPTION 'CARA_SECT_POUT4'
    endif
end subroutine

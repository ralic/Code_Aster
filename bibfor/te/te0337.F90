subroutine te0337(option, nomte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_20
!.......................................................................
!
!     BUT:
!       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
!          CALCUL DU CHAMP ELEMENTAIRE A 10 COMPOSANTES :
!          SOMME/S_ELEMENT(DS,X.DS,Y.DS,Z.DS,X*X.DS,Y*Y.DS,Z*Z.DS,
!                             X*Y.DS,X*Z.DS,Y*Z.DS)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          CES 10 QUANTITES GEOMETRIQUES SONT NOTEES :
!          A1 = S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
!
!       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
!          CALCUL DES 2 VECTEURS DEFINIS AUX NOEUDS DES ELEMENTS
!          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
!          POUR LE PREMIER VECTEUR
!               SOMME/S_ELEMENT(NI.DS,0,0)
!          POUR LE SECOND VECTEUR
!               SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS,Z*NI.DS)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          AVEC X = XM - XG = NJ*XJ - XG
!               Y = YM - YG = NJ*YJ - YG
!               Z = ZM - ZG = NJ*ZJ - ZG
!          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
!                        DU LIGREL DES MAILLES DE SURFACE TRAITE
!
!       3) POUR L'OPTION : 'CARA_SECT_POUT5 ' : 3D_TUYAU
!          CALCUL DU VECTEUR DEFINI AUX NOEUDS DES ELEMENTS
!          AYANT POUR VALEURS AU NOEUD I DE L'ELEMENT:
!          SOMME/S_ELEMENT(NI.COS(M.PHI).P.DS)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/angvxy.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/vdiff.h'
    include 'blas/ddot.h'
    character(len=16) :: nomte, option
    real(kind=8) :: jacpoi, e1(3), e2(3)
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), zero
    real(kind=8) :: gp0(3), gpg(3), xpg(3), vsin(3)
    real(kind=8) :: norgp0, norgpg, angl(3), pgl(3, 3)
    real(kind=8) :: cosphi, sinphi, phi, cosmfi, sinmfi, phi0, ray
    real(kind=8) :: sigau, axgau, aygau, azgau, xgau, ygau, zgau
    real(kind=8) :: axxgau, ayygau, azzgau, axygau, axzgau, ayzgau
    integer :: ipoids, ivf, idfdx, idfdy, igeom, m
    integer :: ndim, nno, ipg, npg1, inumod, nnos, jgano
    integer :: idec, jdec, kdec, ldec
    integer :: ino, jno, ii
    integer :: i, j, isect, iorig, iorifi, iaxe
    integer :: ivect1, ivect2, ivect3, ivect4, ivect5, ivect6
!
!
!
!
    zero = 0.0d0
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
!
    if (option .eq. 'CARA_SECT_POUT3') then
        call jevech('PCASECT', 'E', isect)
!
        do 20 i = 1, 10
            zr(isect+i-1) = 0.0d0
20      continue
!
    else if (option.eq.'CARA_SECT_POU3R') then
        call jevech('PORIGIN', 'L', iorig)
        call jevech('PRAYONM', 'E', isect)
        zr(isect+1-1) = zero
!
    else if (option.eq.'CARA_SECT_POUT4') then
        call jevech('PORIGIN', 'L', iorig)
        call jevech('PVECTU1', 'E', ivect1)
        call jevech('PVECTU2', 'E', ivect2)
!
        do 30 i = 1, 3*nno
            zr(ivect1+i-1) = zero
            zr(ivect2+i-1) = zero
30      continue
!
    else if (option.eq.'CARA_SECT_POUT5') then
        call jevech('PORIGIN', 'L', iorig)
        call jevech('PORIGFI', 'L', iorifi)
        call jevech('PNUMMOD', 'L', inumod)
        call jevech('PCAORIE', 'L', iaxe)
        call jevech('PVECTU1', 'E', ivect1)
        call jevech('PVECTU2', 'E', ivect2)
        call jevech('PVECTU3', 'E', ivect3)
        call jevech('PVECTU4', 'E', ivect4)
        call jevech('PVECTU5', 'E', ivect5)
        call jevech('PVECTU6', 'E', ivect6)
!
        e1(1) = zr(iaxe+1-1)
        e1(2) = zr(iaxe+2-1)
        e1(3) = zr(iaxe+3-1)
!
!         COORDONNES DU POINT P TEL QUE GP EST L'ORIGINE
!         DE L'ANGLE PHI
!
        call vdiff(3, zr(iorifi), zr(iorig), gp0)
        call normev(gp0, norgp0)
!
!         NUMERO DE MODE DE FOURIER
!
        m = zi(inumod)
        do 40 i = 1, 3*nno
            zr(ivect1+i-1) = zero
            zr(ivect2+i-1) = zero
            zr(ivect3+i-1) = zero
            zr(ivect4+i-1) = zero
            zr(ivect5+i-1) = zero
            zr(ivect6+i-1) = zero
40      continue
    endif
!
!--- CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 41 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 50 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
50      continue
41  end do
!
!    ---------------------------
!--- - OPTION : CARA_SECT_POUT3-
!    ---------------------------
!
    if (option .eq. 'CARA_SECT_POUT3') then
!
!---  BOUCLE SUR LES POINTS DE GAUSS
!     ------------------------------
!
        do 60 ipg = 1, npg1
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
!
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!
!---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!       ------------------------------------------
!
            do 70 i = 1, nno
                idec = (i-1)*ndim
                do 70 j = 1, nno
                    jdec = (j-1)*ndim
!
                    nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sx(i,j)
                    ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sy(i,j)
                    nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sz(i,j)
!
70              continue
!
!---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
!     ---------------------------------------------
!
            jac = sqrt (nx*nx + ny*ny + nz*nz)
!
            sigau = zr(ipoids+ipg-1)*jac
!
!---  CALCUL DE AX, AY, AZ = SOMME(X.DS, Y.DS, Z.DS)
!     ----------------------------------------------
!
            axgau = zero
            aygau = zero
            azgau = zero
!
            do 80 ino = 1, nno
                i = igeom + 3*(ino-1) -1
!
                axgau = axgau + zr(ivf+ldec+ino-1) * zr(i+1)
                aygau = aygau + zr(ivf+ldec+ino-1) * zr(i+2)
                azgau = azgau + zr(ivf+ldec+ino-1) * zr(i+3)
80          continue
!
!---   CALCUL DE  AXX, AYY, AZZ, AXY, AXZ, AYZ
!---   = SOMME(X*X.DS, Y*Y.DS, Z*Z.DS, X*Y.DS, X*Z.DS, Y*Z.DS)
!      -------------------------------------------------------
!
            xgau = zero
            ygau = zero
            zgau = zero
!
            do 90 ino = 1, nno
                i = igeom + 3*(ino-1) -1
!
                xgau = xgau + zr(ivf+ldec+ino-1) * zr(i+1)
                ygau = ygau + zr(ivf+ldec+ino-1) * zr(i+2)
                zgau = zgau + zr(ivf+ldec+ino-1) * zr(i+3)
90          continue
!
            axxgau = xgau * xgau
            ayygau = ygau * ygau
            azzgau = zgau * zgau
            axygau = xgau * ygau
            axzgau = xgau * zgau
            ayzgau = ygau * zgau
!
!---  CALCUL DE A1 = S
            zr(isect+1-1) = zr(isect+1-1) + sigau
!---  AX
            zr(isect+2-1) = zr(isect+2-1) + axgau* sigau
!---  AY
            zr(isect+3-1) = zr(isect+3-1) + aygau* sigau
!---  AZ
            zr(isect+4-1) = zr(isect+4-1) + azgau* sigau
!---  AXX
            zr(isect+5-1) = zr(isect+5-1) + axxgau*sigau
!---  AYY
            zr(isect+6-1) = zr(isect+6-1) + ayygau*sigau
!---  AZZ
            zr(isect+7-1) = zr(isect+7-1) + azzgau*sigau
!---  AXY
            zr(isect+8-1) = zr(isect+8-1) + axygau*sigau
!---  AXZ
            zr(isect+9-1) = zr(isect+9-1) + axzgau*sigau
!---  AYZ
            zr(isect+10-1) = zr(isect+10-1) + ayzgau*sigau
!
60      continue
!--- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!---  ET FIN DE L'OPTION 'CARA_SECT_POUT3'
!
!    ---------------------------
!--- - OPTION : CARA_SECT_POU3R-
!    ---------------------------
!
    else if (option.eq.'CARA_SECT_POU3R') then
!
!
!---  BOUCLE SUR LES POINTS DE GAUSS
!     ------------------------------
!
        do 63 ipg = 1, npg1
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
!
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!
!---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!       ------------------------------------------
!
            do 73 i = 1, nno
                idec = (i-1)*ndim
                do 73 j = 1, nno
                    jdec = (j-1)*ndim
!
                    nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sx(i,j)
                    ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sy(i,j)
                    nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sz(i,j)
!
73              continue
!
!---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
!     ---------------------------------------------
!
            jac = sqrt (nx*nx + ny*ny + nz*nz)
            sigau = zr(ipoids+ipg-1)*jac
            xgau = zero
            ygau = zero
            zgau = zero
!
            do 93 ino = 1, nno
                i = igeom + 3*(ino-1) -1
!
                xgau = xgau + zr(ivf+ldec+ino-1) * zr(i+1)
                ygau = ygau + zr(ivf+ldec+ino-1) * zr(i+2)
                zgau = zgau + zr(ivf+ldec+ino-1) * zr(i+3)
93          continue
            ray = (xgau-zr(iorig+1-1))**2.d0 + (ygau-zr(iorig+2-1))** 2.d0 + (zgau-zr(iorig+3-1)&
                  )**2.d0
            ray = sqrt( ray )
!
!---  CALCUL DE A1 = RAYON
            zr(isect+1-1) = zr(isect+1-1) + ray*sigau
!
63      continue
!--- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!---  ET FIN DE L'OPTION 'CARA_SECT_POU3R'
!
!    ---------------------------
!--- - OPTION : CARA_SECT_POUT4-
!    ---------------------------
!
    else if (option.eq.'CARA_SECT_POUT4') then
!
!---  BOUCLE SUR LES POINTS DE GAUSS
!     ------------------------------
!
        do 100 ipg = 1, npg1
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
!
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!
!---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!       ------------------------------------------
!
            do 110 i = 1, nno
                idec = (i-1)*ndim
                do 110 j = 1, nno
                    jdec = (j-1)*ndim
!
                    nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sx(i,j)
                    ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sy(i,j)
                    nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sz(i,j)
!
110              continue
!
!---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
!     ---------------------------------------------
!
            jac = sqrt (nx*nx + ny*ny + nz*nz)
!
            sigau = zr(ipoids+ipg-1)*jac
!
!---  CALCUL DE VECT1(I) = SOMME(NI.DS, 0, 0)
!     ---------------------------------------
!
            do 120 ino = 1, nno
!
                zr(ivect1+3*(ino-1)+1-1) = zr( ivect1+3*(ino-1)+1-1 ) + zr(ivf+ldec+ino-1 )*sigau
120          continue
!
!---  CALCUL DE VECT2(I) = SOMME(X*NI.DS, Y*NI.DS, Z*NI.DS)
!     -----------------------------------------------------
!
            xgau = zero
            ygau = zero
            zgau = zero
!
            do 130 ino = 1, nno
                i = igeom + 3*(ino-1) -1
!
                xgau = xgau + zr(ivf+ldec+ino-1) * zr(i+1)
                ygau = ygau + zr(ivf+ldec+ino-1) * zr(i+2)
                zgau = zgau + zr(ivf+ldec+ino-1) * zr(i+3)
130          continue
!
            do 140 ino = 1, nno
!
                zr(ivect2+3*(ino-1)+1-1) = zr(&
                                           ivect2+3*(ino-1)+1-1) + zr(ivf+ldec+ino-1)*(xgau-zr(io&
                                           &rig+1-1)&
                                           )*sigau
!
                zr(ivect2+3*(ino-1)+2-1) = zr(&
                                           ivect2+3*(ino-1)+2-1) + zr(ivf+ldec+ino-1)*(ygau-zr(io&
                                           &rig+2-1)&
                                           )*sigau
!
                zr(ivect2+3*(ino-1)+3-1) = zr(&
                                           ivect2+3*(ino-1)+3-1) + zr(ivf+ldec+ino-1)*(zgau-zr(io&
                                           &rig+3-1)&
                                           )*sigau
140          continue
!
100      continue
!---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!---   ET FIN DE L'OPTION 'CARA_SECT_POUT4'
!
!     ---------------------------
! --- - OPTION : CARA_SECT_POUT5-
!     ---------------------------
!
    else if (option.eq.'CARA_SECT_POUT5') then
!
!
        do 200 ipg = 1, npg1
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
!
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!
!---    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!       ------------------------------------------
!
            do 210 i = 1, nno
                idec = (i-1)*ndim
                do 210 j = 1, nno
                    jdec = (j-1)*ndim
!
                    nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sx(i,j)
                    ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sy(i,j)
                    nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sz(i,j)
!
210              continue
!
!---  LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE
!     ---------------------------------------------
!
            jac = sqrt (nx*nx + ny*ny + nz*nz)
            jacpoi = jac*zr(ipoids+ipg-1)
!
! ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
!       ------------------------------------------
            do 219 ii = 1, 3
                xpg(ii) = zero
219          continue
            do 220 ino = 1, nno
                i = igeom + 3*(ino-1) -1
                xpg(1) = xpg(1) + zr(ivf+ldec+ino-1) * zr(i+1)
                xpg(2) = xpg(2) + zr(ivf+ldec+ino-1) * zr(i+2)
                xpg(3) = xpg(3) + zr(ivf+ldec+ino-1) * zr(i+3)
220          continue
!
!  CALCUL DU VECTEUR G-PG ET DE L'ANGLE PHI ENTRE G-P0 ET G-PG
!
            call vdiff(3, xpg, zr(iorig), gpg)
            call normev(gpg, norgpg)
            cosphi=ddot(3,gp0,1,gpg,1)
!PM          CALL PROVEC(GP0,GPG,VSIN)
            call provec(gpg, gp0, vsin)
            sinphi=ddot(3,e1,1,vsin,1)
            phi0=atan2(sinphi,cosphi)
!JMP          PHI=-PHI0
            phi=phi0
            cosmfi=cos(m*phi)
            sinmfi=sin(m*phi)
!
!  CALCUL DE PGL MATRICE DE PASSAGE DE X,Y,Z GLOBAL A E1,E2,E3
!
            call provec(gpg, e1, e2)
            call angvxy(e1, e2, angl)
            call matrot(angl, pgl)
!
            do 230 ino = 1, nno
                do 231 ii = 1, 3
!
! CALCUL DE VECT1(I) : TERMES EN UMI(COS(M.PHI)) ET UMO (SIN(M.PHI))
!
                    zr(ivect1+3*(ino-1)+ii-1) = zr(&
                                                ivect1+3*(ino-1)+ ii-1 )+ cosmfi*pgl(1,&
                                                ii)*zr(ivf+ldec+ino-1&
                                                )*jacpoi
                    zr(ivect4+3*(ino-1)+ii-1) = zr(&
                                                ivect4+3*(ino-1)+ ii-1 )+ sinmfi*pgl(1,&
                                                ii)*zr(ivf+ldec+ino-1&
                                                )*jacpoi
!
! CALCUL DE VECT2(I) : TERMES EN VMI(COS(M.PHI)) ET VMO (SIN(M.PHI))
!
                    zr(ivect2+3*(ino-1)+ii-1) = zr(&
                                                ivect2+3*(ino-1)+ ii-1 )+ cosmfi*pgl(2,&
                                                ii)*zr(ivf+ldec+ino-1&
                                                )*jacpoi
                    zr(ivect5+3*(ino-1)+ii-1) = zr(&
                                                ivect5+3*(ino-1)+ ii-1 )+ sinmfi*pgl(2,&
                                                ii)*zr(ivf+ldec+ino-1&
                                                )*jacpoi
!
! CALCUL DE VECT3(I) : TERMES EN WMI(COS(M.PHI)) ET WMO (SIN(M.PHI))
!
                    zr(ivect3+3*(ino-1)+ii-1) = zr(&
                                                ivect3+3*(ino-1)+ ii-1 )+ cosmfi*pgl(3,&
                                                ii)*zr(ivf+ldec+ino-1&
                                                )*jacpoi
                    zr(ivect6+3*(ino-1)+ii-1) = zr(&
                                                ivect6+3*(ino-1)+ ii-1 )+ sinmfi*pgl(3,&
                                                ii)*zr(ivf+ldec+ino-1&
                                                )*jacpoi
231              continue
230          continue
200      continue
! ---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
! ---  ET FIN DE L'OPTION 'CARA_SECT_POUT5'
    endif
end subroutine

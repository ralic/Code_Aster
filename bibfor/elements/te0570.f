      SUBROUTINE TE0570(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C.......................................................................
C
C     BUT:
C       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
C          CALCUL DU CHAMP ELEMENTAIRE A 10 COMPOSANTES :
C          SOMME/S_ELEMENT(DS,X.DS,Y.DS,Z.DS,X*X.DS,Y*Y.DS,Z*Z.DS,
C                             X*Y.DS,X*Z.DS,Y*Z.DS)
C          SUR LES ELEMENTS DE BORD DE COQUE :
C          MEBODKT, MEBODST, MEBOQ4G, MEBOCQ3
C
C          CES 10 QUANTITES GEOMETRIQUES SONT NOTEES :
C          A1 = S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
C
C       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
C          CALCUL DU VECTEUR DEFINIS AUX NOEUDS DES ELEMENTS
C          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
C          SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS,Z*NI.DS,NI.DS,NI.DS*H3/12,0)
C
C          SUR LES ELEMENTS DE BORD DE COQUE :
C          MEBODKT, MEBODST, MEBOQ4G, MEBOCQ3
C
C          AVEC X = XM - XG = NJ*XJ - XG
C               Y = YM - YG = NJ*YJ - YG
C               Z = ZM - ZG = NJ*ZJ - ZG
C          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
C                        DU LIGREL DES MAILLES DE BORD DE COQUE TRAITE
C
C       3) POUR L'OPTION : 'CARA_SECT_POUT5 ' : COQ_TUYAU
C          CALCUL DU VECTEUR DEFINI AUX NOEUDS DES ELEMENTS
C          AYANT POUR VALEURS AU NOEUD I DE L'ELEMENT:
C          SOMME/S_ELEMENT(NI.COS(M.PHI).P.DS)
C          SUR LES ELEMENTS DE BORD DE COQUE :
C          MEBODKT
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      IMPLICIT NONE
      CHARACTER*8        ,ELREFE
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CARAC,FF
      REAL*8             JAC,JACPOI,JACPO2,ZERO,E1(3),E2(3),E3(3),GN1(3)
      REAL*8             XG,YG,ZG,GP0(3),GPG(3),XPG(3),XN1(3),VSIN(3)
      REAL*8             NORGP0,NORGPG,ANGL(3),PGL(3,3),PI,R8PI,RAYON
      REAL*8             EPAIS,COEF,R8PREM,DXDK,DYDK,DZDK,AXGAU,AYGAU
      REAL*8           AZGAU,XGAU,YGAU,ZGAU,AXXGAU,AYYGAU,AZZGAU,SINPHI
      REAL*8             AXYGAU,AXZGAU,AYZGAU,E3XX,E3XY,E3XZ,E3YY,E3YZ
      REAL*8             E3ZZ,COSPHI,PHI,COSMFI,SINMFI,PHI0
      INTEGER            NNO,IPG,ICARAC,NPG,IFF,IDFDK,ICOQU,IOPT
      INTEGER            LDEC,IORIFI,M,ISECT,I,IORIG,IVECT1,INUMOD
      INTEGER            IVECT2,IVECT3,IAXE,INO,II,IPOIDS,IVF,IGEOM
C
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      COMMON /NOMAJE/PGC
      CHARACTER*6 PGC
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CALL ELREF1(ELREFE)
C
      ZERO = 0.0D0
      IOPT=0
      PI=R8PI()
C
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO = ZI(ICARAC+1-1)
      NPG = ZI(ICARAC+3-1)
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
C
      IPOIDS = IFF
      IVF    = IPOIDS + NPG
      IDFDK  = IVF    + NPG * NNO
C
C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- RECUPERATION L'EPAISSEUR DE L'ELEMENT :
C     -------------------------------------
      CALL JEVECH('PCACOQU','L',ICOQU)
      EPAIS = ZR(ICOQU+1-1)
      COEF  = EPAIS*EPAIS*EPAIS/12.0D0
C
      IF (EPAIS.LE.R8PREM( )) THEN
            CALL UTMESS('F','TE0570','L''EPAISSEUR DES ELEMENTS '//
     +                  'DE BORD DE COQUE EST NEGATIVE OU NULLE.')
      ENDIF
C
      IF (OPTION.EQ.'CARA_SECT_POUT3') THEN
          CALL JEVECH('PCASECT','E',ISECT)
          IOPT=3
          DO 10 I = 1, 10
             ZR(ISECT+I-1) = ZERO
10        CONTINUE
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT4') THEN
          CALL JEVECH('PORIGIN','L',IORIG)
          CALL JEVECH('PVECTU1','E',IVECT1)
          CALL JEVECH('PVECTU2','E',IVECT2)
          IOPT=4
          XG = ZR(IORIG+1-1)
          YG = ZR(IORIG+2-1)
          ZG = ZR(IORIG+3-1)
C
          DO 20 I = 1, 6*NNO
             ZR(IVECT1+I-1) = ZERO
 20       CONTINUE
C
      ELSEIF (OPTION.EQ.'CARA_SECT_POUT5') THEN
          CALL JEVECH('PORIGIN','L',IORIG)
          CALL JEVECH('PORIGFI','L',IORIFI)
          CALL JEVECH('PNUMMOD','L',INUMOD)
          CALL JEVECH('PVECTU1','E',IVECT1)
          CALL JEVECH('PVECTU2','E',IVECT2)
          CALL JEVECH('PVECTU3','E',IVECT3)
          IOPT=5
          XG = ZR(IORIG+1-1)
          YG = ZR(IORIG+2-1)
          ZG = ZR(IORIG+3-1)
C
C         COORDONNES DU POINT P TEL QUE GP EST L'ORIGINE
C         DE L'ANGLE PHI
C
          CALL VDIFF(3,ZR(IORIFI),ZR(IORIG),GP0)
          CALL NORMEV(GP0,NORGP0)
C
C         NUMERO DE MODE DE FOURIER
C
          M = ZI(INUMOD)
          DO 21 I = 1, 6*NNO
             ZR(IVECT1+I-1) = ZERO
             ZR(IVECT2+I-1) = ZERO
             ZR(IVECT3+I-1) = ZERO
 21       CONTINUE
C
      ENDIF
C
      CALL JEVECH('PCAORIE','L',IAXE)
      E1(1) = ZR(IAXE+1-1)
      E1(2) = ZR(IAXE+2-1)
      E1(3) = ZR(IAXE+3-1)
C
C     ---------------------------
C --- - OPTION : CARA_SECT_POUT3-
C     ---------------------------
C
      IF (IOPT.EQ.3) THEN
C
C --- BOUCLE SUR LES POINTS DE GAUSS :
C     ------------------------------
        DO 30 IPG=1, NPG
C
          LDEC = (IPG-1)*NNO
C
          DXDK=ZERO
          DYDK=ZERO
          DZDK=ZERO
C
C ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
C       -------------------------------------------------
          DO 40 I=1, NNO
             DXDK=DXDK+ZR(IGEOM+3*(I-1)+1-1)*ZR(IDFDK+LDEC+I-1)
             DYDK=DYDK+ZR(IGEOM+3*(I-1)+2-1)*ZR(IDFDK+LDEC+I-1)
             DZDK=DZDK+ZR(IGEOM+3*(I-1)+3-1)*ZR(IDFDK+LDEC+I-1)
 40       CONTINUE
C
C ---   JACOBIEN :
C       --------
          JAC =SQRT ( DXDK*DXDK + DYDK*DYDK + DZDK*DZDK )
          IF (JAC.LE.R8PREM( )) THEN
            CALL UTMESS('F','TE0570','LE JACOBIEN EST NUL.')
          ENDIF
          JACPOI = JAC*ZR(IPOIDS+IPG-1)*EPAIS
          JACPO2 = JAC*ZR(IPOIDS+IPG-1)*COEF
C
C ---   CALCUL DU VECTEUR E2 TANGENT A LA FIBRE MOYENNE AU POINT
C ---   D'INTEGRATION COURANT :
C
          E2(1) = DXDK/JAC
          E2(2) = DYDK/JAC
          E2(3) = DZDK/JAC
C
C ---   CALCUL DU VECTEUR E3 NORMAL A E1 ET E2
C
          E3(1) = E1(2)*E2(3) - E1(3)*E2(2)
          E3(2) = E1(3)*E2(1) - E1(1)*E2(3)
          E3(3) = E1(1)*E2(2) - E1(2)*E2(1)
C
C ---   CALCUL DE AX, AY, AZ = SOMME(X.DS, Y.DS, Z.DS) :
C       ----------------------------------------------
          AXGAU = ZERO
          AYGAU = ZERO
          AZGAU = ZERO
C
          DO 50 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             AXGAU = AXGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             AYGAU = AYGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             AZGAU = AZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 50      CONTINUE
C
C ---   CALCUL DE  AXX, AYY, AZZ, AXY, AXZ, AYZ
C ---   = SOMME(X*X.DS, Y*Y.DS, Z*Z.DS, X*Y.DS, X*Z.DS, Y*Z.DS) :
C       -------------------------------------------------------
          XGAU = ZERO
          YGAU = ZERO
          ZGAU = ZERO
C
          DO 60 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             ZGAU = ZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 60       CONTINUE
C
          AXXGAU = XGAU * XGAU
          AYYGAU = YGAU * YGAU
          AZZGAU = ZGAU * ZGAU
          AXYGAU = XGAU * YGAU
          AXZGAU = XGAU * ZGAU
          AYZGAU = YGAU * ZGAU
C
C ---   CALCUL DES TERMES EN E3*E3
C
          E3XX = E3(1)*E3(1)*JACPO2
          E3XY = E3(1)*E3(2)*JACPO2
          E3XZ = E3(1)*E3(3)*JACPO2
          E3YY = E3(2)*E3(2)*JACPO2
          E3YZ = E3(2)*E3(3)*JACPO2
          E3ZZ = E3(3)*E3(3)*JACPO2
C
C---  CALCUL DE A1 = S
          ZR(ISECT+1-1)  = ZR(ISECT+1-1)  +        JACPOI
C---  AX
          ZR(ISECT+2-1)  = ZR(ISECT+2-1)  + AXGAU* JACPOI
C---  AY
          ZR(ISECT+3-1)  = ZR(ISECT+3-1)  + AYGAU* JACPOI
C---  AZ
          ZR(ISECT+4-1)  = ZR(ISECT+4-1)  + AZGAU* JACPOI
C---  AXX
          ZR(ISECT+5-1)  = ZR(ISECT+5-1)  + AXXGAU*JACPOI + E3XX
C---  AYY
          ZR(ISECT+6-1)  = ZR(ISECT+6-1)  + AYYGAU*JACPOI + E3YY
C---  AZZ
          ZR(ISECT+7-1)  = ZR(ISECT+7-1)  + AZZGAU*JACPOI + E3ZZ
C---  AXY
          ZR(ISECT+8-1)  = ZR(ISECT+8-1)  + AXYGAU*JACPOI + E3XY
C---  AXZ
          ZR(ISECT+9-1)  = ZR(ISECT+9-1)  + AXZGAU*JACPOI + E3XZ
C---  AYZ
          ZR(ISECT+10-1) = ZR(ISECT+10-1) + AYZGAU*JACPOI + E3YZ
C
 30     CONTINUE
C --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C --- ET FIN DE L'OPTION 'CARA_SECT_POUT3'
C
C     ---------------------------
C --- - OPTION : CARA_SECT_POUT4-
C     ---------------------------
C
      ELSEIF (IOPT.EQ.4) THEN
C
C --- BOUCLE SUR LES POINTS DE GAUSS :
C     ------------------------------
C
        DO 70 IPG=1, NPG
C
          LDEC = (IPG-1)*NNO
C
          DXDK=ZERO
          DYDK=ZERO
          DZDK=ZERO
C
C ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
C       -------------------------------------------------
          DO 80 I=1, NNO
             DXDK=DXDK+ZR(IGEOM+3*(I-1)+1-1)*ZR(IDFDK+LDEC+I-1)
             DYDK=DYDK+ZR(IGEOM+3*(I-1)+2-1)*ZR(IDFDK+LDEC+I-1)
             DZDK=DZDK+ZR(IGEOM+3*(I-1)+3-1)*ZR(IDFDK+LDEC+I-1)
 80       CONTINUE
C
C ---   JACOBIEN :
C       --------
          JAC =SQRT ( DXDK*DXDK + DYDK*DYDK + DZDK*DZDK )
          IF (JAC.LE.R8PREM( )) THEN
            CALL UTMESS('F','TE0570','LE JACOBIEN EST NUL.')
          ENDIF
          JACPOI = JAC*ZR(IPOIDS+IPG-1)*EPAIS
          JACPO2 = JAC*ZR(IPOIDS+IPG-1)*COEF
C
C ---   CALCUL DU VECTEUR E2 TANGENT A LA FIBRE MOYENNE AU POINT
C ---   D'INTEGRATION COURANT :
C       ---------------------
          E2(1) = DXDK/JAC
          E2(2) = DYDK/JAC
          E2(3) = DZDK/JAC
C
C ---   CALCUL DU VECTEUR E3 NORMAL A E1 ET E2
C
          E3(1) = E1(2)*E2(3) - E1(3)*E2(2)
          E3(2) = E1(3)*E2(1) - E1(1)*E2(3)
          E3(3) = E1(1)*E2(2) - E1(2)*E2(1)

C
C ---   CALCUL DES TERMES EN E3*E3
C
          E3XX = E3(1)*E3(1)*JACPO2
          E3XY = E3(1)*E3(2)*JACPO2
          E3XZ = E3(1)*E3(3)*JACPO2
          E3YY = E3(2)*E3(2)*JACPO2
          E3YZ = E3(2)*E3(3)*JACPO2
          E3ZZ = E3(3)*E3(3)*JACPO2
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
          XGAU = ZERO
          YGAU = ZERO
          ZGAU = ZERO
C
          DO 90 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             ZGAU = ZGAU + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 90      CONTINUE
C
C --- CALCUL DE VECT1(I)
C
          DO 110 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
C
             ZR(IVECT1+6*(INO-1)+1-1)  = ZR(IVECT1+6*(INO-1)+1-1) +
     +                        ZR(IVF+LDEC+INO-1)*(XGAU-XG)*JACPOI
C
             ZR(IVECT1+6*(INO-1)+2-1)  = ZR(IVECT1+6*(INO-1)+2-1) +
     +                        ZR(IVF+LDEC+INO-1)*(YGAU-YG)*JACPOI
C
             ZR(IVECT1+6*(INO-1)+3-1)  = ZR(IVECT1+6*(INO-1)+3-1) +
     +                        ZR(IVF+LDEC+INO-1)*(ZGAU-ZG)*JACPOI
C
             ZR(IVECT1+6*(INO-1)+4-1)  = ZR(IVECT1+6*(INO-1)+4-1) +
     +                        ZR(IVF+LDEC+INO-1)*JACPOI
C
             ZR(IVECT1+6*(INO-1)+5-1)  = ZR(IVECT1+6*(INO-1)+5-1) +
     +                        ZR(IVF+LDEC+INO-1)*JACPO2
C
C            PRODUIT VECTORIEL N.(THETA.N).
C
             ZR(IVECT2+6*(INO-1)+1-1)  = ZR(IVECT2+6*(INO-1)+1-1) +
     &                        ZR(IVF+LDEC+INO-1)*(E3YY+E3ZZ)
             ZR(IVECT2+6*(INO-1)+2-1)  = ZR(IVECT2+6*(INO-1)+2-1) -
     &                        ZR(IVF+LDEC+INO-1)*E3XY
             ZR(IVECT2+6*(INO-1)+3-1)  = ZR(IVECT2+6*(INO-1)+3-1) -
     &                        ZR(IVF+LDEC+INO-1)*E3XZ
             ZR(IVECT2+6*(INO-1)+4-1)  = ZR(IVECT2+6*(INO-1)+4-1) +
     &                        ZR(IVF+LDEC+INO-1)*(E3ZZ+E3XX)
             ZR(IVECT2+6*(INO-1)+5-1)  = ZR(IVECT2+6*(INO-1)+5-1) -
     &                        ZR(IVF+LDEC+INO-1)*E3YZ
             ZR(IVECT2+6*(INO-1)+6-1)  = ZR(IVECT2+6*(INO-1)+6-1) +
     &                        ZR(IVF+LDEC+INO-1)*(E3YY+E3XX)

 110      CONTINUE
C
  70    CONTINUE

C ---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C ---  ET FIN DE L'OPTION 'CARA_SECT_POUT4'
C     ---------------------------
C --- - OPTION : CARA_SECT_POUT5-
C     ---------------------------
C
      ELSEIF (IOPT.EQ.5) THEN
      DO 200 IPG = 1, NPG
          DXDK=ZERO
          DYDK=ZERO
          DZDK=ZERO
C
C ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
C       -------------------------------------------------
          LDEC = (IPG-1)*NNO
          DO 210 I=1, NNO
             DXDK=DXDK+ZR(IGEOM+3*(I-1)+1-1)*ZR(IDFDK+LDEC+I-1)
             DYDK=DYDK+ZR(IGEOM+3*(I-1)+2-1)*ZR(IDFDK+LDEC+I-1)
             DZDK=DZDK+ZR(IGEOM+3*(I-1)+3-1)*ZR(IDFDK+LDEC+I-1)
 210       CONTINUE
C
C ---   CALCUL DU RAYON
C
          XN1(1) = ZR(IGEOM+1-1)
          XN1(2) = ZR(IGEOM+2-1)
          XN1(3) = ZR(IGEOM+3-1)
          CALL VDIFF(3,XN1,ZR(IORIG),GN1)
          CALL NORMEV(GN1,RAYON)
C
C ---   JACOBIEN :
C       --------
          JAC =SQRT ( DXDK*DXDK + DYDK*DYDK + DZDK*DZDK )
          JACPOI = JAC*ZR(IPOIDS+IPG-1)
          JACPOI=JACPOI/RAYON/PI
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
          DO 219 II=1,3
             XPG(II) = ZERO
219       CONTINUE
          DO 220 INO = 1, NNO
             I = IGEOM + 3*(INO-1) -1
             XPG(1) = XPG(1) + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             XPG(2) = XPG(2) + ZR(IVF+LDEC+INO-1) * ZR(I+2)
             XPG(3) = XPG(3) + ZR(IVF+LDEC+INO-1) * ZR(I+3)
 220      CONTINUE
C
C  CALCUL DU VECTEUR G-PG ET DE L'ANGLE PHI ENTRE G-P0 ET G-PG
C
          CALL VDIFF(3,XPG,ZR(IORIG),GPG)
          CALL NORMEV(GPG,NORGPG)
          CALL PSCAL(3,GP0,GPG,COSPHI)
CPM          CALL PROVEC(GP0,GPG,VSIN)
          CALL PROVEC(GPG,GP0,VSIN)
          CALL PSCAL(3,E1,VSIN,SINPHI)
          PHI0=ATAN2(SINPHI,COSPHI)
CJMP          PHI=-PHI0
          PHI=PHI0
          COSMFI=COS(M*PHI)
          SINMFI=SIN(M*PHI)
C
C  CALCUL DE PGL MATRICE DE PASSAGE DE X,Y,Z GLOBAL A E1,E2,E3
C
          CALL PROVEC(GPG,E1,E2)
          CALL ANGVXY(E1,E2,ANGL)
          CALL MATROT(ANGL,PGL)
C
          DO 230 INO = 1, NNO
             DO 231 II=1,3
C
C CALCUL DE VECT1(I) : TERMES EN UMI(COS(M.PHI)) ET UMO (SIN(M.PHI))
C
              ZR(IVECT1+6*(INO-1)+II-1)  = ZR(IVECT1+6*(INO-1)+II-1)+
     +                    COSMFI*PGL(1,II)*ZR(IVF+LDEC+INO-1)*JACPOI
              ZR(IVECT1+6*(INO-1)+3+II-1)=ZR(IVECT1+6*(INO-1)+3+II-1)+
     +                    SINMFI*PGL(1,II)*ZR(IVF+LDEC+INO-1)*JACPOI
C
C CALCUL DE VECT2(I) : TERMES EN VMI(COS(M.PHI)) ET VMO (SIN(M.PHI))
C
              ZR(IVECT2+6*(INO-1)+II-1)  = ZR(IVECT2+6*(INO-1)+II-1)+
     +                    COSMFI*PGL(2,II)*ZR(IVF+LDEC+INO-1)*JACPOI
              ZR(IVECT2+6*(INO-1)+3+II-1)=ZR(IVECT2+6*(INO-1)+3+II-1)+
     +                    SINMFI*PGL(2,II)*ZR(IVF+LDEC+INO-1)*JACPOI
C
C CALCUL DE VECT3(I) : TERMES EN WMI(COS(M.PHI)) ET WMO (SIN(M.PHI))
C
              ZR(IVECT3+6*(INO-1)+II-1)  = ZR(IVECT3+6*(INO-1)+II-1)+
     +                    COSMFI*PGL(3,II)*ZR(IVF+LDEC+INO-1)*JACPOI
              ZR(IVECT3+6*(INO-1)+3+II-1)=ZR(IVECT3+6*(INO-1)+3+II-1)+
     +                    SINMFI*PGL(3,II)*ZR(IVF+LDEC+INO-1)*JACPOI
 231         CONTINUE
 230      CONTINUE
  200    CONTINUE
C ---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C ---  ET FIN DE L'OPTION 'CARA_SECT_POUT5'
      ENDIF
C
      END

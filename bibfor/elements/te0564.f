      SUBROUTINE TE0564(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C.......................................................................

C     BUT:
C       1) POUR L'OPTION : 'CARA_SECT_POUT3 ' :
C          CALCUL DU CHAMP ELEMENTAIRE A 6 COMPOSANTES :
C          SOMME/S_ELEMENT(DS,X.DS,Y.DS,X*X.DS,Y*Y.DS,X*Y.DS)
C          SUR LES ELEMENTS DE BORD DES ELEMENTS 2D 
C
C          CES 6 QUANTITES GEOMETRIQUES SONT NOTEES :
C          A1 = S,AX,AY,AXX,AYY,AXY
C
C       2) POUR L'OPTION : 'CARA_SECT_POUT4 ' :
C          CALCUL DES 2 VECTEURS DEFINIS AUX NOEUDS DES ELEMENTS
C          AYANT POURS VALEURS AU NOEUD I DE L'ELEMENT:
C          POUR LE PREMIER VECTEUR
C               SOMME/S_ELEMENT(NI.DS,0,0)
C          POUR LE SECOND VECTEUR
C               SOMME/S_ELEMENT(X*NI.DS,Y*NI.DS)
C          SUR LES ELEMENTS DE BORD DES ELEMENTS 2D 
C
C          AVEC X = XM - XG = NJ*XJ - XG
C               Y = YM - YG = NJ*YJ - YG
C               Z = ZM - ZG = NJ*ZJ - ZG
C          OU (XG,YG,ZG) SONT LES COORDONNEES DU CENTRE GEOMETRIQUE
C                        DU LIGREL DES MAILLES DE BORD (SEG) TRAITEES
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8 ,ELREFE
      CHARACTER*16 NOMTE,OPTION
      REAL*8 JAC,JACPOI,ZERO
      REAL*8 XG,YG,ZG
      REAL*8 R8PREM,DXDK,DYDK,AXGAU,AYGAU
      REAL*8 XGAU,YGAU,AXXGAU,AYYGAU
      REAL*8 AXYGAU
      INTEGER NNO,NNOS,JGANO,NDIM,IPG,NPG,IDFDK,IOPT
      INTEGER LDEC,ISECT,I,IORIG,IVECT1
      INTEGER IVECT2,INO,IPOIDS,IVF,IGEOM



      CALL JEMARQ()

      CALL ELREF1(ELREFE)

      ZERO = 0.0D0
      IOPT = 0
      
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDK,JGANO)

C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

      IF (OPTION.EQ.'CARA_SECT_POUT3') THEN
        CALL JEVECH('PCASECT','E',ISECT)
        IOPT = 3
        DO 10 I = 1,6
          ZR(ISECT+I-1) = ZERO
   10   CONTINUE

      ELSE IF (OPTION.EQ.'CARA_SECT_POUT4') THEN
        CALL JEVECH('PORIGIN','L',IORIG)
        CALL JEVECH('PVECTU1','E',IVECT1)
        CALL JEVECH('PVECTU2','E',IVECT2)
        IOPT = 4
        XG = ZR(IORIG+1-1)
        YG = ZR(IORIG+2-1)
        DO 20 I = 1,2*NNO
          ZR(IVECT1+I-1) = ZERO 
          ZR(IVECT2+I-1) = ZERO
   20   CONTINUE

      END IF

C     ---------------------------
C --- - OPTION : CARA_SECT_POUT3-
C     ---------------------------

      IF (IOPT.EQ.3) THEN

C --- BOUCLE SUR LES POINTS DE GAUSS :
C     ------------------------------
        DO 70 IPG = 1,NPG

          LDEC = (IPG-1)*NNO

          DXDK = ZERO
          DYDK = ZERO

C ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
C       -------------------------------------------------
          DO 40 INO = 1,NNO
            I = IGEOM + 2* (INO-1) - 1
            DXDK = DXDK + ZR(I+1)*ZR(IDFDK+LDEC+INO-1)
            DYDK = DYDK + ZR(I+2)*ZR(IDFDK+LDEC+INO-1)
   40     CONTINUE

C ---   JACOBIEN :
C       --------
          JAC = SQRT(DXDK*DXDK+DYDK*DYDK)
          IF (JAC.LE.R8PREM()) THEN
            CALL U2MESS('F','ELEMENTS4_34')
          END IF
          JACPOI = JAC*ZR(IPOIDS+IPG-1)

C ---   CALCUL DE AX, AY = SOMME(X.DS, Y.DS) :
C       ----------------------------------------------
          AXGAU = ZERO
          AYGAU = ZERO

          DO 50 INO = 1,NNO
            I = IGEOM + 2* (INO-1) - 1
            AXGAU = AXGAU + ZR(IVF+LDEC+INO-1)*ZR(I+1)
            AYGAU = AYGAU + ZR(IVF+LDEC+INO-1)*ZR(I+2)
   50     CONTINUE

C ---   CALCUL DE  AXX, AYY, AXY = SOMME(X*X.DS, Y*Y.DS, X*Y.DS) :
C       -------------------------------------------------------
          XGAU = ZERO
          YGAU = ZERO

          DO 60 INO = 1,NNO
            I = IGEOM + 2* (INO-1) - 1
            XGAU = XGAU + ZR(IVF+LDEC+INO-1)*ZR(I+1)
            YGAU = YGAU + ZR(IVF+LDEC+INO-1)*ZR(I+2)
   60     CONTINUE

          AXXGAU = XGAU*XGAU
          AYYGAU = YGAU*YGAU
          AXYGAU = XGAU*YGAU

C---  CALCUL DE A1 = S
          ZR(ISECT+1-1) = ZR(ISECT+1-1) + JACPOI
C---  AX
          ZR(ISECT+2-1) = ZR(ISECT+2-1) + AXGAU*JACPOI
C---  AY
          ZR(ISECT+3-1) = ZR(ISECT+3-1) + AYGAU*JACPOI
C---  AXX
          ZR(ISECT+4-1) = ZR(ISECT+4-1) + AXXGAU*JACPOI
C---  AYY
          ZR(ISECT+5-1) = ZR(ISECT+5-1) + AYYGAU*JACPOI
C---  AXY
          ZR(ISECT+6-1) = ZR(ISECT+6-1) + AXYGAU*JACPOI

   70   CONTINUE
C --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C --- ET FIN DE L'OPTION 'CARA_SECT_POUT3'

C     ---------------------------
C --- - OPTION : CARA_SECT_POUT4-
C     ---------------------------

      ELSE IF (IOPT.EQ.4) THEN

C --- BOUCLE SUR LES POINTS DE GAUSS :
C     ------------------------------
        DO 110 IPG = 1,NPG

          LDEC = (IPG-1)*NNO

          DXDK = ZERO
          DYDK = ZERO

C ---   DERIVEES DES FONCTION DE FORME SUR L'ELEMENT REEL :
C       -------------------------------------------------
          DO 80 INO = 1,NNO
            I = IGEOM + 2* (INO-1) - 1
            DXDK = DXDK + ZR(I+1)*ZR(IDFDK+LDEC+INO-1)
            DYDK = DYDK + ZR(I+2)*ZR(IDFDK+LDEC+INO-1)
   80     CONTINUE

C ---   JACOBIEN :
C       --------
          JAC = SQRT(DXDK*DXDK+DYDK*DYDK)
          IF (JAC.LE.R8PREM()) THEN
            CALL U2MESS('F','ELEMENTS4_34')
          END IF
          JACPOI = JAC*ZR(IPOIDS+IPG-1)

C---    CALCUL DE VECT1(I) = SOMME(NI.DS, 0)
C       ---------------------------------------
          DO 120 INO = 1, NNO
             ZR(IVECT1+2*(INO-1)+1-1)  = ZR(IVECT1+2*(INO-1)+1-1) +
     &                                   ZR(IVF+LDEC+INO-1)*JACPOI
 120      CONTINUE

C---    CALCUL DE VECT2(I) = SOMME(X*NI.DS, Y*NI.DS)
C       --------------------------------------------
          XGAU = ZERO
          YGAU = ZERO
C
          DO 130 INO = 1, NNO
             I = IGEOM + 2*(INO-1) -1
             XGAU = XGAU + ZR(IVF+LDEC+INO-1) * ZR(I+1)
             YGAU = YGAU + ZR(IVF+LDEC+INO-1) * ZR(I+2)
 130      CONTINUE
C
          DO 140 INO = 1, NNO
             ZR(IVECT2+2*(INO-1))  = ZR(IVECT2+2*(INO-1)) +
     &                ZR(IVF+LDEC+INO-1)*(XGAU-XG)*JACPOI
             ZR(IVECT2+2*(INO-1)+1)  = ZR(IVECT2+2*(INO-1)+1) +
     &                ZR(IVF+LDEC+INO-1)*(YGAU-YG)*JACPOI
 140      CONTINUE
C


  110   CONTINUE

C ---  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C ---  ET FIN DE L'OPTION 'CARA_SECT_POUT4'

      END IF

      CALL JEDEMA()

      END

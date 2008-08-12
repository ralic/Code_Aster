      SUBROUTINE TE0474(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/08/2008   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C          ELEMENT SHB
C    FONCTION REALISEE:
C            OPTION : 'RIGI_MECA      '
C                            CALCUL DES MATRICES ELEMENTAIRES  3D
C            OPTION : 'RIGI_MECA_SENSI' OU 'RIGI_MECA_SENS_C'
C                            CALCUL DU VECTEUR ELEMENTAIRE -DK/DP*U
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      PARAMETER (NBRES=2)
      CHARACTER*8 MODELI,ELREFE
      CHARACTER*4 FAMI
      CHARACTER*2 CODRET(NBRES)
      CHARACTER*8 NOMRES(NBRES),NOMPAR(4)
      CHARACTER*16 NOMTE,PHENOM,NOMSHB,OPTION
      CHARACTER*24 CHVAL,CHCTE
      REAL*8 SIGMA(120),D(36),RE(24,24),RE6(18,18)
      REAL*8 FSTAB(12),RE20(60,60)
      REAL*8 VALRES(NBRES),VALPAR(4)
      LOGICAL LSENS
      REAL*8 NU,E,RE15(45,45)
C      REAL*8 RE(24,24)
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
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
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C --- INITIALISATIONS :
      CALL IDSSHB(NDIM,NNO,NPG,NOMSHB)
C     
C  ===========================================
C  -- MATRICE DE RIGIDITE GEOMETRIQUE
C  ===========================================
      IF (OPTION.EQ.'RIGI_MECA_GE') THEN
C        RECUPERATION DES COORDONNEES DES CONNECTIVITES
C        GEOMETRIE Dans ZR(IGEOM)
        CALL JEVECH('PGEOMER','L',IGEOM)
C        RECUPERATION DES CONTRAINTES DANS ZR(ICONT)
        CALL JEVECH('PCONTRR','L',ICONT)
        IF (NOMSHB.EQ.'SHB8') THEN
           DO 20 I = 1,24
             DO 10 J = 1,24
               RE(I,J) = 0.D0
   10        CONTINUE
   20      CONTINUE
           DO 50 I=1,5
            DO 40 J=1,6
             SIGMA(6*(I-1)+J)=ZR(ICONT+18*(I-1)+J-1)
   40       CONTINUE
   50      CONTINUE
           CALL SH8MEK(ZR(IGEOM),SIGMA,RE)
           CALL JEVECH('PMATUUR','E',IMATUU)
           K = 0
           DO 70 I = 1,24
             DO 60 J = 1,I
               K = K + 1
               ZR(IMATUU+K-1) = RE(I,J)
   60        CONTINUE
   70      CONTINUE
        ELSE IF (NOMSHB.EQ.'SHB6') THEN
           DO 90 I = 1,18
             DO 80 J = 1,18
               RE6(I,J) = 0.D0
   80        CONTINUE
   90      CONTINUE
           DO 110 I=1,5
             DO 100 J=1,6
              SIGMA(6*(I-1)+J)=ZR(ICONT+18*(I-1)+J-1)
  100        CONTINUE
  110      CONTINUE
C
           CALL SH6MEK(ZR(IGEOM),SIGMA,RE6)
           CALL JEVECH('PMATUUR','E',IMATUU)
           K = 0
           DO 130 I = 1,18
             DO 120 J = 1,I
               K = K + 1
               ZR(IMATUU+K-1) = RE6(I,J)
  120        CONTINUE
  130      CONTINUE
C
        ELSE IF (NOMSHB.EQ.'SHB15') THEN
           DO 150 I = 1,45
             DO 140 J = 1,45
               RE15(I,J) = 0.D0
  140        CONTINUE
  150      CONTINUE
           DO 170 I=1,15
             DO 160 J=1,6
              SIGMA(6*(I-1)+J)=ZR(ICONT+18*(I-1)+J-1)
  160        CONTINUE
  170      CONTINUE
C
           CALL SH1MEK(ZR(IGEOM),SIGMA,RE15)
           CALL JEVECH('PMATUUR','E',IMATUU)
           K = 0
           DO 190 I = 1,45
             DO 180 J = 1,I
               K = K + 1
               ZR(IMATUU+K-1) = RE15(I,J)
  180        CONTINUE
  190      CONTINUE
C	   	   	
        ELSE IF (NOMSHB.EQ.'SHB20') THEN
           DO 210 I = 1,60
             DO 200 J = 1,60
               RE20(I,J) = 0.D0
  200        CONTINUE
  210      CONTINUE
           DO 230 I=1,20
             DO 220 J=1,6
              SIGMA(6*(I-1)+J)=ZR(ICONT+18*(I-1)+J-1)
  220        CONTINUE
  230      CONTINUE
           CALL SH2MEK(ZR(IGEOM),SIGMA,RE20)
           CALL JEVECH('PMATUUR','E',IMATUU)
           K = 0
           DO 250 I = 1,60
             DO 240 J = 1,I
               K = K + 1
               ZR(IMATUU+K-1) = RE20(I,J)
  240        CONTINUE
  250      CONTINUE
         END IF        
      END IF
      END

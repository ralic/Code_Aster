      SUBROUTINE CAJGR2(IGRAP,VR,COCAJ1,COCAJ2)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/05/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C-----------------------------------------------------------------------
C CALCUL DES COEFFICIENTS ADIMENSIONNELS DE FORCE D'AMORTISSEMENT
C GRAPPE2
C-----------------------------------------------------------------------
C  IN : IGRAP  : INDICE CARACTERISTIQUE DE LA CONFIGURATION
C                EXPERIMENTALE DE REFERENCE
C  IN : VR     : VITESSE REDUITE
C OUT : COCAJ1 : COEFFICIENT ADIMENSIONNEL DE FORCE D'AMORTISSEMENT
C                POUR UN MOUVEMENT DE TRANSLATION
C OUT : COCAJ2 : COEFFICIENT ADIMENSIONNEL DE FORCE D'AMORTISSEMENT
C                POUR UN MOUVEMENT DE ROTATION
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IGRAP
      REAL*8        VR,COCAJ1,COCAJ2
C
      INTEGER       NCAMAX,NBOMAX,UNIT,NBLOC,IFLAG
      REAL*8        COECA1(10,20,11),COECA2(10,20,11)
      REAL*8        BOCA1(10,20),BOCA2(10,20),BORNE1(10,20)
      REAL*8        ZERO, COEF1(10,20,11),COEF2(10,20,11),BORNE2(10,20)
      CHARACTER*24  NOM1, NOM2
      SAVE          BORNE1, COECA1, BORNE2, COECA2
C ----------------------------------------------------------------------
C
C
      CALL JEMARQ()
      NCAMAX = 11
      NBOMAX = 20
      NBMAX  = 10
      ZERO = 0.0D0
C
      NOM1 = '&&CAJGR2.FLAG'
      NOM2 = '&&OP0143.UNIT_GRAPPES'
C        
C --- ON TESTE L'EXISTENCE DU VECTEUR DES COEFFICIENTS
C     ===============================================
      CALL JEEXIN (NOM1,IRET)
      IF (IRET .EQ. 0) THEN
C
C --- LECTURE DU FICHIER DE DONNEES 
C     =============================
         CALL JEVEUO(NOM2,'L',IUNIT)
         UNIT = ZI(IUNIT-1+1)   
         CALL ULOPEN(UNIT,' ',' ','NEW','O')
C
C ---    BLOC D'INITIALISATION
         DO 10 I = 1,NBMAX
            DO 20 J = 1,NBOMAX
               BOCA1(I,J)  = ZERO
               BOCA2(I,J)  = ZERO
               BORNE1(I,J) = ZERO
               BORNE2(I,J) = ZERO
               DO 30 K = 1,NCAMAX
                   COECA1(I,J,K) = ZERO
                   COECA2(I,J,K) = ZERO
                   COEF1(I,J,K)  = ZERO
                   COEF2(I,J,K)  = ZERO
  30           CONTINUE                         
  20        CONTINUE
  10     CONTINUE
C         
         READ (UNIT,*) NBLOC        
         DO 40 L = 1,NBLOC                   
            READ (UNIT,*) NB1
            IF (NB1 .NE. 0) THEN
               READ (UNIT,*) (BOCA1(L,I),I = 1,NB1)
            ENDIF                                 
            DO 50 I = 1, NB1+1     
               READ (UNIT,*) (COEF1(L,I,J),J = 1,NCAMAX)
  50        CONTINUE
            READ (UNIT,*) NB2
            IF (NB2 .NE. 0) THEN                
               READ (UNIT,*) (BOCA2(L,I),I = 1,NB2)
            ENDIF                                  
            DO 60 I = 1, NB2+1     
               READ (UNIT,*) (COEF2(L,I,J),J = 1,NCAMAX)
  60        CONTINUE 
            READ (UNIT,*)
            DO 70 I = 1,NBMAX
               DO 80 J = 1,NBOMAX
                  BORNE1(I,J) = BOCA1(I,J)
                  BORNE2(I,J) = BOCA2(I,J)
                  DO 90 K = 1,NCAMAX                     
                     COECA1(I,J,K) = COEF1(I,J,K)
                     COECA2(I,J,K) = COEF2(I,J,K)
 90               CONTINUE
 80            CONTINUE
 70         CONTINUE       
            CALL JEDETR(NOM1)
            CALL WKVECT(NOM1,'G V I',1,IFLAG)
            ZI(IFLAG-1+1) = 1
 40      CONTINUE   
      ENDIF
C
 100  CONTINUE
C-----1.CONFIG. ECOULEMENT ASCENDANT TIGE DE COMMANDE CENTREE
C
      IF (IGRAP.EQ.1) THEN
C
        IF (VR .LT. BORNE1(1,1)) THEN
          COCAJ1 = COECA1(1,1,8) +
     &             COECA1(1,1,9)*VR
        ELSE
          COCAJ1 = COECA1(1,2,8) +
     &             COECA1(1,2,9)*VR
        ENDIF
C
        COCAJ2   = COECA2(1,1,8) +
     &             COECA2(1,1,9)*VR
C
C-----2.CONFIG. ECOULEMENT ASCENDANT TIGE DE COMMANDE EXCENTREE
C
      ELSE IF (IGRAP.EQ.2) THEN
C
        COCAJ1 =   COECA1(2,1,8) +
     &             COECA1(2,1,9)*VR
        COCAJ2 =   COECA2(2,1,8) +
     &             COECA2(2,1,9)*VR
C
C-----3.CONFIG. ECOULEMENT DESCENDANT TIGE DE COMMANDE CENTREE
C
      ELSE IF (IGRAP.EQ.3) THEN
C
        IF (VR .LT. BORNE1(3,1)) THEN
          VR2 = VR*VR
          VR3 = VR2*VR
          COCAJ1 = COECA1(3,1,8) +
     &              COECA1(3,1,9)*VR + 
     &              COECA1(3,1,10)*VR2 +
     &              COECA1(3,1,11)*VR3
        ELSE IF (VR .LT. BORNE1(3,2)) THEN
          COCAJ1 = COECA1(3,2,8)
        ELSE
          COCAJ1 = COECA1(3,3,8) +
     &             COECA1(3,3,9)*VR
        ENDIF
C
        IF (VR .LT. BORNE2(3,1)) THEN
          COCAJ2 = COECA2(3,1,8) +
     &              COECA2(3,1,9)*VR
        ELSE IF (VR .LT. BORNE2(3,2)) THEN
          VR2 = VR*VR
          COCAJ2 = COECA2(3,2,8) +
     &              COECA2(3,2,9)*VR + 
     &              COECA2(3,2,10)*VR2
        ELSE
          COCAJ2 = COECA2(3,3,8) + 
     &              COECA2(3,3,9)*VR
        ENDIF
C
C-----4.CONFIG. ECOULEMENT DESCENDANT TIGE DE COMMANDE EXCENTREE
C
      ELSE 
C
        IF (VR .LT. BORNE1(4,1)) THEN
          VR2 = VR*VR
          VR3 = VR2*VR
          COCAJ1 = COECA1(4,1,8) +
     &              COECA1(4,1,9)*VR + 
     &              COECA1(4,1,10)*VR2 +
     &              COECA1(4,1,11)*VR3
        ELSE IF (VR .LT. BORNE1(4,2)) THEN
          VR2 = VR*VR
          VR3 = VR2*VR
          COCAJ1 = COECA1(4,2,8) +
     &              COECA1(4,2,9)*VR + 
     &              COECA1(4,2,10)*VR2 +
     &              COECA1(4,2,11)*VR3
        ELSE
          COCAJ1 = COECA1(4,3,8) + 
     &              COECA1(4,3,9)*VR
        ENDIF
C
        IF (VR .LT. BORNE2(4,1)) THEN
          COCAJ2 = COECA2(4,1,8) +
     &              COECA2(4,1,9)*VR
        ELSE IF (VR .LT. BORNE2(4,2)) THEN
          VR2 = VR*VR
          COCAJ2 = COECA2(4,2,8) +
     &              COECA2(4,2,9)*VR + 
     &              COECA2(4,2,10)*VR2
        ELSE
          COCAJ2 = COECA2(4,3,8) + 
     &              COECA2(4,3,9)*VR
        ENDIF
C
      ENDIF
C
C     FERMETURE DU FICHIER
      IF (IRET .EQ. 0) CALL ULOPEN(-UNIT,' ',' ',' ',' ')
      CALL JEDEMA()
      END

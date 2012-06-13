      SUBROUTINE AFFGEN(TMP,NOM,NEL,NTEL,NAPCIS,FONCIS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER                       NTEL(*)
      CHARACTER*8           NOM
      CHARACTER*19 NAPCIS, FONCIS 
      CHARACTER*24      TMP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C       AFFECTATION DES CARACTERISTIQUES GENERALES CALCULEES
C       A PARTIR DES DONNEES GEOMETRIQUES (RECTANGLE ET CERCLE)
C       RQ : NTEL(1) = NUMERO DU TYPE ELEMENT MECA_POU_D_T
C            NTEL(2) = NUMERO DU TYPE ELEMENT MECA_POU_D_E
C            NTEL(4) = NUMERO DU TYPE ELEMENT MECA_POU_C_T
C            NTEL(5) = NUMERO DU TYPE ELEMENT MEFS_POU_D_T
C            NTEL(6) = NUMERO DU TYPE ELEMENT MECA_POU_D_TG
C     ------------------------------------------------------------------
        REAL*8    EPS,    R8PI,   PI,     ALPHA,  BETA,   CCIS
        REAL*8    HY,     HZ,     EPY,    EPZ,    HYI,    HZI
        REAL*8    A,      B,      A4,     B4,     B3
        REAL*8    CT,     CD,     JX
        REAL*8    RE,     RI,     E
        REAL*8    VALPAY(2), VALPAZ(2), VALPAF
        CHARACTER*24 NOMPA(2), NOMPAF
        DATA    EPS     /1.D-3/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      PI  = R8PI()
C

      IF (.NOT.( NEL.EQ.NTEL(1) .OR. NEL.EQ.NTEL(4) .OR. NEL.EQ.NTEL(2)
     &    .OR.NEL.EQ.NTEL(3).OR.NEL.EQ.NTEL(5) .OR. NEL.EQ.NTEL(6).OR.
     &      NEL.EQ.NTEL(12).OR.NEL.EQ.NTEL(13).OR.
     &    NEL.EQ.NTEL(9).OR.NEL.EQ.NTEL(10).OR.NEL.EQ.NTEL(11).OR.
     &    NEL.EQ.NTEL(7).OR.NEL.EQ.NTEL(8))) THEN
        CALL U2MESS('F','MODELISA_86')
      ENDIF


      CALL JEVEUO(JEXNOM(TMP,NOM),'E',JDGE)
      ISEC = NINT(ZR(JDGE+35))

C
C ---   CALCUL DES CARACTERISTIQUES GENERALES SECTION RECTANGULAIRE
C
C       -  ERREUR   SI  HY  <= 0  OU  HZ  <= 0  OU
C                       EPY <= 0  OU  EPZ <= 0  (TEST DANS AFFDEF)
C
      IF (ISEC.EQ.1) THEN
         DO 10 I = 1 , 2
            IF (I.EQ.1) THEN
               IGEOR = 24
               IGEOC = 32
               IGEN  = 1
               IGEN2 = 37
            ELSE
               IGEOR = 28
               IGEOC = 34
               IGEN  = 12
               IGEN2 = 38
            ENDIF
            ZR(JDGE+IGEOC-1) = 0.D0
            ZR(JDGE+IGEOC)   = 0.D0
            HY  = ZR(JDGE+IGEOR-1)
            HZ  = ZR(JDGE+IGEOR)
            EPY = ZR(JDGE+IGEOR+1)
            EPZ = ZR(JDGE+IGEOR+2)
            HYI = HY - 2.D0*EPY
            HZI = HZ - 2.D0*EPZ
C  A
            ZR(JDGE+IGEN-1) = HY * HZ - HYI * HZI
C  IY
            ZR(JDGE+IGEN)   = (HY*(HZ*HZ*HZ)-HYI*(HZI*HZI*HZI))/12.D0
C  IZ
            ZR(JDGE+IGEN+1) = (HZ*(HY*HY*HY)-HZI*(HYI*HYI*HYI))/12.D0
C  EY
            ZR(JDGE+IGEN+4) = 0.D0
C  EZ
            ZR(JDGE+IGEN+5) = 0.D0
C  RY
            ZR(JDGE+IGEN+7) = HY / 2.D0
C  RZ
            ZR(JDGE+IGEN+8) = HZ / 2.D0
C
C           --- CAS DE LA SECTION  RECTANGULAIRE  PLEINE ---
            IF (ABS(HYI/HY).LE.EPS .OR. ABS(HZI/HZ).LE.EPS) THEN
               A = HY / 2.D0
               B = HZ / 2.D0
               IF (A/B.LT.1.D0) THEN
                  A = HZ / 2.D0
                  B = HY / 2.D0
               ENDIF
               A4 = A**4 * 12.D0
               B4 = B**4
               B3 = B**3
               JX = A*B3*(16.D0/3.D0-3.36D0*B*(1.D0-B4/A4)/A)
C  AY
               IF (NEL.EQ.NTEL(1)) ZR(JDGE+IGEN+2) = 1.2D0
               IF (NEL.EQ.NTEL(2)) ZR(JDGE+IGEN+2) = 0.D0
               IF (NEL.EQ.NTEL(3)) ZR(JDGE+IGEN+2) = 1.2D0
               IF (NEL.EQ.NTEL(4)) ZR(JDGE+IGEN+2) = 1.2D0
               IF (NEL.EQ.NTEL(5)) ZR(JDGE+IGEN+2) = 1.2D0
               IF (NEL.EQ.NTEL(6)) ZR(JDGE+IGEN+2) = 1.2D0
               IF (NEL.EQ.NTEL(12)) ZR(JDGE+IGEN+2) = 0.D0
               IF (NEL.EQ.NTEL(13)) ZR(JDGE+IGEN+2) = 1.2D0
C  AZ
               IF (NEL.EQ.NTEL(1)) ZR(JDGE+IGEN+3) = 1.2D0
               IF (NEL.EQ.NTEL(2)) ZR(JDGE+IGEN+3) = 0.D0
               IF (NEL.EQ.NTEL(3)) ZR(JDGE+IGEN+3) = 1.2D0
               IF (NEL.EQ.NTEL(4)) ZR(JDGE+IGEN+3) = 1.2D0
               IF (NEL.EQ.NTEL(5)) ZR(JDGE+IGEN+3) = 1.2D0
               IF (NEL.EQ.NTEL(6)) ZR(JDGE+IGEN+3) = 1.2D0
               IF (NEL.EQ.NTEL(12)) ZR(JDGE+IGEN+3) = 0.D0
               IF (NEL.EQ.NTEL(13)) ZR(JDGE+IGEN+3) = 1.2D0
C  JX
               ZR(JDGE+IGEN+6) = JX
C  RT
               ZR(JDGE+IGEN+9) = JX * (3.D0*A+1.8D0*B) / (8.D0*A*A*B*B)
C  AI
               ZR(JDGE+IGEN2-1) = 0.D0
C
            ELSE
C
C              --- CAS DU TUBE RECTANGULAIRE ---
               CT = 2.D0*EPY*EPZ*(HY-EPY)*(HY-EPY)*(HZ-EPZ)*(HZ-EPZ)
               CD = HY*EPY + HZ*EPZ - EPY*EPY - EPZ*EPZ
               JX = CT /CD
C              
C	       --- INTERPOLATION DES COEFFICIENTS DE CISAILLEMENT
               ALPHA = (HY - 2.D0 * EPY ) / HY
               BETA = (HZ - 2.D0 * EPZ ) / HZ
               CALL ASSERT((ALPHA.GE.0.D0) .OR. (BETA.GE.0.D0))
               IF (ALPHA.GT.0.95D0 .OR. BETA.GT.0.95D0) THEN
                  CALL U2MESS('F','MODELISA10_15')
               ENDIF
               NOMPA(1)= 'ALPHA'
               NOMPA(2)= 'BETA'
               VALPAY(1)= ALPHA
               VALPAY(2)= BETA
               VALPAZ(1)= BETA
               VALPAZ(2)= ALPHA
               CALL FOINTE('A',NAPCIS,2,NOMPA,VALPAY,AY,IER)
               CALL FOINTE('A',NAPCIS,2,NOMPA,VALPAZ,AZ,IER)       
C
C  AY            
               IF (NEL.EQ.NTEL(2).OR.NEL.EQ.NTEL(12)) THEN
                   ZR(JDGE+IGEN+2) = 0.D0
               ELSE
                   ZR(JDGE+IGEN+2) = AY
               ENDIF
C  AZ
               IF (NEL.EQ.NTEL(2).OR.NEL.EQ.NTEL(12)) THEN
                   ZR(JDGE+IGEN+3) = 0.D0
               ELSE
                   ZR(JDGE+IGEN+3) = AZ
               ENDIF
C  JX
               ZR(JDGE+IGEN+6) = JX
C  RT. TUBE RECTANGULAIRE MINCE D'EPAISSEUR CONSTANTE. RT=JX/2.E.AINT
               AINT = (HY-2.D0*EPY)*(HZ-2.D0*EPZ)
               ZR(JDGE+IGEN+9) = JX/(2.D0*EPZ*AINT)
C  AI
               ZR(JDGE+IGEN2-1)  = HYI * HZI
C
            ENDIF
C AY/AZ POUR TUYAUX ET 3D_FAISCEAU
            IF (NEL.EQ.NTEL(9).OR.NEL.EQ.NTEL(10).OR.
     &          NEL.EQ.NTEL(11).OR.NEL.EQ.NTEL(7)
     &          .OR.NEL.EQ.NTEL(8)) THEN
              ZR(JDGE+IGEN+2) = 0.D0
              ZR(JDGE+IGEN+3) = 0.D0
            ENDIF
C
 10      CONTINUE
C  JG1,JG2,IYR21,IYR22,IZR21,IZR22 :
         DO 11 I=1,6
           ZR(JDGE-1+38+I) = 0.D0
 11      CONTINUE
      ENDIF
C
C ---   CALCUL DES CARACTERISTIQUES GENERALES SECTION CIRCULAIRE
C
C       -  ERREUR   SI RE <= 0  OU  E > RE OU E <= 0  (TEST DANS AFFDEF)
C
      IF (ISEC.EQ.2) THEN
         DO 20 I = 1 , 2
            IF (I.EQ.1) THEN
               IGEOR = 24
               IGEOC = 32
               IGEN  = 1
               IGEN2 = 37
            ELSE
               IGEOR = 28
               IGEOC = 34
               IGEN  = 12
               IGEN2 = 38
            ENDIF
            ZR(JDGE+IGEOR-1) = 0.D0
            ZR(JDGE+IGEOR)   = 0.D0
            ZR(JDGE+IGEOR+1) = 0.D0
            ZR(JDGE+IGEOR+2) = 0.D0
            RE = ZR(JDGE+IGEOC-1)
            E  = ZR(JDGE+IGEOC)
            RI = RE - E
C  A
            ZR(JDGE+IGEN-1) = PI * ( RE*RE - RI*RI )
C  IY
            ZR(JDGE+IGEN)   = PI * ( RE**4 - RI**4 ) / 4.D0
C  IZ
            ZR(JDGE+IGEN+1) = ZR(JDGE+IGEN)
C  EY
            ZR(JDGE+IGEN+4) = 0.D0
C  EZ
            ZR(JDGE+IGEN+5) = 0.D0
C  JX
            ZR(JDGE+IGEN+6) = ZR(JDGE+IGEN) * 2.D0
C  RY
            ZR(JDGE+IGEN+7) = RE
C  RZ
            ZR(JDGE+IGEN+8) = RE
C  RT
            ZR(JDGE+IGEN+9) = RE
C              
C	    --- INTERPOLATION DES COEFFICIENTS DE CISAILLEMENT
C
            ALPHA = RI / RE
            CALL ASSERT((ALPHA .GE. 0.D0) .OR. (ALPHA .LE. 1.D0))
            NOMPAF = 'ALPHA'
            VALPAF = ALPHA
            CALL FOINTE('A',FONCIS,1,NOMPAF,VALPAF,CCIS,IER)
C  AY	    
            IF (NEL.EQ.NTEL(2).OR.NEL.EQ.NTEL(12)) THEN
                ZR(JDGE+IGEN+2) = 0.D0
            ELSE
                ZR(JDGE+IGEN+2) = CCIS
            ENDIF
C  AZ
            IF (NEL.EQ.NTEL(2).OR.NEL.EQ.NTEL(12)) THEN
                ZR(JDGE+IGEN+3) = ZR(JDGE+IGEN+2)
            ELSE
                ZR(JDGE+IGEN+3) = ZR(JDGE+IGEN+2)
            ENDIF
C  AI
            ZR(JDGE+IGEN2-1) = PI * RI * RI
C AY/AZ POUR TUYAUX ET 3D_FAISCEAU
            IF (NEL.EQ.NTEL(9).OR.NEL.EQ.NTEL(10).OR.
     &          NEL.EQ.NTEL(11).OR.NEL.EQ.NTEL(7)
     &          .OR.NEL.EQ.NTEL(8)) THEN
              ZR(JDGE+IGEN+2) = 0.D0
              ZR(JDGE+IGEN+3) = 0.D0
            ENDIF
C
 20      CONTINUE
C  JG1,JG2,IYR21,IYR22,IZR21,IZR22 :
         DO 21 I=1,6
           ZR(JDGE-1+38+I) = 0.D0
 21      CONTINUE
      ENDIF

      CALL JEDEMA()
      END

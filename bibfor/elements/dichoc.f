      SUBROUTINE DICHOC(NBT,NEQ,NNO,NC,ICODMA,
     &                  DUL,UTL,XG,PGL,KLV,
     &                  DULY,DVL,DPE,DVE,FORCE,
     &                  VARMO,VARPL,DIMELE)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NBT,NEQ,NNO,NC,ICODMA,DIMELE
      REAL*8   DUL(NEQ),UTL(NEQ),DVL(NEQ)
      REAL*8   DPE(NEQ),DVE(NEQ)
      REAL*8   KLV(NBT),DULY,XG(6),PGL(3,3)
      REAL*8   VARMO(7),VARPL(7),FORCE(3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C     RELATION DE COMPORTEMENT "DIS_CHOC"
C
C ----------------------------------------------------------------------
C
C IN  :  NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C        NEQ    : NOMBRE DE DDL DE L'ELEMENT
C        ICODMA : ADRESSE DU MATERIAU CODE
C        DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
C        UTL    : DEPLACEMENT COURANT REPERE LOCAL
C        DIMELE : DIMENSION DE L'ELEMENT
C
C OUT :  KLV    : MATRICE TANGENTE
C        DULY   :
C
C =============== DECLARATION DES VARIABLES LOCALES ====================
C
      INTEGER        NBRE1,NBPAR,N
      PARAMETER     (NBRE1=8)
      REAL*8         VALRE1(NBRE1)
      INTEGER        CODRE1(NBRE1)
      CHARACTER*8    NOMPAR,NOMRE1(NBRE1)
C
      REAL*8         XL(6),XD(3),DIRL(6),ZERO,R8PREM,RIGNOR,RIGTAN
      REAL*8         COULOM,DIST12,UTOT,VIT2,VIT3,DEPX,DEPY,DEPZ,PSCA
      REAL*8         VITT,VALPAR,VITY,VITZ,FORT,DIST0,KTY
C
      DATA NOMRE1 /'RIGI_NOR','RIGI_TAN','AMOR_NOR','AMOR_TAN'
     &            ,'COULOMB','DIST_1','DIST_2','JEU'/
C ----------------------------------------------------------------------
C
C --- DEFINITION DES PARAMETRES
      ZERO = 0.D0
      CALL R8INIR(6, ZERO, XL,   1)
      CALL R8INIR(6, ZERO, DIRL, 1)
      CALL R8INIR(3, ZERO, XD,   1)
C     COORDONNEES DANS LE REPERE LOCAL
      IF (DIMELE.EQ.3) THEN
         CALL UTPVGL(NNO,3,PGL,XG,XL)
      ELSE IF (DIMELE.EQ.2) THEN
         CALL UT2VGL(NNO,2,PGL,XG,XL)
      ENDIF
C
C     ELEMENT A 2 NOEUDS
      IF ( NNO.EQ.2 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = 0.D0
         CALL R8INIR (NBRE1,ZERO,VALRE1,1)
C ---    CARACTERISTIQUES DU MATERIAU
C        SI MOT_CLE RIGI_NOR ==> RIGNOR = VALRE1(1)
C        SINON               ==> RIGNOR = KLV(1)
         CALL RCVALA(ICODMA,' ','DIS_CONTACT',NBPAR,NOMPAR,VALPAR,
     &               NBRE1,NOMRE1,VALRE1,CODRE1, 0)
         IF ( CODRE1(1).EQ.0 ) THEN
            RIGNOR = VALRE1(1)
         ELSE
            RIGNOR = KLV(1)
         ENDIF
         RIGTAN = VALRE1(2)
C        AMONOR = VALRE1(3)
C        AMOTAN = VALRE1(4)
         COULOM = VALRE1(5)
         DIST12 = VALRE1(6)+VALRE1(7)
C        DANS L'AXE DU DISCRET
         DULY = DUL(1+NC)-DUL(1)
         UTOT = UTL(1+NC)-UTL(1)
C        VITESSE TANGENTE
         VIT2 = DVL(2+NC)-DVL(2)
         VIT3 = ZERO
         IF ( DIMELE.EQ.3 ) VIT3 = DVL(3+NC)-DVL(3)
C        LONGUEUR DU DISCRET
         CALL VDIFF(DIMELE,XL(1+DIMELE),XL(1),XD)
         CALL DCOPY(DIMELE,DPE(1),   1,DIRL,   1)
         CALL DCOPY(DIMELE,DPE(1+NC),1,DIRL(4),1)
         DEPX = XD(1) - DIST12 + UTOT + DIRL(4) - DIRL(1)
         DEPX = DEPX - R8PREM()
         DEPY = XD(2) + UTL(2+NC) - UTL(2) + DIRL(5) - DIRL(2)
         DEPZ = ZERO
         IF ( DIMELE.EQ.3 ) THEN
            DEPZ = XD(3) + UTL(3+NC) - UTL(3) + DIRL(6) - DIRL(3)
         ENDIF
         CALL DCOPY(DIMELE,DVE(1),   1,DIRL,   1)
         CALL DCOPY(DIMELE,DVE(1+NC),1,DIRL(4),1)
C        VITESSE TANGENTE
         VITY = VIT2 + DIRL(5) - DIRL(2)
         VITZ = ZERO
         IF ( DIMELE.EQ.3 ) THEN
            VITZ = VIT3 + DIRL(6) - DIRL(3)
         ENDIF
         IF (DEPX.LE.ZERO) THEN
            KTY  = RIGNOR
            FORCE(1) = RIGNOR*DEPX
            IF (FORCE(1).GT.ZERO) FORCE(1) = ZERO
            PSCA = VARMO(5)*VITY + VARMO(6)*VITZ
            IF (PSCA.GE.ZERO.AND.VARMO(7).EQ.1.D0) THEN
               VITT = (VITY**2 + VITZ**2)**0.5D0
               FORCE(2) = ZERO
               FORCE(3) = ZERO
               IF (VITT.NE.ZERO) THEN
                  FORCE(2) = -COULOM*FORCE(1)*VITY/VITT
                  FORCE(3) = -COULOM*FORCE(1)*VITZ/VITT
               ENDIF
               VARPL(7) = 1.D0
            ELSE
               FORCE(2) = RIGTAN*(DEPY-VARMO(1)) + VARMO(5)
               FORCE(3) = RIGTAN*(DEPZ-VARMO(2)) + VARMO(6)
               VARPL(7) = ZERO
               FORT = (FORCE(2)**2 + FORCE(3)**2)**0.5D0
               IF (FORT.GT.ABS(COULOM*FORCE(1))) THEN
                  VITT = (VITY**2 + VITZ**2)**0.5D0
                  FORCE(2) = ZERO
                  FORCE(3) = ZERO
                  IF (VITT.NE.ZERO) THEN
                     FORCE(2) = -COULOM*FORCE(1)*VITY/VITT
                     FORCE(3) = -COULOM*FORCE(1)*VITZ/VITT
                     VARPL(7) = 1.D0
                  ENDIF
               ENDIF
            ENDIF
            VARPL(5) = FORCE(2)
            VARPL(6) = FORCE(3)
            FORCE(2) = FORCE(2) + KLV(3)*(UTL(2+NC)-UTL(2))
            IF ( DIMELE.EQ.3 ) THEN
               FORCE(3) = FORCE(3) + KLV(6)*(UTL(3+NC)-UTL(3))
            ENDIF
            IF ( DIMELE.EQ.3 ) THEN
               IF (NEQ.EQ.6) THEN
                  KLV( 1) =  KTY
                  KLV( 7) = -KTY
                  KLV(10) =  KTY
               ELSEIF (NEQ.EQ.12) THEN
                  KLV( 1) =  KTY
                  KLV(22) = -KTY
                  KLV(28) =  KTY
               ENDIF
            ELSE
               IF (NEQ.EQ.4) THEN
                  KLV( 1) =  KTY
                  KLV( 4) = -KTY
                  KLV( 6) =  KTY
               ELSEIF (NEQ.EQ.6) THEN
                  KLV( 1) =  KTY
                  KLV( 7) = -KTY
                  KLV(10) =  KTY
               ENDIF
            ENDIF
         ELSE
            KTY = ZERO
            FORCE(1) = ZERO
            FORCE(2) = ZERO
            FORCE(3) = ZERO
            VARPL(5) = ZERO
            VARPL(6) = ZERO
            VARPL(7) = ZERO
            DO 10 N = 1,NBT
               KLV(N)= ZERO
10          CONTINUE
         ENDIF
         VARPL(1) = DEPY
         VARPL(2) = DEPZ
         VARPL(3) = VITY
         VARPL(4) = VITZ
C
C     ELEMENT A 1 NOEUD
      ELSE
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = 0.D0
         CALL R8INIR (NBRE1,ZERO,VALRE1,1)
C ---    CARACTERISTIQUES DU MATERIAU
C        SI MOT_CLE RIGI_NOR ==> RIGNOR = VALRE1(1)
C        SINON               ==> RIGNOR = KLV(1)
         CALL RCVALA(ICODMA,' ','DIS_CONTACT',NBPAR,NOMPAR,VALPAR,
     &               NBRE1,NOMRE1,VALRE1,CODRE1, 0)
         IF ( CODRE1(1).EQ.0 ) THEN
            RIGNOR = VALRE1(1)
         ELSE
            RIGNOR = KLV(1)
         ENDIF
         RIGTAN = VALRE1(2)
C        AMONOR = VALRE1(3)
C        AMOTAN = VALRE1(4)
         COULOM = VALRE1(5)
         DIST12 = VALRE1(6)
         DIST0  = VALRE1(8)
C        DANS L'AXE DU DISCRET
         DULY = DUL(1)
C        VITESSE TANGENTE
         VIT2 = DVL(2)
         VIT3 = ZERO
         IF ( DIMELE.EQ.3 ) VIT3 = DVL(3)
C        LONGUEUR DU DISCRET
         CALL DCOPY(DIMELE,DPE(1),1,DIRL,1)
         DEPX = UTL(1) + DIST12 + DIRL(1) - DIST0
         DEPY = UTL(2) + DIRL(2)
         DEPZ = ZERO
         IF ( DIMELE.EQ.3 ) DEPZ = UTL(3) + DIRL(3)
         CALL DCOPY(DIMELE,DVE(1),1,DIRL,1)
C        VITESSE TANGENTE
         VITY = VIT2 + DIRL(2)
         VITZ = ZERO
         IF ( DIMELE.EQ.3 ) VITZ = VIT3 + DIRL(3)
         IF (DEPX.GE.ZERO) THEN
            KTY  = RIGNOR
            FORCE(1) = RIGNOR*DEPX
            IF (FORCE(1).LT.ZERO) FORCE(1) = ZERO
            PSCA = VARMO(5)*VITY + VARMO(6)*VITZ
            IF (PSCA.GE.ZERO.AND.VARMO(7).EQ.1.D0) THEN
               VITT = (VITY**2 + VITZ**2)**0.5D0
               FORCE(2) = ZERO
               FORCE(3) = ZERO
               IF (VITT.NE.ZERO) THEN
                  FORCE(2) = COULOM*FORCE(1)*VITY/VITT
                  FORCE(3) = COULOM*FORCE(1)*VITZ/VITT
               ENDIF
               VARPL(7) = 1.D0
            ELSE
               FORCE(2) = RIGTAN*(DEPY-VARMO(1)) + VARMO(5)
               FORCE(3) = RIGTAN*(DEPZ-VARMO(2)) + VARMO(6)
               VARPL(7) = ZERO
               FORT = (FORCE(2)**2 + FORCE(3)**2)**0.5D0
               IF (FORT.GT.ABS(COULOM*FORCE(1))) THEN
                  VITT = (VITY**2 + VITZ**2)**0.5D0
                  FORCE(2) = ZERO
                  FORCE(3) = ZERO
                  IF (VITT.NE.ZERO) THEN
                     FORCE(2) = COULOM*FORCE(1)*VITY/VITT
                     FORCE(3) = COULOM*FORCE(1)*VITZ/VITT
                     VARPL(7) = 1.D0
                  ENDIF
               ENDIF
            ENDIF
            VARPL(5) = FORCE(2)
            VARPL(6) = FORCE(3)
            FORCE(2) = FORCE(2) + KLV(3)*UTL(2)
            IF ( DIMELE.EQ.3 ) FORCE(3) = FORCE(3) + KLV(6)*UTL(3)
            KLV(1) = KTY
         ELSE
            KTY  = ZERO
            FORCE(1) = ZERO
            FORCE(2) = ZERO
            FORCE(3) = ZERO
            VARPL(5) = ZERO
            VARPL(6) = ZERO
            VARPL(7) = ZERO
            DO 20 N = 1,NBT
               KLV(N)= ZERO
20          CONTINUE
         ENDIF
         VARPL(1) = DEPY
         VARPL(2) = DEPZ
         VARPL(3) = VITY
         VARPL(4) = VITZ
      ENDIF
C
      END

      SUBROUTINE DICHOC (NBT,NEQ,NNO,NC,ICODMA,DUL,UTL,XG,PGL,
     &            KLV,KTY2,DULY,DVL,DPE,DVE,FOR2,FOR3,VARMO,VARPL)
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NBT,NEQ,NNO,NC,ICODMA
      REAL*8  DUL(NEQ),UTL(NEQ),DVL(NEQ)
      REAL*8  DPE(NEQ),DVE(NEQ)
      REAL*8  KLV(NBT),KTY,KTY2,DULY,XG(6),PGL(3,3)
      REAL*8  VARMO(7),VARPL(7)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_21
C
C     RELATION DE COMPORTEMENT "DIS_CONTACT"
C
C ----------------------------------------------------------------------
C
C IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       ICODMA : ADRESSE DU MATERIAU CODE
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
C       UTL    : DEPLACEMENT COURANT REPERE LOCAL
C
C OUT : KLV    : MATRICE TANGENTE
C       KTY2   :
C       DULY   :
C
C**************** DECLARATION DES VARIABLES LOCALES ********************
C
      PARAMETER    ( NBRE1 = 8 )
      REAL*8       XL(6), XD(3), DIRI(6), DIRL(6)
      REAL*8       VALRE1(NBRE1)
      CHARACTER*2  CODRE1(NBRE1)
      CHARACTER*8  NOMPAR, NOMRE1(NBRE1)
C
      DATA NOMRE1/'RIGI_NOR','RIGI_TAN','AMOR_NOR','AMOR_TAN'
     &           ,'COULOMB','DIST_1','DIST_2','JEU'/
C
C ----------------------------------------------------------------------
C --- DEFINITION DES PARAMETRES
C
      ZERO = 0.D0
      EPSI = 1.D-20
      CALL UTPVGL(NNO,3,PGL,XG,XL)
      IF (NNO.NE.2) GOTO 100
C
CC   CAS NNO = 2
CC
      NBPAR = 0
      NOMPAR = ' '
      VALPAR = 0.D0
      CALL R8INIR (NBRE1,ZERO,VALRE1,1)
C
C --- CARACTERISTIQUES DU MATERIAU
C
C     MOT_CLE RIGI_NOR OBLIGATOIRE
C
      NBRE2 = 1
      CALL RCVALA(ICODMA,' ','DIS_CONTACT',NBPAR,NOMPAR,VALPAR,NBRE2,
     &                            NOMRE1,VALRE1,CODRE1,'FM')
      CALL RCVALA(ICODMA,' ','DIS_CONTACT',NBPAR,NOMPAR,VALPAR,NBRE1,
     &                            NOMRE1,VALRE1,CODRE1, ' ' )
      RIGNOR = VALRE1(1)
      RIGTAN = VALRE1(2)
C      AMONOR = VALRE1(3)
C      AMOTAN = VALRE1(4)
      COULOM = VALRE1(5)
      DIST12 = VALRE1(6)+VALRE1(7)
C
      DULY = DUL(1+NC)-DUL(1)
      UTOT = UTL(1+NC)-UTL(1)
C      VITN = DVL(1+NC)-DVL(1)
      VIT2 = DVL(2+NC)-DVL(2)
      VIT3 = DVL(3+NC)-DVL(3)
      CALL VDIFF(3,XL(4),XL(1),XD)
      CALL DCOPY(3,DPE(1),1,DIRL,1)
      CALL DCOPY(3,DPE(1+NC),1,DIRL(4),1)
      DEPX = XD(1) - DIST12 + UTOT + DIRL(4) - DIRL(1)
      DEPX = DEPX - EPSI
      DEPY = XD(2) + UTL(2+NC) - UTL(2) + DIRL(5) - DIRL(2)
      DEPZ = XD(3) + UTL(3+NC) - UTL(3) + DIRL(6) - DIRL(3)
      CALL DCOPY(3,DVE(1),1,DIRL,1)
      CALL DCOPY(3,DVE(1+NC),1,DIRL(4),1)
C      VITX = VITN + DIRL(4) - DIRL(1)
      VITY = VIT2 + DIRL(5) - DIRL(2)
      VITZ = VIT3 + DIRL(6) - DIRL(3)
      IF (DEPX.LE.ZERO) THEN
         KTY = RIGNOR
         KTY2 = RIGNOR*DEPX
         IF (KTY2.GT.ZERO) KTY2 = ZERO
         PSCA = VARMO(5)*VITY + VARMO(6)*VITZ
         IF (PSCA.GE.ZERO.AND.VARMO(7).EQ.1.D0) THEN
           VITT = (VITY**2 + VITZ**2)**0.5D0
           FOR2 = ZERO
           FOR3 = ZERO
           IF (VITT.NE.ZERO) THEN
            FOR2 = -COULOM*KTY2*VITY/VITT
            FOR3 = -COULOM*KTY2*VITZ/VITT
           ENDIF
           VARPL(7) = 1.D0
         ELSE
           FOR2 = RIGTAN*(DEPY-VARMO(1)) + VARMO(5)
           FOR3 = RIGTAN*(DEPZ-VARMO(2)) + VARMO(6)
           VARPL(7) = ZERO
           FORT = (FOR2**2 + FOR3**2)**0.5D0
           IF (FORT.GT.ABS(COULOM*KTY2)) THEN
             VITT = (VITY**2 + VITZ**2)**0.5D0
             FOR2 = ZERO
             FOR3 = ZERO
             IF (VITT.NE.ZERO) THEN
               FOR2 = -COULOM*KTY2*VITY/VITT
               FOR3 = -COULOM*KTY2*VITZ/VITT
               VARPL(7) = 1.D0
             ENDIF
           ENDIF
         ENDIF
         VARPL(5) = FOR2
         VARPL(6) = FOR3
         FOR2 = FOR2 + KLV(3)*(UTL(2+NC)-UTL(2))
         FOR3 = FOR3 + KLV(6)*(UTL(3+NC)-UTL(3))
         IF (NEQ.EQ.6) THEN
           KLV(1) = KTY
           KLV(7) = -KTY
           KLV(10) = KTY
         ELSEIF (NEQ.EQ.12) THEN
           KLV(1) = KTY
           KLV(22) = -KTY
           KLV(28) = KTY
         ENDIF
      ELSE
         KTY = ZERO
         KTY2 = ZERO
         VARPL(5) = ZERO
         VARPL(6) = ZERO
         VARPL(7) = ZERO
         FOR2 = ZERO
         FOR3 = ZERO
         DO 1 N = 1,NBT
          KLV(N)= ZERO
   1     CONTINUE
      ENDIF
      VARPL(1) = DEPY
      VARPL(2) = DEPZ
      VARPL(3) = VITY
      VARPL(4) = VITZ
      GOTO 9999
 100  CONTINUE
C
C    CAS NNO = 1 A TRAITER
C ----------------------------------------------------------------------
C
      NBPAR = 0
      NOMPAR = ' '
      VALPAR = 0.D0
      CALL R8INIR (NBRE1,ZERO,VALRE1,1)
C
C --- CARACTERISTIQUES DU MATERIAU
C
      CALL RCVALA(ICODMA,' ','DIS_CONTACT',NBPAR,NOMPAR,VALPAR,NBRE1,
     &                            NOMRE1,VALRE1,CODRE1, ' ' )
      RIGNOR = VALRE1(1)
      RIGTAN = VALRE1(2)
C      AMONOR = VALRE1(3)
C      AMOTAN = VALRE1(4)
      COULOM = VALRE1(5)
      DIST12 = VALRE1(6)
      DIST0 = VALRE1(8)
C
C
      DULY = DUL(1)
C      VITN = DVL(1)
      VIT2 = DVL(2)
      VIT3 = DVL(3)
      CALL DCOPY(3,DPE(1),1,DIRL,1)
      DEPX = UTL(1) + DIST12 + DIRL(1) - DIST0
      DEPY = UTL(2) + DIRL(2)
      DEPZ = UTL(3) + DIRL(3)
      CALL DCOPY(3,DVE(1),1,DIRL,1)
C      VITX = VITN + DIRL(1)
      VITY = VIT2 + DIRL(2)
      VITZ = VIT3 + DIRL(3)
      IF (DEPX.GE.ZERO) THEN
         KTY = RIGNOR
         KTY2 = RIGNOR*DEPX
         IF (KTY2.LT.ZERO) KTY2 = ZERO
         PSCA = VARMO(5)*VITY + VARMO(6)*VITZ
         IF (PSCA.GE.ZERO.AND.VARMO(7).EQ.1.D0) THEN
           VITT = (VITY**2 + VITZ**2)**0.5D0
           FOR2 = ZERO
           FOR3 = ZERO
           IF (VITT.NE.ZERO) THEN
            FOR2 = COULOM*KTY2*VITY/VITT
            FOR3 = COULOM*KTY2*VITZ/VITT
           ENDIF
           VARPL(7) = 1.D0
         ELSE
           FOR2 = RIGTAN*(DEPY-VARMO(1)) + VARMO(5)
           FOR3 = RIGTAN*(DEPZ-VARMO(2)) + VARMO(6)
           VARPL(7) = ZERO
           FORT = (FOR2**2 + FOR3**2)**0.5D0
           IF (FORT.GT.ABS(COULOM*KTY2)) THEN
             VITT = (VITY**2 + VITZ**2)**0.5D0
             FOR2 = ZERO
             FOR3 = ZERO
             IF (VITT.NE.ZERO) THEN
               FOR2 = COULOM*KTY2*VITY/VITT
               FOR3 = COULOM*KTY2*VITZ/VITT
               VARPL(7) = 1.D0
             ENDIF
           ENDIF
         ENDIF
         VARPL(5) = FOR2
         VARPL(6) = FOR3
         FOR2 = FOR2 + KLV(3)*UTL(2)
         FOR3 = FOR3 + KLV(6)*UTL(3)
         KLV(1) = KTY
      ELSE
         KTY = ZERO
         KTY2 = ZERO
         FOR2 = ZERO
         FOR3 = ZERO
         VARPL(5) = ZERO
         VARPL(6) = ZERO
         VARPL(7) = ZERO
         DO 2 N = 1,NBT
          KLV(N)= ZERO
   2     CONTINUE
      ENDIF
      VARPL(1) = DEPY
      VARPL(2) = DEPZ
      VARPL(3) = VITY
      VARPL(4) = VITZ
C
 9999 CONTINUE
      END

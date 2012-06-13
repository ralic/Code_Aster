      SUBROUTINE NMCEAI(NUMEDD,DEPDEL,DEPPR1,DEPPR2,DEPOLD,
     &                  SDPILO,RHO,ETA,ISXFE,F,INDIC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER       INDIC
      CHARACTER*24  NUMEDD
      CHARACTER*19  SDPILO,DEPDEL,DEPOLD,DEPPR1,DEPPR2
      REAL*8        ETA,RHO,F
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - SELECTION PARAMETRE)
C
C CALCUL DU PARAMETRE DE SELECTION DE TYPE ANGL_INCR_DEPL
C
C ----------------------------------------------------------------------
C
C
C IN  NUMEDD : NUME_DDL
C IN  SDPILO : SD PILOTAGE
C IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
C IN  DEPOLD : INCREMENT DE DEPLACEMENT PAS DE TEMPS PRECEDENT
C IN  DEPPR1 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
C IN  DEPPR2 : INCREMENT DE DEPLACEMENT K-1.F_PILO
C IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
C IN  ETA    : PARAMETRE DE PILOTAGE
C IN  ISXFE  : INDIQUE SI LE MODELE EST UN MODELE XFEM
C OUT F      : VALEUR DU CRITERE
C OUT INDIC  : 0 CRITERE NON UTILISABLE
C              1 CRITERE UTILISABLE
C
C
C
C
      CHARACTER*8   K8BID
      REAL*8        SCA,NODUP,COEF,NODUP1,NODUP2
      INTEGER       JDEPDE,JDU0,JDU1,JDEPOL
      INTEGER       NEQ,IRET,I,J,IBID
      CHARACTER*19  PROFCH,CHAPIL,CHAPIC,SELPIL
      INTEGER       JCOEE,JCOEF,IDEEQ,JPLSL
      REAL*8        DN,DC,DP,DA
      LOGICAL       ISXFE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      IF(ISXFE) THEN
        CHAPIL = SDPILO(1:14)//'.PLCR'
        CALL JEVEUO(CHAPIL(1:19)//'.VALE','L',JCOEF)
        CHAPIC = SDPILO(1:14)//'.PLCI'
        CALL JEVEUO(CHAPIC(1:19)//'.VALE','L',JCOEE)
      ELSE
C
C --- ACCES VECTEUR DE SELCTION CMP DX/DY/DZ
C
        SELPIL = SDPILO(1:14)//'.PLSL'
        CALL JEVEUO(SELPIL(1:19)//'.VALE','L',JPLSL)
      ENDIF

C
C --- INITIALISATIONS--------------------------------
C
      SCA    = 0.D0
      NODUP  = 0.D0
      NODUP1 = 0.D0
      NODUP2 = 0.D0
      F      = 0.D0
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID,IRET)
      CALL DISMOI('F','PROF_CHNO',DEPDEL,'CHAM_NO' ,IBID  ,PROFCH,IRET)
      CALL JEVEUO(PROFCH(1:19)//'.DEEQ','L',IDEEQ)
C
C --- ACCES AUX VECTEURS SOLUTIONS
C
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(DEPPR1(1:19)//'.VALE','L',JDU0)
      CALL JEVEUO(DEPPR2(1:19)//'.VALE','L',JDU1)
      CALL JEVEUO(DEPOLD(1:19)//'.VALE','L',JDEPOL)

C
C --- CALCUL DE L'ANGLE
C
      IF(ISXFE) THEN
      DO 26 I = 1,NEQ
         IF(ZI(IDEEQ-1+2*I).GT.0) THEN
            IF(ZR(JCOEE+I-1).EQ.0.D0) THEN
                SCA    = SCA    + ZR(JDEPOL+I-1)*
     &            ZR(JCOEF+I-1)**2*(ZR(JDEPDE+I-1)
     &                          + RHO*ZR(JDU0+I-1)
     &                          + ETA*ZR(JDU1+I-1))
                NODUP1  = NODUP1 +
     &             ZR(JCOEF+I-1)**2*(ZR(JDEPDE+I-1)
     &                           + RHO*ZR(JDU0+I-1)
     &                           + ETA*ZR(JDU1+I-1))**2
                NODUP2  = NODUP2 +
     &           ZR(JCOEF+I-1)**2*ZR(JDEPOL+I-1)**2
            ELSE
            DA  = 0.D0
            DN  = 0.D0
            DC  = 0.D0
            DP  = 0.D0
            DO 30 J = I+1,NEQ
               IF(ZR(JCOEE+I-1).EQ.ZR(JCOEE+J-1)) THEN
               DA = DA + ZR(JCOEF+I-1)*ZR(JDEPOL+I-1)+
     &                  ZR(JCOEF+J-1)*ZR(JDEPOL+J-1)
                       DN = DN + ZR(JCOEF+I-1)*ZR(JDEPDE+I-1)+
     &                  ZR(JCOEF+J-1)*ZR(JDEPDE+J-1)
                       DC = DC + ZR(JCOEF+I-1)*ZR(JDU0-1+I)+
     &                  ZR(JCOEF+J-1)*ZR(JDU0-1+J)
                       DP = DP + ZR(JCOEF+I-1)*ZR(JDU1-1+I)+
     &                  ZR(JCOEF+J-1)*ZR(JDU1-1+J)
                 ENDIF
 30            CONTINUE
               SCA    = SCA    + DA*(DN+RHO*DC+ETA*DP)
               NODUP1  = NODUP1 + (DN+RHO*DC+ETA*DP)**2
               NODUP2  = NODUP2 + DA**2
            ENDIF
         ENDIF
 26   CONTINUE
      NODUP = NODUP1*NODUP2
      ELSE
         DO 25 I = 1,NEQ
         COEF   = ZR(JPLSL-1+I)
         SCA    = SCA + (ZR(JDEPOL+I-1)*(ZR(JDEPDE+I-1)
     &                 + RHO*ZR(JDU0+I-1)
     &                 +  ETA*ZR(JDU1+I-1)))*COEF
            NODUP  = NODUP  + (ZR(JDEPDE+I-1)
     &                 + RHO*ZR(JDU0+I-1)
     &                 + ETA*ZR(JDU1+I-1))**2
 25      CONTINUE
      ENDIF

      IF(NODUP.EQ.0.D0) THEN
       INDIC = 0
       F  = 0.D0
      ELSE
       INDIC = 1
       F   = SCA / SQRT(NODUP)
      ENDIF
       F   = -F
C
      CALL JEDEMA()
      END

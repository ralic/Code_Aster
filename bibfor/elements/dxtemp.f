      SUBROUTINE DXTEMP ( NOMTE, TSUP, TINF, TMOY, INDITH )
      IMPLICIT  NONE
      REAL*8        TMOY(4), TINF(4), TSUP(4)
      LOGICAL       INDITH
      CHARACTER*16  NOMTE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C     CALCUL DES TEMPERATURES AUX NOEUDS POUR LES ELEMENTS DE PLAQUE
C     DKT, DST, DKQ, DSQ ET Q4G A PARTIR DE LA DONNEE DES TEMPERATURES
C     EN ENTREE DE L'OPTION NON LINEAIRE .
C
C     IN   K16 NOMTE     NOM DU TYPE D'ELEMENT
C
C     OUT  R8  TSUP(4)   TEMPERATURES AUX NOEUDS DU FEUILLET SUPERIEUR
C     OUT  R8  TINF(4)   TEMPERATURES AUX NOEUDS DU FEUILLET INFERIEUR
C     OUT  R8  TMOY(4)   TEMPERATURES AUX NOEUDS DU FEUILLET MOYEN
C     OUT  L   INDITH    LOGICAL = .TRUE. S'IL Y A DES TEMPERATURES
C                                = .FALSE. SINON
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       I, IBID, IER, ITEMP, JCARA, NNO, ITAB(8), IRET
      REAL*8        DEUX, EPAIS, TINF1, TMOY1, TSUP1, VALPU(2), ZERO
      CHARACTER*8   NOMPU(2)
C     ------------------------------------------------------------------
C
C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0
      DEUX = 2.0D0
C
      INDITH = .FALSE.
C
      DO 10 I = 1,4
        TINF(I) = ZERO
        TMOY(I) = ZERO
        TSUP(I) = ZERO
   10 CONTINUE
C
C --- RECUPERATION DES CARACTERISTIQUES DE COQUE :
C     ------------------------------------------
      CALL JEVECH('PCACOQU','L',JCARA)
      EPAIS  = ZR(JCARA)
C
      IF (NOMTE(1:8).EQ.'MEDKTR3 ' .OR. NOMTE(1:8).EQ.'MEDSTR3 ' .OR.
     &    NOMTE(1:8).EQ.'MEGRDKT'  .OR. NOMTE(1:8).EQ.'MEDKTG3 ') THEN
         NNO = 3
      ELSE IF (NOMTE(1:8).EQ.'MEDKQU4 ' .OR.
     &         NOMTE(1:8).EQ.'MEDKQG4 ' .OR.
     &         NOMTE(1:8).EQ.'MEDSQU4 ' .OR.
     &         NOMTE(1:8).EQ.'MEQ4QU4 ') THEN
        NNO = 4
      ELSE
          CALL U2MESK('F','ELEMENTS_14',1,NOMTE(1:8))
      END IF
C
C --- RECUPERATION DE LA TEMPERATURE :
C     ==============================
C     -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
C        ---------------------------------------
      CALL TECACH ('OON','PTEMPER',8,ITAB,IRET)
      ITEMP=ITAB(1)
      IF (IRET.EQ.0 .OR. IRET.EQ.3) THEN
        INDITH = .TRUE.
        DO 20 I = 1,NNO
            IF (IRET.EQ.3)
     &         CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
            TMOY(I) = ZR(ITEMP+3*(I-1))
            TINF(I) = ZR(ITEMP+3*(I-1)+1)
            TSUP(I) = ZR(ITEMP+3*(I-1)+2)
   20   CONTINUE
      END IF
C     -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS' :
C        ------------------------------------------------------
      CALL TECACH('NNN','PTEMPEF',1,ITEMP,IRET)
      IF (IRET.EQ.0) THEN
        INDITH = .TRUE.
        NOMPU(1) = 'INST'
        NOMPU(2) = 'EPAIS'
        CALL JEVECH('PINSTPR','L',IBID)
        VALPU(1) = ZR(IBID)
        VALPU(2) = ZERO
        CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TMOY1,IER)
        VALPU(2) = -EPAIS/DEUX
        CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TINF1,IER)
        VALPU(2) = +EPAIS/DEUX
        CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TSUP1,IER)
        DO 30,I = 1,NNO
          TMOY(I) = TMOY1
          TINF(I) = TINF1
          TSUP(I) = TSUP1
   30   CONTINUE
      END IF
C
      END

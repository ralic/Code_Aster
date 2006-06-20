      LOGICAL FUNCTION IDENOB(OBJ1,OBJ2)
      IMPLICIT NONE
      CHARACTER*(*) OBJ1,OBJ2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/06/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------
C  BUT : DETERMINER L'IDENTITE DE 2 OBJETS JEVEUX
C  IN  K*    OBJ1   : NOM DU 1ER OBJET JEVEUX
C  IN  K*    OBJ2   : NOM DU 2EME OBJET JEVEUX

C     RESULTAT:
C       IDENOB : .TRUE.    SI OBJ1(*) == OBJ2(*)
C                    OU SI OBJ1 ET OBJ2 SONT INEXISTANTS TOUS LES 2
C                .FALSE.   SINON
C ----------------------------------------------------------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
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
C ----------------------------------------------------------------------
      CHARACTER*24 OB1,OB2,K241,K242
      CHARACTER*1 TYP1,TYP2,KBID
      CHARACTER*4 GENR1,GENR2,GENR,XOUS1,XOUS2,XOUS,TYPE
      INTEGER IRET1,IRET2,LTYP1,LTYP2,LTYP,L,L1,L2,K,IAD1,IAD2,IBID
      INTEGER IOBJ,NBOBJ

C -DEB------------------------------------------------------------------

      CALL JEMARQ()
      OB1 = OBJ1
      OB2 = OBJ2
      IDENOB = .TRUE.

      IF (OB1.EQ.OB2) GO TO 220


      CALL JEEXIN(OB1,IRET1)
      CALL JEEXIN(OB2,IRET2)
      IF (IRET1.EQ.0) THEN
        IF (IRET2.GT.0) GO TO 210
        GO TO 220
      ELSE
        IF (IRET2.EQ.0) GO TO 210
      END IF


C     -- DETERMINATION DE TYPE: R/C/K8,...
C     -------------------------------------
      CALL JELIRA(OB1,'TYPE',IBID,TYP1)
      CALL JELIRA(OB2,'TYPE',IBID,TYP2)
      IF (TYP1.NE.TYP2) THEN
        GO TO 210
      END IF

      IF (TYP1.EQ.'K') THEN
        CALL JELIRA(OB1,'LTYP',LTYP1,KBID)
        CALL JELIRA(OB2,'LTYP',LTYP2,KBID)
        IF (LTYP1.NE.LTYP2) THEN
          GO TO 210
        ELSE
          LTYP = LTYP1
        END IF


        IF (LTYP.EQ.8) THEN
          TYPE = 'K8'
        ELSE IF (LTYP.EQ.16) THEN
          TYPE = 'K16'
        ELSE IF (LTYP.EQ.24) THEN
          TYPE = 'K24'
        ELSE IF (LTYP.EQ.32) THEN
          TYPE = 'K32'
        ELSE IF (LTYP.EQ.80) THEN
          TYPE = 'K80'
        END IF
      ELSE
        TYPE = TYP1
      END IF



C     -- DETERMINATION DE XOUS ET GENR
C     -------------------------------------
      CALL JELIRA(OB1,'XOUS',IBID,XOUS1)
      CALL JELIRA(OB2,'XOUS',IBID,XOUS2)
      IF (XOUS1.NE.XOUS2) THEN
        GO TO 210
      ELSE
        XOUS = XOUS1
      END IF

      CALL JELIRA(OB1,'GENR',IBID,GENR1)
      CALL JELIRA(OB2,'GENR',IBID,GENR2)
      IF (GENR1.NE.GENR2) THEN
        GO TO 210
      ELSE
        GENR = GENR1
      END IF



C     3- ON COMPARE LE CONTENU DES OBJETS
C     -------------------------------------

C     3.1 : CAS DES OBJETS SIMPLES :
C     ------------------------------
      IF (XOUS.EQ.'S') THEN
        IF (GENR.EQ.'V') THEN

          CALL JELIRA(OB1,'LONMAX',L1,KBID)
          CALL JELIRA(OB2,'LONMAX',L2,KBID)
          IF (L1.NE.L2) THEN
            GO TO 210
          END IF

          CALL JELIRA(OB1,'LONUTI',L1,KBID)
          CALL JELIRA(OB2,'LONUTI',L2,KBID)
          IF (L1.NE.L2) THEN
            GO TO 210
          ELSE
            L = L1
          END IF

          CALL JEVEUO(OB1,'L',IAD1)
          CALL JEVEUO(OB2,'L',IAD2)

          IF (TYPE.EQ.'R') THEN
            DO 10,K = 1,L
              IF (ZR(IAD1-1+K).NE.ZR(IAD2-1+K)) GO TO 210
   10       CONTINUE

          ELSE IF (TYPE.EQ.'I') THEN
            DO 20,K = 1,L
              IF (ZI(IAD1-1+K).NE.ZI(IAD2-1+K)) GO TO 210
   20       CONTINUE

          ELSE IF (TYPE.EQ.'C') THEN
            DO 30,K = 1,L
              IF (ZC(IAD1-1+K).NE.ZC(IAD2-1+K)) GO TO 210
   30       CONTINUE

          ELSE IF (TYPE.EQ.'L') THEN
            DO 40,K = 1,L
              IF (.NOT. (ZL(IAD1-1+K).OR. (.NOT.
     &            ZL(IAD2-1+K)))) GO TO 210
   40       CONTINUE

          ELSE IF (TYPE.EQ.'K8') THEN
            DO 50,K = 1,L
              IF (ZK8(IAD1-1+K).NE.ZK8(IAD2-1+K)) GO TO 210
   50       CONTINUE

          ELSE IF (TYPE.EQ.'K16') THEN
            DO 60,K = 1,L
              IF (ZK16(IAD1-1+K).NE.ZK16(IAD2-1+K)) GO TO 210
   60       CONTINUE

          ELSE IF (TYPE.EQ.'K24') THEN
            DO 70,K = 1,L
              IF (ZK24(IAD1-1+K).NE.ZK24(IAD2-1+K)) GO TO 210
   70       CONTINUE

          ELSE IF (TYPE.EQ.'K32') THEN
            DO 80,K = 1,L
              IF (ZK32(IAD1-1+K).NE.ZK32(IAD2-1+K)) GO TO 210
   80       CONTINUE

          ELSE IF (TYPE.EQ.'K80') THEN
            DO 90,K = 1,L
              IF (ZK80(IAD1-1+K).NE.ZK80(IAD2-1+K)) GO TO 210
   90       CONTINUE

          ELSE
            CALL UTMESS('F','IDENOB','STOP1')
          END IF


        ELSE IF (GENR.EQ.'N') THEN
C       ------------------------------
          CALL JELIRA(OB1,'NOMMAX',L1,KBID)
          CALL JELIRA(OB2,'NOMMAX',L2,KBID)
          IF (L1.NE.L2) THEN
            GO TO 210
          END IF

          CALL JELIRA(OB1,'NOMUTI',L1,KBID)
          CALL JELIRA(OB2,'NOMUTI',L2,KBID)
          IF (L1.NE.L2) THEN
            GO TO 210
          ELSE
            L = L1
          END IF

          DO 100,K = 1,L
            CALL JENUNO(JEXNUM(OB1,K),K241)
            CALL JENUNO(JEXNUM(OB2,K),K242)
            IF (K241.NE.K242) GO TO 210
  100     CONTINUE



        ELSE
          CALL UTMESS('F','IDENOB','A FAIRE 1 ...')
        END IF


C     3.2 : CAS DES COLLECTIONS :
C     ------------------------------
      ELSE
        IF (GENR.EQ.'V') THEN

          CALL JELIRA(OB1,'NMAXOC',L1,KBID)
          CALL JELIRA(OB2,'NMAXOC',L2,KBID)
          IF (L1.NE.L2) THEN
            GO TO 210
          END IF

          CALL JELIRA(OB1,'NUTIOC',L1,KBID)
          CALL JELIRA(OB2,'NUTIOC',L2,KBID)
          IF (L1.NE.L2) THEN
            GO TO 210
          END IF
          NBOBJ = L1

          DO 200,IOBJ = 1,NBOBJ

            CALL JELIRA(JEXNUM(OB1,IOBJ),'LONMAX',L1,KBID)
            CALL JELIRA(JEXNUM(OB2,IOBJ),'LONMAX',L2,KBID)
            IF (L1.NE.L2) THEN
              GO TO 210
            END IF

            CALL JELIRA(JEXNUM(OB1,IOBJ),'LONUTI',L1,KBID)
            CALL JELIRA(JEXNUM(OB2,IOBJ),'LONUTI',L2,KBID)
            IF (L1.NE.L2) THEN
              GO TO 210
            ELSE
              L = L1
            END IF

            CALL JEVEUO(JEXNUM(OB1,IOBJ),'L',IAD1)
            CALL JEVEUO(JEXNUM(OB2,IOBJ),'L',IAD2)

            IF (TYPE.EQ.'R') THEN
              DO 110,K = 1,L
                IF (ZR(IAD1-1+K).NE.ZR(IAD2-1+K)) GO TO 210
  110         CONTINUE

            ELSE IF (TYPE.EQ.'I') THEN
              DO 120,K = 1,L
                IF (ZI(IAD1-1+K).NE.ZI(IAD2-1+K)) GO TO 210
  120         CONTINUE

            ELSE IF (TYPE.EQ.'C') THEN
              DO 130,K = 1,L
                IF (ZC(IAD1-1+K).NE.ZC(IAD2-1+K)) GO TO 210
  130         CONTINUE

            ELSE IF (TYPE.EQ.'L') THEN
              DO 140,K = 1,L
                IF (.NOT. (ZL(IAD1-1+K).OR.
     &              (.NOT.ZL(IAD2-1+K)))) GO TO 210
  140         CONTINUE

            ELSE IF (TYPE.EQ.'K8') THEN
              DO 150,K = 1,L
                IF (ZK8(IAD1-1+K).NE.ZK8(IAD2-1+K)) GO TO 210
  150         CONTINUE

            ELSE IF (TYPE.EQ.'K16') THEN
              DO 160,K = 1,L
                IF (ZK16(IAD1-1+K).NE.ZK16(IAD2-1+K)) GO TO 210
  160         CONTINUE

            ELSE IF (TYPE.EQ.'K24') THEN
              DO 170,K = 1,L
                IF (ZK24(IAD1-1+K).NE.ZK24(IAD2-1+K)) GO TO 210
  170         CONTINUE

            ELSE IF (TYPE.EQ.'K32') THEN
              DO 180,K = 1,L
                IF (ZK32(IAD1-1+K).NE.ZK32(IAD2-1+K)) GO TO 210
  180         CONTINUE

            ELSE IF (TYPE.EQ.'K80') THEN
              DO 190,K = 1,L
                IF (ZK80(IAD1-1+K).NE.ZK80(IAD2-1+K)) GO TO 210
  190         CONTINUE

            ELSE
              CALL UTMESS('F','IDENOB','STOP1')
            END IF
  200     CONTINUE

        ELSE
          CALL UTMESS('F','IDENOB','A FAIRE 2 ...')
        END IF
      END IF

      GO TO 220

  210 CONTINUE
      IDENOB = .FALSE.

  220 CONTINUE


      CALL JEDEMA()
      END

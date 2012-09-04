      SUBROUTINE CNSPRJ(CNS1Z,CORREZ,BASEZ,CNS2Z,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CNS1Z,CORREZ,BASEZ,CNS2Z
      INTEGER IRET
C ------------------------------------------------------------------
C BUT : PROJETER UN CHAM_NO_S  SUR UN AUTRE MAILLAGE
C ------------------------------------------------------------------
C     ARGUMENTS:
C CNS1Z  IN/JXIN  K19 : CHAM_NO_S A PROJETER
C CORREZ IN/JXIN  K16 : NOM DE LA SD CORRESP_2_MAILLA
C BASEZ  IN       K1  : BASE DE CREATION POUR CNS2Z : G/V
C CNS2Z  IN/JXOUT K19 : CHAM_NO_S RESULTAT DE LA PROJECTION
C IRET   OUT      I   : CODE RETOUR :
C                       0 -> OK
C                       1 -> ECHEC DE LA PROJECTION
C ------------------------------------------------------------------
C    ON NE TRAITE QUE LES CHAMPS REELS (R8) OU COMPLEXES (C16)
C
C
C REMARQUE :
C   LA PROJECTION EST APPROCHEE DANS LE CAS OU TOUS LES NOEUDS
C   DE LA MAILLE M1 NE PORTENT PAS LES MEMES DDLS.
C   LE TRAITEMENT EST EXPLIQUE DANS LA REPONSE A LA FICHE 9259
C
      CHARACTER*24 VALK(3)
C     ------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*1 BASE
      CHARACTER*3 TSCA
      CHARACTER*8 MA1,MA2,NOMGD,NOMCMP,NOMNO2
      CHARACTER*16 CORRES
      CHARACTER*19 CNS1,CNS2
      INTEGER JCNS1C,JCNS1L,JCNS1V,JCNS1K,JCNS1D
      INTEGER JCNS2C,JCNS2L,JCNS2V,JCNS2K,JCNS2D
      INTEGER NBNO1,NCMP,IBID,JXXK1,IACONB,IACONU,IACOCF,GD,NBNO2
      INTEGER IDECAL,INO2,ICMP,ICO1,ICO2,INO1,NUNO1,KALARM
      REAL*8 V1,V2,COEF1,COETOT,VRMOY
      COMPLEX*16 V1C,V2C,VCMOY
      LOGICAL LEXACT
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CNS1 = CNS1Z
      CNS2 = CNS2Z
      BASE = BASEZ
      CORRES = CORREZ
      IRET = 0
      KALARM=0


C------------------------------------------------------------------
C     1- RECUPERATION DES OBJETS ET INFORMATIONS DE CNS1 :
C     ----------------------------------------------------

      CALL JEVEUO(CNS1//'.CNSK','L',JCNS1K)
      CALL JEVEUO(CNS1//'.CNSD','L',JCNS1D)
      CALL JEVEUO(CNS1//'.CNSC','L',JCNS1C)
      CALL JEVEUO(CNS1//'.CNSV','L',JCNS1V)
      CALL JEVEUO(CNS1//'.CNSL','L',JCNS1L)

      MA1 = ZK8(JCNS1K-1+1)
      NOMGD = ZK8(JCNS1K-1+2)
      NBNO1 = ZI(JCNS1D-1+1)
      NCMP = ZI(JCNS1D-1+2)

      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)


C------------------------------------------------------------------
C     2- RECUPERATION DES OBJETS ET INFORMATIONS DE CORRES :
C     ----------------------------------------------------
      CALL JEVEUO(CORRES//'.PJXX_K1','L',JXXK1)
      CALL JEVEUO(CORRES//'.PJEF_NB','L',IACONB)
      CALL JEVEUO(CORRES//'.PJEF_NU','L',IACONU)
      CALL JEVEUO(CORRES//'.PJEF_CF','L',IACOCF)

      MA2 = ZK24(JXXK1-1+2)


C------------------------------------------------------------------
C     3- QUELQUES VERIFS :
C     ------------------------
      IF (TSCA.NE.'R' .AND. TSCA.NE.'C') THEN
C        -- ON NE TRAITE QUE LES CHAMPS R/C :
        IRET = 1
        GOTO 60

      ENDIF
C     TEST SUR IDENTITE DES 2 MAILLAGES
      CALL ASSERT(ZK24(JXXK1-1+1).EQ.MA1)

      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD),GD)
      IF (GD.EQ.0) CALL U2MESK('F','CALCULEL_67',1,NOMGD)


C------------------------------------------------------------------
C     4- ALLOCATION DE CNS2 :
C     ------------------------
      CALL DETRSD('CHAM_NO_S',CNS2)
      CALL CNSCRE(MA2,NOMGD,NCMP,ZK8(JCNS1C),BASE,CNS2)
      CALL JEVEUO(CNS2//'.CNSK','L',JCNS2K)
      CALL JEVEUO(CNS2//'.CNSD','L',JCNS2D)
      CALL JEVEUO(CNS2//'.CNSC','L',JCNS2C)
      CALL JEVEUO(CNS2//'.CNSV','E',JCNS2V)
      CALL JEVEUO(CNS2//'.CNSL','E',JCNS2L)

      NBNO2 = ZI(JCNS2D-1+1)

C------------------------------------------------------------------
C     5- CALCUL DES VALEURS DE CNS2 :
C     -------------------------------
      IDECAL = 0
      DO 50,INO2 = 1,NBNO2
        NBNO1 = ZI(IACONB-1+INO2)
        IF (NBNO1.EQ.0) GOTO 50
        DO 40,ICMP = 1,NCMP

C          -- ON COMPTE (ICO1) LES NOEUDS PORTANT LE DDL :
C             ON COMPTE AUSSI (ICO2) CEUX DONT LE COEF EST > 0
C             ON CALCULE LA VALEUR MOYENNE SUR LA MAILLE (VXMOY)
C             ON CALCULE LA SOMME DES COEF > 0 (COETOT)
          ICO1 = 0
          ICO2 = 0
          VRMOY = 0.D0
          VCMOY = DCMPLX(0.D0,0.D0)
          COETOT = 0.D0
          DO 10,INO1 = 1,NBNO1
            NUNO1 = ZI(IACONU+IDECAL-1+INO1)
            COEF1 = ZR(IACOCF+IDECAL-1+INO1)
            IF (ZL(JCNS1L-1+ (NUNO1-1)*NCMP+ICMP)) THEN
              ICO1 = ICO1 + 1
              IF (COEF1.GT.0.D0) THEN
                ICO2 = ICO2 + 1
                COETOT = COETOT + COEF1
              ENDIF
              IF (TSCA.EQ.'R') THEN
                VRMOY = VRMOY + ZR(JCNS1V-1+ (NUNO1-1)*NCMP+ICMP)

              ELSE
                VCMOY = VCMOY + ZC(JCNS1V-1+ (NUNO1-1)*NCMP+ICMP)
              ENDIF
            ENDIF
   10     CONTINUE
          IF (ICO1.EQ.0) GOTO 40
          ZL(JCNS2L-1+ (INO2-1)*NCMP+ICMP) = .TRUE.


C         -- SI COETOT EST FAIBLE, LA PROJECTION N'EST PAS PRECISE :
C            L'EMISSION DE L'ALARME EST COUTEUSE, ON LA LIMITE :
          IF (COETOT.LT.1.D-3.AND.KALARM.LE.6) THEN
            KALARM=KALARM+1
            CALL JENUNO(JEXNUM(MA2//'.NOMNOE',INO2),NOMNO2)
            NOMCMP=ZK8(JCNS1C-1+ICMP)
            VALK(1)=NOMGD
            VALK(2)=NOMNO2
            VALK(3)=NOMCMP
            CALL U2MESK('A','CALCULEL4_9',3,VALK)
          ENDIF


C          -- 3 CAS DE FIGURE POUR L'INTERPOLATION :
C          ----------------------------------------
          IF (ICO1.EQ.NBNO1) THEN
C            1 : NORMAL ON PREND TOUS LES NOEUDS N1
            LEXACT = .TRUE.
            COETOT = 1.D0

          ELSEIF (ICO2.GT.0) THEN
C            2 : ON PREND LES NOEUDS N1 DE COEF > 0
            LEXACT = .FALSE.

          ELSE
C            3 : ON FAIT UNE MOYENNE ARITHMETIQUE
            IF (TSCA.EQ.'R') THEN
              ZR(JCNS2V-1+ (INO2-1)*NCMP+ICMP) = VRMOY/ICO1

            ELSE
              ZC(JCNS2V-1+ (INO2-1)*NCMP+ICMP) = VCMOY/ICO1
            ENDIF
            GOTO 40

          ENDIF


          IF (TSCA.EQ.'R') THEN
            V2 = 0.D0
            DO 20,INO1 = 1,NBNO1
              NUNO1 = ZI(IACONU+IDECAL-1+INO1)
              COEF1 = ZR(IACOCF+IDECAL-1+INO1)
              IF (ZL(JCNS1L-1+ (NUNO1-1)*NCMP+ICMP)) THEN
                IF (LEXACT .OR. COEF1.GT.0) THEN
                  V1 = ZR(JCNS1V-1+ (NUNO1-1)*NCMP+ICMP)
                  V2 = V2 + COEF1*V1
                ENDIF
              ENDIF
   20       CONTINUE
            ZR(JCNS2V-1+ (INO2-1)*NCMP+ICMP) = V2/COETOT

          ELSEIF (TSCA.EQ.'C') THEN
            V2C = DCMPLX(0.D0,0.D0)
            DO 30,INO1 = 1,NBNO1
              NUNO1 = ZI(IACONU+IDECAL-1+INO1)
              COEF1 = ZR(IACOCF+IDECAL-1+INO1)
              IF (ZL(JCNS1L-1+ (NUNO1-1)*NCMP+ICMP)) THEN
                IF (LEXACT .OR. COEF1.GT.0) THEN
                  V1C = ZC(JCNS1V-1+ (NUNO1-1)*NCMP+ICMP)
                  V2C = V2C + COEF1*V1C
                ENDIF
              ENDIF
   30       CONTINUE
            ZC(JCNS2V-1+ (INO2-1)*NCMP+ICMP) = V2C/COETOT
          ENDIF
   40   CONTINUE
        IDECAL = IDECAL + NBNO1
   50 CONTINUE

   60 CONTINUE
      CALL JEDEMA()
      END

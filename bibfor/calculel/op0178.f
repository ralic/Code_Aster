      SUBROUTINE OP0178(IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C     COMMANDE:  ENGENDRE_TEST
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8,TYPTES
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*8 KBID
      CHARACTER*24 NOMFI,OBJ
      CHARACTER*10 FORMR,PRECI
      CHARACTER*100 FORM1
      CHARACTER*3 TYPE
      INTEGER RESUME,SOMMI,LONUTI,LONMAX
      LOGICAL ULEXIS
      REAL*8  SOMMR


      CALL JEMARQ()
      CALL INFMAJ()


C     -- RECUPERATION DES DONNEES :
C     ----------------------------
C
      IFIC  = 0
      NOMFI = ' '
      CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IFIC , N1 )
      IF ( .NOT. ULEXIS( IFIC ) ) THEN
         CALL ULOPEN ( IFIC, ' ', NOMFI, 'NEW', 'O' )
      ENDIF

      CALL GETVTX ( ' ', 'FORMAT_R' , 0,1,1, FORMR  ,IBID)
      CALL GETVTX ( ' ', 'PREC_R'   , 0,1,1, PRECI  ,IBID)
      CALL GETVTX ( ' ', 'TYPE_TEST', 0,1,1, TYPTES ,IBID)


C     -- CAS : TOUT:'OUI'
C    -----------------------------------------
      CALL GETVTX(' ','TOUT',0,1,1,KBID,N1)
      IF (N1.EQ.1) THEN
        CALL JELSTC('G',' ',0,0,KBID,NBVAL)
        NBOBJ = -NBVAL
        CALL WKVECT('&&OP0178.LISTE','V V K24',NBOBJ,IALIOB)
        CALL JELSTC('G',' ',0,NBOBJ,ZK24(IALIOB),NBVAL)

        DO 10 I = 1,NBOBJ
          OBJ = ZK24(IALIOB-1+I)
          IF (OBJ(1:1).EQ.'&') GO TO 10
          CALL TSTOBJ(OBJ,RESUME,SOMMI,SOMMR,LONUTI,LONMAX,TYPE,IRET)
          IF (IRET.EQ.0) THEN
C             -- TEST_RESU/RESUME:
            IF (TYPTES.EQ.'RESUME') THEN
              WRITE (IFIC,1001) OBJ,RESUME
            ELSE IF (TYPTES.EQ.'SOMME') THEN
C             -- TEST_RESU/S_I(OU S_R) :
              IF ((TYPE.EQ.'R') .OR. (TYPE.EQ.'C')) THEN
                FORM1 = '(''_F(NOM='''''',A24,'''''',S_R='','//FORMR//
     +              ','',PRECISION='//PRECI(1:LXLGUT(PRECI))//'),'')'
                WRITE (IFIC,FORM1) OBJ,SOMMR
              ELSE IF (TYPE.EQ.'I') THEN
                WRITE (IFIC,1003) OBJ,SOMMI
              END IF
            END IF
          END IF
   10   CONTINUE

      END IF


C     -- CAS : CO: L_CO
C    -----------------------------------------
      CALL GETVID(' ','CO',0,1,0,KBID,N1)
      IF (N1.LT.0) THEN
        NCO = -N1
        CALL WKVECT('&&OP0178.LCO','V V K8',NCO,IALICO)
        CALL GETVID(' ','CO',0,1,NCO,ZK8(IALICO),IBID)

        DO 30 ICO = 1,NCO
          CALL JELSTC('G',ZK8(IALICO-1+ICO),1,0,KBID,NBVAL)
          IF (NBVAL.EQ.0) GO TO 30
          NBOBJ = -NBVAL
          CALL WKVECT('&&OP0178.LISTE','V V K24',NBOBJ,IALIOB)
          CALL JELSTC('G',ZK8(IALICO-1+ICO),1,NBOBJ,ZK24(IALIOB),NBVAL)

          DO 20 I = 1,NBOBJ
            OBJ = ZK24(IALIOB-1+I)
            CALL TSTOBJ(OBJ,RESUME,SOMMI,SOMMR,LONUTI,LONMAX,TYPE,IRET)
            IF (IRET.EQ.0) THEN
C               -- TEST_RESU/RESUME:
              IF (TYPTES.EQ.'RESUME') THEN
                WRITE (IFIC,1001) OBJ,RESUME
              ELSE IF (TYPTES.EQ.'SOMME') THEN
C               -- TEST_RESU/S_I(OU S_R) :
                IF ((TYPE.EQ.'R') .OR. (TYPE.EQ.'C')) THEN
                  FORM1 = '(''_F(NOM='''''',A24,'''''',S_R='','//FORMR//
     +                ','',PRECISION='//PRECI(1:LXLGUT(PRECI))//'),'')'
                  WRITE (IFIC,FORM1) OBJ,SOMMR
                ELSE IF (TYPE.EQ.'I') THEN
                  WRITE (IFIC,1003) OBJ,SOMMI
                END IF
              END IF
            END IF
   20     CONTINUE

          CALL JEDETR('&&OP0178.LISTE')
   30   CONTINUE

      END IF


      CALL JEDEMA()

 1001 FORMAT ('_F(NOM=''',A24,''',RESUME =',I15,',PRECISION=0.,),')
 1003 FORMAT ('_F(NOM=''',A24,''',S_I=',I15,',PRECISION=0.,),')
      END

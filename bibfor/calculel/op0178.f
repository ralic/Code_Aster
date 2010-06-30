      SUBROUTINE OP0178()
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER        RESUME, SOMMI, LONUTI, LONMAX, NI
      REAL*8         SOMMR
      LOGICAL        ULEXIS
      CHARACTER*3    TYPE
      CHARACTER*8    KBID, FORMAT, TYPTES
      CHARACTER*10   FORMR,PRECI
      CHARACTER*19   NOMSD
      CHARACTER*24   NOMFI,OBJ
      CHARACTER*100  FORM1


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
C
      FORMAT = 'ASTER'
      CALL GETVTX ( ' ', 'FORMAT'   , 0,1,1, FORMAT, IBID )
C
      CALL GETVTX ( ' ', 'FORMAT_R' , 0,1,1, FORMR  ,IBID )
      CALL GETVTX ( ' ', 'PREC_R'   , 0,1,1, PRECI  ,IBID )
      CALL GETVTX ( ' ', 'TYPE_TEST', 0,1,1, TYPTES ,IBID )
C
C ---- FORMAT TEST_RESU / STANDARD
C
      IF ( FORMAT .EQ. 'ASTER' ) THEN
         CALL GETVID ( ' ', 'CO', 1,1,0, KBID, N1 )
         NCO =-N1
         CALL WKVECT ( '&&OP0178.LCO', 'V V K8', NCO, IALICO )
         CALL GETVID ( ' ', 'CO', 1,1,NCO, ZK8(IALICO), IBID )

         DO 100 ICO = 1,NCO
            NOMSD = ZK8(IALICO+ICO-1)(1:8)//'           '
C
            CALL EXISD ( 'RESULTAT', NOMSD, IRET )
            IF ( IRET .EQ. 1 ) THEN
               CALL ENGTRS ( IFIC, NOMSD, TYPTES, PRECI, FORMR )
               GOTO 100
            ENDIF
C
            CALL EXISD ( 'TABLE', NOMSD, IRET )
            IF ( IRET .EQ. 1 ) THEN
               CALL ENGTTB ( IFIC, NOMSD, TYPTES, PRECI, FORMR )
               GOTO 100
            ENDIF
C
            CALL EXISD ( 'CHAM_ELEM', NOMSD, IRET )
            IF ( IRET .EQ. 1 ) THEN
               CALL ENGTCE ( IFIC, NOMSD, TYPTES, PRECI, FORMR )
               GOTO 100
            ENDIF
C
            CALL EXISD ( 'CHAM_NO', NOMSD, IRET )
            IF ( IRET .EQ. 1 ) THEN
               CALL ENGTCN ( IFIC, NOMSD, TYPTES, PRECI, FORMR )
               GOTO 100
            ENDIF
 100     CONTINUE
         CALL JEDETR ( '&&OP0178.LCO' )
C
C ---- FORMAT TEST_RESU / OBJET
C
      ELSE

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
          CALL TSTOBJ(OBJ,'NON',RESUME,SOMMI,SOMMR,LONUTI,LONMAX,TYPE,
     &                IRET,NI)
          IF (IRET.EQ.0) THEN
C             -- TEST_RESU/S_I(OU S_R) :
              IF ((TYPE.EQ.'R') .OR. (TYPE.EQ.'C')) THEN
                FORM1 = '(''_F(NOM='''''',A24,'''''',S_R='','//FORMR//
     +              ','',PRECISION='//PRECI(1:LXLGUT(PRECI))//'),'')'
                WRITE (IFIC,FORM1) OBJ,SOMMR
              ELSE IF (TYPE.EQ.'I') THEN
                WRITE (IFIC,1003) OBJ,SOMMI
              END IF
          END IF
   10   CONTINUE
      END IF
C
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
            CALL TSTOBJ(OBJ,'NON',RESUME,SOMMI,SOMMR,LONUTI,LONMAX,TYPE,
     &                  IRET,NI)
            IF (IRET.EQ.0) THEN
C               -- TEST_RESU/S_I(OU S_R) :
                IF ((TYPE.EQ.'R') .OR. (TYPE.EQ.'C')) THEN
                  FORM1 = '(''_F(NOM='''''',A24,'''''',S_R='','//FORMR//
     +                ','',PRECISION='//PRECI(1:LXLGUT(PRECI))//'),'')'
                  WRITE (IFIC,FORM1) OBJ,SOMMR
                ELSE IF (TYPE.EQ.'I') THEN
                  WRITE (IFIC,1003) OBJ,SOMMI
                END IF
            END IF
   20     CONTINUE
          CALL JEDETR('&&OP0178.LISTE')
   30   CONTINUE
      END IF
      ENDIF
C
 9999 CONTINUE
C
      CALL JEDEMA()
C
 1003 FORMAT ('_F(NOM=''',A24,''',S_I=',I15,',PRECISION=0.,),')
C
      END

      SUBROUTINE RSUTNU(RESU,MOTCLE,IOCC,KNUM,NBORDR,PREC,CRIT,IER)
      IMPLICIT   NONE
      INTEGER IOCC,NBORDR,IER
      REAL*8 PREC
      CHARACTER*(*) RESU,MOTCLE,KNUM,CRIT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE VABHHTS J.PELLET
C        RECUPERATION DES NUMEROS D'ORDRE DANS UNE STRUCTURE DE DONNEES
C     DE TYPE "RESULTAT" A PARTIR DES VARIABLES D'ACCES UTILISATEUR
C     LES ACCES : NUME_ORDRE
C                 FREQ, INST, NOEUD_CMP, ...
C                 TOUT_ORDRE (PAR DEFAUT)
C     ------------------------------------------------------------------
C IN  : RESU   : NOM DE LA STRUCTURE DE DONNEES
C IN  : MOTCLE : NOM DU MOT CLE FACTEUR
C IN  : IOCC   : NUMERO D'OCCURENCE
C IN  : KNUM   : NOM JEVEUX DU VECTEUR ZI POUR ECRIRE LA LISTE DES NUME
C OUT : NBORDR : NOMBRE DE NUMERO D'ORDRE
C IN  : PREC   : PRECISION DEMANDEE
C IN  : CRIT   : CRITERE DEMANDE
C OUT : IER    : CODE RETOUR, = 0 : OK
C     ------------------------------------------------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER IBID,N1,N2,NBACC,IRET,JPARA,IACC,IAD,IUT,LXLGUT,NBVAL
      INTEGER VALI(4)
      INTEGER JVAL,NBVA2,JNCH,II,IVAL,NBTROU,LG,LACCR
      INTEGER IORD,JORDR,JORD1,JORD2,NBORDT,NBINST,NBFREQ
      INTEGER NBTROP,INDI,JORDR3 ,LONG1,JORDR1,JORDR2,ITROU,I,INDIIS
      REAL*8 R8B
      REAL*8 VALR
      CHARACTER*4 CTYP
      CHARACTER*24 VALK(2)
      CHARACTER*8 K8B
      CHARACTER*16 CONCEP,NOMCMD,NOMACC
      CHARACTER*19 KNACC,KVACC,KNMOD,LISTR,RESUIN,KNUM2
      COMPLEX*16 C16B
      CHARACTER*8 K8BID
      LOGICAL LTOUT,LINST,LFREQ,LORDR,GETEXM
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(K8B,CONCEP,NOMCMD)
      KNACC = '&&RSUTNU.NOM_ACCES '
      KVACC = '&&RSUTNU.VALE_ACCES'
      KNMOD = '&&RSUTNU.NOEUD_CMP '
      NBORDR = 0
      IER = 0

      RESUIN = RESU
       CALL JELIRA(RESUIN//'.ORDR','LONUTI',IRET,K8B)
      IF (IRET.EQ.0) THEN
        IER = 10
        GO TO 100
      END IF

C     --- CAS "NUME_ORDRE" ---
      CALL GETVIS(MOTCLE,'NUME_ORDRE',IOCC,1,0,IBID,N1)
      IF (N1.NE.0) THEN
        NBORDR = -N1
        CALL WKVECT(KNUM,'V V I',NBORDR,JORDR)
        CALL GETVIS(MOTCLE,'NUME_ORDRE',IOCC,1,NBORDR,ZI(JORDR),N1)
        GO TO 100
      END IF

C     --- CAS "NUME_MODE","INST","FREQ", ... ---
      N2 = 0
      CALL RSNOPA(RESU,0,KNACC,NBACC,IBID)
      CALL JEEXIN(KNACC,IRET)
      IF (IRET.GT.0) CALL JEVEUO(KNACC,'L',JPARA)
      IF (NBACC.NE.0) THEN
        CALL RSORAC(RESU,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,IORD,1,
     &              IBID)
        DO 40 IACC = 1,NBACC
          IF (.NOT.GETEXM(MOTCLE,ZK16(JPARA-1+IACC))) GOTO 40

          CTYP = '    '
          CALL RSADPA(RESU,'L',1,ZK16(JPARA-1+IACC),IORD,1,IAD,CTYP)
          IF (CTYP(1:1).EQ.'I') THEN
            CALL GETVIS(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,0,IBID,N2)
          ELSE IF (CTYP(1:1).EQ.'R') THEN
            CALL GETVR8(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,0,R8B,N2)
          ELSE IF (CTYP(1:1).EQ.'K') THEN
            CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,0,K8BID,N2)
            IF (ZK16(JPARA-1+IACC) (1:9).EQ.'NOEUD_CMP') N2 = N2/2
          END IF

          IF (N2.NE.0) THEN
            CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,NBORDT,
     &                  1,IBID)
            CALL WKVECT('&&RSUTNU.N1','V V I',NBORDT,JORD1)
            CALL WKVECT('&&RSUTNU.N2','V V I',NBORDT,JORD2)
            NBVAL = -N2
            IUT = LXLGUT(CTYP)
            CALL WKVECT(KVACC,'V V '//CTYP(1:IUT),NBVAL,JVAL)
            IF (CTYP(1:1).EQ.'I') THEN
              CALL GETVIS(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                    ZI(JVAL),N2)
            ELSE IF (CTYP(1:1).EQ.'R') THEN
              CALL GETVR8(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                    ZR(JVAL),N2)
            ELSE IF (CTYP(1:2).EQ.'K8') THEN
              CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                    ZK8(JVAL),N2)
            ELSE IF (CTYP(1:3).EQ.'K16') THEN
              IF (ZK16(JPARA-1+IACC) (1:9).EQ.'NOEUD_CMP') THEN
                NBVA2 = 2*NBVAL
                CALL WKVECT(KNMOD,'V V K8',NBVA2,JNCH)
                CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVA2,
     &                      ZK8(JNCH),N2)
                DO 10 II = 1,NBVAL
                  ZK16(JVAL+II-1) = ZK8(JNCH+ (2*II-1)-1)//
     &                              ZK8(JNCH+ (2*II)-1)
   10           CONTINUE
                CALL JEDETR(KNMOD)
              ELSE
                CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                      ZK16(JVAL),N2)
              END IF
            ELSE IF (CTYP(1:3).EQ.'K24') THEN
              CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                    ZK24(JVAL),N2)
            ELSE IF (CTYP(1:3).EQ.'K32') THEN
              CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                    ZK32(JVAL),N2)
            ELSE IF (CTYP(1:3).EQ.'K80') THEN
              CALL GETVTX(MOTCLE,ZK16(JPARA-1+IACC),IOCC,1,NBVAL,
     &                    ZK80(JVAL),N2)
            END IF
            NBORDR = 1
            DO 20 IVAL = 1,NBVAL
              IF (CTYP(1:1).EQ.'I') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),ZI(JVAL-1+IVAL),R8B,
     &                      K8B,C16B,PREC,CRIT,ZI(JORD2),NBORDT,NBTROU)
              ELSE IF (CTYP(1:1).EQ.'R') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),IBID,
     &                      ZR(JVAL-1+IVAL),K8B,C16B,PREC,CRIT,
     &                      ZI(JORD2),NBORDT,NBTROU)
              ELSE IF (CTYP(1:2).EQ.'K8') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),IBID,R8B,
     &                      ZK8(JVAL-1+IVAL),C16B,PREC,CRIT,ZI(JORD2),
     &                      NBORDT,NBTROU)
              ELSE IF (CTYP(1:3).EQ.'K16') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),IBID,R8B,
     &                      ZK16(JVAL-1+IVAL),C16B,PREC,CRIT,ZI(JORD2),
     &                      NBORDT,NBTROU)
              ELSE IF (CTYP(1:3).EQ.'K24') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),IBID,R8B,
     &                      ZK24(JVAL-1+IVAL),C16B,PREC,CRIT,ZI(JORD2),
     &                      NBORDT,NBTROU)
              ELSE IF (CTYP(1:3).EQ.'K32') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),IBID,R8B,
     &                      ZK32(JVAL-1+IVAL),C16B,PREC,CRIT,ZI(JORD2),
     &                      NBORDT,NBTROU)
              ELSE IF (CTYP(1:3).EQ.'K80') THEN
                CALL RSORAC(RESU,ZK16(JPARA-1+IACC),IBID,R8B,
     &                      ZK80(JVAL-1+IVAL),C16B,PREC,CRIT,ZI(JORD2),
     &                      NBORDT,NBTROU)
              END IF
              IF (NBTROU.EQ.1) THEN
                CALL I2TRGI(ZI(JORD1),ZI(JORD2),NBTROU,NBORDR)
              ELSE IF (NBTROU.GT.1) THEN
                VALK (1) = RESU
                CALL U2MESG('A+','UTILITAI8_38',1,VALK,0,0,0,0.D0)
                LG = MAX(1,LXLGUT(ZK16(JPARA-1+IACC)))
                IF (CTYP(1:1).EQ.'I') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALI (1) = ZI(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_39',1,VALK,1,VALI,0,0.D0)
                ELSE IF (CTYP(1:1).EQ.'R') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALR = ZR(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_40',1,VALK,0,0,1,VALR)
                ELSE IF (CTYP(1:2).EQ.'K8') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK8(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',2,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K16') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK16(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',2,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K24') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK24(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',2,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K32') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK32(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',2,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K80') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK80(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',2,VALK,0,0,0,0.D0)
                END IF
                VALI (1) = NBTROU
                VALI (2) = ZI(JORD2)
                VALI (3) = ZI(JORD2+1)
                VALI (4) = ZI(JORD2+2)
                CALL U2MESG('A','UTILITAI8_46',0,' ',4,VALI,0,0.D0)
                CALL I2TRGI(ZI(JORD1),ZI(JORD2),NBTROU,NBORDR)
              ELSE IF (NBTROU.EQ.0) THEN
                VALK (1) = RESU
                CALL U2MESG('A+','UTILITAI8_47',1,VALK,0,0,0,0.D0)
                LG = MAX(1,LXLGUT(ZK16(JPARA-1+IACC)))
                IF (CTYP(1:1).EQ.'I') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALI (1) = ZI(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_39',0,' ',1,VALI,0,0.D0)
                ELSE IF (CTYP(1:1).EQ.'R') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALR = ZR(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_40',0,' ',0,0,1,VALR)
                ELSE IF (CTYP(1:2).EQ.'K8') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK8(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',1,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K16') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK16(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',1,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K24') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK24(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',1,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:3).EQ.'K32') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK32(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',1,VALK,0,0,0,0.D0)
                ELSE IF (CTYP(1:1).EQ.'K80') THEN
                  VALK (1) = ZK16(JPARA-1+IACC) (1:LG)
                  VALK (2) = ZK80(JVAL-1+IVAL)
                  CALL U2MESG('A+','UTILITAI8_41',1,VALK,0,0,0,0.D0)
                END IF
                CALL U2MESG('A','VIDE_1',0,' ',0,0,0,0.D0)
                IER = IER + 10
              ELSE IF (NBTROU.LT.0) THEN
                CALL U2MESS('F','DVP_1')
              END IF
   20       CONTINUE
            NBORDR = NBORDR - 1
            IF (NBORDR.NE.0) THEN
              CALL WKVECT(KNUM,'V V I',NBORDR,JORDR)
              DO 30 IORD = 0,NBORDR - 1
                ZI(JORDR+IORD) = ZI(JORD1+IORD)
   30         CONTINUE
            END IF
            CALL JEDETR('&&RSUTNU.N1')
            CALL JEDETR('&&RSUTNU.N2')
            GO TO 100
          END IF
   40   CONTINUE
      END IF

      LINST = GETEXM(MOTCLE,'LIST_INST')
      IF (LINST) THEN
        CALL GETVID(MOTCLE,'LIST_INST',IOCC,1,1,LISTR,N1)
        IF (N1.NE.0) THEN
          CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,NBORDT,1,
     &                IBID)
          NOMACC = 'INST'
          CALL JEVEUO(LISTR//'.VALE','L',LACCR)
          CALL JELIRA(LISTR//'.VALE','LONMAX',NBINST,K8B)
          CALL WKVECT('&&RSUTNU.N1','V V I',NBORDT,JORD1)
          CALL WKVECT('&&RSUTNU.N2','V V I',NBORDT,JORD2)
          NBORDR = 1
          DO 50 IORD = 0,NBINST - 1
            CALL RSORAC(RESU,NOMACC,IBID,ZR(LACCR+IORD),K8B,C16B,PREC,
     &                  CRIT,ZI(JORD2),NBORDT,NBTROU)
            IF (NBTROU.EQ.0) THEN
              IER = IER + 1
              VALK (1)= NOMACC
              VALR = ZR(LACCR+IORD)
              CALL U2MESG('A','UTILITAI8_56',1,VALK,0,0,1,VALR)
            ELSE IF (NBTROU.LT.0) THEN
              CALL U2MESS('F','DVP_1')
            ELSE
              IF (NBTROU.GT.1) THEN
                VALK (1) = RESU
                VALR = ZR(LACCR+IORD)
                VALI (1) = NBTROU
                CALL U2MESG('A','UTILITAI8_57',1,VALK,1,VALI,1,VALR)
              END IF
              CALL I2TRGI(ZI(JORD1),ZI(JORD2),NBTROU,NBORDR)
            END IF
   50     CONTINUE
          NBORDR = NBORDR - 1
          IF (NBORDR.NE.0) THEN
            CALL WKVECT(KNUM,'V V I',NBORDR,JORDR)
            DO 60 IORD = 0,NBORDR - 1
              ZI(JORDR+IORD) = ZI(JORD1+IORD)
   60       CONTINUE
          END IF
          CALL JEDETR('&&RSUTNU.N1')
          CALL JEDETR('&&RSUTNU.N2')
          GO TO 100
        END IF
      END IF

      LFREQ = GETEXM(MOTCLE,'LIST_FREQ')
      IF (LFREQ) THEN
        CALL GETVID(MOTCLE,'LIST_FREQ',IOCC,1,1,LISTR,N1)
        IF (N1.NE.0) THEN
          CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,NBORDT,1,
     &                IBID)
          NOMACC = 'FREQ'
          CALL JEVEUO(LISTR//'.VALE','L',LACCR)
          CALL JELIRA(LISTR//'.VALE','LONMAX',NBFREQ,K8B)
          CALL WKVECT('&&RSUTNU.N1','V V I',NBORDT,JORD1)
          CALL WKVECT('&&RSUTNU.N2','V V I',NBORDT,JORD2)
          NBORDR = 1
          DO 70 IORD = 0,NBFREQ - 1
            CALL RSORAC(RESU,NOMACC,IBID,ZR(LACCR+IORD),K8B,C16B,PREC,
     &                  CRIT,ZI(JORD2),NBORDT,NBTROU)
            IF (NBTROU.EQ.0) THEN
              IER = IER + 1
              VALK (1) = NOMACC
              VALR = ZR(LACCR+IORD)
              CALL U2MESG('A','UTILITAI8_58',1,VALK,0,0,1,VALR)
            ELSE IF (NBTROU.LT.0) THEN
              CALL U2MESS('F','DVP_1')
            ELSE
              IF (NBTROU.GT.1) THEN
                VALK (1) = RESU
                VALR = ZR(LACCR+IORD)
                VALI (1) = NBTROU
                CALL U2MESG('A','UTILITAI8_59',1,VALK,1,VALI,1,VALR)
              END IF
              CALL I2TRGI(ZI(JORD1),ZI(JORD2),NBTROU,NBORDR)
            END IF
   70     CONTINUE
          NBORDR = NBORDR - 1
          IF (NBORDR.NE.0) THEN
            CALL WKVECT(KNUM,'V V I',NBORDR,JORDR)
            DO 80 IORD = 0,NBORDR - 1
              ZI(JORDR+IORD) = ZI(JORD1+IORD)
   80       CONTINUE
          END IF
          CALL JEDETR('&&RSUTNU.N1')
          CALL JEDETR('&&RSUTNU.N2')
          GO TO 100
        END IF
      END IF

      LORDR = GETEXM(MOTCLE,'LIST_ORDRE')
      IF (LORDR) THEN
        CALL GETVID(MOTCLE,'LIST_ORDRE',IOCC,1,1,LISTR,N1)
        IF (N1.NE.0) THEN
          CALL JEVEUO(LISTR//'.VALE','L',LACCR)
          CALL JELIRA(LISTR//'.VALE','LONMAX',NBORDR,K8B)
          CALL WKVECT(KNUM,'V V I',NBORDR,JORDR)
          DO 90 IORD = 0,NBORDR - 1
            ZI(JORDR+IORD) = ZI(LACCR+IORD)
   90     CONTINUE
          GO TO 100
        END IF
      END IF

C     --- LE DERNIER: 'TOUT_ORDRE' VALEUR PAR DEFAUT ---

      LTOUT = GETEXM(MOTCLE,'TOUT_ORDRE')
      IF (LTOUT) THEN
        CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,NBORDR,1,
     &              IBID)
        CALL WKVECT(KNUM,'V V I',NBORDR,JORDR)
        CALL RSORAC(RESU,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &              ZI(JORDR),NBORDR,IBID)
      END IF

  100 CONTINUE


C     9- ON VERIFIE QUE LES NUMEROS D'ORDRE TROUVES APPARTIENNENT
C        BIEN A RESU ; SINON ON LES RETIRE DE LA LISTE :
C     ------------------------------------------------------------
      IF (NBORDR.GT.0) THEN
         CALL JELIRA(RESUIN//'.ORDR','LONUTI',LONG1,K8B)
         CALL JEVEUO(RESUIN//'.ORDR','L',JORDR1)
         CALL JEVEUO(KNUM,'L',JORDR2)
         NBTROP=0
         DO 777,I=1,NBORDR
            ITROU= INDIIS(ZI(JORDR1),ZI(JORDR2-1+I),1,LONG1)
            IF (ITROU.EQ.0) NBTROP=NBTROP+1
777      CONTINUE

         IF (NBTROP.GT.0) THEN
           KNUM2='&&RSUTNU.KNUM2'
           IF ((NBORDR-NBTROP).EQ.0) CALL U2MESS('F','UTILITAI4_53')
           CALL WKVECT(KNUM2,'V V I',NBORDR-NBTROP,JORDR3)
           INDI=0
           DO 778,I=1,NBORDR
             ITROU= INDIIS(ZI(JORDR1),ZI(JORDR2-1+I),1,LONG1)
             IF (ITROU.GT.0) THEN
               INDI=INDI+1
               ZI(JORDR3-1+INDI)= ZI(JORDR2-1+I)
             END IF
778        CONTINUE
           CALL JEDETR(KNUM)
           CALL JEDUPO(KNUM2, 'V', KNUM, .FALSE.)
           CALL JEDETR(KNUM2)
           NBORDR=INDI
         END IF


      END IF




      CALL JEDETR('&&RSUTNU.NOM_ACCES')
      CALL JEDETR('&&RSUTNU.VALE_ACCES')
      CALL JEDETR('&&RSUTNU.NOEUD_CMP')
      CALL JEDETR('&&RSUTNU.DOUBLE')

      CALL JEDEMA()
      END

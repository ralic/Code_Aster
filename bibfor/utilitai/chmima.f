      SUBROUTINE CHMIMA(NOMSD,NOMSY,TYPMAX,NOCHAM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER                       NBORDR
      CHARACTER*(*)     NOMSD,NOMSY, TYPMAX,NOCHAM
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C      AFFECTATION DU CHAMP-GD DE NOM NOCHAM  AVEC LES
C      VALEURS MINMAX EN TOUT POINT DES CHAMPS-GD DE TYPE
C      NOMSY DU RESULTAT DE NOM NOMSD
C ----------------------------------------------------------------------
C IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
C IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP A CHERCHER.
C IN  : TYPMAX : TYPE D'OPERATION A EFFECTUER
C VAR : NOCHAM : NOM DU CHAMP CONTENANT LES MINMAX DES
C                CHAMPS DE TYPE NOMSY DU RESULTAT NOMSD.
C
C ----------------------------------------------------------------------
      CHARACTER*4  CTYP
      CHARACTER*8  TYPMA, K8B, CRIT, NOMA, NOMN, VALEUR
      CHARACTER*19 PRNO, PRN2
      CHARACTER*16 NOMS2
      CHARACTER*19  NOCHA2, CHEXTR, KNUM
      CHARACTER*24 NOMNOE
      CHARACTER*5 SUFV
      INTEGER      IARG
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IAD ,IB ,IE ,IN ,INOE ,INUMER
      INTEGER IRET ,IVALE ,J ,JDDLX ,JDDLY ,JDDLZ ,JDLRX
      INTEGER JDLRY ,JDLRZ ,JORDR ,JVPNT ,N2 ,NBNOE ,NC
      INTEGER NEQ ,NP ,NVALE
      REAL*8 EPSI ,RS1 ,X ,Y ,Z
C-----------------------------------------------------------------------
      CALL JEMARQ()
      KNUM   = '&&CHMIMA.NUME_ORDRE'
      NOMS2  = NOMSY
      NOCHA2 = NOCHAM
      TYPMA  = TYPMAX
C
C     --- LECTURE DU MOT-CLE TYPE_RESU ---
C
      CALL GETVTX(' ','TYPE_RESU'  ,1,IARG,1,VALEUR,N2)
C
C     --- RECUPERATION DES NUMEROS D'ORDRE ---
C
      CALL GETVR8(' ','PRECISION',1,IARG,1,EPSI,NP)
      CALL GETVTX(' ','CRITERE'  ,1,IARG,1,CRIT,NC)
C
      CALL RSUTNU ( NOMSD, ' ', 0, KNUM, NBORDR, EPSI, CRIT, IRET )
      IF ( NBORDR .EQ. 0 ) THEN
          CALL U2MESS('F','UTILITAI_23')
      ENDIF
      CALL JEVEUO ( KNUM, 'L', JORDR )
C
C     --- TRAITEMENT DU PREMIER NUMERO D'ORDRE ---
C
      CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR),CHEXTR,IRET)
C
C      --- INITIALISATION DE NOCHAM AVEC CHEXTR ---
      CALL COPISD('CHAMP_GD','G',CHEXTR(1:19),NOCHA2(1:19))
C
C     --- RECUPERATION DES VALEURS DU CHAMP-GD ---
      CALL JEEXIN(NOCHA2(1:19)//'.VALE',IRET)
      IF (IRET.GT.0) THEN
        SUFV= '.VALE'
      ELSE
        SUFV= '.CELV'
      END IF
      CALL JELIRA(NOCHA2(1:19)//SUFV,'LONMAX',NEQ,K8B)
      CALL JEVEUO(NOCHA2(1:19)//SUFV,'E',NVALE)
C
      CALL WKVECT('&&CHMIMA.INST','V V I',NEQ,INUMER)
      DO 1 I =1,NEQ
         ZI(INUMER+I-1) = ZI(JORDR)
   1  CONTINUE
C
C     --- BOUCLE SUR LES NUMEROS D'ORDRE ---
C
       IF (TYPMA.EQ.'MAXI    ') THEN
C
          DO 10 I=2,NBORDR
C
C         - RECUPERATION DU CHAMP DE TYPE NOMSY
C           CORRESPONDANT AU NUMERO D'ORDRE COURANT
C
            CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR+I-1),CHEXTR,IRET)
C
C         - RECUPERATION DU VALE DU CHAMP EXTRAIT
C
            CALL JEVEUO(CHEXTR//SUFV,'L',IVALE)
C
            DO 12 J =1,NEQ
               IF (ZR(IVALE+J-1).GT.ZR(NVALE+J-1)) THEN
                   ZR(NVALE+J-1) = ZR(IVALE+J-1)
                   ZI(INUMER+J-1) = ZI(JORDR+I-1)
               ENDIF
   12       CONTINUE
C
   10     CONTINUE
C
       ELSEIF (TYPMA.EQ.'MAXI_ABS') THEN
C
          DO 20 I=2,NBORDR
C
C         - RECUPERATION DU CHAMP DE TYPE NOMSY
C           CORRESPONDANT AU NUMERO D'ORDRE COURANT
C
            CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR+I-1),CHEXTR,IRET)
C
C         - RECUPERATION DU VALE DU CHAMP EXTRAIT
C
            CALL JEVEUO(CHEXTR//SUFV,'L',IVALE)
C
            DO 22 J =1,NEQ
C
               IF (ABS(ZR(IVALE+J-1)).GT.ABS(ZR(NVALE+J-1))) THEN
                   ZR(NVALE+J-1) = ZR(IVALE+J-1)
                   ZI(INUMER+J-1) = ZI(JORDR+I-1)
               ENDIF
   22       CONTINUE
C
   20     CONTINUE
C
       ELSEIF (TYPMA.EQ.'MINI    ') THEN
C
          DO 30 I=2,NBORDR
C
C         - RECUPERATION DU CHAMP DE TYPE NOMSY
C           CORRESPONDANT AU NUMERO D'ORDRE COURANT
C
            CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR+I-1),CHEXTR,IRET)
C
C         - RECUPERATION DU VALE DU CHAMP EXTRAIT
C
            CALL JEVEUO(CHEXTR//SUFV,'L',IVALE)
C
            DO 32 J =1,NEQ
C
               IF (ZR(IVALE+J-1).LT.ZR(NVALE+J-1)) THEN
                   ZR(NVALE+J-1) = ZR(IVALE+J-1)
                   ZI(INUMER+J-1) = ZI(JORDR+I-1)
               ENDIF
   32       CONTINUE
C
   30     CONTINUE
C
       ELSEIF (TYPMA.EQ.'MINI_ABS') THEN
C
          DO 40 I=2,NBORDR
C
C         - RECUPERATION DU CHAMP DE TYPE NOMSY
C           CORRESPONDANT AU NUMERO D'ORDRE COURANT
C
            CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR+I-1),CHEXTR,IRET)
C
C         - RECUPERATION DU VALE DU CHAMP EXTRAIT
C
            CALL JEVEUO(CHEXTR//SUFV,'L',IVALE)
C
            DO 42 J =1,NEQ
C
               IF (ABS(ZR(IVALE+J-1)).LT.ABS(ZR(NVALE+J-1))) THEN
                   ZR(NVALE+J-1) = ZR(IVALE+J-1)
                   ZI(INUMER+J-1) = ZI(JORDR+I-1)
               ENDIF
   42       CONTINUE
C
   40     CONTINUE
C
       ELSEIF (TYPMA.EQ.'NORM_TRA') THEN
          CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR),CHEXTR,IRET)
          CALL JEVEUO(CHEXTR//'.VALE','L',IVALE)
          CALL DISMOI('F','PROF_CHNO',CHEXTR,'CHAM_NO' ,IB,PRNO,IE)
          CALL DISMOI('F','NOM_MAILLA'   ,CHEXTR,'CHAM_NO' ,IB,NOMA,IE)
          CALL DISMOI('F','NB_NO_MAILLA',NOMA ,'MAILLAGE',NBNOE,K8B,IE)
          NOMNOE = NOMA//'.NOMNOE'
C
          DO 56 J = 0 , NEQ-1
             ZR(NVALE+J) = ZR(IVALE+J)
 56       CONTINUE
          IF ( NBORDR .EQ. 1 ) GOTO 58
C
          CALL WKVECT('&&CHMIMA.DDL.DX' ,'V V I',NBNOE,JDDLX)
          CALL WKVECT('&&CHMIMA.DDL.DY' ,'V V I',NBNOE,JDDLY)
          CALL WKVECT('&&CHMIMA.DDL.DZ' ,'V V I',NBNOE,JDDLZ)
          CALL WKVECT('&&CHMIMA.DDL.DRX','V V I',NBNOE,JDLRX)
          CALL WKVECT('&&CHMIMA.DDL.DRY','V V I',NBNOE,JDLRY)
          CALL WKVECT('&&CHMIMA.DDL.DRZ','V V I',NBNOE,JDLRZ)
          CALL WKVECT('&&CHMIMA.VALE_P.NT','V V R',NBNOE,JVPNT)
C
          DO 50 IN = 0 , NBNOE-1
             CALL JENUNO(JEXNUM(NOMNOE,IN+1),NOMN)
             CALL POSDDL ( 'CHAM_NO', CHEXTR, NOMN, 'DX', INOE,
     &                     ZI(JDDLX+IN) )
             CALL POSDDL ( 'CHAM_NO', CHEXTR, NOMN, 'DY', INOE,
     &                     ZI(JDDLY+IN) )
             CALL POSDDL ( 'CHAM_NO', CHEXTR, NOMN, 'DZ', INOE,
     &                     ZI(JDDLZ+IN) )
             CALL POSDDL ( 'CHAM_NO', CHEXTR, NOMN, 'DRX', INOE,
     &                     ZI(JDLRX+IN) )
             CALL POSDDL ( 'CHAM_NO', CHEXTR, NOMN, 'DRY', INOE,
     &                     ZI(JDLRY+IN) )
             CALL POSDDL ( 'CHAM_NO', CHEXTR, NOMN, 'DRZ', INOE,
     &                     ZI(JDLRZ+IN) )
             X = ZR(IVALE+ZI(JDDLX+IN)-1)
             Y = ZR(IVALE+ZI(JDDLY+IN)-1)
             IF ( ZI(JDDLZ+IN) .NE. 0 ) THEN
                Z = ZR(IVALE+ZI(JDDLZ+IN)-1)
             ELSE
                Z = 0.D0
             ENDIF
             ZR(JVPNT+IN) = SQRT( X**2 + Y**2 + Z**2 )
             ZR(NVALE+ZI(JDDLX+IN)-1) = X
             ZR(NVALE+ZI(JDDLY+IN)-1) = Y
             IF ( ZI(JDDLZ+IN) .NE. 0 )
     &          ZR(NVALE+ZI(JDDLZ+IN)-1) = Z
             ZI(INUMER+ZI(JDDLX+IN)-1) = ZI(JORDR)
             ZI(INUMER+ZI(JDDLY+IN)-1) = ZI(JORDR)
             ZI(INUMER+ZI(JDDLZ+IN)-1) = ZI(JORDR)
             IF ( ZI(JDLRX+IN) .NE. 0 )
     &            ZR(NVALE+ZI(JDLRX+IN)-1) = ZR(IVALE+ZI(JDLRX+IN)-1)
             IF ( ZI(JDLRY+IN) .NE. 0 )
     &            ZR(NVALE+ZI(JDLRY+IN)-1) = ZR(IVALE+ZI(JDLRY+IN)-1)
             IF ( ZI(JDLRZ+IN) .NE. 0 )
     &            ZR(NVALE+ZI(JDLRZ+IN)-1) = ZR(IVALE+ZI(JDLRZ+IN)-1)
 50       CONTINUE
C
          DO 52 I = 2 , NBORDR
            CALL RSEXCH('F',NOMSD,NOMS2,ZI(JORDR+I-1),CHEXTR,IRET)
            CALL DISMOI('F','PROF_CHNO',CHEXTR,'CHAM_NO',IB,PRN2,IE)
            IF ( PRN2 .NE. PRNO ) THEN
              CALL U2MESS('F','UTILITAI_26')
            ENDIF
            CALL JEVEUO(CHEXTR//'.VALE','L',IVALE)
C
            DO 54 IN = 0 , NBNOE-1
               X = ZR(IVALE+ZI(JDDLX+IN)-1)
               Y = ZR(IVALE+ZI(JDDLY+IN)-1)
               IF ( ZI(JDDLZ+IN) .NE. 0 ) THEN
                  Z = ZR(IVALE+ZI(JDDLZ+IN)-1)
               ELSE
                  Z = 0.D0
               ENDIF
               RS1 = SQRT( X**2 + Y**2 + Z**2 )
               IF ( RS1 .GT. ZR(JVPNT+IN) ) THEN
                  ZR(JVPNT+IN) = RS1
                  ZI(INUMER+ZI(JDDLX+IN)-1) = ZI(JORDR+I-1)
                  ZI(INUMER+ZI(JDDLY+IN)-1) = ZI(JORDR+I-1)
                  ZI(INUMER+ZI(JDDLZ+IN)-1) = ZI(JORDR+I-1)
                  ZR(NVALE+ZI(JDDLX+IN)-1) = X
                  ZR(NVALE+ZI(JDDLY+IN)-1) = Y
                  IF ( ZI(JDDLZ+IN) .NE. 0 )
     &                 ZR(NVALE+ZI(JDDLZ+IN)-1) = Z
                  IF ( ZI(JDLRX+IN) .NE. 0 )
     &              ZR(NVALE+ZI(JDLRX+IN)-1) = ZR(IVALE+ZI(JDLRX+IN)-1)
                  IF ( ZI(JDLRY+IN) .NE. 0 )
     &              ZR(NVALE+ZI(JDLRY+IN)-1) = ZR(IVALE+ZI(JDLRY+IN)-1)
                  IF ( ZI(JDLRZ+IN) .NE. 0 )
     &              ZR(NVALE+ZI(JDLRZ+IN)-1) = ZR(IVALE+ZI(JDLRZ+IN)-1)
               ENDIF
 54         CONTINUE
C
 52       CONTINUE
          CALL JEDETR ( '&&CHMIMA.VALE_P.NT' )
C
      ENDIF
C
 58   CONTINUE
      IF ( VALEUR(1:4) .EQ. 'INST' ) THEN
        IF ( TYPMA.EQ.'NORM_TRA') THEN
          IF (  NBORDR.NE.1 ) THEN
            DO 102 IN = 0 , NBNOE-1
              CALL RSADPA(NOMSD,'L',1,'INST',
     &                    ZI(INUMER+ZI(JDDLX+IN)-1),0,IAD,CTYP)
              ZR(NVALE+ZI(JDDLX+IN)-1) = ZR(IAD)
              ZR(NVALE+ZI(JDDLY+IN)-1) = ZR(IAD)
              IF ( ZI(JDDLZ+IN) .NE. 0 )
     &           ZR(NVALE+ZI(JDDLZ+IN)-1) = ZR(IAD)
              IF ( ZI(JDLRX+IN) .NE. 0 )
     &           ZR(NVALE+ZI(JDLRX+IN)-1) = ZR(IAD)
              IF ( ZI(JDLRY+IN) .NE. 0 )
     &           ZR(NVALE+ZI(JDLRY+IN)-1) = ZR(IAD)
              IF ( ZI(JDLRZ+IN) .NE. 0 )
     &           ZR(NVALE+ZI(JDLRZ+IN)-1) = ZR(IAD)
 102        CONTINUE
            CALL JEDETR ( '&&CHMIMA.DDL.DX' )
            CALL JEDETR ( '&&CHMIMA.DDL.DY' )
            CALL JEDETR ( '&&CHMIMA.DDL.DZ' )
            CALL JEDETR ( '&&CHMIMA.DDL.DRX' )
            CALL JEDETR ( '&&CHMIMA.DDL.DRY' )
            CALL JEDETR ( '&&CHMIMA.DDL.DRZ' )
          ELSE
            DO 110 J = 0 , NEQ-1
              CALL RSADPA(NOMSD,'L',1,'INST',ZI(INUMER+J),0,IAD,CTYP)
              ZR(NVALE+J) = ZR(IAD)
 110        CONTINUE
          ENDIF
        ELSE
          DO 120 J = 0 , NEQ-1
            CALL RSADPA(NOMSD,'L',1,'INST',ZI(INUMER+J),0,IAD,CTYP)
            ZR(NVALE+J) = ZR(IAD)
 120      CONTINUE
        ENDIF
      ELSE
        IF (TYPMA.EQ.'NORM_TRA') THEN
          CALL JEDETR ( '&&CHMIMA.DDL.DX' )
          CALL JEDETR ( '&&CHMIMA.DDL.DY' )
          CALL JEDETR ( '&&CHMIMA.DDL.DZ' )
          CALL JEDETR ( '&&CHMIMA.DDL.DRX' )
          CALL JEDETR ( '&&CHMIMA.DDL.DRY' )
          CALL JEDETR ( '&&CHMIMA.DDL.DRZ' )
        ENDIF
      ENDIF
C
      CALL JEDETR('&&CHMIMA.INST')
      CALL JEDETR(KNUM)
C
      CALL JEDEMA()
      END

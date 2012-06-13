      SUBROUTINE RVOUEX(MCF,IOCC,NCHPT,LSTCMP,LSTMAC,LSTNAC,IRET)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24                     LSTCMP,LSTMAC,LSTNAC
      CHARACTER*(*)     MCF,     NCHPT
      INTEGER               IOCC,                            IRET
C**********************************************************************
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C
C  OPERATION REALISEE
C  ------------------
C     CONSTRUCTION DES VECTEURS DES MAILLES ET/OU NOEUDS ACTIFS
C
C  ARGUMENTS EN ENTREE
C  -------------------
C     IOCC   : NUMERO DE L' OCCURENCE TRAITEE
C     NCHPT  : NOM DU CHAM_GD A TRAITER
C     LSTCMP : NOM DU VECTEUR DES NUMEROS DE CMP MISES EN JEU
C
C  ARGUMENTS EN SORTIE
C  -------------------
C     LSTMAC : NOM DE L' OJB 'V V I' DES NUMEROS DE MAILLES ACTIVES
C     LSTNAC : NOM DE L' OJB 'V V I' DES NUMEROS DE NOEUDS ACTIFS
C     IRET   : CODE RETOUR 1 = OK, 0 = KO
C
C  REMARQUE
C  --------
C     SUIVANT LA NATURE DU CHAMP TRAITE UN SEUL DES OJB LSTMAC ET
C     LSTNAC EST CONSTRUIT
C
C**********************************************************************
C
C
C
C  VARIABLES LOCALES
C  -----------------
      INTEGER      ADR,ALISTE,ACNCIN,ALSMAC,ALSNAC,ACMP,ADRVLC,AREPE
      INTEGER      NBTMA,NBM,NBMAC,NBNAC,NBCRB,NBMALU
      INTEGER      I,IN,N,M,LIBRE,N1,IBID,IGREL,JNUMA,J
      INTEGER      IBIB,IE,IMOLO,JCELD,N2,KK,IER,NBVARI,NBR
      INTEGER      II,JMMAIL,NBTROU,NBCMP,NBCMP1,NC,JCMP,JCMP1,NTC
      CHARACTER*4  DOCU
      CHARACTER*8  NMAILA,COURBE,K8B,NOMGD,RESUCO,NOMVAR,NUM
      CHARACTER*15 NCONEC
      CHARACTER*16 MOTCLE(2),TYPMCL(2),NCHSYM
      CHARACTER*19 NCHP19
      CHARACTER*24 NCNCIN,NREPE,LISMAI,MALIST,NOMOBJ,VALK(3)
      INTEGER      IARG
      DATA NBVARI /100/
C**********************************************************************
C
      CALL JEMARQ()
C
      CALL JEVEUO(JEXNUM(LSTCMP,IOCC),'L',ACMP)
      MALIST = '&&RVOUEX_MALIST'
C
      CALL GETVID ( MCF, 'CHEMIN', IOCC,IARG,0, ZK8, NBCRB )
      NBCRB = -NBCRB
      IF ( NBCRB .NE. 0 ) THEN
         CALL GETVID(MCF,'CHEMIN',IOCC,IARG,NBCRB,COURBE,IBIB)
      ENDIF
C
      NCHP19 = NCHPT
      IRET   = 1
C
      IF ( NCHP19(1:1) .NE. '&' ) THEN
C
         CALL JEEXIN(NCHP19//'.DESC',IBID)
         IF (IBID.GT.0) THEN
           CALL JELIRA(NCHP19//'.DESC','DOCU',IBID,DOCU)
         ELSE

C          -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :

           CALL CELCEL('NBVARI_CST',NCHP19,'V','&&RVOUEX.CHAMEL1')
           NCHP19= '&&RVOUEX.CHAMEL1'
           CALL CELVER(NCHP19,'NBSPT_1','COOL',KK)
           IF (KK.EQ.1) THEN
             CALL DISMOI('F','NOM_GD',NCHP19,'CHAMP',IBID,NOMGD,IE)
             CALL U2MESK('I','PREPOST_36',1,NOMGD)
             CALL CELCEL('PAS_DE_SP',NCHP19,'V','&&RVOUEX.CHAMEL2')
             NCHP19= '&&RVOUEX.CHAMEL2'
           END IF
           CALL JELIRA(NCHP19//'.CELD','DOCU',IBID,DOCU)
         END IF
C
         CALL DISMOI('F','NOM_MAILLA',NCHP19,'CHAMP',IBID,NMAILA,IE)
         NCONEC = NMAILA//'.CONNEX'
         NCNCIN = '&&OP0051.CONNECINVERSE  '
C
         CALL JELIRA(NCONEC,'NMAXOC',NBTMA,K8B)
C
         NBTROU = 0
         JMMAIL = 1
C
         CALL GETVTX ( MCF, 'NOM_CMP', IOCC,IARG,0, K8B, NC )
         IF ( NC.LT.0 .AND. NBCRB.EQ.0 ) THEN
            NBCMP = -NC
            CALL WKVECT ('&&RVOUEX.NOM_CMP','V V K8', NBCMP, JCMP )
            CALL GETVTX ( MCF,'NOM_CMP',IOCC,IARG,NBCMP,ZK8(JCMP),NC)
C
C VERIFICATION QUE LES COMPOSANTES DEMANDEES
C APPARTIENNENT BIEN AU CHAMP
C
            CALL GETVID ( MCF, 'RESULTAT', IOCC,IARG,1,RESUCO,IBIB )
            IF(IBIB.NE.0) THEN
              NOMOBJ = '&&RVOUEX.NOM_CMP1'
              CALL JEEXIN(NOMOBJ,IER)
              IF(IER.NE.0) CALL JEDETR (NOMOBJ)
              CALL UTNCMP ( NCHP19, NBCMP1, NOMOBJ )
              CALL JEVEUO ( NOMOBJ, 'L', JCMP1 )
              CALL GETVTX ( MCF, 'NOM_CHAM' , IOCC,IARG,1, NCHSYM, N1 )
              NBR=NBCMP1
              IF(ZK8(JCMP1).EQ.'VARI') NBR=NBVARI
              DO 102 I=1,NBCMP
                DO 103 J=1,NBR
                  IF(ZK8(JCMP1).EQ.'VARI') THEN
                    CALL CODENT(J,'G',NUM)
                    NOMVAR = 'V'//NUM
                    IF (ZK8(JCMP-1+I) .EQ. NOMVAR) GOTO 102
                  ELSE
                    IF (ZK8(JCMP-1+I) .EQ. ZK8(JCMP1-1+J)) GOTO 102
                  ENDIF
103             CONTINUE
                VALK(1) = ZK8(JCMP-1+I)
                VALK(2) = NCHSYM
                VALK(3) = RESUCO
                CALL U2MESK('F','POSTRELE_65',3,VALK)
102          CONTINUE
            ENDIF
C
            CALL UTMACH ( NCHP19, NBCMP, ZK8(JCMP), 'NU',
     &                                                 MALIST, NBTROU)
            IF ( NBTROU .NE. 0 ) CALL JEVEUO ( MALIST, 'L', JMMAIL )
            CALL JEDETR ( '&&RVOUEX.NOM_CMP' )
         ENDIF
C
         CALL GETVTX ( MCF, 'TOUT_CMP', IOCC,IARG,0, K8B, NTC )
         IF ( NTC.LT.0 .AND. NBCRB.EQ.0 ) THEN
            NOMOBJ = '&&RVOUEX.NOMCMP.USER'
            CALL UTNCMP ( NCHP19, NBCMP, NOMOBJ )
            CALL JEVEUO ( NOMOBJ, 'L', JCMP )
            CALL UTMACH ( NCHP19, NBCMP, ZK8(JCMP), 'NU',
     &                                                 MALIST, NBTROU )
            IF ( NBTROU .NE. 0 ) CALL JEVEUO ( MALIST, 'L', JMMAIL )
            CALL JEDETR ( NOMOBJ )
         ENDIF
C
         IF ( DOCU .EQ. 'CHML' ) THEN
C             ----------------
            CALL JEVEUO(NCHP19//'.CELK','L',ADR)
            NREPE  = ZK24(ADR)(1:19)//'.REPE'
            CALL JEVEUO(NREPE,'L',AREPE)
C
            IF ( NBCRB .NE. 0 ) THEN
C
               CALL RVFMAI ( COURBE, LSTMAC )
C
            ELSE
C
               CALL RVGNOE ( MCF, IOCC, NMAILA, LSTNAC, 0, IBID )
C
               CALL GETVTX ( MCF, 'GROUP_MA', IOCC,IARG,0, K8B, N1 )
               CALL GETVTX ( MCF, 'MAILLE',   IOCC,IARG,0, K8B, N2 )
               IF ( (N1+N2) .EQ. 0 ) THEN
                  NBMALU = 0
               ELSE
                  LISMAI = '&&RVOUEX.NUME_MAIL'
                  MOTCLE(1) = 'GROUP_MA'
                  MOTCLE(2) = 'MAILLE'
                  TYPMCL(1) = 'GROUP_MA'
                  TYPMCL(2) = 'MAILLE'
                  CALL RELIEM(' ',NMAILA,'NU_MAILLE',MCF,IOCC,2,
     &                        MOTCLE,TYPMCL,LISMAI,NBMALU)
                  CALL JEVEUO(LISMAI,'L',JNUMA)
               ENDIF
C
               CALL JEEXIN(NCNCIN,N2)
               IF ( N2 .EQ. 0 ) CALL CNCINV (NMAILA,IBID,0,'V',NCNCIN )
C
               CALL JELIRA(LSTNAC,'LONMAX',NBNAC,K8B)
               CALL JEVEUO(LSTNAC,'L',ALSNAC)
C
               CALL JECREO('&&RVOUEX.LISTE.ENTIER','V V I')
               CALL JEECRA('&&RVOUEX.LISTE.ENTIER','LONMAX',NBTMA,' ')
               CALL JEVEUO('&&RVOUEX.LISTE.ENTIER','E',ALISTE)
C
               LIBRE = 1
               CALL JEVEUO(JEXATR(NCNCIN,'LONCUM'),'L',ADRVLC)
               CALL JEVEUO(JEXNUM(NCNCIN,1)       ,'L',ACNCIN)
C
               DO 100, IN = 1, NBNAC, 1
                  N   = ZI(ALSNAC + IN-1)
                  NBM = ZI(ADRVLC + N+1-1) - ZI(ADRVLC + N-1)
                  ADR = ZI(ADRVLC + N-1)
C
                  CALL I2TRGI(ZI(ALISTE),ZI(ACNCIN + ADR-1),NBM,LIBRE)
C
 100           CONTINUE
C
               NBMAC = LIBRE - 1
               LIBRE = 1
C
               CALL JEVEUO(NCHP19//'.CELD','L',JCELD)
C
               DO 110, I = 1, NBMAC, 1
                  M    = ZI(ALISTE + I-1)
                  IF ( NBTROU .NE. 0 ) THEN
                     DO 112 II = 1, NBTROU
                        IF ( M .EQ. ZI(JMMAIL+II-1) ) GOTO 114
 112                 CONTINUE
                     GOTO 110
 114                 CONTINUE
                  ENDIF
                  IF ( M .NE. 0 ) THEN
                     IF ( NBMALU .NE. 0 ) THEN
                        DO 402, J = 1, NBMALU, 1
                           IF ( M .EQ. ZI(JNUMA+J-1) ) GOTO 404
 402                    CONTINUE
                        GOTO 110
 404                    CONTINUE
                     ENDIF
                     IGREL = ZI(AREPE + 2*(M-1))
                     IMOLO=ZI(JCELD-1+ZI(JCELD-1+4+IGREL) +2)
                     IF ( IGREL.NE.0 .AND. IMOLO.GT.0) THEN
                        ZI(ALISTE + LIBRE-1) = ZI(ALISTE + I-1)
                        LIBRE = LIBRE + 1
                     ENDIF
                  ENDIF
 110           CONTINUE
C
               NBMAC = LIBRE - 1
C
               IF ( NBMAC .GT. 0 ) THEN
C
                  CALL WKVECT ( LSTMAC,'V V I', NBMAC, ALSMAC )
C
                  DO 120, I = 1, NBMAC, 1
                     ZI(ALSMAC + I-1) = ZI(ALISTE + I-1)
 120              CONTINUE
C
               ELSE
C
                  IRET = 0
C
               ENDIF
            ENDIF
C
            CALL JEDETR ( '&&RVOUEX.LISTE.ENTIER' )
            CALL JEDETR ( '&&RVOUEX.NUME_MAIL'    )
C
         ELSE
C             ----------------
C
            IF ( NBCRB .NE. 0 ) THEN
C
               CALL I2FNOE ( COURBE, LSTNAC )
C
            ELSE
C
              CALL RVGNOE (MCF,IOCC,NMAILA,LSTNAC,NBTROU,ZI(JMMAIL))
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      CALL JEDETR ( MALIST )
      CALL DETRSD('CHAM_ELEM','&&RVOUEX.CHAMEL1')
      CALL DETRSD('CHAM_ELEM','&&RVOUEX.CHAMEL2')
      CALL JEDEMA()
      END

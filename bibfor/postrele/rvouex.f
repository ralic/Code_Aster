      SUBROUTINE RVOUEX(MCF,IOCC,NCHPT,LSTCMP,LSTMAC,LSTNAC,IRET)
      IMPLICIT NONE
      CHARACTER*24                     LSTCMP,LSTMAC,LSTNAC
      CHARACTER*(*)     MCF,     NCHPT
      INTEGER               IOCC,                            IRET
C**********************************************************************
C MODIF POSTRELE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C  VARIABLES LOCALES
C  -----------------
      INTEGER      ADR,ALISTE,ACNCIN,ALSMAC,ALSNAC,ACMP,ADRVLC,AREPE
      INTEGER      NBTMA,NBM,NBMAC,NBNAC,NBCRB,INDMOT,NBMALU
      INTEGER      I,IN,N,M,LIBRE,N1,IBID,IGREL,JNUMA,ILISMA,J
      INTEGER      IBIB,IE,IMOLO,JCELD,N2,KK
      CHARACTER*4  DOCU
      CHARACTER*8  NMAILA,COURBE,K8B,NOMGD
      CHARACTER*15 NCONEC
      CHARACTER*19 NCHP19
      CHARACTER*24 NCNCIN,NREPE,LISMAI,NOMMAI
C**********************************************************************
C
      CALL JEMARQ()
      CALL JEVEUO(JEXNUM(LSTCMP,IOCC),'L',ACMP)
C
      CALL GETVID(MCF,'CHEMIN',IOCC,1,0,ZK8,NBCRB)
C
      NBCRB = -NBCRB
C
      IF ( NBCRB .NE. 0 ) THEN
C
         CALL GETVID(MCF,'CHEMIN',IOCC,1,NBCRB,COURBE,IBIB)
C
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
             CALL UTMESS('I','RVOUEX','LE CHAMP DE: '//NOMGD
     &          //' A DES ELEMENTS AYANT DES SOUS-POINTS.'
     &          //' CES ELEMENTS NE SERONT PAS TRAITES.')
             CALL CELCEL('PAS_DE_SP',NCHP19,'V','&&RVOUEX.CHAMEL2')
             NCHP19= '&&RVOUEX.CHAMEL2'
           END IF


           CALL JELIRA(NCHP19//'.CELD','DOCU',IBID,DOCU)
         END IF
         CALL DISMOI('F','NOM_MAILLA',NCHP19,'CHAMP',IBID,NMAILA,IE)
         NOMMAI = NMAILA//'.NOMMAI         '
C
         NCONEC = NMAILA//'.CONNEX'
         NCNCIN = '&&OP0051.CONNECINVERSE  '
C
         CALL JELIRA(NCONEC,'NMAXOC',NBTMA,K8B)
C
         IF ( DOCU .EQ. 'CHML' ) THEN
C
            CALL JEVEUO(NCHP19//'.CELK','L',ADR)
            NREPE  = ZK24(ADR)(1:19)//'.REPE'
            CALL JEVEUO(NREPE,'L',AREPE)
C
            IF ( NBCRB .NE. 0 ) THEN
C
               CALL RVFMAI(COURBE,LSTMAC)
C
            ELSE
C
               CALL RVGNOE ( MCF, IOCC, NMAILA, LSTNAC )
C
               CALL GETVEM(NMAILA,'GROUP_MA',MCF,'GROUP_MA',
     +                                            IOCC,1,0,K8B,N1)
               CALL GETVEM(NMAILA,'MAILLE',MCF,'MAILLE',
     +                                            IOCC,1,0,K8B,N2)
               IF ( (N1+N2) .EQ. 0 ) THEN
                  NBMALU = 0
               ELSE
                  LISMAI = 'RVOUEX.NOM_MAIL'
                  INDMOT = 0
               CALL RECMAI ( MCF, IOCC, INDMOT, NMAILA, LISMAI, NBMALU)
                  CALL JEVEUO ( LISMAI, 'L', ILISMA )
               CALL WKVECT('&&RVOUEX.NUME_MAIL','V V I', NBMALU, JNUMA )
                  DO 400, I = 1, NBMALU, 1
               CALL JENONU(JEXNOM(NOMMAI,ZK8(ILISMA+I-1)),ZI(JNUMA+I-1))
 400              CONTINUE
                  CALL JEDETR ( LISMAI )
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
C
            IF ( NBCRB .NE. 0 ) THEN
C
               CALL I2FNOE ( COURBE, LSTNAC )
C
            ELSE
C
               CALL RVGNOE ( MCF, IOCC, NMAILA, LSTNAC )
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      CALL DETRSD('CHAM_ELEM','&&RVOUEX.CHAMEL1')
      CALL DETRSD('CHAM_ELEM','&&RVOUEX.CHAMEL2')
      CALL JEDEMA()
      END

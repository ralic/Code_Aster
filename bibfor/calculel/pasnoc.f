      SUBROUTINE PASNOC ( NBEL, LIGREL, NBGREL, LONGR, NCMPMX, VALE,
     &                    NOMCMP,  CELD, CONNEX, POINT,
     &                    LILI, NBNOEU,
     &                    BASE, GRAN, NOMA , CHAMN )
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER           LIGREL(*),LONGR(*),CELD(*),CONNEX(*),
     &                  POINT(*),DESCR
      COMPLEX*16        VALE(*)
      CHARACTER*(*)     BASE, CHAMN,
     &                  LILI, GRAN, NOMA
      CHARACTER*8       NOMCMP(*)
C--------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C--------------------------------------------------------------------
C   NBEL  : NOMBRE D'ELEMENTS DU LIGREL ( DU MAILLAGE)
C   LIGREL: LIGREL COMPLET
C   NBGREL: NOMBRE DE GRELS
C   LONGR : POINTEUR DE LONGUEUR DE LIGREL
C   NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C   VALE  : VALEURS DU CHAM_ELEM
C   NOMCMP: NOMS DES CMP
C   NOMEL : NOMS DES MAILLES SUPPORTS DES ELEMENTS
C   CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
C   CONNEX: CONNECTIVITES DES MAILLES
C   POINT : POINTEUR DANS LES CONNECTIVITES
C   NOMNOS: NOMS DES NOEUDS
C--------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      DIGDEL,IMODEL,ILONG
      CHARACTER*1  TYPVAL
      CHARACTER*3  K3B
      CHARACTER*8  K8B, NOMCP, NOMNO
      CHARACTER*19 NOLILI, CHAMNO
      LOGICAL      EXISDG
      COMPLEX*16   C16B
C
      CALL JEMARQ()
      NOLILI = LILI
      TYPVAL = 'C'
      LONVAL = NBNOEU * NCMPMX
      CALL DISMOI('F','NB_EC',GRAN,'GRANDEUR',NECG,K8B,IE)
      CALL WKVECT('&&PASNOC.VALCOMPNO'  ,'V V C',LONVAL,JVALE)
      CALL WKVECT('&&PASNOC.DESC_NOEUD' ,'V V I',NECG*NBNOEU,JDESC)
      CALL WKVECT('&&PASNOC.NBCOMP_AFFE','V V I',NBNOEU,JNBCA)
      CALL WKVECT('&&PASNOC.DDL_AFFE'   ,'V V I',NCMPMX*NBNOEU,JADDL)
      CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
      CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
C
C     -- DETERMINATION DU NOMBRE MAXIMUM DE SOUS_POINTS ---
      ICOMAX = 0
      DO 4 IGRE = 1 , NBGREL
        ICOEF=MAX(1,CELD(4))
        IF(ICOEF.GT.ICOMAX) ICOMAX = ICOEF
 4    CONTINUE
C
      CALL JEVEUO(NOLILI//'.REPE','L',IREPE)
C
C -- IPOSG : POSITION DE LA COMPOSANTE DANS LA GRANDEUR
C -- IPOSV : POSITION DE LA COMPOSANTE DANS LE .VALE
C
      CALL WKVECT('&&PASNOC.POSG'   ,'V V I'  ,NCMPMX*ICOEF,IPOSG)
      CALL WKVECT('&&PASNOC.POSV'   ,'V V I'  ,NCMPMX      ,IPOSV)
      CALL WKVECT('&&PASNOC.COEF'   ,'V V I'  ,NCMPMX*ICOEF,ICOE )
      CALL WKVECT('&&PASNOC.NCMP'   ,'V V K8' ,NCMPMX*ICOEF,INOM )
      CALL WKVECT('&&PASNOC.VAL'    ,'V V C'  ,NCMPMX*ICOEF,IVAL )
      CALL WKVECT('&&PASNOC.ENT_COD','V V I'  ,10          ,IAEC )
C
      DO 12 IMAIL =1,NBEL
         IGREL = ZI(IREPE+2*(IMAIL-1)+1-1)
         IF( IGREL .EQ. 0 ) GOTO 12
         IELG = ZI(IREPE+2*(IMAIL-1)+2-1)
         MODE=CELD(CELD(4+IGREL)+2)
         IF(MODE.EQ.0) GO TO 12
         IPOIN1 = LONGR(IGREL)
         JMOD = IMODEL+ZI(ILONG-1+MODE)-1
         NEC = NBEC(ZI(JMOD-1+2))
         CALL DGMODE(MODE,IMODEL,ILONG,NEC,ZI(IAEC))
         IAD=CELD(CELD(4+IGREL)+8)
         NSCAL = DIGDEL(MODE)
         ICOEF=MAX(1,CELD(4))
         NSCA  = NSCAL*ICOEF
         NCMPP = 0
         NCMP2 = 0
         DO 23 I = 1 , NCMPMX
            IF ( EXISDG(ZI(IAEC),I) ) THEN
               NCMPP = NCMPP+1
               DO 93 JCO = 1 , ICOEF
                  ZI(IPOSG-1+(NCMPP-1)*ICOEF+JCO) = I
  93           CONTINUE
            ENDIF
 23      CONTINUE
         NCMP2 = NCMPP
         NPCALC = NSCAL / NCMPP
C
C --- STOCKAGE DES NOMS DE COMPOSANTES ---
C
         DO 42 I = 1 , NCMP2
            IF ( ICOEF .GT. 1 ) THEN
               DO 43 JCO = 1 , ICOEF
                  CALL CODENT(JCO,'G',K3B)
                  NOMCP = NOMCMP(ZI(IPOSG-1+I))
                  IUTIL = LXLGUT(NOMCP)
                  IF ( GRAN .EQ. 'VAR2_R' ) IUTIL = IUTIL - 2
                  ZK8(INOM-1+(I-1)*ICOEF+JCO)=NOMCP(1:IUTIL)//'_'//K3B
 43            CONTINUE
            ELSE
               ZK8(INOM-1+I) = NOMCMP(ZI(IPOSG-1+I))
            ENDIF
 42      CONTINUE
C
         IEL = LIGREL(IPOIN1+IELG-1)
         NBVAL = ICOEF*NCMP2
C-----------

C-----------
         IACHML = IAD + NSCA * (IELG-1)
C
         IPOIN = POINT(IEL)
         NBNO  = POINT(IEL+1) - IPOIN
         NCOU  = NPCALC / NBNO
         DO 17 ICOU = 1 , NCOU
            DO 18 IN = 1 , NBNO
               NUNO  = CONNEX(IPOIN-1+IN)
               ZI(JNBCA-1+NUNO) = NBVAL
               J = IACHML-1+NCMPP*ICOEF*(IN-1)
     &                                  +(ICOU-1)*NCMPP*ICOEF*NBNO
               DO 50 I = 1,NCMP2
                  DO 51 JCO = 1,ICOEF
                     ZC(IVAL-1+(I-1)*ICOEF+JCO) =
     &                                        VALE(J+I+(JCO-1)*NCMPP)
                     ZI(ICOE-1+(I-1)*ICOEF+JCO) = JCO
 51               CONTINUE
 50            CONTINUE
C
               DO 10 ICMP = 1 , NBVAL
                 J = INDIK8 ( NOMCMP, ZK8(INOM-1+ICMP), 1, NCMPMX )
                 IF ( J .NE. 0 ) THEN
                   IEC = ( J - 1 ) / 30  + 1
                   JJ = J -  30 * ( IEC - 1 )
                   DESCR= ZI(JDESC-1+(NUNO-1)*NECG+IEC)
                   DESCR = IOR(DESCR,2**JJ)
                   ZI(JDESC-1+(NUNO-1)*NECG+IEC)=DESCR
                 ELSE
                   CALL U2MESS('F','CALCULEL4_52')
                 ENDIF
                 IND = J + (NUNO-1)*NCMPMX
                 ZI(JADDL-1+IND) = ZI(JADDL-1+IND) + 1
                 ZC(JVALE-1+IND) = ZC(JVALE-1+IND) + ZC(IVAL-1+ICMP)
   10          CONTINUE
   18       CONTINUE
   17    CONTINUE
   12 CONTINUE
C
      DO 80 INO = 1 , NBNOEU
         DO 82 IC = 1 , NCMPMX
            IND = IC + (INO-1)*NCMPMX
            IF ( ZI(JADDL-1+IND) .GT. 1 ) THEN
               ZC(JVALE-1+IND) = ZC(JVALE-1+IND) / ZI(JADDL-1+IND)
            ENDIF
 82      CONTINUE
 80   CONTINUE
C
      CALL AFCHNO ( CHAMN, BASE, GRAN, NOMA, NBNOEU, ZI(JNBCA),
     &              ZI(JDESC), LONVAL, TYPVAL, R8B, ZC(JVALE), K8B )
C
      CALL JEDETR('&&PASNOC.VALCOMPNO')
      CALL JEDETR('&&PASNOC.DESC_NOEUD')
      CALL JEDETR('&&PASNOC.NBCOMP_AFFE')
      CALL JEDETR('&&PASNOC.DDL_AFFE')
      CALL JEDETR('&&PASNOC.ENT_COD')
      CALL JEDETR('&&PASNOC.POSG')
      CALL JEDETR('&&PASNOC.POSV')
      CALL JEDETR('&&PASNOC.COEF')
      CALL JEDETR('&&PASNOC.NCMP')
      CALL JEDETR('&&PASNOC.VAL')
C
      CALL JEDEMA()
      END

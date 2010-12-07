      SUBROUTINE CMPCHA(NOMCHA,NOMCMP,NUMCMP,NCMP1,NCMPMX)
      IMPLICIT NONE
      CHARACTER*19      NOMCHA
      CHARACTER*(*)            NOMCMP,NUMCMP
      INTEGER                                NCMP1,NCMPMX
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 12/04/2010   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ----------------------------------------------------------------------
C BUT : FOURNIR LE NOMBRE ET LE NOM DES COMPOSANTES DU CHAMP NOMCHA
C       AINSI QUE LA CORRESPONDANCE "COMPOSANTE CHAMP <=> COMPOSANTE
C       GRANDEUR ASSOCIEE"
C       FONCTIONNE AVEC LES CARTES, LES CHAM_NO ET LES CHAM_EL*
C ----------------------------------------------------------------------
C ARGUMENTS :
C ===========
C NOMCHA  IN  K24 : NOM DU CHAMP
C NOMCMP  IN  K*  : NOM DU VECTEUR JEVEUX QUI CONTIENDRA EN SORTIE LE
C                   NOM DES COMPOSANTES PRESENTES DANS LE CHAMP
C NUMCMP  IN  K*  : NOM DU VECTEUR JEVEUX QUI CONTIENDRA EN SORTIE LA
C                   CORRESPONDANCE ENTRE LE NUMERO D'1 CMP DU CHAMP
C                   ET LE NUMERO D'1 CMP DE LA GRANDEUR ASSOCIEE
C NCMP1   OUT I   : NOMBRE DE COMPOSANTES EFFECTIVEMENT PRESENTES DANS
C                   LE CHAMP
C NCMPMX  OUT I   : NOMBRE DE COMPOSANTES DE LA GRANDEUR ASSOCIEE
C ----------------------------------------------------------------------
C RESPONSABLE SELLENET N.SELLENET
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       ZI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C     ----------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
      INTEGER       IFM,NIV,IBID,IRET,NBGR,JCELD,NEC,JCMPGD,JNOCMP
      INTEGER       JNUCMP,IGR,IMOLO,JMOLO,GD,NBPT,IPT,K,IADG,ICMP
      INTEGER       JDESC,LONG,JPRNO,JNUEQ,NBNO,INO,NCMP
      INTEGER       NGRMX,NBEDIT,IGD,IENT,ICO,DEBGD
      LOGICAL       EXISDG,DIFF
      CHARACTER*1   KBID
      CHARACTER*8   NOMGD,MA
      CHARACTER*16  NOMSYM,TYPSD
      CHARACTER*19  NOMCH2,PROFCN
C
      NOMCH2 = NOMCHA
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C
      CALL DISMOI('F','TYPE_CHAMP',NOMCHA,'CHAMP',IBID,TYPSD,IRET)
C
      IF ( TYPSD.EQ.'NOEU' ) THEN
         CALL DISMOI('F','NOM_GD',NOMCH2,'CHAM_NO',IBID,NOMGD,IBID)
      ELSEIF ( TYPSD(1:2).EQ.'EL' ) THEN
         CALL DISMOI('F','NOM_GD',NOMCH2,'CHAM_ELEM',IBID,NOMGD,IBID)
      ELSEIF ( TYPSD.EQ.'CART' ) THEN
         CALL DISMOI('F','NOM_GD',NOMCH2,'CARTE',IBID,NOMGD,IBID)
      ELSE
         CALL ASSERT(.FALSE.)
      END IF
C
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IBID)
      CALL DISMOI('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NCMPMX,KBID,IBID)
      CALL DISMOI('F','NUM_GD',NOMGD,'GRANDEUR',GD,KBID,IBID)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',JCMPGD)
C
      CALL WKVECT(NUMCMP,'V V I',NCMPMX,JNUCMP)
C
      NCMP1 = 0
C
C     -- CAS DES CHAM_NO
C
      IF ( TYPSD.EQ.'NOEU' ) THEN
         CALL JEVEUO(NOMCH2//'.DESC','L',JDESC)
         CALL DISMOI('F','NOM_MAILLA',NOMCH2,'CHAM_NO',IBID,MA,IBID)
         CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNO,KBID,IBID)
C
C        -- CAS DES CHAM_NO A REPRESENTATION CONSTANTE :
         IF (ZI(JDESC-1+2).LT.0) THEN
            PROFCN = ' '
            CALL JELIRA(NOMCH2//'.DESC','LONMAX',LONG,KBID)
            CALL ASSERT(LONG.EQ.(2+NEC))
            IADG = JDESC - 1 + 3
            DO 10,ICMP = 1,NCMPMX
               IF (EXISDG(ZI(IADG),ICMP)) THEN
                  NCMP1 = NCMP1 + 1
                  ZI(JNUCMP-1+ICMP) = 1
               END IF
   10       CONTINUE
C
C        -- CAS DES CHAM_NO A PROF_CHNO:
         ELSE
            CALL DISMOI('F','PROF_CHNO',NOMCH2,'CHAM_NO',IBID,PROFCN,
     &                  IBID)
            CALL JEVEUO(JEXNUM(PROFCN//'.PRNO',1),'L',JPRNO)
            CALL JEVEUO(PROFCN//'.NUEQ','L',JNUEQ)
            DO 30,INO = 1,NBNO
               NCMP = ZI(JPRNO-1+ (INO-1)* (NEC+2)+2)
               IF (NCMP.NE.0) THEN
                  IADG = JPRNO - 1 + (INO-1)* (NEC+2) + 3
                  DO 20,ICMP = 1,NCMPMX
                     IF (EXISDG(ZI(IADG),ICMP)) THEN
                        NCMP1 = NCMP1 + 1
                        ZI(JNUCMP-1+ICMP) = 1
                     END IF
   20             CONTINUE
               ENDIF
   30       CONTINUE
         END IF
C
C     -- CAS DES CHAM_ELEM
C
      ELSEIF ( TYPSD(1:2).EQ.'EL' ) THEN
         CALL JEVEUO(NOMCH2//'.CELD','L',JCELD)
         NBGR = ZI(JCELD-1+2)
C
         DO 70 IGR = 1,NBGR
            IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
            IF (IMOLO.EQ.0) GOTO 70
            CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
            CALL ASSERT(ZI(JMOLO-1+1).LE.3)
            CALL ASSERT(ZI(JMOLO-1+2).EQ.GD)
            DIFF = (ZI(JMOLO-1+4).GT.10000)
            NBPT = MOD(ZI(JMOLO-1+4),10000)
C
            DO 60,IPT = 1,NBPT
               K = 1
               IF (DIFF) K = IPT
               IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
               DO 50,ICMP = 1,NCMPMX
                  IF (EXISDG(ZI(IADG),ICMP)) THEN
                     NCMP1 = NCMP1 + 1
                     ZI(JNUCMP-1+ICMP) = 1
                  END IF
   50          CONTINUE
   60        CONTINUE
   70    CONTINUE
C
C     -- CAS DES CARTES
C
      ELSEIF ( TYPSD.EQ.'CART' ) THEN
         CALL JEVEUO(NOMCH2//'.DESC','L',JDESC)
         NGRMX = ZI(JDESC-1+2)
         NBEDIT = ZI(JDESC-1+3)
         DO 80 IGD = 1,NBEDIT
            IENT = ZI(JDESC-1+3+2*IGD)
            IF (IENT.NE.0) THEN
               DEBGD = 3 + 2*NGRMX + (IGD-1)*NEC + 1
               DO 90 ICMP = 1,NCMPMX
                  IF (EXISDG(ZI(JDESC-1+DEBGD),ICMP)) THEN
                     NCMP1 = NCMP1 + 1
                     ZI(JNUCMP-1+ICMP) = 1
                  ENDIF
   90          CONTINUE
            ENDIF
   80    CONTINUE
C
      ELSE
         CALL ASSERT(.FALSE.)
      END IF
C
      CALL WKVECT(NOMCMP,'V V K8',NCMP1,JNOCMP)
      NCMP1 = 0
      DO 100,ICMP = 1,NCMPMX
         IF (ZI(JNUCMP-1+ICMP).EQ.1) THEN
            NCMP1 = NCMP1 + 1
            ZI(JNUCMP-1+ICMP) = NCMP1
            ZK8(JNOCMP-1+NCMP1) = ZK8(JCMPGD-1+ICMP)
         END IF
  100 CONTINUE
C
      CALL JEDEMA()
      END

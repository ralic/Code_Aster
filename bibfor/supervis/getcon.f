      SUBROUTINE GETCON(NOMRES,IOB,ISHF,ILNG,CTYPE,LCON,IADVAR,NOMOB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 06/11/2007   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE  CRP_6
      IMPLICIT NONE
      CHARACTER*(*) NOMRES
      INTEGER CTYPE,LCON,IOB,ISHF,ILNG
      INTEGER IADVAR,LOC
      CHARACTER*8 NOMOB
C IN  NOMRES  K*  NOM DU CONCEPT DEMANDE
C IN  IBOB    I  POUR UNE COLLECTION : NUMERO DE L OBJET
C IN  ISHF    I  POUR UN VECTEUR     : SHIFT DANS LE VECTEUR
C IN  ILNG    I  POUR UN VECTEUR     : NB DE VALEURS REQUISES APRES ISHF
C OUT CTYPE   I   LE TYPE : CTYPE (1=REEL, 2=ENTIER, 3=COMPLEXE,
C                                  4=K8,5=K16,6=K24,7=K32,8=K80)
C                                  0=PAS DE VALEUR, <0=ERREUR
C OUT LCON    I  LISTE DES LONGUEURS : NOMBRE DE VALEURS
C OUT IADVAR  I  LISTE DES ADRESSES DU TABLEAU
C OUT NOMOB   K* POUR UNE COLLECTION : NOM DE L OBJET SI EXISTE

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8     K8BID
      CHARACTER*4     TYPE
      CHARACTER*2     ACCES
      CHARACTER*1     XOUS,GENR
      INTEGER         LXLGUT
      INTEGER         JRES,IRET,IBID,I,LOBJ,IAD,KK
      CHARACTER*32    NOML32, JEXNUM
C     ------------------------------------------------------------------
      CTYPE = -1
C             123456789.123456789.12
      NOML32='                      '
      NOML32=NOMRES
C     AU DELA DE 24 : RESERVE JEVEUX &&xxxx
      CALL ASSERT(LXLGUT(NOML32).LE.24)
      CALL JJVERN(NOML32,0,IRET)
      IF(IRET.EQ.0)THEN
C     CET OBJET N'EXISTE PAS
         GOTO 999
      ENDIF
      CALL JELIRA(NOML32,'XOUS',IBID,XOUS)
      CALL JELIRA(NOML32,'GENR',IBID,GENR)
      NOMOB='        '
      IF(XOUS.EQ.'X')THEN
C     ------------------------------------------------------------------
C     CET OBJET EST UNE COLLECTION, ON VEUT SON ELEMENT NUMERO IOB
C     ------------------------------------------------------------------
         CTYPE=0
         CALL JEEXIN(JEXNUM(NOML32,IOB),IRET)
         IF (IRET.LE.0) GOTO 999
         CALL JELIRA(NOML32,'ACCES',IBID,ACCES)
         IF (ACCES.EQ.'NO') THEN
            CALL JENUNO(JEXNUM(NOML32,IOB),NOMOB)
         ENDIF
         CALL JEVEUO(JEXNUM(NOML32,IOB),'L',JRES)
         CALL JELIRA(JEXNUM(NOML32,IOB),'LONMAX',LOBJ,K8BID)
         CALL JELIRA(JEXNUM(NOML32,IOB),'TYPELONG',IBID,TYPE)
         LCON   = LOBJ
         IF(TYPE.EQ.'R')THEN
C     LES VALEURS SONT REELLES
            CTYPE=1
            IADVAR=LOC(ZR(JRES))
         ELSE IF(TYPE.EQ.'I')THEN
C     LES VALEURS SONT ENTIERES
            CTYPE=2
            IADVAR=LOC(ZI(JRES))
         ELSE IF(TYPE.EQ.'C')THEN
C     LES VALEURS SONT COMPLEXES
            CTYPE=3
            IADVAR=LOC(ZC(JRES))
         ELSE IF(TYPE.EQ.'K8')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=4
            IADVAR=LOC(ZK8(JRES))
         ELSE IF(TYPE.EQ.'K16')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=5
            IADVAR=LOC(ZK16(JRES))
         ELSE IF(TYPE.EQ.'K24')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=6
            IADVAR=LOC(ZK24(JRES))
         ELSE IF(TYPE.EQ.'K32')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=7
            IADVAR=LOC(ZK32(JRES))
         ELSE IF(TYPE.EQ.'K80')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=8
            IADVAR=LOC(ZK80(JRES))
         ELSE
C     TYPE INCONNU
            CTYPE=0
         ENDIF
      ELSE IF((XOUS.EQ.'S').AND.(GENR.NE.'N'))THEN
C     ------------------------------------------------------------------
C     CET OBJET EXISTE ET EST SIMPLE. ON PEUT AVOIR SA VALEUR
C     ------------------------------------------------------------------
         CALL JEVEUO(NOML32,'L',JRES)
C          CALL JELIRA(NOML32,'LONUTI',LCON,K8BID)
C          IF (LCON.EQ.0) THEN
C              CALL JELIRA(NOML32,'LONMAX',LCON,K8BID)
C          ENDIF
         CALL JELIRA(NOML32,'LONMAX',LCON,K8BID)
         IF (ILNG.NE.0) LCON=ILNG
         CALL JELIRA(NOML32,'TYPELONG',IBID,TYPE)
         IF(TYPE.EQ.'R')THEN
C     LES VALEURS SONT REELLES
            CTYPE=1
            IADVAR=LOC(ZR(JRES+ISHF))
         ELSE IF(TYPE.EQ.'I')THEN
C     LES VALEURS SONT ENTIERES
            CTYPE=2
            IADVAR=LOC(ZI(JRES+ISHF))
         ELSE IF(TYPE.EQ.'C')THEN
C     LES VALEURS SONT COMPLEXES
            CTYPE=3
            IADVAR=LOC(ZC(JRES+ISHF))
         ELSE IF(TYPE.EQ.'K8')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=4
            IADVAR=LOC(ZK8(JRES+ISHF))
         ELSE IF(TYPE.EQ.'K16')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=5
            IADVAR=LOC(ZK16(JRES+ISHF))
         ELSE IF(TYPE.EQ.'K24')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=6
            IADVAR=LOC(ZK24(JRES+ISHF))
         ELSE IF(TYPE.EQ.'K32')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=7
            IADVAR=LOC(ZK32(JRES+ISHF))
         ELSE IF(TYPE.EQ.'K80')THEN
C     LES VALEURS SONT DES CHAINES
            CTYPE=8
            IADVAR=LOC(ZK80(JRES+ISHF))
         ELSE
C     TYPE INCONNU
            CTYPE=0
         ENDIF
      ELSE IF((XOUS.EQ.'S').AND.(GENR.EQ.'N'))THEN
C     ------------------------------------------------------------------
C     CET OBJET EST SIMPLE MAIS C EST UN REPERTOIRE DE NOMS
C     ------------------------------------------------------------------
C          CALL JELIRA(NOML32,'NOMUTI',LCON,K8BID)
C          IF (LCON.EQ.0) THEN
C              CALL JELIRA(NOML32,'NOMMAX',LCON,K8BID)
C          ENDIF
         CALL JELIRA(NOML32,'NOMMAX',LCON,K8BID)
         CALL JELIRA(NOML32,'TYPELONG',IBID,TYPE)
         CALL JEDETR('&&GETCON.PTEUR_NOM')
         CALL WKVECT('&&GETCON.PTEUR_NOM','V V '//TYPE,LCON,IAD)
         IF (TYPE.EQ.'K8') THEN
           DO 51, KK=1,LCON
             CALL JENUNO(JEXNUM(NOML32,KK),ZK8(IAD-1+KK))
51         CONTINUE
           CTYPE=4
           IADVAR=LOC(ZK8(IAD))
         ELSE IF (TYPE.EQ.'K16') THEN
           DO 52, KK=1,LCON
             CALL JENUNO(JEXNUM(NOML32,KK),ZK16(IAD-1+KK))
52         CONTINUE
           CTYPE=5
           IADVAR=LOC(ZK16(IAD))
         ELSE IF (TYPE.EQ.'K24') THEN
           DO 53, KK=1,LCON
             CALL JENUNO(JEXNUM(NOML32,KK),ZK24(IAD-1+KK))
53         CONTINUE
           CTYPE=6
           IADVAR=LOC(ZK24(IAD))
         END IF
      ENDIF

 999  CONTINUE
      END

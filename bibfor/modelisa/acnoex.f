      SUBROUTINE ACNOEX ( NOMA,TYPE,LISTE,NB,NO1,NO2 )
      IMPLICIT  REAL*8  ( A-H , O-Z )
      CHARACTER*8         NOMA, LISTE(*)
      CHARACTER*4         TYPE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/02/98   AUTEUR JMBHH01 J.M.PROIX 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     RECHERCHE DES NOEUDS EXTREMITES D'UNE LISTE DE MAILLES
C     UTILISE PAR DEFI_ARC
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : TYPE   : 'TOUT', 'GRMA', 'LIMA'
C IN  : LISTE  : VECTEUR DE K8( NB) : LISTE DES MAILLES OU GROUPES
C IN  : NB     : DIMENSION DE LISTE
C OUT : NO1    : NOEUD EXTREMITE DE L'ENSEMBLE DES MAILLES
C OUT : NO2    : NOEUD AUTRE EXTREMITE DE L'ENSEMBLE DES MAILLES
C ----------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8 C8
      CHARACTER*24 MLGGMA, MLGNMA, MLGCNX,C24
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL DISMOI('I','NB_NO_MAILLA',NOMA,'MAILLAGE',NBN,C24,IR)
      CALL WKVECT('&&ACNOEX','V V I',NBN,JNBN)
      MLGGMA = NOMA//'.GROUPEMA'
      MLGNMA = NOMA//'.NOMMAI'
      MLGCNX = NOMA//'.CONNEX'
      IF (TYPE.EQ.'TOUT') THEN
         CALL DISMOI('I','NB_MA_MAILLA',NOMA,'MAILLAGE',NBM,C24,IR)
         DO 52 IM = 1,NBM
            CALL JEVEUO(JEXNUM(MLGCNX,IM),'L',JDNO)
            NN1 = ZI(JDNO)
            NN2 = ZI(JDNO+1)
            ZI(JNBN+NN1-1)=ZI(JNBN+NN1-1)+1
            ZI(JNBN+NN2-1)=ZI(JNBN+NN2-1)+1
  52     CONTINUE
      ELSE IF (TYPE.EQ.'GRMA') THEN
         DO 53 IG = 1,NB
            CALL JEVEUO(JEXNOM(MLGGMA,LISTE(IG)),'L',JDGM)
            CALL JELIRA(JEXNOM(MLGGMA,LISTE(IG)),'LONMAX',NM,C8)
            DO 54 IM = 1,NM
               IMG = ZI(JDGM+IM-1)
               CALL JEVEUO(JEXNUM(MLGCNX,IMG),'L',JDNO)
               NN1 = ZI(JDNO)
               NN2 = ZI(JDNO+1)
               ZI(JNBN+NN1-1)=ZI(JNBN+NN1-1)+1
               ZI(JNBN+NN2-1)=ZI(JNBN+NN2-1)+1
  54        CONTINUE
  53     CONTINUE
      ELSE IF (TYPE.EQ.'LIMA') THEN
         DO 55 IM = 1,NB
            CALL JENONU(JEXNOM(MLGNMA,LISTE(IM)),NUMMAI)
            CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
            NN1 = ZI(JDNO)
            NN2 = ZI(JDNO+1)
            ZI(JNBN+NN1-1)=ZI(JNBN+NN1-1)+1
            ZI(JNBN+NN2-1)=ZI(JNBN+NN2-1)+1
  55     CONTINUE
      ENDIF
C
C     RECHERCHE DES EXTREMITES DE L'ARC
C     
      NO1 = 0
      NO2 = 0
      DO 56 IN = 1, NBN
         NN = ZI(JNBN+IN-1)
         IF (NN. EQ. 1) THEN
            IF (NO1.EQ.0) THEN
                NO1 = IN
            ELSE IF (NO2.EQ.0) THEN
                NO2 = IN
            ELSE
               CALL UTDEBM('E','ACNOEX','  ')
               CALL UTIMPK('L','L''ENSEMBLE DES MAILLES'//
     +           'COMPORTE PLUS DE 2 EXTREMITES',0,' ')
               CALL UTFINM( )
            ENDIF
         ENDIF
  56  CONTINUE
C
C     CAS OU LES EXTREMITES DE L'ARC SONT IDENTIQUES
C     ARC FERME 
C     
      IF(NO1.EQ.NO2) THEN
         CALL UTDEBM('E','ACNOEX','DEFI_ARC')
         CALL UTIMPK('L','L''ENSEMBLE DES MAILLES'//
     +           ' FORME UN CERCLE : A SUBDIVISER ',0,' ')
         CALL UTFINM( )
      ENDIF
      CALL JEDETR('&&ACNOEX')
      CALL JEDEMA()
      END

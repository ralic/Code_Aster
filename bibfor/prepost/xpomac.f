      SUBROUTINE XPOMAC(MALINI,MAILC,LISTNO,NBNOC,NBMAC,MAXFEM,
     &                  CNS1,CNS2,CES1,CES2,CESVI1,CESVI2)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 10/07/2007   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT

      IMPLICIT NONE

      INTEGER       NBNOC,NBMAC
      CHARACTER*8   MALINI,MAXFEM
      CHARACTER*19  CNS1,CNS2,CES1,CES2,CESVI1,CESVI2
      CHARACTER*24  MAILC,LISTNO
C
C
C      TRAITEMENT DES MAILLES DE MAILC
C       - POUR POST_MAIL_XFEM : LES MAILLES DE MAILC ET LES NOEUDS 
C                               ASSOCI�S SONT COPIES DANS MAXFEM A 
C                               L'IDENTIQUE
C       - POUR POST_CHAM_XFEM : LES NOEUDS ASSOCI�S A MAILC
C                               SONT COPIES DANS RES_XFEM A 
C                               L'IDENTIQUE
C
C   IN
C       MALINI : MAILLAGE SAIN
C       MAILC  : LISTE DES NUMEROS DES MAILLES NON SOUS-DECOUPEES
C       LISTNO : LISTE DES NUMEROS DES NOEUDS CLASSIQUES
C       NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
C       NBMAC  : NOMBRE DE MAILLES CLASSIQUES DU MAILLAGE FISSURE
C       MAXFEM : MAILLAGE FISSURE
C       CNS1   : CHAMP_NO_S DU DEPLACEMENT EN ENTREE
C       CES1   : CHAMP_ELEM_S DE CONTRAINTES EN ENTREE
C
C   OUT
C       MAXFEM : MAILLAGE FISSURE (SI POST_MAIL_XFEM)
C       CNS2   : CHAMP_NO_S DU DEPLACEMENT EN SORTIE
C       CES2   : CHAMP_ELEM_S DE CONTRAINTES EN SORTIE


C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------

      INTEGER       JDIRMA,JMAC,IRET,NBNO,NNO,JDIRNO,INO,I
      INTEGER       NBMA,IMA,JNO,IER
      CHARACTER*8   K8B
      CHARACTER*16  K16B,NOMCMD
      CHARACTER*24  DIRMAI,DIRNOE

      CALL JEMARQ()
      
      CALL JEEXIN(MAILC,IER)
      IF (IER.EQ.0) GO TO 999

C     NOM DE LA COMMANDE (POST_MAIL_XFEM OU POST_CHAM_XFEM)
      CALL GETRES(K8B,K16B,NOMCMD)

C     CREATION DU TABLEAU DE CORRESPONDANCE DES NUMEROS DE MAILLES
C     SOIT IMA LE NUMERO D'UNE MAILLE DU MAILLAGE INITIAL
C     ALORS ZI(ITAB-1+IMA) EST LE NUMERO DE CETTE MAILLE DANS LE
C     MAILLAGE X-FEM
      CALL JEVEUO(MAILC,'L',JMAC)
      DIRMAI = '&&XPOMAC.DIRMAI'
      CALL DISMOI('F','NB_MA_MAILLA',MALINI,'MAILLAGE',NBMA,K8B,IRET)
      CALL WKVECT( DIRMAI, 'V V I', NBMA, JDIRMA )
       
      DO 100 I = 1,NBMAC
C        NUMERO DE LA MAILLE DANS LE MAILLAGE INITIAL
         IMA = ZI(JMAC-1+I)
C        NUMERO DE LA MAILLE DANS LE MAILLAGE X-FEM
         ZI(JDIRMA-1+IMA)=I
 100  CONTINUE

C     CREATION DU TABLEAU DE CORRESPONDANCE DES NUMEROS DE NOEUDS
C     SOIT INO LE NUMERO D'UN NOEUD DU MAILLAGE INITIAL
C     ALORS ZI(ITAB-1+INO) EST LE NUMERO DE CE NOEUD DANS LE
C     MAILLAGE X-FEM
      DIRNOE = '&&XPOMAC.DIRNOE'
      CALL DISMOI('F','NB_NO_MAILLA',MALINI,'MAILLAGE',NBNO,K8B,IRET)
      CALL WKVECT( DIRNOE, 'V V I', NBNO, JDIRNO )
      CALL JEEXIN(LISTNO,IER)
      CALL ASSERT(IER.NE.0)
      CALL JEVEUO(LISTNO,'L',JNO)

      DO 110 I = 1,NBNOC
C        NUMERO DU NOEUD DANS LE MAILLAGE INITIAL
         INO = ZI(JNO-1+I)
C        NUMERO DU NOEUD DANS LE MAILLAGE X-FEM
         ZI(JDIRNO-1+INO)=I
 110  CONTINUE

      CALL JEDETR(LISTNO)

      IF (NOMCMD.EQ.'POST_MAIL_XFEM') THEN

C       COPIE MAILLES DE MAILC ET NOEUDS SOUS-JACENTS DANS MAXFEM
        CALL XPOCO1(ZI(JDIRMA),NBMA,ZI(JDIRNO),NBNO,MALINI,MAXFEM)

      ELSEIF (NOMCMD.EQ.'POST_CHAM_XFEM') THEN

C       COPIE DEPLACEMENT DES NOEUDS SOUS-JACENTS DANS MAXFEM
        CALL XPOCO2(MALINI,MAXFEM,ZI(JDIRNO),NBNO,ZI(JDIRMA),NBMA,
     &              CNS1,CNS2,CES1,CES2,CESVI1,CESVI2)

      ENDIF

      CALL JEDETR(DIRMAI)
      CALL JEDETR(DIRNOE)
      IF (NOMCMD.EQ.'POST_MAIL_XFEM') CALL JEDETR(MAILC)

 999  CONTINUE

      CALL JEDEMA()
      END

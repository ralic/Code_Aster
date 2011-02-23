      SUBROUTINE CGMAXF ( MOFAZ, IOCC, NOMAZ, LISMAZ, NBMA )
      IMPLICIT   NONE
      INTEGER             IOCC, NBMA
      CHARACTER*(*)       MOFAZ, NOMAZ, LISMAZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 22/02/2011   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C       CGMAXF -- TRAITEMENT DE L'OPTION FISS_XFEM
C                 DU MOT FACTEUR CREA_GROUP_MA DE
C                 LA COMMANDE DEFI_GROUP
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES 
C                                   DU TYPE XFEM DEMANDE PAR
C                                   L'UTILISATEUR
C  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C -------------------------------------------------------
C
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER        I,II,IMA,IFISS,INDIC,INO
      INTEGER        NBNO,NBNOT,NFISS,NMAX,NBMALO,NBMALA
      INTEGER        JLMAS, IDLIST, JINDIC,JFISS,JTEM3,JTEM4,JTEM5,JSTNO
      INTEGER        IBID,IRET,TEST,VALENO
      CHARACTER*8    NOMA, K8BID,NOMAIL,FISS
      CHARACTER*16   MOTFAC,TYPGRP
      CHARACTER*19   STNO
      CHARACTER*24   LISMAI,LISMAR,LISMAN,MAIFIS

C     -----------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ================
      NBMALA=0
      NBMA = 0
      MOTFAC    = MOFAZ
      NOMA      = NOMAZ
      LISMAI    = LISMAZ
C
C --  RECUPERATION DU TYPE DE MAILLE XFEM :
      CALL GETVTX(MOTFAC,'TYPE_GROUP',IOCC,1,1,TYPGRP,IBID)
      CALL GETVID(MOTFAC,'FISSURE',IOCC,1,0,K8BID,NFISS)
      NFISS = -NFISS
      CALL WKVECT('&&CGMAXF.FISS', 'V V K8',NFISS,JFISS)
      CALL GETVID(MOTFAC,'FISSURE',IOCC,1,NFISS,ZK8(JFISS),IBID)
      
C
C --- TYPE DE MAILLE = 'HEAVISIDE', 'CRACKTIP' OU  'MIXTE'
C     ====================================================
      IF ((TYPGRP.EQ.'HEAVISIDE') .OR. (TYPGRP.EQ.'CRACKTIP') .OR.
     &    (TYPGRP.EQ.'MIXTE')) THEN
     
        CALL WKVECT('&&CGMAXF.TEM3','V V I',  NFISS,JTEM3)
        CALL WKVECT('&&CGMAXF.TEM4','V V I',  NFISS,JTEM4)
C
C --- TYPE DE MAILLE = 'HEAVISIDE'
        IF (TYPGRP.EQ.'HEAVISIDE') THEN
          MAIFIS = '.MAILFISS  .HEAV'
          INDIC  = 1
C
C --- TYPE DE MAILLE = 'CRACKTIP'
        ELSEIF (TYPGRP.EQ.'CRACKTIP') THEN
          MAIFIS = '.MAILFISS  .CTIP'
          INDIC  = 3
C
C --- TYPE DE MAILLE = 
        ELSEIF (TYPGRP.EQ.'MIXTE') THEN
          MAIFIS = '.MAILFISS  .HECT'
          INDIC  = 5
        ENDIF                   
C --- POUR CHAQUE FISSURE XFEM DE TYPE INDIC  
        DO 10 IFISS=1,NFISS            
          FISS =   ZK8(JFISS-1+IFISS)
          CALL JEVEUO(FISS(1:8)//MAIFIS,'L',ZI(JTEM3-1+IFISS))
          CALL JEVEUO(FISS(1:8)//'.MAILFISS .INDIC','L',JINDIC)
          ZI(JTEM4-1+IFISS) = ZI(JINDIC+INDIC)
          NBMA  = NBMA + ZI(JINDIC+INDIC)
 10     CONTINUE
C --- CREATION ET REMPLISSAGE DU VECTEUR DE SORTIE
        IF (NBMA.GT.0) THEN
          CALL WKVECT ( LISMAI, 'V V I', NBMA, IDLIST )
          DO 40 IFISS=1,NFISS
            JLMAS = ZI(JTEM3-1+IFISS)
            DO 20 I = 1 , ZI(JTEM4-1+IFISS)
              NBMALA = NBMALA + 1
              ZI(IDLIST+NBMALA-1) = ZI(JLMAS+I-1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',ZI(JLMAS+I-1)),NOMAIL)
 20         CONTINUE
 40       CONTINUE
        ENDIF
        CALL JEDETR('&&CGMAXF.TEM3')
        CALL JEDETR('&&CGMAXF.TEM4')
C
C --- TYPE DE MAILLE = 'XFEM'
C     ============================
      ELSEIF (TYPGRP.EQ.'XFEM') THEN
C
        LISMAN = '&&CGMAXF.TEM1'
        CALL XTMAFI(NOMA,0,ZK8(JFISS),NFISS,LISMAI,LISMAN,NBMA)
        CALL JEDETR(LISMAN)       
C
C --- TYPE DE MAILLE = 'FISSUREE'
C     ============================
      ELSEIF (TYPGRP.EQ.'FISSUREE') THEN
      
        CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOT,K8BID,IRET)
        CALL DISMOI('F','NB_NO_MAX','&CATA','CATALOGUE',NMAX,K8BID,IRET)
        
C       POUR DIMENSIONNER GROSSIEREMENT LA LISTE DES MAILLES
        LISMAN = '&&CGMAXF.TEM1'
        LISMAR = '&&CGMAXF.TEM2'   
        CALL XTMAFI(NOMA,0,ZK8(JFISS),NFISS,LISMAR,LISMAN,NBMA)
        CALL JEDETR(LISMAR)
        CALL JEDETR(LISMAN)
        
        CALL WKVECT ( '&&CGMAXF.TEM3', 'V V I', NBNOT, JTEM3 )
        CALL WKVECT ( '&&CGMAXF.TEM4' , 'V V I', NMAX, JTEM4 )
        CALL WKVECT ( '&&CGMAXF.TEM5' , 'V V I', NBMA, JTEM5 )
        
        NBMALO = 0
        
C       POUR CHAQUE FISSURE
        DO 50 IFISS=1,NFISS   
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS(1:8)//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          
C         RECUPERATION DE TOUTES MAILLES XFEM DE LA FISSURE COURANTE
          LISMAN = '&&CGMAXF.TEM1'
          CALL XTMAFI(NOMA,0,FISS(1:8),1,LISMAR,LISMAN,NBMALA)
          CALL JEVEUO(LISMAR,'L',JLMAS)

C         POUR CHAQUE MAILLE XFEM DE LA FISSURE COURANTE
          DO 51 II=1,NBMALA     
C           RECUPERATION DES NOEUDS
            CALL GMGNRE(NOMA,NBNOT,ZI(JTEM3),ZI(JLMAS+II-1),1,
     &                  ZI(JTEM4),NBNO,'TOUS')        
C           TRI DES NOEUDS SELON LEUR STATUS XFEM
            TEST = 1
            DO 511 INO=1,NBNO
              VALENO = ZI(JSTNO+ZI(JTEM4+INO-1)-1)
              IF (VALENO.EQ.0) THEN
                TEST = 0
              ENDIF
 511        CONTINUE
C           MAILLES QUI REPOSENT SUR LES NOEUDS AU STATUS <> 0
            IF (TEST.EQ.1) THEN
              NBMALO = NBMALO + 1
              ZI(JTEM5+NBMALO-1) = ZI(JLMAS+II-1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',ZI(JTEM5+NBMALO-1))
     &                          ,NOMAIL)
            ENDIF                         
 51       CONTINUE 
          CALL JEDETR(LISMAR)
          CALL JEDETR(LISMAN)
          CALL JEDETR(STNO)          
 50     CONTINUE
        CALL WKVECT ( LISMAI, 'V V I', NBMALO, IDLIST )
        DO 60 IMA=1,NBMALO
          ZI(IDLIST-1+IMA) = ZI(JTEM5-1+IMA)
 60     CONTINUE
        CALL JEDETR('&&CGMAXF.TEM3')
        CALL JEDETR('&&CGMAXF.TEM4')
        CALL JEDETR('&&CGMAXF.TEM5')
        NBMA = NBMALO

      ENDIF
C
C --- FIN
C     ===
C
C --- MENAGE
C
      CALL JEDETR('&&CGMAXF.FISS')
C
      CALL JEDEMA()
C
      END

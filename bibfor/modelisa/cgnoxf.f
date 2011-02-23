      SUBROUTINE CGNOXF (MOFAZ, IOCC, NOMAZ, LISNOZ, NBNO)
      IMPLICIT NONE
      INTEGER IOCC,NBNO
      CHARACTER*(*) MOFAZ,NOMAZ,LISNOZ
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
C       CGNOXF -- TRAITEMENT DE L'OPTION FISS_XFEM
C                 DU MOT FACTEUR CREA_GROUP_NO DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
C      DE TOUS LES NOEUDS DE TYPE XFEM DEFINI PAR L'UTILISATEUR.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
C                                   DU TYPE XFEM DEMANDE PAR
C                                   L'UTILISATEUR
C  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
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
      COMMON  / KVARJE /ZK8(1),ZK16(1),ZK24(1),ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------
C
C
C --------- VARIABLES LOCALES ---------------------------
      INTEGER        IBID,   IRET
      INTEGER        N1,     IFISS,  NFISS
      INTEGER        INO,    VALENO, NBNOT 
      INTEGER        IDLIST, JNOEU,  JFISS,  JSTNO
      CHARACTER*8    NOMA,   K8BID,  NOMNOE, FISS
      CHARACTER*16   MOTFAC, TYPGRP
      CHARACTER*19   STNO
      CHARACTER*24   LISNOE
C
      REAL*8         X0(3), VECNOR(3), ANGLE(2)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ================
      MOTFAC    = MOFAZ
      NOMA      = NOMAZ
      LISNOE    = LISNOZ
      NBNO = 0

      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOT,K8BID,IRET)
      CALL WKVECT('&&CGNOXF.NOEU','V V I',  NBNOT,JNOEU)
C
C --  RECUPERATION DU TYPE GROUPE :
C     ============================
      CALL GETVTX(MOTFAC,'TYPE_GROUP',IOCC,1,1,TYPGRP,N1)
C
C --  RECUPERATION DES NOMS DES FISSURES :
C     ===================================
      CALL GETVID(MOTFAC,'FISSURE',IOCC,1,0,K8BID,NFISS)
      NFISS = -NFISS
      CALL WKVECT('&&CGNOXF.FISS', 'V V K8',NFISS,JFISS)
      CALL GETVID(MOTFAC,'FISSURE',IOCC,1,NFISS,ZK8(JFISS),IBID)
C
C --- TYPE DE NOEUD = 'HEAVISIDE'
C     ============================
      IF (TYPGRP.EQ.'HEAVISIDE') THEN
        DO 10 IFISS=1,NFISS           
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 110 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.EQ.1) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
            ENDIF
 110     CONTINUE
          CALL JEDETR(STNO)
 10     CONTINUE
C
C --- TYPE DE NOEUD = 'CRACKTIP'
C     ============================
      ELSEIF (TYPGRP.EQ.'CRACKTIP') THEN
        DO 11 IFISS=1,NFISS           
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 111 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.EQ.2) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
             ENDIF
 111      CONTINUE
          CALL JEDETR(STNO)
 11     CONTINUE
C
C --- TYPE DE NOEUD = 'MIXTE'
C     ============================
      ELSEIF (TYPGRP.EQ.'MIXTE') THEN
        DO 12 IFISS=1,NFISS           
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 112 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.EQ.3) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
            ENDIF
 112      CONTINUE
 12     CONTINUE
C
C --- TYPE DE NOEUD = 'XFEM'
C     ============================
      ELSEIF (TYPGRP.EQ.'XFEM') THEN
        DO 13 IFISS=1,NFISS           
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 113 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.NE.0) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
            ENDIF
 113     CONTINUE
          CALL JEDETR(STNO)
 13     CONTINUE
      ELSE   
         CALL ASSERT(.FALSE.)
      ENDIF

      IF (NBNO.NE.0) THEN    
        CALL WKVECT (LISNOE, 'V V I', NBNO, IDLIST )
 
        DO 20 INO=1,NBNO
          ZI(IDLIST+INO-1)=ZI(JNOEU+INO-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',ZI(IDLIST+INO-1))
     &        ,NOMNOE)
 20     CONTINUE
      ENDIF
C
C --- FIN
C     ===
C
C --- MENAGE
C
 999  CONTINUE
      CALL JEDETR('&&CGNOXF.FISS')
      CALL JEDETR('&&CGNOXF.NOEU')
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END

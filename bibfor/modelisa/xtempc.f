      SUBROUTINE XTEMPC(NFISS,FISS,FONREE,CHAR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/04/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      NFISS
      CHARACTER*8  FISS(NFISS),CHAR
      CHARACTER*4  FONREE
C
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (AFFE_CHAR_THER)
C
C ANNULER LES DDLS X-FEM ASSOCIES AUX FISSURES FISS(1:NFISS)
C -> AFFE_CHAR_THER   / ECHANGE_PAROI / FISSURE / TEMP_CONTINUE
C -> AFFE_CHAR_THER_F / ECHANGE_PAROI / FISSURE / TEMP_CONTINUE
C
C ----------------------------------------------------------------------
C
C IN     NFISS  : NOMBRE DE FISSURES
C IN     FISS   : LISTE DES NOMS DES FISSURES
C IN     FONREE : 'REEL' OU 'FONC'
C IN-OUT CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C
C ----------------------------------------------------------------------
C
      CHARACTER*32 JEXNUM
      CHARACTER*19 LISREL,STANO
      CHARACTER*8  NOMA,NOMO,K8BID,BETAF,NOMNOE(1),DDLH(1),DDLE(1)
      CHARACTER*4  TYPVAL
      COMPLEX*16   CBID
      INTEGER      NREL,IFISS,JSTANO,IBID,IER,NBNO,INO,ISTAN,NDIM(1)
      REAL*8       RBID,BETAR,COEFR(1)
C
      DATA DDLH /'H1'/
      DATA DDLE /'E1'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      LISREL = '&&CXTEMPC.RLISTE'
      NREL = 0
C
C --- INITIALISATION DES ARGUMENTS IN COMMUNS POUR APPEL A AFRELA
C
      NDIM(1) = 0
      COEFR(1) = 1.D0
      CALL ASSERT(FONREE.EQ.'REEL' .OR. FONREE.EQ.'FONC')
      TYPVAL = FONREE
      BETAR = 0.D0
      BETAF = '&FOZERO'
C
C --- MAILLAGE ET MODELE
C
      CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,NOMO,IER)
      CALL DISMOI('F','NOM_MAILLA',NOMO,'MODELE',IBID,NOMA,IER)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IER)
C
C     ---------------------------------------
C --- BOUCLE SUR LES FISSURES
C     ---------------------------------------
C
      DO 1000 IFISS = 1,NFISS
C
        STANO=FISS(IFISS)//'.STNO'
        CALL JEVEUO(STANO//'.VALE','L',JSTANO)
C
C       -------------------------------------
C ----- BOUCLE SUR LES NOEUDS DU MAILLAGE
C       -------------------------------------
C
        DO 1100 INO= 1,NBNO
          ISTAN = ZI(JSTANO-1+INO)
          IF (ISTAN.GT.0) THEN
            CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMNOE',INO),NOMNOE(1))
C
C           MISE A ZERO DDL HEAVISIDE
            IF (ISTAN.EQ.1) THEN
              CALL AFRELA(COEFR,CBID,DDLH,NOMNOE,NDIM,RBID,1,BETAR,CBID,
     &                    BETAF,'REEL',TYPVAL,'12',0.D0,LISREL)
              NREL = NREL + 1
C
C           MISE A ZERO DDL CRACK-TIP
            ELSE IF (ISTAN.EQ.2) THEN
              CALL AFRELA(COEFR,CBID,DDLE,NOMNOE,NDIM,RBID,1,BETAR,CBID,
     &                    BETAF,'REEL',TYPVAL,'12',0.D0,LISREL)
              NREL = NREL + 1
C
C           MISE A ZERO DDLS HEAVISIDE ET CRACK-TIP
            ELSE IF (ISTAN.EQ.3) THEN
              CALL AFRELA(COEFR,CBID,DDLH,NOMNOE,NDIM,RBID,1,BETAR,CBID,
     &                    BETAF,'REEL',TYPVAL,'12',0.D0,LISREL)
              CALL AFRELA(COEFR,CBID,DDLE,NOMNOE,NDIM,RBID,1,BETAR,CBID,
     &                    BETAF,'REEL',TYPVAL,'12',0.D0,LISREL)
              NREL = NREL + 2
            ELSE
              CALL ASSERT(.FALSE.)
            END IF
C
          END IF
C
 1100   CONTINUE
C       -------------------------------------
C ----- FIN BOUCLE SUR LES NOEUDS DU MAILLAGE
C       -------------------------------------
 1000 CONTINUE
C     ---------------------------------------
C --- FIN BOUCLE SUR LES FISSURES
C     ---------------------------------------
C
C --- AFFECTATION DES RELATIONS LINEAIRES DANS LE LIGREL DE CHARGE
C
      CALL ASSERT(NREL.GT.0)
      CALL AFLRCH(LISREL,CHAR)
C
9999  CONTINUE
      CALL JEDEMA()
      END

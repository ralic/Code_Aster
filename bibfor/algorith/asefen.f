      SUBROUTINE ASEFEN(GLOB, NOMSY,MASSE,ID,STAT,NEQ,NBSUP,NSUPP,
     +           NOMSUP, DEPSUP,ZRCREP )
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER           NSUPP(*)
      REAL*8            DEPSUP(NBSUP,*),ZRCREP(NBSUP,NEQ,*)
      CHARACTER*(*)     STAT,NOMSUP(NBSUP,*),MASSE
      CHARACTER*16      NOMSY
      LOGICAL           GLOB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2002   AUTEUR CIBHHPD D.NUNEZ 
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        CALCUL DES TERMES D'ENTRAINEMENT
C     ------------------------------------------------------------------
C IN  : NOMSY  : OPTION DE CALCUL
C IN  : ID     : LA DIRECTION
C IN  : STAT   : MODE STATIQUES
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : DEPSUP : VECTEUR DES DEPLACEMENTS DES SUPPORTS
C OUT : ZRCREP : VECTEUR DES RECOMBINAISONS MODALES
C     ------------------------------------------------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*4  TYPCMO
      CHARACTER*8  K8B, NOEU, CMP, NOMCMP(3),NOMA,GRNOEU
      CHARACTER*16 MONACC,TYPCOM
      CHARACTER*19 CHEXTR,MOTFAC
      CHARACTER*24 OBJ1, OBJ2
      CHARACTER*80 NOMCAS
      COMPLEX*16   CBID
C     ------------------------------------------------------------------
      DATA  NOMCMP / 'DX' , 'DY' , 'DZ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IRET)
      OBJ1 = NOMA//'.GROUPENO'
      OBJ2 = NOMA//'.NOMNOE'
      NTOU = 0 
      NC = 0
      NG = 0
      NN = 0 
      NNO = 0
      NGR = 0
      NCAS = 0
      NOCAS = 0
      NBNOGR = 0 
      ICOGN = 0
      ICONO = 0 
      CALL GETFAC('COMB_DEPL_APPUI',NOCC)
C
C RECUPERATION DES OCCURENCES COMB_DEPL_APPUI POUR RECUPERER LES 
C LISTES DE CAS.
C
      DO 1 IOC = 1,NOCC
       CALL GETVTX('COMB_DEPL_APPUI','TOUT',IOC,1,0,K8B ,NT)
       NTOU =NTOU-NT
       CALL GETVIS('COMB_DEPL_APPUI','LIST_CAS',IOC,1,0,IBID,NC)
       IF (NC.NE.0) NCAS = NCAS-NC
 1    CONTINUE
C
      IF (GLOB) THEN 
          CALL GETFAC('DEPL_MULT_APPUI',NCAS)
      ENDIF
      IF ((NTOU.EQ.0).OR.GLOB) THEN
        CALL WKVECT('ASEFEN.LISTE','V V I',NCAS,JCAS)
        CALL WKVECT('ASEFEN.LCAS','V V I',NCAS,JLICA)
        JLCAS =JCAS

        IF (GLOB) THEN 
          JCASN = JCAS
          DO 2 II = 1,NCAS
            CALL GETVIS('DEPL_MULT_APPUI','NUME_CAS',II,1,1,
     &           ZI(JCASN),NC)
            JCASN = JCASN +NC
 2        CONTINUE
         ELSE
           DO 11 IOC = 1,NOCC
            CALL GETVIS('COMB_DEPL_APPUI','LIST_CAS',IOC,1,0,IBID,NC)
            IF (NC.NE.0) THEN
              NC = -NC
              CALL GETVIS('COMB_DEPL_APPUI','LIST_CAS',IOC,1,NC,
     &           ZI(JLCAS),NC)
              JLCAS =JLCAS+NC
            ENDIF
 11        CONTINUE
          ENDIF
        DO 18 ICAS = 1, NCAS    
          LCAS = ZI(JCAS+ICAS-1)  
          MOTFAC = 'DEPL_MULT_APPUI'
          CALL GETFAC(MOTFAC,NBOC2)
          DO 20 II =1, NBOC2   
           CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
C ON PARCOURT LES NUME_CAS DE DEPL_MULT_APPUI POUR TROUVER CEUX 
C CONCERNES PAS COMB_MULT_APPUI
           IF (NUCAS.EQ.LCAS) THEN
             NOCAS = NOCAS+1
             ZI(JLICA+ NOCAS-1 ) = NUCAS
C ON A STOCKE LE NUME_CAS TRAITE DANS UN TABLEAU
             CALL GETVID(MOTFAC,'NOEUD',II,1,0,NOEU,NN)
C RECUPERATION DES DIMENSIONS DES DIFFERENTS TABLEAUX DE 
C STOCKAGE DE NOEUDS ET GRNO
              IF (NN.NE.0) THEN
                NNO = NNO-NN
              ELSE
                CALL GETVID(MOTFAC,'GROUP_NO',II,1,0,K8B,NG)
                IF (NG.NE.0) NGR = NGR -NG
             ENDIF
          ENDIF
 20     CONTINUE
 18    CONTINUE
       IF (NNO.NE.0) CALL WKVECT('ASEFEN.NOEUD','V V K8',NNO,JNOE)
       IF (NGR.NE.0) THEN
         CALL WKVECT('ASEFEN.GROUP_NO','V V K8',NGR,JGRN)
         CALL WKVECT('ASEFEN.LIST_GR','V V I',NGR,IDN)
       ENDIF
C
       ICOGN = JGRN
       ICONO = JNOE
       DO 19 ICAS = 1, NOCAS   
        DO 30 II =1, NBOC2   
         CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
C
         IF (NUCAS.EQ.ZI(JLICA +ICAS-1 )) THEN
            IF (NNO.NE.0) THEN
              CALL GETVID(MOTFAC,'NOEUD',II,1,0,ZK8(ICONO),NN)
              IF (NN.NE.0) THEN
                NN = -NN
                CALL GETVID(MOTFAC,'NOEUD',II,1,NN,ZK8(ICONO),NNI)
                ICONO = ICONO+NN
              ENDIF
            ENDIF 
            IF (NGR.NE.0) THEN
              CALL GETVID(MOTFAC,'GROUP_NO',II,1,0,K8B,NG)
              IF (NG.NE.0) THEN
                 NNG = -NG
                 CALL GETVID(MOTFAC,'GROUP_NO',II,1,NNG,
     &                       ZK8(ICOGN),NG)
                 ICOGN = ICOGN+NNG
              ENDIF
             ENDIF
           ENDIF              
 30      CONTINUE
 19     CONTINUE
C      
        DO 39 ICAS = 1, NOCAS   
          DO 50 II =1, NBOC2   
           CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
           IF (NUCAS.EQ.ZI(JLICA+ICAS-1).AND.(NGR.NE.0)) THEN        
            DO 70 IGR = 1, NGR
              GRNOEU = ZK8(JGRN+IGR-1)
              CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONMAX',NN,K8B)
              ZI(IDN-1+IGR) = NN
              NBNOGR = NBNOGR+NN
 70         CONTINUE
           ENDIF
 50      CONTINUE
 39     CONTINUE

      IF (NBNOGR.NE.0) THEN
         CALL WKVECT('ASEFEN.NOEUDGR','V V K8',NBNOGR,INOE)
      ENDIF
        DO 29 ICAS = 1, NOCAS   
          DO 40 II =1, NBOC2   
           CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
           IF (NUCAS.EQ.ZI(JLICA +ICAS-1 )) THEN
             IF (NBNOGR.GT.0) THEN
                  DO 80 IGR = 1, NGR
                  CALL JEVEUO(JEXNOM(OBJ1,GRNOEU),'L',JDGN)
                  DO 90 J = 1,ZI(IDN+IGR-1)
                    CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN-1+J)),NOEU)
                    ZK8(INOE+IGR*J-1) = NOEU
 90               CONTINUE
 80             CONTINUE
              ENDIF
            ENDIF
 40      CONTINUE
 29     CONTINUE
      ENDIF
C
      CMP = NOMCMP(ID)
      DO 10 IS = 1,NSUPP(ID)
       IF (NTOU.NE.0)  NOEU   = NOMSUP(IS,ID)
       IF (NNO.NE.0) THEN
         DO 15 INO =1,NNO
          IF (ZK8(JNOE+INO-1).EQ.NOMSUP(IS,ID)) NOEU =ZK8(JNOE+INO-1)
 15      CONTINUE
       ENDIF
       IF (NGR.NE.0) THEN
         DO 25 INO =1,NBNOGR
           IF (ZK8(INOE+INO-1).EQ.NOMSUP(IS,ID)) NOEU =ZK8(INOE+INO-1)
 25     CONTINUE
       ENDIF

CC RECUPERATION DES LISTE_CAS DE DEPL_MULT_APPUI
           
         MONACC = NOEU//CMP
         XX1    = DEPSUP(IS,ID)
         CALL RSORAC(STAT,'NOEUD_CMP',IBID,R8B,MONACC,CBID,R8B,K8B,
     +                                                  IORDR,1,NBTROU)
         CALL RSEXCH(STAT,NOMSY,IORDR,CHEXTR,IRET)
         CALL JEEXIN(CHEXTR//'.VALE',IBID)
         IF (IBID.GT.0) THEN
           CALL JEVEUO(CHEXTR//'.VALE','L',JVALE)
         ELSE
           CALL JEVEUO(CHEXTR//'.CELV','L',JVALE)
         END IF
         DO 12 IN = 1,NEQ
            XXX = ZR(JVALE+IN-1) * XX1
            ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) + XXX*XXX
 12     CONTINUE
 10   CONTINUE
C
      CALL JEDETC('V','ASEFEN',1)
      CALL JEDEMA()
      END

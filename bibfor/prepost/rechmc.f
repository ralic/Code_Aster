      SUBROUTINE RECHMC( NDIM, TEMPS, ORIDEF, TABREV, TABMDB,
     +                   NOREV, SIGMRV, NOMDB, SIGMDB)
C
      IMPLICIT     NONE
      INTEGER      NDIM, NBNO, NOREV, NOMDB
      REAL*8       TEMPS
      CHARACTER*8  ORIDEF, TABREV, TABMDB
      CHARACTER*19 SIGMRV, SIGMDB
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DES CHAMPS DES CONTRAINTES MECANIQUES ASSOCIES AU ---
C ------- : REVETEMENT ET AU METAL DE BASE -----------------------------
C ======================================================================
C IN  : NDIM   : DIMENSION DE L'ESPACE ---------------------------------
C --- : NBNO   : NOMBRE DE NOEUDS --------------------------------------
C --- : ORIDEF : ORIENTATION DU DEFAUT ---------------------------------
C --- : TABREV : TABLE ASSOCIEE AU REVETEMENT --------------------------
C --- : TABMDB : TABLE ASSOCIEE AU METAL DE BASE -----------------------
C OUT : NOREV  : NOMBRE DE NOEUDS COTE REVETEMENT ----------------------
C --- : SIGMRV : CHAMP DES CONTRAINTES ASSOCIE AU REVETEMENT -----------
C --- : NOMDB  : NOMBRE DE NOEUDS COTE METAL DE BASE -------------------
C --- : SIGMDB : CHAMP DES CONTRAINTES ASSOCIE AU METAL DE BASE --------
C ======================================================================
C ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----- FIN COMMUNS NORMALISES  JEVEUX  -------------------------------
C ======================================================================
      INTEGER       JSIGMR, JSIGMB, IBID, II, JCOORX, JCOORY, IRET
      INTEGER       JREVXX, JREVYY, JREVXY, JMDBXX, JMDBYY, JMDBXY
      REAL*8        LPREC, RT, COST, SINT, SICOT
      COMPLEX*16    CBID
      CHARACTER*8   LCRIT, K8B
      CHARACTER*19  TMPREV, TMPMDB, COORXX, COORYY
      CHARACTER*19  REVXX, REVYY, REVXY, MDBXX, MDBYY, MDBXY
      CHARACTER*24  VALK(2)
C ======================================================================
      CALL JEMARQ()
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      LCRIT  = 'RELATIF'
      LPREC  = 1.0D-06
      TMPREV = '&&RECHMC.TMPREV'
      TMPMDB = '&&RECHMC.TMPMDB'
      REVXX  = '&&RECHMC.REVXX'
      REVYY  = '&&RECHMC.REVYY'
      REVXY  = '&&RECHMC.REVXY'
      MDBXX  = '&&RECHMC.MDBXX'
      MDBYY  = '&&RECHMC.MDBYY'
      MDBXY  = '&&RECHMC.MDBXY'
      COORXX = '&&RECHMC.COORXX'
      COORYY = '&&RECHMC.COORYY'
C ======================================================================
C --- RECUPERATION DES SOUS-TABLES ASSOCIEES A L'INSTANT COURANT -------
C ======================================================================
      CALL TBEXTB (TABREV, 'V', TMPREV, 1, 'INST', 'EQ',
     +             IBID, TEMPS, CBID, K8B, LPREC, LCRIT, IRET )
      IF ( IRET .EQ. 10 ) THEN
         VALK(1) = 'INST'
         VALK(2) = TABREV
         CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
      ELSEIF ( IRET .EQ. 20 ) THEN
         VALK(1) = TABREV
         VALK(2) = 'INST'
         CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
      ENDIF
      CALL TBEXTB (TABMDB, 'V', TMPMDB, 1, 'INST', 'EQ',
     +             IBID, TEMPS, CBID, K8B, LPREC, LCRIT, IRET )
      IF ( IRET .EQ. 10 ) THEN
         VALK(1) = 'INST'
         VALK(2) = TABMDB
         CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
      ELSEIF ( IRET .EQ. 20 ) THEN
         VALK(1) = TABMDB
         VALK(2) = 'INST'
         CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
      ENDIF
C ======================================================================
C --- PROBLEME EN DIMENSION 2 ------------------------------------------
C ======================================================================
      IF ( NDIM.EQ.2 ) THEN
         IF ( ORIDEF.EQ.'CIRC' ) THEN
C ======================================================================
C --- RECUPERATION DE LA LISTE DE CONTRAINTE SIYY COTE REVETEMENT ------
C ======================================================================
            CALL TBEXVE ( TMPREV, 'SIYY', SIGMRV, 'V', NOREV, K8B )
C ======================================================================
C --- RECUPERATION DE LA LISTE DE CONTRAINTE SIYY COTE METAL DE BASE ---
C ======================================================================
            CALL TBEXVE ( TMPMDB, 'SIYY', SIGMDB, 'V', NOMDB, K8B )
         ELSE
C ======================================================================
C --- RECUPERATION DE LA LISTE DE CONTRAINTE SIZZ COTE REVETEMENT ------
C ======================================================================
            CALL TBEXVE ( TMPREV, 'SIZZ', SIGMRV, 'V', NOREV, K8B )
C ======================================================================
C --- RECUPERATION DE LA LISTE DE CONTRAINTE SIZZ COTE METAL DE BASE ---
C ======================================================================
            CALL TBEXVE ( TMPMDB, 'SIZZ', SIGMDB, 'V', NOMDB, K8B )
         ENDIF
C ======================================================================
C --- PROBLEME EN DIMENSION 3 ------------------------------------------
C ======================================================================
      ELSE
         IF ( ORIDEF.EQ.'CIRC' ) THEN
C ======================================================================
C --- RECUPERATION DE LA LISTE DE CONTRAINTE SIZZ COTE REVETEMENT ------
C ======================================================================
            CALL TBEXVE ( TMPREV, 'SIZZ', SIGMRV, 'V', NOREV, K8B )
C ======================================================================
C --- RECUPERATION DE LA LISTE DE CONTRAINTE SIZZ COTE METAL DE BASE ---
C ======================================================================
            CALL TBEXVE ( TMPMDB, 'SIZZ', SIGMDB, 'V', NOMDB, K8B )
         ELSE
C ======================================================================
C --- PASSAGE DE LA BASE CARTESIENNE (MODELE 3D) A LA BASE -------------
C --- CYLINDRIQUE ------------------------------------------------------
C ======================================================================
            CALL WKVECT ( SIGMRV, 'V V R8', NOREV, JSIGMR )
            CALL WKVECT ( SIGMDB, 'V V R8', NOMDB, JSIGMB )
            CALL TBEXVE ( TMPREV, 'COOR_X', COORXX, 'V', NOREV, K8B )
            CALL TBEXVE ( TMPREV, 'COOR_Y', COORYY, 'V', NOREV, K8B )
            CALL JEVEUO ( COORXX , 'L', JCOORX )
            CALL JEVEUO ( COORYY , 'L', JCOORY )
            RT    =     ZR(JCOORX)*ZR(JCOORX) + ZR(JCOORY)*ZR(JCOORY)
            COST  =     ZR(JCOORX)*ZR(JCOORX) / RT
            SINT  =     ZR(JCOORY)*ZR(JCOORY) / RT
            SICOT = 2 * ZR(JCOORX)*ZR(JCOORY) / RT
C ======================================================================
C --- RECUPERATION DES LISTES DE CONTRAINTE SIXX - SIYY - SIXY ---------
C --- COTE REVETEMENT --------------------------------------------------
C ======================================================================
            CALL TBEXVE ( TMPREV, 'SIXX', REVXX, 'V', NOREV, K8B )
            CALL TBEXVE ( TMPREV, 'SIYY', REVYY, 'V', NOREV, K8B )
            CALL TBEXVE ( TMPREV, 'SIXY', REVXY, 'V', NOREV, K8B )
            CALL JEVEUO ( REVXX , 'L', JREVXX )
            CALL JEVEUO ( REVYY , 'L', JREVYY )
            CALL JEVEUO ( REVXY , 'L', JREVXY )
            DO 10 II = 1,NOREV
               ZR(JSIGMR-1+II) =  SINT * ZR(JREVXX-1+II) +
     +                            COST * ZR(JREVYY-1+II) -
     +                            SICOT* ZR(JREVXY-1+II)
 10         CONTINUE
C ======================================================================
C --- RECUPERATION DES LISTES DE CONTRAINTE SIXX - SIYY - SIXY ---------
C --- COTE METAL DE BASE -----------------------------------------------
C ======================================================================
            CALL TBEXVE ( TMPMDB, 'SIXX', MDBXX, 'V', NOMDB, K8B )
            CALL TBEXVE ( TMPMDB, 'SIYY', MDBYY, 'V', NOMDB, K8B )
            CALL TBEXVE ( TMPMDB, 'SIXY', MDBXY, 'V', NOMDB, K8B )
            CALL JEVEUO ( MDBXX , 'L', JMDBXX )
            CALL JEVEUO ( MDBYY , 'L', JMDBYY )
            CALL JEVEUO ( MDBXY , 'L', JMDBXY )
            DO 20 II = 1,NOMDB
               ZR(JSIGMB-1+II) =  SINT * ZR(JMDBXX-1+II) +
     +                            COST * ZR(JMDBYY-1+II) -
     +                            SICOT* ZR(JMDBXY-1+II)
 20         CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- DESTRUCTION DES TABLES INUTILES ----------------------------------
C ======================================================================
      CALL JEDETC('V','&&RECHMC',1)
C ======================================================================
      CALL JEDEMA( )
C ======================================================================
      END

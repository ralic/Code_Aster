      SUBROUTINE ASCALC ( RESU, MASSE, MOME, PSMO, STAT, NBMODE, NEQ,
     +                    NORDR, KNOMSY, NBOPT, NDIR, MONOAP, MUAPDE,
     +                    NBSUP, NSUPP, TYPCMO, TEMPS, COMDIR, TYPCDI,
     +                    TRONC, AMORT, SPECTR, ASSPEC, NOMSUP, REASUP,
     +                    DEPSUP, TCOSUP, CORFRE )
      IMPLICIT  NONE
      INTEGER       NDIR(*),TCOSUP(*),NORDR(*),NSUPP(*)
      REAL*8        AMORT(*),SPECTR(*),ASSPEC(*),DEPSUP(*),REASUP(*)
      CHARACTER*(*) RESU,MASSE,MOME,PSMO,STAT,TYPCMO,TYPCDI,
     +              KNOMSY(*),NOMSUP(*)
      LOGICAL       MONOAP, MUAPDE, COMDIR, TRONC, CORFRE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/10/2008   AUTEUR DURAND C.DURAND 
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
C TOLE CRP_21
C     ------------------------------------------------------------------
C
C     UTILISE PAR LA COMMANDE : COMB_SISM_MODAL
C
C     ------------------------------------------------------------------
C IN  : RESU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : MASSE  : MATRICE ASSEMBLEE
C IN  : MOME   : MODES MECANIQUES
C IN  : PSMO   : PSEUDO-MODES (SI PRISE EN COMPTE DE LA TRONCATURE)
C IN  : STAT   : MODE STATIQUES (CAS MULTI-SUPPORT)
C IN  : NBMODE : NOMBRE DE MODES
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NORDR  : NUMERO D'ORDRE DES MODES MECANIQUES
C IN  : KNOMSY : LES OPTIONS DE CALCUL
C IN  : NBOPT  : NOMBRE D'OPTION DE CALCUL
C IN  : NDIR   : DIRECTIONS DE CALCUL
C IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
C                =.FALSE. , CAS DU MULTI-SUPPORT
C IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
C                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
C IN  : NBSUP  : NOMBRE DE SUPPORT
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : TYPCMO : TYPE DE RECOMBINAISON DES MODES
C IN  : TEMPS  : DUREE FORTE DU SEISME (TYPCMO='DSC')
C IN  : COMDIR : =.TRUE.  , COMBINAISON DES DIRECTIONS
C                =.FALSE. , PAS DE COMBINAISON DES DIRECTIONS
C IN  : TYPCDI : TYPE DE COMBINAISON DES DIRECTIONS
C IN  : TRONC  : =.TRUE.  , PRISE EN COMPTE DE LA TRONCATURE
C                =.FALSE. , PAS DE PRISE EN COMPTE DE LA TRONCATURE
C IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
C IN  : SPECTR : VECTEUR DES SPECTRES MODAUX
C IN  : ASSPEC : VECTEUR DES ASYMPTOTES DES SPECTRES AUX SUPPORTS
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
C IN  : DEPSUP : VECTEUR DES DEPLACEMENTS DES SUPPORTS
C IN  : TCOSUP : TYPE DE RECOMBINAISON DES SUPPORTS
C IN  : CORFRE : =.TRUE.  , CORRECTION DES FREQUENCES
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IBID, ID, IOPT, IRET, JCREP, JDIR, JMOD, JREP1, 
     &              JTABS, JVAL, NBMODE, NBOPT, NBPARA, NBPARI, NBPARK,
     &              NBPARR, NBSUP, NDEPL, NEQ, JREP2
      PARAMETER     ( NBPARA = 5 )
      REAL*8        TEMPS
      LOGICAL       PRIM, SECON, GLOB
      CHARACTER*4   CTYP
      CHARACTER*8   K8B, NUME
      CHARACTER*16  NOMSY, NOMSY2, NOPARA(NBPARA)
      CHARACTER*19  KVEC, KVAL
      CHARACTER*24  KVX1, KVX2, KVE2, KVE3, KVE4
C
      DATA  NOPARA /        'OMEGA2'          , 'MASS_GENE'       ,
     +  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ'  /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      KVEC = '&&ASCALC.VAL_PROPRE'
      KVAL = '&&ASCALC.GRAN_MODAL'
      KVX1 = '&&ASCALC.REP_MO1'
      KVX2 = '&&ASCALC.REP_MO2'
      KVE2 = '&&ASCALC.C_REP_MOD'
      KVE3 = '&&ASCALC.REP_DIR' 
      KVE4 = '&&ASCALC.TABS'   
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IBID,NUME,IRET)
C
      CALL GETFAC ( 'COMB_DEPL_APPUI', NDEPL )
      IF ( NDEPL .NE. 0 ) THEN
         PRIM = .TRUE.
         SECON =.TRUE.
         GLOB = .FALSE.
      ELSE
         PRIM = .FALSE.
         SECON =.FALSE.
         GLOB = .TRUE.
      ENDIF
C
C     --- BOUCLE SUR LES OPTIONS DE CALCUL "NOMSY" ---
      DO 10 IOPT = 1,NBOPT
         NOMSY = KNOMSY(IOPT)
         NOMSY2 = NOMSY
         IF (NOMSY(1:4).EQ.'VITE') NOMSY2 = 'DEPL'
         IF (NOMSY(1:4).EQ.'ACCE') NOMSY2 = 'DEPL'
         CALL VPRECU ( MOME, NOMSY2, NBMODE, NORDR, KVEC, 
     +                 NBPARA, NOPARA, K8B, KVAL, K8B,
     +                 NEQ, NBMODE, CTYP, NBPARI, NBPARR, NBPARK )
         CALL JEVEUO(KVEC,'L',JMOD)
         CALL JEVEUO(KVAL,'L',JVAL)
         CALL WKVECT(KVX1, 'V V R', 3*NEQ*NBSUP,JREP1)
         CALL WKVECT(KVX2, 'V V R', 3*NEQ*NBSUP,JREP2)
         CALL WKVECT(KVE2, 'V V R', 3*NEQ*NBSUP,JCREP)
         CALL WKVECT(KVE3, 'V V R',       3*NEQ,JDIR )
         CALL WKVECT(KVE4, 'V V R',   NBSUP*NEQ,JTABS)
C
C        ---------------------------------------------------------------
C                        REPONSE PRIMAIRE OU GLOBAL
C        ---------------------------------------------------------------
C
C        --- BOUCLE SUR LES DIRECTIONS ----
         DO 20 ID = 1,3
            IF (NDIR(ID).EQ.1) THEN
C
C              --- CALCUL DES REPONSE MODALES ---
C
C              --- COMBINAISON DES REPONSES MODALES ---
               CALL ASCORM ( MONOAP, TYPCMO, NBSUP, NSUPP, NEQ, 
     +                       NBMODE, ZR(JREP1), ZR(JREP2), AMORT, 
     +                       ZR(JVAL), ID, TEMPS, ZR(JCREP), ZR(JTABS),
     +                       NOMSY, ZR(JMOD), REASUP, SPECTR, CORFRE,
     +                       MUAPDE, TCOSUP)
C
C              --- PRISE EN COMPTE DES EFFETS D'ENTRAINEMENT ---
C              --- DANS LE CAS DE CALCUL DE REPONSE GLOBALE  ---
C
               IF ( (.NOT.MONOAP) .AND. GLOB ) THEN
                  CALL ASEFEN ( MUAPDE, NOMSY2, ID, STAT, NEQ, NBSUP,
     +                          NDIR, NSUPP, MASSE, NOMSUP, DEPSUP,
     +                          ZR(JCREP))
               ENDIF
C
C              ----CALCUL DE L ACCELERATION ABSOLUE
C
               CALL ASACCE ( NOMSY, MONOAP, MUAPDE, NBSUP, NEQ, NBMODE,
     +                       ID, NUME, ZR(JMOD), ZR(JVAL), ASSPEC,
     +                       ZR(JCREP) )

C
C              --- PRISE EN COMPTE DE LA TRONCATURE ---
C              --- DANS LE CAS DE CALCUL DE REPONSE GLOBALE  ---

               IF ( TRONC ) THEN
                  CALL ASTRON ( NOMSY, PSMO, MONOAP, MUAPDE, NBSUP,
     +                          NSUPP, NEQ, NBMODE, ID, ZR(JMOD),
     +                          ZR(JVAL), ASSPEC, NOMSUP, REASUP,
     +                          ZR(JCREP) )
               ENDIF
C
C              --- CALCUL DES RECOMBINAISONS PAR DIRECTIONS---
               CALL ASDIR ( MONOAP, MUAPDE, ID, NEQ, NBSUP, NSUPP,
     +                      TCOSUP, ZR(JCREP), ZR(JDIR) )
            ENDIF
 20      CONTINUE
C
C        --- STOCKAGE ---
C
         CALL ASSTOC ( MOME, RESU, NOMSY, NEQ, ZR(JDIR), NDIR,
     +                 COMDIR, TYPCDI, GLOB, PRIM )
C
C        ---------------------------------------------------------------
C                            REPONSE SECONDAIRE
C        ---------------------------------------------------------------
         IF ( SECON ) THEN
C
C            --- PRISE EN COMPTE DES EFFETS D'ENTRAINEMENT ---
C            --- DANS LE CAS DE CALCUL DE REPONSE GLOBALE  ---
C
            IF ( NOMSY(1:11) .NE. 'ACCE_ABSOLU' ) THEN 
               CALL ASECON ( NOMSY, NEQ, MOME, RESU )
            ENDIF
C
         ENDIF
C
         CALL JEDETR ( KVEC )
         CALL JEDETR ( KVAL )
         CALL JEDETR ( KVX1 )
         CALL JEDETR ( KVX2 )
         CALL JEDETR ( KVE2 )
         CALL JEDETR ( KVE3 )
         CALL JEDETR ( KVE4 )

 10   CONTINUE
C
      CALL JEDEMA()
      END

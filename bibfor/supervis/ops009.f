      SUBROUTINE OPS009 ( ICMD , ICOND , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 11/02/98   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_20
C     MACRO_MADMACS
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
C     ------------------------------------------------------------------
      INTEGER      VERSIO
      CHARACTER*8  K8B, MAILLA, NUMDDL, RIGID, MASSE, AMORT, MODES
      CHARACTER*8  FORMAT, TYPE, NOMDID, NOMDBM, NOMMED
      CHARACTER*16 FICHIE, NOMCMD
C     ------------------------------------------------------------------
      IF (ICOND .NE. -1) GOTO 9999
C     ------------------------------------------------------------------
      CALL JEMARQ ( )
      CALL GETRES ( K8B, K8B, NOMCMD )
      ISIX = 6
C
      IFICH = 0
      CALL GETVTX(' ','FICHIER'   ,1,1,1,FICHIE,N0)
      IF ( N0 .NE. 0 ) THEN
        CALL GETLTX(' ','FICHIER' ,1,1,1,IFICH,N0)
      ENDIF
C
      CALL GETVTX(' ','FORMAT'    ,1,1,1,FORMAT,N0)
      CALL GETLTX(' ','FORMAT'    ,1,1,1,IFORMA,N0)
C
      CALL GETVIS(' ','VERSION'   ,1,1,1,VERSIO,N0)
C
      CALL GETVID(' ','MAILLAGE'  ,1,1,1,MAILLA,N1)
C
      CALL GETVID(' ','NUME_DDL'  ,1,1,1,NUMDDL,N2)
C
      CALL GETVID(' ','CHARGE'    ,1,1,0,K8B   ,N3)
      IF ( N3 .NE. 0 ) THEN
         NBCHAR = -N3
         CALL WKVECT('&&OPS009.CHARGES','V V K8',NBCHAR,LCHA)
         CALL GETVID(' ','CHARGE',1,1,NBCHAR,ZK8(LCHA),N3)
      ENDIF
C
      IELEMR = 0
      CALL GETVID(' ','MATR_ELEM_RIGI'  ,1,1,1,RIGID,N4)
      IF ( N4 .NE. 0 ) IELEMR = 1
      CALL GETVID(' ','MATR_RIGI'       ,1,1,1,RIGID,N4)
C
      IELEMM = 0
      CALL GETVID(' ','MATR_ELEM_MASS'  ,1,1,1,MASSE,N5)
      IF ( N5 .NE. 0 ) IELEMM = 1
      CALL GETVID(' ','MATR_MASS'       ,1,1,1,MASSE,N5)
C
      IELEMC = 0
      IASSEC = 0
      CALL GETVID(' ','MATR_ELEM_AMOR'  ,1,1,1,AMORT,N6)
      IF ( N6 .NE. 0 ) IELEMC = 1
      CALL GETVID(' ','MATR_AMOR'       ,1,1,1,AMORT,N6)
      IF ( N6 .NE. 0 ) IASSEC = 1
C
      CALL GETVID(' ','MODE_MECA' ,1,1,1,MODES,N7)
      CALL GETVIS(' ','NMAX_MODE' ,1,1,1,NMAXM,N7)
C
C     --- INTERFACE ---
C
      CALL GETFAC('INTERFACE',NBINTE)
      IF ( NBINTE .NE. 0 ) THEN
         INOE = 0
         IDDL = 0
         DO 10 IOCC = 1 , NBINTE
            CALL GETVID('INTERFACE','NOEUD'    ,IOCC,1,0,K8B,L1)
            CALL GETVID('INTERFACE','GROUP_NO' ,IOCC,1,0,K8B,L2)
            CALL GETVTX('INTERFACE','DDL_ACTIF',IOCC,1,0,K8B,L3)
            CALL GETVTX('INTERFACE','MASQUE'   ,IOCC,1,0,K8B,L4)
            INOE = INOE - L1 - L2
            IDDL = IDDL - L3 - L4
 10      CONTINUE
         CALL WKVECT('&&OPS009.NOM'   ,'V V K24',NBINTE,LNOM)
         CALL WKVECT('&&OPS009.LG_NOM','V V I',  NBINTE,JLGN)
         CALL WKVECT('&&OPS009.NOE'   ,'V V I'  ,NBINTE,JNOE)
         CALL WKVECT('&&OPS009.DDL'   ,'V V I'  ,NBINTE,JDDL)
         CALL WKVECT('&&OPS009.NOMNOE','V V K8' ,INOE  ,LNOE)
         CALL WKVECT('&&OPS009.NOMDDL','V V K8' ,IDDL  ,LDDL)
         CALL WKVECT('&&OPS009.LG_DDL','V V I'  ,IDDL  ,JLGD)
         INOE = 1
         IDDL = 1
         DO 20 IOCC = 1 , NBINTE
            CALL GETVTX('INTERFACE','NOM',IOCC,1,1,ZK24(LNOM+IOCC-1),L0)
            CALL GETLTX('INTERFACE','NOM',IOCC,1,1,ZI(JLGN+IOCC-1),L0)
            CALL GETVID('INTERFACE','NOEUD',IOCC,1,0,K8B,L1)
            IF ( L1 .NE. 0 ) THEN
              NBNO = -L1
              ZI(JNOE+IOCC-1) = NBNO
              CALL GETVID('INTERFACE','NOEUD',IOCC,1,NBNO,
     +                                        ZK8(LNOE+INOE-1),L1)
              INOE = INOE + NBNO
            ELSE
              CALL GETVID('INTERFACE','GROUP_NO',IOCC,1,0,K8B,L2)
              NBGN = -L2
              ZI(JNOE+IOCC-1) = -NBGN
              CALL GETVID('INTERFACE','GROUP_NO',IOCC,1,NBGN,
     +                                        ZK8(LNOE+INOE-1),L2)
              INOE = INOE + NBGN
            ENDIF
            CALL GETVTX('INTERFACE','DDL_ACTIF',IOCC,1,0,K8B,L3)
            IF ( L3 .NE. 0 ) THEN
              NBAC = -L3
              ZI(JDDL+IOCC-1) = NBAC
              CALL GETVTX('INTERFACE','DDL_ACTIF',IOCC,1,NBAC,
     +                                        ZK8(LDDL+IDDL-1),L3)
              CALL GETLTX('INTERFACE','DDL_ACTIF',IOCC,1,NBAC,
     +                                        ZI(JLGD+IDDL-1),L3)
              IDDL = IDDL + NBAC
            ELSE
              CALL GETVTX('INTERFACE','MASQUE',IOCC,1,0,K8B,L4)
              NBMA = -L4
              ZI(JDDL+IOCC-1) = -NBMA
              CALL GETVTX('INTERFACE','MASQUE',IOCC,1,NBMA,
     +                                        ZK8(LDDL+IDDL-1),L4)
              CALL GETLTX('INTERFACE','MASQUE',IOCC,1,NBMA,
     +                                        ZI(JLGD+IDDL-1),L4)
              IDDL = IDDL + NBMA
            ENDIF
 20      CONTINUE
      ENDIF
C
C     ---------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C     ---------------------------------------------------------------
      IF ( NBINTE .NE. 0 ) THEN
        TYPE   = 'CRAIGB'
C
C       --- COMMANDE DEFI_INTERF_DYNA ---
C
        INOE = 1
        IDDL = 1
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , NOMDID )
        CALL SMDCMD(ICMD,NOMDID,'DEFI_INTERF_DYNA',IERUSR)
          CALL PUTVID('NUME_DDL',1,NUMDDL,IERUSR)
          DO 100 INT = 1 , NBINTE
            CALL SMDMCF('INTERFACE',IERUSR)
            CALL PUTVTX('NOM',1,ZK24(LNOM+INT-1),ZI(JLGN+INT-1),IERUSR)
            CALL PUTVTX('TYPE',1,TYPE,ISIX,IERUSR)
            IF ( ZI(JNOE+INT-1) .GT. 0 ) THEN
              NBNO = ZI(JNOE+INT-1)
              CALL PUTVID('NOEUD',NBNO,ZK8(LNOE+INOE-1),IERUSR)
              INOE = INOE + NBNO
            ELSE
              NBGR = -ZI(JNOE+INT-1)
              CALL PUTVID('GROUP_NO',NBGR,ZK8(LNOE+INOE-1),IERUSR)
              INOE = INOE + NBGR
            ENDIF
            IF ( ZI(JDDL+INT-1) .GT. 0 ) THEN
              NBAC = ZI(JDDL+INT-1)
              CALL PUTVTX('DDL_ACTIF',NBAC,ZK8(LDDL+IDDL-1),
     +                                     ZI(JLGD+IDDL-1),IERUSR)
              IDDL = IDDL + NBAC
            ELSE
              NBMA = -ZI(JDDL+INT-1)
              CALL PUTVTX('MASQUE',NBMA,ZK8(LDDL+IDDL-1),
     +                                  ZI(JLGD+IDDL-1),IERUSR)
              IDDL = IDDL + NBMA
            ENDIF
            CALL SMFMCF(IERUSR)
 100      CONTINUE
        CALL SMFCMD(IERUSR)
C
C       --- COMMANDE DEFI_BASE_MODALE ---
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , NOMDBM )
        CALL SMDCMD(ICMD,NOMDBM,'DEFI_BASE_MODALE',IERUSR)
          CALL SMDMCF('CLASSIQUE',IERUSR)
            CALL PUTVID('INTERF_DYNA',1,NOMDID,IERUSR)
            CALL PUTVID('MODE_MECA',1,MODES,IERUSR)
            CALL PUTVIS('NMAX_MODE',1,NMAXM,IERUSR)
          CALL SMFMCF(IERUSR)
        CALL SMFCMD(IERUSR)
C
C       --- COMMANDE MACR_ELEM_DYNA ---
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , NOMMED )
        CALL SMDCMD(ICMD,NOMMED,'MACR_ELEM_DYNA',IERUSR)
          CALL PUTVID('BASE_MODALE',1,NOMDBM,IERUSR)
        CALL SMFCMD(IERUSR)
C
      ENDIF
C
C     --- COMMANDE IMPR_RESU POUR TRANSFERER LE MAILLAGE ---
C
      ICMD = ICMD + 1
      K8B = '        '
      CALL SMDCMD(ICMD,K8B,'IMPR_RESU',IERUSR)
        CALL SMDMCF('RESU',IERUSR)
          IF ( IFICH .NE. 0 ) THEN
            CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
          ENDIF
          CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
          CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
          CALL PUTVID('MAILLAGE',1,MAILLA       ,IERUSR)
        CALL SMFMCF(IERUSR)
      CALL SMFCMD(IERUSR)
C
C     --- COMMANDE IMPR_CHARGE POUR TRANSFERER LE CHARGEMENT ---
C
      ICMD = ICMD + 1
      K8B = '        '
      CALL SMDCMD(ICMD,K8B,'IMPR_CHARGE',IERUSR)
        IF ( IFICH .NE. 0 ) THEN
          CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
        ENDIF
        CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
        CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
        CALL PUTVID('CHARGE' ,NBCHAR,ZK8(LCHA),IERUSR)
      CALL SMFCMD(IERUSR)
C
C     --- COMMANDE IMPR_MATRICE POUR TRANSFERER LES MATRICES ---
C
      ICMD = ICMD + 1
      K8B = '        '
      CALL SMDCMD(ICMD,K8B,'IMPR_MATRICE',IERUSR)
        IF ( IELEMR .EQ. 1 ) THEN
           CALL SMDMCF('MATR_ELEM',IERUSR)
             IF ( IFICH .NE. 0 ) THEN
               CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
             ENDIF
             CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
             CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
             CALL PUTVID('MATRICE',1,RIGID,IERUSR)
           CALL SMFMCF(IERUSR)
        ELSE
           CALL SMDMCF('MATR_ASSE',IERUSR)
             IF ( IFICH .NE. 0 ) THEN
               CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
             ENDIF
             CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
             CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
             CALL PUTVID('MATRICE',1,RIGID,IERUSR)
           CALL SMFMCF(IERUSR)
        ENDIF
        IF ( IELEMM .EQ. 1 ) THEN
           CALL SMDMCF('MATR_ELEM',IERUSR)
             IF ( IFICH .NE. 0 ) THEN
               CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
             ENDIF
             CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
             CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
             CALL PUTVID('MATRICE',1,MASSE,IERUSR)
           CALL SMFMCF(IERUSR)
        ELSE
           CALL SMDMCF('MATR_ASSE',IERUSR)
             IF ( IFICH .NE. 0 ) THEN
               CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
             ENDIF
             CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
             CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
             CALL PUTVID('MATRICE',1,MASSE,IERUSR)
           CALL SMFMCF(IERUSR)
        ENDIF
        IF ( IELEMC .EQ. 1 ) THEN
           CALL SMDMCF('MATR_ELEM',IERUSR)
             IF ( IFICH .NE. 0 ) THEN
               CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
             ENDIF
             CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
             CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
             CALL PUTVID('MATRICE',1,AMORT,IERUSR)
           CALL SMFMCF(IERUSR)
        ENDIF
        IF ( IASSEC .EQ. 1 ) THEN
           CALL SMDMCF('MATR_ASSE',IERUSR)
             IF ( IFICH .NE. 0 ) THEN
               CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
             ENDIF
             CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
             CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
             CALL PUTVID('MATRICE',1,AMORT,IERUSR)
           CALL SMFMCF(IERUSR)
        ENDIF
      CALL SMFCMD(IERUSR)
C
      IF ( NBINTE .EQ. 0 ) THEN
C
C       --- COMMANDE IMPR_RESU POUR TRANSFERER LES MODES ---
C
        ICMD = ICMD + 1
        K8B = '        '
        CALL SMDCMD(ICMD,K8B,'IMPR_RESU',IERUSR)
          CALL SMDMCF('RESU',IERUSR)
            IF ( IFICH .NE. 0 ) THEN
              CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
            ENDIF
            CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
            CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
            CALL PUTVID('RESULTAT',1,MODES        ,IERUSR)
          CALL SMFMCF(IERUSR)
        CALL SMFCMD(IERUSR)
C
      ELSE
C
C       --- COMMANDE IMPR_MACR_ELEM POUR TRANSFERER LE RESTE ---
C
        ICMD = ICMD + 1
        K8B = '        '
        CALL SMDCMD(ICMD,K8B,'IMPR_MACR_ELEM',IERUSR)
          IF ( IFICH .NE. 0 ) THEN
             CALL PUTVTX('FICHIER',1,FICHIE,IFICH,IERUSR)
          ENDIF
          CALL PUTVTX('FORMAT'  ,1,FORMAT,IFORMA,IERUSR)
          CALL PUTVIS('VERSION' ,1,VERSIO       ,IERUSR)
          CALL PUTVID('MACR_ELEM_DYNA',1,NOMMED,IERUSR)
        CALL SMFCMD(IERUSR)
C
      ENDIF
C     ---------------------------------------------------------------
C
C     ---  DESTRUCTION DES OBJETS DE TRAVAIL ---
C
      CALL JEDETR ( '&&OPS009.CHARGES' )
      IF ( NBINTE .NE. 0 ) THEN
         CALL JEDETR ( '&&OPS009.NOM'    )
         CALL JEDETR ( '&&OPS009.LG_NOM' )
         CALL JEDETR ( '&&OPS009.NOE'    )
         CALL JEDETR ( '&&OPS009.DDL'    )
         CALL JEDETR ( '&&OPS009.LG_DDL' )
         CALL JEDETR ( '&&OPS009.NOMNOE' )
         CALL JEDETR ( '&&OPS009.NOMDDL' )
      ENDIF
C
      IF ( IERUSR .GT. 0 ) THEN
         CALL UTMESS('E',NOMCMD,'ERREURS CONSTATEES DANS LA MACRO')
         IER = IER + IERUSR
      ENDIF
C
      CALL JEDEMA ( )
 9999 CONTINUE
      END

      SUBROUTINE OPS015 ( ICMD , ICOND , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 30/06/98   AUTEUR CIBHHGB G.BERTRAND 
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
C     MACRO_CHAR_F_U
C     BUT : PRODUIRE UNE CHARGE CINEMATIQUE VENANT DE LIAISON_CHAMNO
C           EN VUE D'UN CALCUL D'UNE CHARGE LIMITE
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
      INTEGER      N1, N2, N3, N4, N5, N6, N7
      INTEGER      ICARA, INFO
      INTEGER      IERUSR, I6B, I9B
      REAL*8       UN
      REAL*8       COEF
      CHARACTER*6  NUMLAG
      CHARACTER*8  NOMRES
      CHARACTER*8  MODELE, CHMAT, CARAEL, CHARGE
      CHARACTER*8  VECEL, MATEL, NUMDDL, CHAMNO
      CHARACTER*9  K9B
      CHARACTER*16 NOMCMD, TYPRES
C     ------------------------------------------------------------------
      IF ( ICOND .NE. -1 ) GOTO 9999
C     ------------------------------------------------------------------
      CALL JEMARQ ( )
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
      UN        = 1.0D0
C
C --- RECUPERATION DU MODELE (MOT CLE OBLIGATOIRE) :
C     --------------------------------------------
      CALL GETVID(' ','MODELE'  ,1,1,1,MODELE,N1)
C
C --- RECUPERATION DE LA CHARGE (MOT CLE OBLIGATOIRE) :
C     -----------------------------------------------
      CALL GETVID(' ','CHARGE'  ,1,1,1,CHARGE,N2)
C
C --- RECUPERATION DU CHAM_MATER (MOT CLE OBLIGATOIRE) :
C     ------------------------------------------------
      CALL GETVID(' ','CHAM_MATER'  ,1,1,1,CHMAT,N3)
C
C --- RECUPERATION DU CARA_ELEM :
C     -------------------------
      ICARA = 0
      CALL GETVID(' ','CARA_ELEM'  ,1,1,1,CARAEL,N4)
      IF (N4.NE.0) THEN
         ICARA = 1
      ENDIF
C
C --- RECUPERATION DE LA VALEUR DU SECOND MEMBRE DE LA
C --- RELATION CINEMATIQUE A IMPOSER :
C     ------------------------------
      CALL GETVR8(' ','COEF_IMPO',1,1,1,COEF,N5)
      IF (N5.EQ.0) THEN
        COEF = UN
      ENDIF
C
C --- RECUPERATION DU TEXTE SIGNIFIANT SI LES LAGRANGES
C --- ENCADRENT LES NOEUDS PHYSIQUES IMPLIQUES DANS LA RELATION
C --- LINEAIRE OU S'ILS SONT MIS APRES :
C     --------------------------------
      CALL GETVTX(' ','NUME_LAGR',1,1,1,NUMLAG,N6)
      IF (N6.EQ.0) THEN
        NUMLAG = 'APRES'
      ENDIF
C
C --- RECUPERATION DU NIVEAU D'INFORMATION QUE L'ON SOUHAITE
C --- DANS LES COMMANDES GENEREES :
C     ---------------------------
      CALL GETVIS(' ','INFO',1,1,1,INFO,N7)
      IF (N7.EQ.0) THEN
        INFO = 1
      ENDIF
C
C     ---------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C     ---------------------------------------------------------------
C
C --- COMMANDE CALC_MATR_ELEM :
C     =======================
C --- ON CALCULE LE MATR_ELEM DES MATRICES ELEMENTAIRES
C --- DE RIGIDITE UNIQUEMENT POUR GENERER LE NUME_DDL
C --- SUR-LEQUEL S'APPUIERA LE CHAMNO UTILISE POUR ECRIRE LA
C --- RELATION LINEAIRE ENTRE DDLS :
C     ----------------------------
         ICMD = ICMD + 1
         I9B = 9
         K9B = 'RIGI_MECA'
         CALL GCNCON('.',MATEL)
         CALL SMDCMD(ICMD,MATEL,'CALC_MATR_ELEM',IERUSR)
           CALL PUTVID('MODELE',1,MODELE,IERUSR)
           CALL PUTVID('CHAM_MATER',1,CHMAT,IERUSR)
           CALL PUTVID('CHARGE',1,CHARGE,IERUSR)
           IF (ICARA.EQ.1) THEN
             CALL PUTVID('CARA_ELEM',1,CARAEL,IERUSR)
           ENDIF
           CALL PUTVTX('OPTION',1,K9B,I9B,IERUSR)
C
         CALL SMFCMD(IERUSR)
C
C --- COMMANDE NUME_DDL :
C     =================
C --- ON DEFINIT LE NUME_DDL ASSOCIE AU MATR_ELEM DEFINI
C --- PRECEDEMMENT POUR CONSTRUIRE LE CHAMNO UTILISE POUR ECRIRE LA
C --- RELATION LINEAIRE ENTRE DDLS :
C     ----------------------------
         ICMD = ICMD + 1
         CALL GCNCON('.',NUMDDL)
         CALL SMDCMD(ICMD,NUMDDL,'NUME_DDL',IERUSR)
           CALL PUTVID('MATR_RIGI',1,MATEL,IERUSR)
           CALL PUTVIS('INFO',1,INFO,IERUSR)

         CALL SMFCMD(IERUSR)
C
C --- COMMANDE CALC_VECT_ELEM :
C     =======================
C --- ON CALCULE LE VECT_ELEM DU AU CHARGEMENT UTILISATEUR
C --- DE NOM CHARGE  :
C     -------------
         ICMD = ICMD + 1
         I9B = 9
         K9B = 'CHAR_MECA'
         CALL GCNCON('.',VECEL)
         CALL SMDCMD(ICMD,VECEL,'CALC_VECT_ELEM',IERUSR)
           CALL PUTVID('CHAM_MATER',1,CHMAT,IERUSR)
           CALL PUTVID('CHARGE',1,CHARGE,IERUSR)
           IF (ICARA.EQ.1) THEN
             CALL PUTVID('CARA_ELEM',1,CARAEL,IERUSR)
           ENDIF
           CALL PUTVTX('OPTION',1,K9B,I9B,IERUSR)
C
         CALL SMFCMD(IERUSR)
C
C --- COMMANDE ASSE_VECTEUR :
C     =====================
C --- ON CONSTRUIT LE CHAMNO QUI VA ETRE UTILISE POUR ECRIRE LA
C --- RELATION LINEAIRE ENTRE DDLS :
C     ----------------------------
         ICMD = ICMD + 1
         CALL GCNCON('.',CHAMNO)
         CALL SMDCMD(ICMD,CHAMNO,'ASSE_VECTEUR',IERUSR)
           CALL PUTVID('VECT_ELEM',1,VECEL,IERUSR)
           CALL PUTVID('NUME_DDL',1,NUMDDL,IERUSR)
           CALL PUTVIS('INFO',1,INFO,IERUSR)
C
         CALL SMFCMD(IERUSR)
C
C --- COMMANDE AFFE_CHAR_MECA :
C     =======================
C --- ON CONSTRUIT LA RELATION CINEMATIQUE IMPLIQUANT TOUS LES
C --- DDLS PHYSIQUES DU MODELE ET DONT LES COEFFICIENTS SONT
C --- LES TERMES DU CHAMNO DEFINI DANS LA COMMANDE PRECEDENTE.
C --- ON EMPLOIE LE MOT CLE LIAISON_CHAMNO A CET EFFET :
C     ------------------------------------------------
         I6B = 6
         ICMD = ICMD + 1
         CALL SMDCMD(ICMD,NOMRES,'AFFE_CHAR_MECA',IERUSR)
           CALL PUTVID('MODELE',1,MODELE,IERUSR)
C
           CALL SMDMCF('LIAISON_CHAMNO',IERUSR)
             CALL PUTVID('CHAM_NO',1,CHAMNO,IERUSR)
             CALL PUTVR8('COEF_IMPO' ,1,COEF,IERUSR)
             CALL PUTVTX('NUME_LAGR' ,1,NUMLAG,I6B,IERUSR)
           CALL SMFMCF(IERUSR)
C
           CALL PUTVIS('INFO',1,INFO,IERUSR)
C
         CALL SMFCMD(IERUSR)
C
      IF ( IERUSR .GT. 0 ) THEN
         CALL UTMESS('E',NOMCMD,'ERREURS CONSTATEES DANS LA MACRO')
         IER = IER + IERUSR
      ENDIF
C
      CALL JEDETC('V','&&OPS015',1)
      CALL JEDEMA ( )
C
 9999 CONTINUE
      END

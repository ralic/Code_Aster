      SUBROUTINE NMPRCA(MODELE,NUMEDD,NUMFIX,MATE  ,CARELE,
     &                  COMREF,COMPOR,LISCHA,METHOD,SOLVEU,
     &                  FONACT,PARMET,CARCRI,SDIMPR,SDSTAT,
     &                  SDDISC,SDTIME,NUMINS,VALINC,SOLALG,
     &                  MATASS,MAPREC,DEFICO,RESOCO,SDDYNA,
     &                  MEELEM,MEASSE,VEELEM,VEASSE,DEPEST,
     &                  LDCCVG,FACCVG,CODERE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_21
C
      IMPLICIT NONE
      INTEGER      FONACT(*)
      INTEGER      NUMINS,LDCCVG,FACCVG
      REAL*8       PARMET(*)
      CHARACTER*16 METHOD(*)
      CHARACTER*19 MAPREC,MATASS
      CHARACTER*24 SDIMPR,SDTIME,SDSTAT
      CHARACTER*19 LISCHA,SOLVEU,SDDISC,SDDYNA
      CHARACTER*24 MODELE,MATE, CARELE, COMREF, COMPOR
      CHARACTER*24 NUMEDD,NUMFIX
      CHARACTER*24 CARCRI
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*24 CODERE
      CHARACTER*19 VEELEM(*),VEASSE(*)
      CHARACTER*19 MEELEM(*),MEASSE(*)
      CHARACTER*19 SOLALG(*),VALINC(*)
      CHARACTER*19 DEPEST
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION - DEPL. DONNE)
C
C PROJECTION DU CHAMP DONNE SUR L'ESPACE DES CONDITIONS AUX LIMITES
C CINEMATIQUEMENT ADMISSIBLES
C
C ----------------------------------------------------------------------
C
C
C IN  MODELE : MODELE
C IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
C IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
C IN  MATE   : CHAMP MATERIAU
C IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
C IN  COMREF : VARI_COM DE REFERENCE
C IN  COMPOR : COMPORTEMENT
C IN  LISCHA : LISTE DES CHARGES
C IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
C IN  SOLVEU : SOLVEUR
C IN  FONACT : FONCTIONNALITES ACTIVEES
C IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
C IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
C IN  SDIMPR : SD AFFICHAGE
C IN  SDSTAT : SD STATISTIQUES
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  SDTIME : SD TIMER
C IN  NUMINS : NUMERO D'INSTANT
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
C IN  MATASS : MATRICE ASSEMBLEE
C IN  MAPREC : MATRICE DE PRECONDITIONNEMENT (GCPC)
C IN  DEFICO : SD DEFINITION CONTACT
C IN  RESOCO : SD RESOLUTION CONTACT
C IN  SDDYNA : SD POUR LA DYNAMIQUE
C IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
C IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
C IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
C IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
C IN  DEPEST : DEPLACEMENT ESTIME
C OUT LDCCVG : CODE RETOUR INTEGRATION DU COMPORTEMENT
C                0 - OK
C                1 - ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                2 - ERREUR DANS LES LDC SUR LA NON VERIFICATION DE
C                    CRITERES PHYSIQUES
C                3 - SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C OUT FACCVG : CODE RETOUR FACTORISATION MATRICE GLOBALE
C                0 - OK
C                1 - MATRICE SINGULIERE
C                3 - ON NE SAIT PAS SI SINGULIERE
C OUT CODERE : CHAM_ELEM CODE RETOUR INTEGRATION LDC
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      JNUM
      INTEGER      JDEP1,JDEP2,JSOL1,JSOL2
      INTEGER      NEQ, I,IRET
      CHARACTER*8  K8BID
      CHARACTER*19 DEPSO1,DEPSO2,CNCINE
      CHARACTER*19 SOLU1,SOLU2,CNDONN,CNPILO,CNCIND
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      SOLU1     = '&&CNPART.CHP2'
      SOLU2     = '&&CNPART.CHP3'
      CNDONN    = '&&CNCHAR.DONN'
      CNPILO    = '&&CNCHAR.PILO'
      CNCIND    = '&&CNCHAR.CINE'
      CALL VTZERO(SOLU1)
      CALL VTZERO(SOLU2)
      CALL VTZERO(CNDONN)
      CALL VTZERO(CNPILO)
      CALL VTZERO(CNCIND)
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID,IRET)
      LDCCVG    = 0
      FACCVG    = 0
C
C --- DECOMPACTION DES VARIABLES CHAPEAUX
C
      CALL NMCHEX(SOLALG,'SOLALG','DEPSO1',DEPSO1)
      CALL NMCHEX(SOLALG,'SOLALG','DEPSO2',DEPSO2)
      CALL NMCHEX(VEASSE,'VEASSE','CNCINE',CNCINE)
C
C --- CALCUL DE LA MATRICE GLOBALE
C
      CALL NMPRMA(MODELE,MATE  ,CARELE,COMPOR,CARCRI,
     &            PARMET,METHOD,LISCHA,NUMEDD,NUMFIX,
     &            SOLVEU,COMREF,SDIMPR,SDSTAT,SDTIME,
     &            SDDISC,SDDYNA,NUMINS,FONACT,DEFICO,
     &            RESOCO,VALINC,SOLALG,VEELEM,MEELEM,
     &            MEASSE,MAPREC,MATASS,CODERE,FACCVG,
     &            LDCCVG)
C
      IF ((FACCVG.EQ.1).OR.(FACCVG.EQ.2)) GOTO 9999
      IF (LDCCVG.EQ.1) GOTO 9999
C
C --- CALCUL DU SECOND MEMBRE
C --- PRISE EN COMPTE DES CL DUALISEES
C
      CALL NMASSD(MODELE,NUMEDD,LISCHA,FONACT,DEPEST,
     &            VEASSE,MATASS,CNPILO,CNDONN)
C
C --- PRISE EN COMPTE DES CL ELIMINEES
C
      CALL COPISD('CHAMP_GD','V',CNCINE,CNCIND)
      CALL NMACIN(FONACT,MATASS,DEPEST,CNCIND)
C
C --- RESOLUTION
C
      CALL NMRESO(FONACT,CNDONN,CNPILO,CNCIND,SOLVEU,
     &            MAPREC,MATASS,SOLU1 ,SOLU2 )
C
C --- CORRECTION DU DEPLACEMENT DONNE POUR LE RENDRE
C --- CINEMATIQUEMENT ADMISSIBLE
C
      CALL JEVEUO(NUMEDD(1:14)// '.NUME.DELG','L',JNUM)
      CALL JEVEUO(SOLU1 (1:19)//'.VALE','L',JSOL1 )
      CALL JEVEUO(SOLU2 (1:19)//'.VALE','L',JSOL2 )
      CALL JEVEUO(DEPSO1(1:19)//'.VALE','E',JDEP1 )
      CALL JEVEUO(DEPSO2(1:19)//'.VALE','E',JDEP2 )
C
C --- LES LAGRANGES NE SONT PAS MODIFIES
C
      DO 10 I = 1, NEQ
        IF (ZI(JNUM+I-1).EQ.0) THEN
          ZR(JDEP1+I-1) = ZR(JDEP1+I-1)+ZR(JSOL1+I-1)
          ZR(JDEP2+I-1) =               ZR(JSOL2+I-1)
        ENDIF
  10  CONTINUE
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END

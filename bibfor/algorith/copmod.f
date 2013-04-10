      SUBROUTINE COPMOD( BASEMO, CHAMP, NEQ, NUMER, NBMODE, TYPC, 
     &                   BMODR , BMODZ )
      IMPLICIT NONE
C***********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/04/2013   AUTEUR BOYERE E.BOYERE 
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
C
C                              FONCTION 
C     _______________________________________________________________
C    | EXTRAIRE, DANS UN VECTEUR DE TRAVAIL, DES CHAMPS D'UN CONCEPT |
C    | MODE_MECA AVEC LA POSSIBILITE DE MODIFIER LA NUMEROTATION.    |
C    |_______________________________________________________________|
C
C ---------
C EXEMPLES: CALL COPMOD (BASE,'DEPL',NEQ,' '  ,NBMOD,'R',ZR(JBASE),CBID)
C --------- CALL COPMOD (BASE,'DEPL',NEQ,NUDDL,NBMOD,'R',ZR(JBASE),CBID)
C           CALL COPMOD (BASE,'DEPL',NEQ,NUDDL,NBMOD,'C',RBID,ZC(JBASE))

C
C                     DESCRIPTIVE DES VARIABLES
C   ___________________________________________________________________ 
C  | IN > BASEMO : BASE MODALE D'ENTREE (MODE_MECA)                [K8]|
C  | IN > CHAMP  : NOM DE CHAMP A EXTRAIRE (EX. 'DEPL')            [K*]|
C  | IN > NEQ    : NOMBRE D'EQUATIONS DANS LE SYSTEME ASSEMBLE      [I]|
C   ___________________________________________________________________ 
C  | IN > NUMER  :                                                 [K*]|
C  |        SOIT - BLANC (' ') SI ON VEUT CONSERVER LA NUMEROTATION    |
C  |               INITIALE                                            |
C  |        SOIT - LE NOM DE CONCEPT NUME_DDL OU SD_PROF_CHNO DONNANT  |
C  |               LA NOUVELLE NUMEROTATION DES CHAMPS                 |
C   ___________________________________________________________________ 
C  | IN > NBMODE : NOMBRE DE MODES DANS BASEMO                      [I]|
C  | IN > TYPC   : TYPE DES CHAMPS A COPIER ('R'/'C')              [K1]|
C   ___________________________________________________________________ 
C  |OUT < BMODR  : VECTEUR DE TRAVAIL (REEL) DE SORTIE             [R8]|
C  |OUT < BMODZ  : VECTEUR DE TRAVAIL (COMPLEXE) DE SORTIE        [C16]|
C   ___________________________________________________________________ 
C
      INCLUDE 'jeveux.h'
C   ___________________________________________________________________ 
C
C  - 0 - INITIALISATIONS DIVERSES
C   ___________________________________________________________________ 
C
C     0.1 - DECLARATION DES VARIABLES D'ENTREE/SORTIE
C
      CHARACTER*1         TYPC
      CHARACTER*8         BASEMO
      CHARACTER*(*)        NUMER
      CHARACTER*(*)       CHAMP
      INTEGER             NEQ, NBMODE
      REAL*8              BMODR(NEQ*NBMODE)
      COMPLEX*16          BMODZ(NEQ*NBMODE)
C
C     0.2 - DECLARATION DES VARIABLES LOCALES
C
      LOGICAL             MODNUM, EXNUME
      INTEGER             I,IB,IRET, PROCHO
      INTEGER             JREF, JDEEQ, JVAL
      CHARACTER*19        NUMER1, NUMER2, NOMCHA, TMPCHA
      CHARACTER*24        MAILL1, MAILL2, VALK(4), CREFE(2),
     &                    VALCHA
C
C     0.3 - ACTUALISATION DE LA VALEUR DE LA MARQUE COURANTE
C
      CALL JEMARQ()
C  ____________________________________________________________________ 
C
C  - 1 - RECHERCHE DES INFORMATIONS SUR LES CHAMPS DANS LA BASE MODALE
C  ____________________________________________________________________ 
C
C     1.1 - CHERCHER UN OBJET .REFE DANS UN CHAMP DE LA BASE MODALE
C
C     1.1.1 - RECUPERER LE NOM DE CHAMP DU 1ER NUMERO ORDRE
C
      CALL RSEXCH('F',BASEMO, CHAMP, 1, NOMCHA, IRET )
C
C     1.1.2 - POUR TRAITER LES CAS AVEC SS-STRUCTURATION, TESTER SI
C             L'OBJET .REFE EXISTE DANS CE CHAMP, SI NON (IRET.EQ.0)
C             RECUPERER LE .REFE DU CHAMP DE DEPLACEMENT   
C
      CALL JEEXIN ( NOMCHA(1:19)//'.REFE' , IRET )
      IF (IRET.EQ.0) CALL RSEXCH('F',BASEMO, 'DEPL', 1, NOMCHA, IRET )
C
      CALL JEVEUO ( NOMCHA(1:19)//'.REFE', 'L', JREF )
C
C     1.2 - EXTRAIRE LE NOM DE MAILLAGE .REFE[1] ET DU NUME_DDL .REFE[2]
C
      MAILL1  = ZK24(JREF)(1:8)
      NUMER1  = ZK24(JREF+1)(1:19)
C
C     1.3 - TRAITEMENT DES CAS AVEC UN PROF_CHNO ET NON PAS UN NUME_DDL
C           COMPLET. 
C
      EXNUME = .FALSE.
      NUMER2 = NUMER
      IF (NUMER1(15:15).EQ.' ') NUMER1 = NUMER1(1:14)//'.NUME'
      IF (NUMER2(15:15).EQ.' ') THEN
        EXNUME = .TRUE.
        NUMER2 = NUMER2(1:14)//'.NUME'
      ELSE 
C       --- ON NE FAIT PAS DE TEST DE COMPATIBILITE SUR LES MAILLAGES
C         - SI ON NE DISPOSE PAS DE NUMEDDL COMPLET
C         - IMPORTANT : LE TEST DOIT SE FAIRE QUAND MEME EN DEHORS DE
C                       L'APPEL A COPMOD (VOIR OP0072 PAR EXEMPLE)
        MAILL2 = MAILL1
      ENDIF
C
C     1.4 - LIBERER L'OBJET .REFE PARCE QU'ON N'EN A PLUS BESOIN
C
      CALL JELIBE(NOMCHA(1:19)//'.REFE')
C  ____________________________________________________________________ 
C                                                                       
C  - 2 - RECHERCHE DES INFORMATIONS SUR LA NUMEROTATION FINALE          
C  ____________________________________________________________________ 
C
C     2.1 - NOUVELLE NUMEROTATION ? (SUR UN UNIQUE MAILLAGE)
C           RESTITUTION SUR SQUELLETE : CAS SPECIAL
C
      CALL JEEXIN(MAILL1(1:8)//'.INV.SKELETON',IRET)
      MODNUM = .FALSE.
      IF (NUMER2.NE.' ') THEN
        IF ((NUMER2.NE.NUMER1).AND.(IRET.EQ.0).AND.(EXNUME)) THEN
          MODNUM = .TRUE.
          CALL DISMOI ('F','NOM_MAILLA',NUMER2(1:14),'NUME_DDL',IB,
     &                 MAILL2,IRET)
        ENDIF
      ENDIF
C
C     2.2 - SI OUI, VERIFIER LA COMPATIB. DES 2 MAILLAGES DES NUME_DDL
C
      IF (MODNUM) THEN
        IF (MAILL1.NE.MAILL2) THEN
          VALK (1) = NUMER2
          VALK (2) = MAILL2
          VALK (3) = NUMER1
          VALK (4) = MAILL1
          CALL U2MESG('F', 'ALGORITH12_62',4,VALK,0,0,0,0.D0)
        ENDIF
      ENDIF
C
C     2.3 - RECUPERER L'OBJET .DEEQ
C
      IF (MODNUM) THEN
        CALL JEVEUO (NUMER2//'.DEEQ', 'L', JDEEQ )
      ELSE 
        CALL JEVEUO (NUMER1//'.DEEQ', 'L', JDEEQ )
      ENDIF
C  ____________________________________________________________________ 
C                                                                       
C  - 3 - RECOPIE DES CHAMPS ET MODIFICATION DE LA NUMER. SI NECESSAIRE  
C  ____________________________________________________________________ 
C
C     3.1 - BOUCLE SUR LES MODES DE LA BASE
      DO 10 I = 1, NBMODE
C       3.1.1 - EXTRAIRE LE NOM DU CHAMP D'INTERET (NOMCHA)
        CALL RSEXCH ('F',BASEMO, CHAMP, I, NOMCHA, IRET )
C
C       3.1.2 - NOUVELLE NUMER.? ALORS CREER UN NOUVEAU CHAMP TEMPORAIRE
C               AVEC LA BONNE NUMEROTATION 
        IF (MODNUM) THEN
          CREFE(1) = MAILL2
          CREFE(2) = NUMER2
          TMPCHA   = '&&COPMOD.CHAMP'
          CALL VTCREA ( TMPCHA, CREFE, 'V', TYPC, NEQ )
          CALL VTCOPY ( NOMCHA, TMPCHA, ' ', IRET )
          IF ( IRET.NE.0 ) THEN
            VALK(1) = NOMCHA
            VALK(2) = TMPCHA
            VALK(3) = CREFE(2)(1:8)
            CALL U2MESK('A','UTILITAI_24',3,VALK)
          ENDIF
          NOMCHA = TMPCHA
        ENDIF
C
C       3.1.3 - OBTENIR L'OBJET DES VALEURS DU CHAMP (.VALE OU .CELV)
C               POUR LES CHAM_NO ET CHAM_ELEM RESPECTIVEMENT
        VALCHA = NOMCHA(1:19)//'.VALE'
        CALL JEEXIN (NOMCHA(1:19)//'.VALE', IRET)
        IF (IRET.LE.0) VALCHA = NOMCHA(1:19)//'.CELV'
        CALL JEVEUO (VALCHA, 'L', JVAL)
C
C       3.1.4 - COPIER LES VALEURS DU CHAMP DANS LE VECTEUR DE SORTIE
        IF (TYPC.NE.'C') THEN
          CALL DCOPY (NEQ , ZR(JVAL),1,BMODR((I-1)*NEQ+1) ,1)
        ELSE 
          CALL ZCOPY (NEQ , ZC(JVAL),1,BMODZ((I-1)*NEQ+1) ,1)
        ENDIF
C
C       3.1.5 - MENAGE ET LIBERATION DE LA MEMOIRE SELON LE BESOIN
        CALL JELIBE ( VALCHA )
        IF (MODNUM) THEN
          IF (VALCHA(21:24).EQ.'VALE') THEN
            CALL DETRSD ( 'CHAM_NO', TMPCHA )
          ELSE 
            CALL DETRSD ( 'CHAM_ELEM', TMPCHA )
          ENDIF
        ENDIF
C
C       3.1.6 - ANNULER LES DDL DE LAGRANGE S'IL S'AGIT DES CHAMPS DE
C               DEPLACEMENTS
        IF ( CHAMP.EQ.'DEPL' ) THEN
          CALL ZERLAG (TYPC,BMODR((I-1)*NEQ+1),BMODZ((I-1)*NEQ+1),NEQ, 
     &                 ZI(JDEEQ))
        ENDIF

   10 CONTINUE
C     FIN DE LA BOUCLE (3.1) SUR LES MODES
C  ____________________________________________________________________ 
C
      CALL JEDEMA()
      END

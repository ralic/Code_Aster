      SUBROUTINE ARLVER(MOD,LGMA,NGMA,NOMZ,MODEL,CINE,DM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C ----------------------------------------------------------------------
C  FILTRE, REGROUPEMENT, VERIFICATION GROUPES DE MAILLES POUR ARLEQUIN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    MOD        :  SD MODELE
C CHARACTER*8    LGMA(NGMA) :  NOMS DES GROUPES DE MAILLES
C CHARACTER*(10) NOMZ       :  SD DOMAINE
C
C VARIABLES D'ENTREE/SORTIE
C INTEGER        NGMA       :  ENTREE : NOMBRE DE GROUPES DE MAILLES
C                              SORTIE : NOMBRE DE MAILLES DANS .GROUPEMA
C
C VARIABLES DE SORTIE 
C CHARACTER*8    MODEL      :  MODELISATION ASSOCIEE AUX MAILLES
C                              '3D','AXIS', 'CPLAN' OU 'DPLAN'
C CHARACTER*8    CINE       :  CINEMATIQUE ASSOCIEE AUX MAILLES
C                              'SOLIDE' OU 'COQUE'
C INTEGER        DM         :  DIMENSION DE LA VARIETE
C                              SI SOLIDE : DIMENSION DE L'ESPACE
C                              SI COQUE  : DIMENSION DE L'ESPACE - 1
C
C SD DE SORTIE
C NOM.GROUPEMA : LISTE TRIEE DES MAILLES DU DOMAINE NOM (MA1,MA2,...) 
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C --- VARIABLES
      CHARACTER*8   MOD,LGMA(*),MODEL,CINE,MODELZ,CINEZ,MAIL
      CHARACTER*10  NOM
      CHARACTER*16  NOMTE
      CHARACTER*(*) NOMZ
      INTEGER       DM,NGMA,NMA,NTOT,NLI,IMA,ILI,N,P0,P1,P2,Q0,Q1,Q2,I,J

      NOM = NOMZ

C --- LECTURE DONNEES

      CALL JEMARQ()     
      CALL JEVEUO(MOD//'.MODELE    .NOMA','L',P0)
      MAIL = ZK8(P0)
      CALL JEVEUO(MOD//'.MODELE    .REPE','L',P1)
      CALL JELIRA(MOD//'.MODELE    .LIEL','NMAXOC',NLI,ZK8)
      CALL JEVEUO(MOD//'.MAILLE','L',P2)
      
C --- ALLOCATION OBJET TEMPORAIRE

      CALL WKVECT('&&ARLVER.COMPTEUR','V V I',NLI,Q0)
      CALL WKVECT('&&ARLVER.TE','V V I',NLI,Q1)

      DO 10 I = 1, NLI
        ZI(Q0-1+I) = 0
 10   CONTINUE

C --- LIEL UTILISEES PAR LGMA

      DO 20 I = 1, NGMA

        CALL JEEXIN(JEXNOM(MAIL//'.GROUPEMA',LGMA(I)),J)
        IF (J.EQ.0) THEN
          CALL UTMESS('F','ARLVER',LGMA(I)//' N''EXISTE PAS')
        ENDIF

        CALL JEVEUO(JEXNOM(MAIL//'.GROUPEMA',LGMA(I)),'L',P0)
        CALL JELIRA(JEXNOM(MAIL//'.GROUPEMA',LGMA(I)),'LONMAX',NMA,ZK8)
        
        DO 20 J = 1, NMA

          IMA = ZI(P0-1+J)
          ILI = ZI(P1+2*(IMA-1))

          IF (ILI.NE.0) THEN

            N = ZI(Q0-1+ILI)
            IF (N.EQ.0) ZI(Q1-1+ILI) = ZI(P2-1+IMA)
            ZI(Q0-1+ILI) = N + 1

          ENDIF

 20   CONTINUE

C --- VERIFICATION COHERENCE MODELISATION / CINEMATIQUE 

      NTOT = 0

      DO 30 ILI = 1, NLI

        N = ZI(Q0-1+ILI)
        IF (N.EQ.0) GOTO 30

        CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ZI(Q1-1+ILI)),NOMTE)

        IF (NOMTE(1:4).EQ.'MEDP') THEN

          MODELZ = 'DPLAN'
          CINEZ = 'SOLIDE'
          IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) GOTO 40

        ELSEIF (NOMTE(1:4).EQ.'MECP') THEN

          MODELZ = 'CPLAN'
          CINEZ = 'SOLIDE'
          IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) GOTO 40

        ELSEIF (NOMTE(1:4).EQ.'MEAX') THEN

          MODELZ = 'AXIS'
          CINEZ = 'SOLIDE'
          IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) GOTO 40

        ELSEIF (NOMTE(1:5).EQ.'MECA_') THEN

          MODELZ = '3D'
          CINEZ = 'SOLIDE'
          IF ((NOMTE(6:9).EQ.'TETR').OR.
     &        (NOMTE(6:9).EQ.'PENT').OR.
     &        (NOMTE(6:9).EQ.'HEXA')) GOTO 40
            
        ELSEIF (NOMTE(1:4).EQ.'METD') THEN

          MODELZ = 'DPLAN'
          CINEZ = 'COQUE'
          GOTO 40
            
        ELSEIF (NOMTE(1:4).EQ.'METC') THEN

          MODELZ = 'CPLAN'
          CINEZ = 'COQUE'
          GOTO 40

        ELSEIF (NOMTE(1:4).EQ.'MECX') THEN

          MODELZ = 'AXIS'
          CINEZ = 'COQUE'
          GOTO 40

        ELSEIF (NOMTE(1:4).EQ.'MEDK') THEN

          MODELZ = '3D'
          CINEZ = 'COQUE'
          GOTO 40

        ELSEIF (NOMTE(1:4).EQ.'MEDS') THEN

          MODELZ = '3D'
          CINEZ = 'COQUE'
          GOTO 40

        ELSEIF (NOMTE(1:4).EQ.'MEQ4') THEN

          MODELZ = '3D'
          CINEZ = 'COQUE'
          GOTO 40          
              
        ELSEIF (NOMTE(1:4).EQ.'MEC3') THEN

          MODELZ = '3D'
          CINEZ = 'COQUE'
          GOTO 40

        ENDIF

        ZI(Q0-1+ILI) = 0
        GOTO 30

 40     CONTINUE
 
        IF (NTOT.EQ.0) THEN
          MODEL = MODELZ
          CINE = CINEZ
        ELSE
          IF (MODELZ.NE.MODEL) CALL UTMESS('F','ARLVER',
     &      'PLUSIEURS MODELISATIONS POUR UN MEME GROUPE DE MAILLES')
          IF (CINEZ.NE.CINE) CALL UTMESS('F','ARLVER',
     &      'PLUSIEURS CINEMATIQUES POUR UN MEME GROUPE DE MAILLES')
        ENDIF

        NTOT = NTOT + N

 30   CONTINUE

      IF (NTOT.EQ.0) CALL UTMESS('F','ARLVER',
     &                            'MODELISATION INDISPONIBLE')

C --- DIMENSION DE LA VARIETE

      IF (MODEL(1:2).EQ.'3D') THEN
        DM = 3
      ELSE 
        DM = 2
      ENDIF

      IF (CINE(1:5).EQ.'COQUE') DM = DM - 1

C --- ALLOCATION .GROUPEMA

      CALL WKVECT(NOM//'.GROUPEMA','V V I',NTOT,Q1)

C --- COPIE DES MAILLES VALIDES

      Q2 = Q1
 
      DO 50 I = 1, NGMA

        CALL JEVEUO(JEXNOM(MAIL//'.GROUPEMA',LGMA(I)),'L',P0)
        CALL JELIRA(JEXNOM(MAIL//'.GROUPEMA',LGMA(I)),'LONMAX',NMA,ZK8)
  
        DO 50 J = 1, NMA

          IMA = ZI(P0-1+J)
          ILI = ZI(P1+2*(IMA-1))

          IF ((ILI.NE.0).AND.(ZI(Q0-1+ILI).NE.0)) THEN
            ZI(Q2) = IMA
            Q2 = Q2 + 1
          ENDIF

 50   CONTINUE

      NGMA = NTOT
      CALL TRI(ZI(Q1),ZI,0,NGMA)

C --- DESALLOCATION 

      CALL JEDETR('&&ARLVER.COMPTEUR')
      CALL JEDETR('&&ARLVER.TE')
      CALL JEDEMA()

      END

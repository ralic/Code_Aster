      SUBROUTINE ORILGM ( MODELZ )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT   NONE
      CHARACTER*(*)       MODELZ
C ======================================================================
C MODIF MODELISA  DATE 30/03/2004   AUTEUR CIBHHPD S.VANDENBERGHE 
C
C     ORILGM  --  LE BUT EST DE REORIENTER, SI C'EST NECESSAIRE,
C                 LES MAILLES DE PEAU DE GROUPES DE MAILLES
C                 DONNES SOUS LES MOTS CLES :
C                 'ORIE_PEAU_2D' EN 2D
C                 'ORIE_PEAU_3D' ET 'ORIE_NORM_COQUE' EN 3D
C                 DE TELLE FACON A CE QUE LA NORMALE A LA MAILLE DE
C                 PEAU SOIT EXTERIEURE AU VOLUME.
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELZ         IN    K*      NOM DU MODELE
C
C ========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32     JEXNUM, JEXNOM
C -----  VARIABLES LOCALES
      INTEGER       JNOMA, NDIM, IER, NBOCC, IOCC, NG, JJJ, NGR, IGR,
     +              NORIEN, NIV, IFM, IMA, NUMA, INO, JGRO, JDES,
     +              NBMAIL, N1, N2, N3, K, NOEUD, NBNO,
     +              NBF1, NBF2, NBF3, NOR, NGT, NLP ,I, NORIT
      REAL*8        VECT(3)
      LOGICAL       LOGIC, ORIVEC, LNC, LP2D, LP3D
      CHARACTER*8   K8B, NOMA, MODELE, NNOEUD, GMAT
      CHARACTER*16  MOFA2D, MOFA3D, MOTFAC,MOFB3D
      CHARACTER*24  MAMOD, NOMNOE, GRMAMA, CONNEX
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
      CALL INFNIV ( IFM , NIV )
C
C
C --- INITIALISATIONS :
C     ---------------
      MODELE = MODELZ
      MOFA2D = 'ORIE_PEAU_2D'
      MOFA3D = 'ORIE_PEAU_3D'
      MOFB3D = 'ORIE_NORM_COQUE'
C
C --- RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
C     ------------------------------------------
      MAMOD = MODELE(1:8)//'.MODELE    .NOMA'
      CALL JEVEUO(MAMOD,'L',JNOMA)
      NOMA = ZK8(JNOMA)
      NOMNOE = NOMA//'.NOMNOE'
      GRMAMA = NOMA//'.GROUPEMA'
      CONNEX = NOMA//'.CONNEX'
C
C --- RECUPERATION DE LA DIMENSION (2 OU 3) DU PROBLEME :
C     -------------------------------------------------
      CALL DISMOI('F','DIM_GEOM',MODELE,'MODELE',NDIM,K8B,IER)
      IF ( NDIM .GT. 1000 )  NDIM = 3
      
      
      
C
      CALL GETFAC ( MOFA2D, NBF1 )
      CALL GETFAC ( MOFA3D, NBF2 )
      CALL GETFAC ( MOFB3D, NBF3 )
      
      IF ( ( NBF1 .GT. 0 ) .AND. ( NDIM .EQ. 3 ) ) THEN
     
         CALL UTMESS ( 'F',MOFA2D,'VOUS AVEZ UTILISE LE MOT CLE'//
     &   ' ORIE_PEAU_2D ALORS QUE LE PROBLEME EST 3D.'//
     &   ' UTILISEZ ORIE_PEAU_3D' )
      
      ENDIF
      
      IF ( ( NBF2 .GT. 0 ) .AND. ( NDIM .EQ. 2 ) ) THEN
     
         CALL UTMESS ( 'F',MOFA3D,'VOUS AVEZ UTILISE LE MOT CLE'//
     &   ' ORIE_PEAU_3D ALORS QUE LE PROBLEME EST 2D.'//
     &   ' UTILISEZ ORIE_PEAU_2D' )
      
      ENDIF
      
      
       
C
      LP2D=.FALSE.
      LP3D=.FALSE.
      LNC=.FALSE.
      NLP=0
      IF(NBF1.NE.0)THEN
         LP2D=.TRUE.
         NLP=1+NLP
      ENDIF
      IF(NBF2.NE.0)THEN
         LP3D=.TRUE.
         NLP=1+NLP
      ENDIF
      IF(NBF3.NE.0)THEN
         LNC=.TRUE.
         NLP=1+NLP
      ENDIF
C         
      NOR = NBF1 + NBF2 + NBF3
      NGT = 0
      NORIT=0
      NORIEN=0
C
C     BOUCLE PRINCIPALE
C     -----------------
C
      DO 10 I=1,NLP
C         
C     INITIALISATIONS : ORIE_PEAU_2D, ORIE_PEAU_3D
C     --------------------------------------------
         IF( I.EQ.1 .AND. (LP2D .OR. LP3D))THEN
            ORIVEC=.FALSE.
            IF(LP2D)THEN
               MOTFAC=MOFA2D
            ELSE
               MOTFAC=MOFA3D
            ENDIF
            LOGIC=.TRUE.
            GOTO 20
         ENDIF
C
C     INITIALISATION : ORIE_NORM_COQUE
C     --------------------------------
         IF(LNC)THEN
            MOTFAC=MOFB3D
            LOGIC=.FALSE.
         ENDIF
C
 20      CONTINUE
C
         CALL GETFAC(MOTFAC,NBOCC)
         NGT=NGT+NBOCC
C
C
         DO 30 IOCC=1,NBOCC
C
C           CAS ORIE_NORM_COQUE: RECUPERATION DU NOEUD 
C           OU DU GROUPE DE NOEUDS
C           ------------------------------------------
            IF(.NOT.LOGIC)THEN
C
               CALL GETVR8(MOTFAC,'VECT_NORM',IOCC,1,0,VECT,N1)
               IF (N1.NE.0)THEN
                  CALL GETVR8(MOTFAC,'VECT_NORM',IOCC,1,NDIM,VECT,N1)
                  IF(N1.NE.NDIM ) CALL UTMESS('F',MOTFAC,
     .                 'MAUVAIS NOMBRE DE VALEURS POUR VECT_NORM')
                  CALL GETVID(MOTFAC,'NOEUD',IOCC,1,0,K8B,N2)
                  IF(N2.NE.0)THEN
                     CALL GETVID (MOTFAC,'NOEUD',IOCC,1,1,NNOEUD,N2)
                     CALL JENONU (JEXNOM(NOMNOE,NNOEUD),NOEUD)
                     IF(NOEUD.EQ.0)CALL UTMESS('F',MOTFAC,
     .                    'ERREUR DONNEES : LE NOEUD '//NNOEUD//
     .                    ' N''EXISTE PAS')
                  ELSE
                     CALL GETVID(MOTFAC,'GROUP_NO',IOCC,1,1,K8B,N3)
                     IF ( N3 .NE. 0 ) THEN
                        CALL GETVID(MOTFAC,'GROUP_NO',IOCC,1,1,
     +                       NNOEUD,N3)
                        CALL UTNONO(' ',NOMA,'NOEUD',NNOEUD,K8B,IER)
                        IF ( IER .EQ. 10 ) THEN
                           CALL UTDEBM('F',MOTFAC,'ERREUR DONNEES')
                           CALL UTIMPK('L','LE GROUP_NO N''EXISTE '//
     +                      'PAS ',1,NNOEUD)
                           CALL UTFINM()
                        ELSEIF ( IER .EQ. 1 ) THEN
                           CALL UTDEBM('A',MOTFAC,'TROP DE NOEUDS '//
     .                          'DANS LE GROUP_NO')
                           CALL UTIMPK('L','  NOEUD UTILISE: ',1,K8B)
                           CALL UTFINM( )
                        ENDIF
                        CALL JENONU ( JEXNOM(NOMNOE,K8B), NOEUD )
                     ELSE
                        CALL UTMESS('F',MOTFAC,'LORSQUE VECT_NORM '//
     .                       'EST PRESENT IL FAUT QUE NOEUD OU '//
     .                       'GROUP_NO LE SOIT AUSSI')
                     ENDIF
                  ENDIF
                  ORIVEC = .TRUE.
               ELSE
                  ORIVEC = .FALSE.
               ENDIF
            ENDIF
C
C           RECUPERATION DES GROUPES DE MAILLES A ORIENTER
C           ----------------------------------------------
            CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',
     +                                       IOCC,1,0,K8B,NG)
            IF (NG.NE.0) THEN
               NG = -NG
               CALL WKVECT ('&&ORILGM.WORK','V V K8',NG,JJJ)
               CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',
     +              IOCC,1,NG,ZK8(JJJ),NGR)
C
               DO 40 IGR = 1, NGR
                  GMAT = ZK8(JJJ+IGR-1)
C
C                 CAS ORIE_NORM_COQUE: VERIFICATION QUE LE NOEUD
C                 (OU GROUPE DE NOEUDS) APPARTIENT AU GROUPE DE MAILLES
C                 -----------------------------------------------------
                  IF ( ORIVEC ) THEN
                     CALL JELIRA (JEXNOM(GRMAMA,GMAT),'LONMAX',
     +                    NBMAIL,K8B)
                     CALL JEVEUO (JEXNOM(GRMAMA,GMAT),'L',JGRO)
                     DO 50 IMA = 1, NBMAIL
                        NUMA = ZI(JGRO-1+IMA)
                        CALL JELIRA(JEXNUM(CONNEX,NUMA),'LONMAX',
     +                       NBNO,K8B)
                        CALL JEVEUO(JEXNUM(CONNEX,NUMA),'L',JDES)
                        DO 60 INO = 1, NBNO
                           IF ( ZI(JDES+INO-1) .EQ. NOEUD ) GOTO 70
 60                     CONTINUE
 50                  CONTINUE
C                    ..SI CE N'EST PAS LE CAS, ON INFORME L'UTILISATEUR
C                    --------------------------------------------------
                     CALL UTDEBM('A',MOTFAC,'ATTENTION')
                     IF ( N2 .NE. 0 ) THEN
                        CALL UTIMPK('L','LE NOEUD ',1,NNOEUD)
                     ELSE
                        CALL UTIMPK('L','LE GROUP_NO ',1,NNOEUD)
                     ENDIF
                     CALL UTIMPK('S',' N''APPARTIENT PAS AU GROUP_MA ',
     +                    1,GMAT)
                     CALL UTFINM()
 70                  CONTINUE
                  ENDIF
C
C                 REORIENTATION DU GROUP_MA COURANT
C                 ---------------------------------
                  NORIEN=0
                  CALL ORIGMA(MODELE,GMAT,NORIEN,LOGIC,
     .                 ORIVEC,VECT,NOEUD,.FALSE.)
                  IF ( NORIEN .NE. 0 ) THEN
                     WRITE(IFM,*)GMAT,NORIEN,' MAILLES '//
     &              ' REORIENTEES '
                     NORIT=NORIT+NORIEN
                  ELSE
                     WRITE(IFM,*)GMAT,NORIEN,' MAILLE '//
     &               ' REORIENTEE '
                  ENDIF
 40            CONTINUE
C
            ENDIF
C
            CALL JEDETR ('&&ORILGM.WORK')
C
 30      CONTINUE
C
 10   CONTINUE
C
      IF ( NORIT .NE. 0 ) THEN
             WRITE(IFM,*)'AU TOTAL',NORIT,' MAILLES '//
     &          ' REORIENTEES '
      ENDIF
C
      IF ( NGT .NE. NOR ) THEN
         CALL UTDEBM('F', 'ORILGM', 'BUG LORS DU TRAITEMENT')
         CALL UTIMPI('L', '   NGT = ', 1, NGT )
         CALL UTIMPI('L', '   NOR = ', 1, NOR )
         CALL UTIMPI('L', 'CONTACTER L''ASSISTANCE', 0, NOR )
         CALL UTFINM()
      ENDIF
C
      CALL JEDEMA()
      END

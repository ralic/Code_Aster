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
C.======================================================================
C MODIF MODELISA  DATE 06/11/2001   AUTEUR CIBHHLV L.VIVAN 
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
C.========================= DEBUT DES DECLARATIONS ====================
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
     +              NBF1, NBF2, NBF3, NOR, NGT
      REAL*8        VECT(3)
      LOGICAL       LOGIC, ORIVEC
      CHARACTER*8   K8B, NOMA, MODELE, NNOEUD, GMAT
      CHARACTER*16  MOFA2D, MOFA3D, MOTFAC,MOFB3D
      CHARACTER*24  MAMOD, NOMNOE, GRMAMA, CONNEX
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
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
      NOR = NBF1 + NBF2 + NBF3
      NGT = 0
C
C --- RECUPERATION DES GROUPES DE MAILLES DE PEAU A REOREINTER :
C     --------------------------------------------------------
      NORIEN = 0
      DO 50 K=1,2
        IF (K.EQ.2) THEN
          MOTFAC = MOFB3D
C
          CALL GETVR8 ( MOTFAC, 'VECT_NORM' , IOCC,1,0, VECT, N1 )
          IF ( N1 .NE. 0 ) THEN
            CALL GETVR8 ( MOTFAC, 'VECT_NORM' , IOCC,1,NDIM, VECT, N1 )
            IF ( N1 .NE. NDIM ) CALL UTMESS('F',MOTFAC,
     .                     'MAUVAIS NOMBRE DE VALEURS POUR VECT_NORM')
            CALL GETVID ( MOTFAC, 'NOEUD', IOCC,1,0, K8B, N2 )
            IF ( N2 .NE. 0 ) THEN
              CALL GETVID ( MOTFAC, 'NOEUD', IOCC,1,1, NNOEUD, N2 )
              CALL JENONU ( JEXNOM(NOMNOE,NNOEUD), NOEUD )
              IF ( NOEUD .EQ. 0 ) CALL UTMESS('F',MOTFAC,
     .          'ERREUR DONNEES : LE NOEUD '//NNOEUD//' N''EXISTE PAS ')
            ELSE
              CALL GETVID ( MOTFAC, 'GROUP_NO', IOCC,1,1, K8B, N3 )
              IF ( N3 .NE. 0 ) THEN
                CALL GETVID ( MOTFAC, 'GROUP_NO', IOCC,1,1, NNOEUD, N3 )
                CALL UTNONO ( ' ', NOMA, 'NOEUD', NNOEUD, K8B, IER )
                IF ( IER .EQ. 10 ) THEN
                  CALL UTDEBM('F',MOTFAC,'ERREUR DONNEES')
                  CALL UTIMPK('L','LE GROUP_NO N''EXISTE PAS ',1,NNOEUD)
                  CALL UTFINM()
                ELSEIF ( IER .EQ. 1 ) THEN
               CALL UTDEBM('A',MOTFAC,'TROP DE NOEUDS DANS LE GROUP_NO')
                  CALL UTIMPK('L','  NOEUD UTILISE: ',1,K8B)
                  CALL UTFINM( )
                ENDIF
                CALL JENONU ( JEXNOM(NOMNOE,K8B), NOEUD )
              ELSE
                CALL UTMESS('F',MOTFAC,'LORSQUE VECT_NORM EST '//
     .            'PRESENT IL FAUT QUE NOEUD OU GROUP_NO LE SOIT AUSSI')
              ENDIF
            ENDIF
            ORIVEC = .TRUE.
          ELSE
            ORIVEC = .FALSE.
          ENDIF
        ELSE IF (NDIM.EQ.2) THEN
          MOTFAC = MOFA2D
          ORIVEC=.FALSE.
        ELSE
          MOTFAC = MOFA3D
          ORIVEC = .FALSE.
        ENDIF
        LOGIC = K.EQ.1
        CALL GETFAC ( MOTFAC, NBOCC )
        NGT = NGT + NBOCC
C
        DO 10 IOCC = 1, NBOCC
          CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',
     +                                       IOCC,1,0,K8B,NG)
          IF (NG.NE.0) THEN
            NG = -NG
            CALL WKVECT ('&&ORILGM.WORK','V V K8',NG,JJJ)
            CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',
     +                                      IOCC,1,NG,ZK8(JJJ),NGR)
            DO 20 IGR = 1, NGR
              GMAT = ZK8(JJJ+IGR-1)
C ----------- ON VERIFIE QUE LE NOEUD APPARTIENT AU GROUP_MA
              IF ( ORIVEC ) THEN
                CALL JELIRA (JEXNOM(GRMAMA,GMAT),'LONMAX',NBMAIL,K8B)
                CALL JEVEUO (JEXNOM(GRMAMA,GMAT),'L',JGRO)
                DO 22 IMA = 1, NBMAIL
                  NUMA = ZI(JGRO-1+IMA)
                  CALL JELIRA(JEXNUM(CONNEX,NUMA),'LONMAX',NBNO,K8B)
                  CALL JEVEUO(JEXNUM(CONNEX,NUMA),'L',JDES)
                  DO 24 INO = 1, NBNO
                     IF ( ZI(JDES+INO-1) .EQ. NOEUD ) GOTO 30
  24              CONTINUE
  22            CONTINUE
                CALL UTDEBM('F',MOTFAC,'ERREUR DANS LES DONNEES')
                IF ( N2 .NE. 0 ) THEN
                   CALL UTIMPK('L','LE NOEUD ',1,NNOEUD)
                ELSE
                   CALL UTIMPK('L','LE GROUP_NO ',1,NNOEUD)
                ENDIF
               CALL UTIMPK('S',' N''APPARTIENT PAS AU GROUP_MA ',1,GMAT)
                CALL UTFINM()
  30            CONTINUE
              ENDIF
C
C ----------- REORIENTATION DU GROUP_MA COURANT :
C             ---------------------------------
              CALL ORIGMA ( MODELE, GMAT, NORIEN, LOGIC,
     .                                  ORIVEC, VECT, NOEUD, .FALSE. )
  20        CONTINUE
          ENDIF
          IF ( NORIEN .NE. 0 ) 
     .           WRITE(IFM,*)'AU TOTAL ',NORIEN,' MAILLES REORIENTEES'
          CALL JEDETR ('&&ORILGM.WORK')
  10    CONTINUE
  50  CONTINUE
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

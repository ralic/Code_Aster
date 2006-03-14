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
C MODIF MODELISA  DATE 13/03/2006   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER       IBID, IFM , NIV, NBF1, NBF2, NBF3, IRET, NBPAR, 
     +              JNOMA, JJJ, JGRO, N1, N2, N3, NOEUD, IOCC,
     +              IER, NDIM, IGR, NG, NBMAIL, NORIT, NORIEN
      REAL*8        VECT(3), R8B, PREC, ARMIN
      COMPLEX*16    CBID
      LOGICAL       REORIE, ORIVEC
      CHARACTER*8   K8B, NOMA, MODELE, NNOEUD, GMAT
      CHARACTER*16  MOFA2D, MOFA3D, MOFB3D
      CHARACTER*19  NOMT19
      CHARACTER*24  MAMOD, NOMNOE, GRMAMA, PARA
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
      CALL INFNIV ( IFM , NIV )
C
C --- INITIALISATIONS :
C     ---------------
      MODELE = MODELZ
C
      NORIT  = 0
      REORIE = .TRUE.
C
      MOFA2D = 'ORIE_PEAU_2D'
      MOFA3D = 'ORIE_PEAU_3D'
      MOFB3D = 'ORIE_NORM_COQUE'
C
      CALL GETFAC ( MOFA2D, NBF1 )
      CALL GETFAC ( MOFA3D, NBF2 )
      CALL GETFAC ( MOFB3D, NBF3 )
C
C --- RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
C     ------------------------------------------
      MAMOD = MODELE(1:8)//'.MODELE    .NOMA'
      CALL JEVEUO(MAMOD,'L',JNOMA)
      NOMA = ZK8(JNOMA)
      NOMNOE = NOMA//'.NOMNOE'
      GRMAMA = NOMA//'.GROUPEMA'
C
C --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE :
C     -------------------------------------------
      CALL JEEXIN ( NOMA//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( NOMA , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, CBID, K8B,
     +                K8B, R8B , PARA, K8B, IBID, ARMIN, CBID,
     +                K8B, IRET )
         IF ( IRET .EQ. 0 ) THEN
            PREC = ARMIN*1.D-06
         ELSEIF ( IRET .EQ. 1 ) THEN
            PREC = 1.D-10
         ELSE
            CALL UTMESS('F','ORILGM',
     + 'PROBLEME POUR RECUPERER UNE GRANDEUR DANS LA TABLE "CARA_GEOM"')
         ENDIF
      ELSE
         CALL UTMESS('F','ORILGM',
     +            'LA TABLE "CARA_GEOM" N''EXISTE PAS DANS LE MAILLAGE')
      ENDIF
C
C --- RECUPERATION DE LA DIMENSION (2 OU 3) DU PROBLEME :
C     -------------------------------------------------
      CALL DISMOI('F','DIM_GEOM',MODELE,'MODELE',NDIM,K8B,IER)
      IF ( NDIM .GT. 1000 )  NDIM = 3
C
C --- COMPATIBILITE DU PROBLEME AVEC LES MOTS CLES FACTEUR :
C     ----------------------------------------------------
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
C --- TRAITEMENT DE 'ORIE_PEAU_2D' :
C     ----------------------------
C
      DO 100 IOCC = 1 , NBF1
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA2D, 'GROUP_MA',
     +                                           IOCC, 1, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA2D, 'GROUP_MA',
     +                                     IOCC, 1, NG, ZK8(JJJ), NG )
         DO 110 IGR = 1, NG
            GMAT = ZK8(JJJ+IGR-1)
            CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONMAX', NBMAIL, K8B )
            CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
            NORIEN=0
            CALL ORILMA ( MODELE, NOMA, NDIM,  ZI(JGRO), NBMAIL, NORIEN,
     +                    REORIE, PREC )
            NORIT = NORIT + NORIEN
            WRITE(IFM,1000) GMAT, NORIEN
 110     CONTINUE
         CALL JEDETR ( '&&ORILGM.WORK' )
 100  CONTINUE
C
C --- TRAITEMENT DE 'ORIE_PEAU_3D' :
C     ----------------------------
C
      DO 200 IOCC = 1 , NBF2
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA3D, 'GROUP_MA',
     +                                           IOCC, 1, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA3D, 'GROUP_MA',
     +                                     IOCC, 1, NG, ZK8(JJJ), NG )
         DO 210 IGR = 1, NG
            GMAT = ZK8(JJJ+IGR-1)
            CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONMAX', NBMAIL, K8B )
            CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
            NORIEN=0
            CALL ORILMA ( MODELE, NOMA, NDIM, ZI(JGRO), NBMAIL, NORIEN, 
     +                    REORIE, PREC )
            NORIT = NORIT + NORIEN
            WRITE(IFM,1000) GMAT, NORIEN
 210     CONTINUE
         CALL JEDETR ( '&&ORILGM.WORK' )
 200  CONTINUE
C
C --- TRAITEMENT DE 'ORIE_NORM_COQUE' :
C     -------------------------------
C
      DO 300 IOCC = 1 , NBF3
         ORIVEC = .FALSE.
         CALL GETVR8 ( MOFB3D, 'VECT_NORM', IOCC,1,0, VECT, N1 )
         IF (N1.NE.0)THEN
            ORIVEC = .TRUE.
            CALL GETVR8 ( MOFB3D, 'VECT_NORM', IOCC,1,NDIM, VECT, N1 )
            IF(N1.NE.NDIM ) CALL UTMESS('F',MOFB3D,
     .                 'MAUVAIS NOMBRE DE VALEURS POUR VECT_NORM')
            CALL GETVID ( MOFB3D, 'NOEUD', IOCC,1,0, K8B, N2 )
            IF (N2.NE.0)THEN
               CALL GETVID ( MOFB3D, 'NOEUD', IOCC,1,1, NNOEUD, N2 )
               CALL JENONU (JEXNOM(NOMNOE,NNOEUD),NOEUD)
               IF(NOEUD.EQ.0)CALL UTMESS('F',MOFB3D,
     .         'ERREUR DONNEES : LE NOEUD '//NNOEUD//' N''EXISTE PAS')
            ELSE
               CALL GETVID(MOFB3D,'GROUP_NO',IOCC,1,1,NNOEUD,N3)
               CALL UTNONO(' ',NOMA,'NOEUD',NNOEUD,K8B,IER)
               IF ( IER .EQ. 10 ) THEN
                  CALL UTDEBM('F',MOFB3D,'ERREUR DONNEES')
                  CALL UTIMPK('L','LE GROUP_NO N''EXISTE PAS '
     +                                                      ,1,NNOEUD)
                  CALL UTFINM()
               ELSEIF ( IER .EQ. 1 ) THEN
                  CALL UTDEBM('A',MOFB3D,'TROP DE NOEUDS '//
     .                                             'DANS LE GROUP_NO')
                  CALL UTIMPK('L','  NOEUD UTILISE: ',1,K8B)
                  CALL UTFINM( )
               ENDIF
               CALL JENONU ( JEXNOM(NOMNOE,K8B), NOEUD )
            ENDIF
         ENDIF
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFB3D, 'GROUP_MA',
     +                                           IOCC, 1, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFB3D, 'GROUP_MA',
     +                                     IOCC, 1, NG, ZK8(JJJ), NG )
         IF ( ORIVEC ) THEN
            DO 310 IGR = 1, NG
               GMAT = ZK8(JJJ+IGR-1)
               CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONMAX', NBMAIL,K8B)
               CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
               NORIEN=0
               CALL ORVLMA ( NOMA, ZI(JGRO), NBMAIL, NORIEN,
     +                                             VECT, NOEUD, PREC )
               NORIT = NORIT + NORIEN
               WRITE(IFM,1000) GMAT, NORIEN
 310        CONTINUE
         ELSE
            DO 320 IGR = 1, NG
               GMAT = ZK8(JJJ+IGR-1)
               CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONMAX', NBMAIL,K8B)
               CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
               NORIEN=0
               CALL ORNORM ( NOMA, ZI(JGRO), NBMAIL, NORIEN )
               NORIT = NORIT + NORIEN
               WRITE(IFM,1000) GMAT, NORIEN
 320        CONTINUE
         ENDIF
         CALL JEDETR ( '&&ORILGM.WORK' )
 300  CONTINUE
C
      IF ( NORIT .NE. 0 )  WRITE(IFM,1010) NORIT
C
 1000 FORMAT('GROUP_MA: ',A8,I7,' MAILLE(S) REORIENTEE(S) ')
 1010 FORMAT('AU TOTAL ', I7, ' MAILLE(S) REORIENTEE(S) ')
C
      CALL JEDEMA()
      END

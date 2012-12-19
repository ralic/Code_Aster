      SUBROUTINE ORILGM ( NOMA )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      CHARACTER*8    NOMA
C ======================================================================
C MODIF MODELISA  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C -----  VARIABLES LOCALES
      INTEGER       IBID, IFM , NIV, NBF1, NBF2, NBF3, IRET, NBPAR,
     &               JJJ, JGRO, N1, N2, N3, NOEUD, IOCC, NGV,
     &              IER, NDIM, IGR, NG, NBMAIL, NORIT, NORIEN, NTRAIT,
     &              JJV,NBMAVO, JMAVO, NBMATO, IMA, NBMAVI, JMAVI, K,
     &              JGV, NCF3, NGS, JGS, NBMASU, JMASU
      REAL*8        VECT(3), R8B, PREC, ARMIN
      COMPLEX*16    CBID
      LOGICAL       REORIE, ORIVEC
      CHARACTER*8   K8B, GMAT
      CHARACTER*16  MOFA2D, MOFA3D, MOFB3D, MOFC3D
      CHARACTER*19  NOMT19
      CHARACTER*24   NOMNOE, GRMAMA, PARA, NNOEUD
      CHARACTER*24 VALK
      INTEGER      IARG
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
      CALL INFNIV ( IFM , NIV )
C
C --- INITIALISATIONS :
C     ---------------
C
      NORIT  = 0
      REORIE = .TRUE.
C
      MOFA2D = 'ORIE_PEAU_2D'
      MOFA3D = 'ORIE_PEAU_3D'
      MOFB3D = 'ORIE_NORM_COQUE'
      MOFC3D = 'ORIE_LIGNE'
C
      CALL GETFAC ( MOFA2D, NBF1 )
      CALL GETFAC ( MOFA3D, NBF2 )
      CALL GETFAC ( MOFB3D, NBF3 )
      CALL GETFAC ( MOFC3D, NCF3 )
C
C --- RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
C     ------------------------------------------
      NOMNOE = NOMA//'.NOMNOE'
      GRMAMA = NOMA//'.GROUPEMA'
C
C --- RECUPERATION DE LA DIMENSION (2 OU 3) DU PROBLEME :
C     -------------------------------------------------
      CALL DISMOI('F','Z_CST',NOMA,'MAILLAGE',NDIM,K8B,IER)
      IF ( K8B(1:3) .EQ. 'OUI' ) THEN
         NDIM = 2
      ELSE
         NDIM = 3
      ENDIF
C
C --- COMPATIBILITE DU PROBLEME AVEC LES MOTS CLES FACTEUR :
C     ----------------------------------------------------
      IF ( ( NBF1 .GT. 0 ) .AND. ( NDIM .EQ. 3 ) ) THEN
         CALL U2MESS('F','MODELISA5_95')
      ENDIF
      IF ( ( NBF2 .GT. 0 ) .AND. ( NDIM .EQ. 2 ) ) THEN
         CALL U2MESS('F','MODELISA5_96')
      ENDIF
C
C --- TRAITEMENT DE 'ORIE_PEAU_2D' :
C     ----------------------------
C
      DO 100 IOCC = 1 , NBF1
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA2D, 'GROUP_MA',
     &                                           IOCC,IARG, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA2D, 'GROUP_MA',
     &                                     IOCC,IARG, NG, ZK8(JJJ), NG )
C        PRESENCE DE GROUP_MA_SURF ?
C        ---------------------------
         CALL GETVTX(MOFA2D,'GROUP_MA_SURF',IOCC,IARG, 0,K8B, NGS )
         IF(NGS.NE.0)THEN
           NGS = -NGS
           CALL WKVECT ( '&&ORILGM.WORK2', 'V V K8', NGS, JGS )
           CALL GETVEM ( NOMA, 'GROUP_MA', MOFA2D, 'GROUP_MA_SURF',
     &                                     IOCC,IARG, NGS, ZK8(JGS),
     &                                      NGS )
           CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMATO,
     &                 K8B,IRET)
           CALL WKVECT ('&&ORILGM.WORK3','V V I', NBMATO, JJV )
           DO 101 IMA = 1, NBMATO
              ZI(JJV+IMA-1)=0
 101       CONTINUE
           DO 102 IGR = 1, NGS
             GMAT = ZK8(JGS+IGR-1)
             CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONMAX', NBMAVI, K8B )
             CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JMAVI )
             DO 103 IMA=1,NBMAVI
                ZI(JJV+ZI(JMAVI+IMA-1)-1)=1
 103         CONTINUE
 102       CONTINUE
C          NOMBRE DE MAILLES 'VOLUMIQUES' (SANS DOUBLON) : NBMASU
           NBMASU=0
           DO 104 IMA = 1, NBMATO
             NBMASU=NBMASU+ZI(JJV+IMA-1)
 104       CONTINUE
C          LISTE DES MAILLES 'VOLUMIQUES' (SANS DOUBLON) : ZI(JMASU)
           CALL WKVECT('&&ORILGM.GROUP_MA_SURF','V V I',NBMASU,JMASU)
           K=0
           DO 105 IMA = 1, NBMATO
             IF(ZI(JJV+IMA-1).EQ.1) THEN
                   K=K+1
                   ZI(JMASU+K-1)=IMA
             ENDIF
 105       CONTINUE
           CALL JEDETR('&&ORILGM.WORK3')
           CALL JEDETR('&&ORILGM.WORK2')
         ELSE
           NBMASU=0
           CALL WKVECT('&&ORILGM.GROUP_MA_SURF','V V I',1,JMASU)
         ENDIF
C
         DO 110 IGR = 1, NG
            GMAT = ZK8(JJJ+IGR-1)
            CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONUTI', NBMAIL, K8B )
            CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
            WRITE(IFM,1000) GMAT, NBMAIL
            NORIEN=0
            CALL ORILMA ( NOMA, NDIM,  ZI(JGRO), NBMAIL, NORIEN, NTRAIT,
     &                    REORIE, NBMASU, ZI(JMASU) )
            NORIT = NORIT + NORIEN
            WRITE(IFM,1100) NORIEN
            IF (NTRAIT.NE.0) WRITE(IFM,1110) NTRAIT
 110     CONTINUE
         CALL JEDETR ( '&&ORILGM.WORK' )
         CALL JEDETR('&&ORILGM.GROUP_MA_SURF')
 100  CONTINUE
C
C --- TRAITEMENT DE 'ORIE_PEAU_3D' :
C     ----------------------------
C
      DO 200 IOCC = 1 , NBF2
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA3D, 'GROUP_MA',
     &                                           IOCC,IARG, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFA3D, 'GROUP_MA',
     &                                     IOCC,IARG, NG, ZK8(JJJ), NG )

C        PRESENCE DE GROUP_MA_VOLU ?
C        ---------------------------
         CALL GETVTX(MOFA3D,'GROUP_MA_VOLU',IOCC,IARG, 0,K8B, NGV )
         IF(NGV.NE.0)THEN
           NGV = -NGV
           CALL WKVECT ( '&&ORILGM.WORK2', 'V V K8', NGV, JGV )
           CALL GETVEM ( NOMA, 'GROUP_MA', MOFA3D, 'GROUP_MA_VOLU',
     &                                     IOCC,IARG, NGV, ZK8(JGV),
     &                                      NGV )
           CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMATO,
     &                 K8B,IRET)
           CALL WKVECT ('&&ORILGM.WORK3','V V I', NBMATO, JJV )
           DO 201 IMA = 1, NBMATO
              ZI(JJV+IMA-1)=0
 201       CONTINUE
           DO 202 IGR = 1, NGV
             GMAT = ZK8(JGV+IGR-1)
             CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONMAX', NBMAVI, K8B )
             CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JMAVI )
             DO 203 IMA=1,NBMAVI
                ZI(JJV+ZI(JMAVI+IMA-1)-1)=1
 203         CONTINUE
 202       CONTINUE
C          NOMBRE DE MAILLES 'VOLUMIQUES' (SANS DOUBLON) : NBMAVO
           NBMAVO=0
           DO 204 IMA = 1, NBMATO
             NBMAVO=NBMAVO+ZI(JJV+IMA-1)
 204       CONTINUE
C          LISTE DES MAILLES 'VOLUMIQUES' (SANS DOUBLON) : ZI(JMAVO)
           CALL WKVECT('&&ORILGM.GROUP_MA_VOLU','V V I',NBMAVO,JMAVO)
           K=0
           DO 205 IMA = 1, NBMATO
             IF(ZI(JJV+IMA-1).EQ.1) THEN
                   K=K+1
                   ZI(JMAVO+K-1)=IMA
             ENDIF
 205       CONTINUE
           CALL JEDETR('&&ORILGM.WORK3')
           CALL JEDETR('&&ORILGM.WORK2')
         ELSE
           NBMAVO=0
           CALL WKVECT('&&ORILGM.GROUP_MA_VOLU','V V I',1,JMAVO)
         ENDIF
C
C
         DO 210 IGR = 1, NG
            GMAT = ZK8(JJJ+IGR-1)
            CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONUTI', NBMAIL, K8B )
            CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
            WRITE(IFM,1000) GMAT,  NBMAIL
            NORIEN=0
            CALL ORILMA ( NOMA, NDIM, ZI(JGRO), NBMAIL, NORIEN, NTRAIT,
     &                    REORIE, NBMAVO, ZI(JMAVO) )
            NORIT = NORIT + NORIEN
            WRITE(IFM,1100) NORIEN
            IF (NTRAIT.NE.0) WRITE(IFM,1110) NTRAIT
 210     CONTINUE
         CALL JEDETR('&&ORILGM.WORK' )
         CALL JEDETR('&&ORILGM.GROUP_MA_VOLU')
 200  CONTINUE
C
C --- TRAITEMENT DE 'ORIE_NORM_COQUE':
C     -------------------------------
C
      DO 300 IOCC = 1 , NBF3
         ORIVEC = .FALSE.
         CALL GETVR8 ( MOFB3D, 'VECT_NORM', IOCC,IARG,0, VECT, N1 )
         IF (N1.NE.0)THEN
            ORIVEC = .TRUE.
            CALL GETVR8 ( MOFB3D, 'VECT_NORM', IOCC,IARG,-N1, VECT, N1 )
            CALL GETVTX ( MOFB3D, 'NOEUD', IOCC,IARG,0, K8B, N2 )
            IF (N2.NE.0)THEN
               CALL GETVTX ( MOFB3D, 'NOEUD', IOCC,IARG,1, NNOEUD, N2 )
               CALL JENONU (JEXNOM(NOMNOE,NNOEUD),NOEUD)
               IF(NOEUD.EQ.0)CALL U2MESK('F','MODELISA5_97',1,NNOEUD)
            ELSE
               CALL GETVTX(MOFB3D,'GROUP_NO',IOCC,IARG,1,NNOEUD,N3)
               CALL UTNONO(' ',NOMA,'NOEUD',NNOEUD,K8B,IER)
               IF ( IER .EQ. 10 ) THEN
                  VALK = NNOEUD
                  CALL U2MESG('F', 'MODELISA8_75',1,VALK,0,0,0,0.D0)
               ELSEIF ( IER .EQ. 1 ) THEN
                  VALK = K8B
                  CALL U2MESG('A', 'SOUSTRUC_87',1,VALK,0,0,0,0.D0)
               ENDIF
               CALL JENONU ( JEXNOM(NOMNOE,K8B), NOEUD )
            ENDIF
         ENDIF
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFB3D, 'GROUP_MA',
     &                                           IOCC,IARG, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFB3D, 'GROUP_MA',
     &                                     IOCC,IARG, NG, ZK8(JJJ), NG )
         IF ( ORIVEC ) THEN
            DO 310 IGR = 1, NG
               GMAT = ZK8(JJJ+IGR-1)
               CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONUTI', NBMAIL,K8B)
               CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
               WRITE(IFM,1000) GMAT,  NBMAIL
               NORIEN=0
               CALL ORVLMA(NOMA,ZI(JGRO),NBMAIL,NORIEN,VECT,NOEUD)
               NORIT = NORIT + NORIEN
               WRITE(IFM,1100) NORIEN
 310        CONTINUE
         ELSE
            DO 320 IGR = 1, NG
               GMAT = ZK8(JJJ+IGR-1)
               CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONUTI', NBMAIL,K8B)
               CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
               WRITE(IFM,1000) GMAT,  NBMAIL
               NORIEN=0
               CALL ORNORM ( NOMA, ZI(JGRO), NBMAIL, REORIE, NORIEN )
               NORIT = NORIT + NORIEN
               WRITE(IFM,1100) NORIEN
 320        CONTINUE
         ENDIF
         CALL JEDETR ( '&&ORILGM.WORK' )
 300  CONTINUE
C
C --- TRAITEMENT DE 'ORIE_LIGNE':
C     ------------------------------
C
      DO 400 IOCC = 1 , NCF3
         ORIVEC = .FALSE.
         CALL GETVR8 ( MOFC3D, 'VECT_TANG', IOCC,IARG,0, VECT, N1 )
         IF (N1.NE.0)THEN
            ORIVEC = .TRUE.
            CALL GETVR8 ( MOFC3D, 'VECT_TANG', IOCC,IARG,-N1, VECT, N1 )
            CALL GETVTX ( MOFC3D, 'NOEUD', IOCC,IARG,0, K8B, N2 )
            IF (N2.NE.0)THEN
               CALL GETVTX ( MOFC3D, 'NOEUD', IOCC,IARG,1, NNOEUD, N2 )
               CALL JENONU (JEXNOM(NOMNOE,NNOEUD),NOEUD)
               IF(NOEUD.EQ.0)CALL U2MESK('F','MODELISA5_97',1,NNOEUD)
            ELSE
               CALL GETVTX(MOFC3D,'GROUP_NO',IOCC,IARG,1,NNOEUD,N3)
               CALL UTNONO(' ',NOMA,'NOEUD',NNOEUD,K8B,IER)
               IF ( IER .EQ. 10 ) THEN
                  VALK = NNOEUD
                  CALL U2MESG('F', 'MODELISA8_75',1,VALK,0,0,0,0.D0)
               ELSEIF ( IER .EQ. 1 ) THEN
                  VALK = K8B
                  CALL U2MESG('A', 'SOUSTRUC_87',1,VALK,0,0,0,0.D0)
               ENDIF
               CALL JENONU ( JEXNOM(NOMNOE,K8B), NOEUD )
            ENDIF
         ENDIF
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFC3D, 'GROUP_MA',
     &                                           IOCC,IARG, 0, K8B, NG )
         NG = -NG
         CALL WKVECT ( '&&ORILGM.WORK', 'V V K8', NG, JJJ )
         CALL GETVEM ( NOMA, 'GROUP_MA', MOFC3D, 'GROUP_MA',
     &                                     IOCC,IARG, NG, ZK8(JJJ), NG )
         IF ( ORIVEC ) THEN
            DO 410 IGR = 1, NG
               GMAT = ZK8(JJJ+IGR-1)
               CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONUTI', NBMAIL,K8B)
               CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
               WRITE(IFM,1000) GMAT,  NBMAIL
               NORIEN=0
               CALL ORVLSE(NOMA,ZI(JGRO),NBMAIL,NORIEN,VECT,NOEUD)
               NORIT = NORIT + NORIEN
               WRITE(IFM,1100) NORIEN
 410        CONTINUE
         ELSE
            DO 420 IGR = 1, NG
               GMAT = ZK8(JJJ+IGR-1)
               CALL JELIRA (JEXNOM(GRMAMA,GMAT), 'LONUTI', NBMAIL,K8B)
               CALL JEVEUO (JEXNOM(GRMAMA,GMAT), 'L', JGRO )
               WRITE(IFM,1000) GMAT,  NBMAIL
               NORIEN=0
               CALL ORNORM ( NOMA, ZI(JGRO), NBMAIL, REORIE, NORIEN )
               NORIT = NORIT + NORIEN
               WRITE(IFM,1100) NORIEN
 420        CONTINUE
         ENDIF
         CALL JEDETR ( '&&ORILGM.WORK' )
 400  CONTINUE
C
      IF ( NORIT .NE. 0 )  WRITE(IFM,1010) NORIT
C
 1000 FORMAT('TRAITEMENT DU GROUP_MA: ',A8,' DE ',I7,' MAILLES')
 1100 FORMAT(24X,I7,' MAILLE(S) ONT ETE ORIENTEE(S)')
 1110 FORMAT(24X,I7,' MAILLE(S) N''ONT PAS ETE TRAITEE(S) ')
 1010 FORMAT('AU TOTAL ', I7, ' MAILLE(S) ORIENTEE(S) ')
C
      CALL JEDEMA()
      END

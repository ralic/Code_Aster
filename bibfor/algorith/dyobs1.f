      SUBROUTINE DYOBS1(MOTFAC,MAILLA,NBOCC ,LSUIVI,NTOBS ,
     &                  NBSUIV)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/10/2010   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSBALE
C
      IMPLICIT     NONE
      CHARACTER*16 MOTFAC      
      CHARACTER*8  MAILLA
      INTEGER      NBOCC
      INTEGER      NTOBS,NBSUIV
      LOGICAL      LSUIVI(NBOCC)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (OBSERVATION - CREATION SD)
C
C VERIFICATION DES DONNEES ET COMPTAGE DES FONCTIONS
C
C ----------------------------------------------------------------------
C
C
C IN  MOTFAC : MOT-CLEF FACTEUR POUR OBSERVATION
C IN  MAILLA : NOM DU MAILLAGE
C IN  NBOCC  : NOMBRE D'OCCURENCES DU MOT-CLEF FACTEUR OBSERVATION
C OUT LSUIVI : POUR CHAQUE OBSERVATION DIT SI C'EST UN SUIVI_DDL OU PAS
C OUT NBSUIV : NOMBRE D'OBSERVATION EN MODE SUIVI_DDL 
C OUT NTOBS  : NOMBRE D'OBSERVATIONS PAR INSTANT D'OBSERVATION
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
      CHARACTER*32       JEXNOM
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      N1,N2,N3,N4,INO,IMA,IGNO,IOCC,NBN
      INTEGER      IBID,IRET,NCHP,NCMP,I,NBNO,NBMA,NBGN
      INTEGER      JNOE,JGRN,JMAI,NBPT,NBGM,IGMA
      INTEGER      JCMP,JGRM,NBM,II,NN
      INTEGER      N6,N7,N8
      LOGICAL      CHAMNO,CHAMES,CHAMEV,LSTOP
      CHARACTER*8  K8B,MO,SDDL
      CHARACTER*16 NOCHP(50)
      CHARACTER*24 GRPNO,NOMNOE,NOMMAI,GRPMA
      CHARACTER*24 VALK(3)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      GRPNO  = MAILLA//'.GROUPENO'
      GRPMA  = MAILLA//'.GROUPEMA'
      NOMNOE = MAILLA//'.NOMNOE'
      NOMMAI = MAILLA//'.NOMMAI'
C
C --- INITIALISATIONS
C
      NTOBS  = 0
      NBSUIV = 0
      II     = 0
C
      DO 1 IOCC = 1 , NBOCC
        CALL GETVTX(MOTFAC,'SUIVI_DDL',IOCC,1,1,SDDL,IRET )
        IF (SDDL.EQ.'NON')THEN
          LSUIVI(IOCC) = .FALSE.
        ELSE
          II = II+1
          IF (II.GT.4) CALL U2MESS('F','OBSERVATION_85')
          LSUIVI(IOCC) = .TRUE.
        ENDIF
 1    CONTINUE

      DO 10 IOCC = 1 , NBOCC
C
C --- VERIFICATION DES CHAMPS 
C
         CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC,1,0,K8B,N1)
         NCHP = -N1
         CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC,1,NCHP,NOCHP,N1)
         CHAMNO = .FALSE.
         CHAMES = .FALSE.
         CHAMEV = .FALSE.
         DO 12 I = 1 , NCHP
            IF ( NOCHP(I)(1:4) .EQ. 'DEPL' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'TEMP' ) THEN
               CHAMNO = .TRUE.               
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'VITE' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'ACCE' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'FORC_NODA' .AND.
     &              LSUIVI(IOCC)) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VALE_CONT' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' ) THEN
               CHAMES = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               CHAMEV = .TRUE.
            ELSE
               CALL ASSERT(.FALSE.)   
            ENDIF
 12      CONTINUE
         IF ( CHAMES .AND. CHAMEV   .OR.
     &        CHAMNO .AND. CHAMES   .OR.
     &        CHAMNO .AND. CHAMEV )  THEN
            IF (LSUIVI(IOCC)) THEN
               CALL U2MESS('F','OBSERVATION_86')
            ELSE 
               CALL U2MESS('F','OBSERVATION_87')
           ENDIF
         ENDIF
C
C --- VERIFICATION DES COMPOSANTES 
C
         CALL GETVTX(MOTFAC,'NOM_CMP' , IOCC,1,0, K8B , N2 )
         NCMP = -N2
         CALL WKVECT('&&DYOBS1.LIST_CMP','V V K8',NCMP,JCMP)
         CALL GETVTX(MOTFAC,'NOM_CMP',IOCC,1,NCMP,
     &        ZK8(JCMP),IRET)
C
C --- VERIFICATION DES NOEUDS ET MAILLES 
C
         CALL GETVTX(MOTFAC,'NOEUD'   ,IOCC,1,0,K8B ,N1 )
         CALL GETVTX(MOTFAC,'GROUP_NO',IOCC,1,0,K8B ,N2 )
         IF (CHAMEV.OR.CHAMES) THEN
           CALL GETVTX(MOTFAC,'MAILLE'  ,IOCC,1,0,K8B ,N3 )
           CALL GETVIS(MOTFAC,'POINT'   ,IOCC,1,0,IBID,N4 )
         ELSE
           N3     = 0
           N4     = 0
         ENDIF         
C         
         IF(LSUIVI(IOCC))THEN
           CALL GETVTX(MOTFAC,'GROUP_MA',IOCC,1,0,K8B ,N6 )
           CALL GETVTX(MOTFAC,'VALE_MAX' ,IOCC,1,1,K8B ,N7 )
           CALL GETVTX(MOTFAC,'VALE_MIN' ,IOCC,1,1,K8B ,N8 )
         ENDIF
C
         IF ( N1 .NE. 0 ) THEN
            NBNO = -N1
            CALL WKVECT ('&&DYOBS1.LIST_NOEU','V V K8',NBNO,JNOE)
            CALL GETVTX (MOTFAC,'NOEUD',IOCC,1,NBNO,
     &                   ZK8(JNOE),N1)
            DO 20 INO = 0 , NBNO-1
               CALL JEEXIN ( JEXNOM(NOMNOE,ZK8(JNOE+INO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  VALK (1) = ZK8(JNOE+INO)
                  VALK (2) = MAILLA
                  CALL U2MESG('F', 'OBSERVATION_2',2,VALK,0,0,0,0.D0)
               ENDIF
 20         CONTINUE
         ENDIF
         IF ( N2 .NE. 0 ) THEN
            NBGN = -N2
            CALL WKVECT ('&&DYOBS1.LIST_GRNO','V V K8',NBGN,JGRN)
            CALL GETVTX (MOTFAC,'GROUP_NO',IOCC,1,NBGN,
     &                   ZK8(JGRN),N2)
            DO 22 IGNO = 0 , NBGN-1
               CALL JEEXIN ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  VALK (1) = ZK8(JGRN+IGNO)
                  VALK (2) = MAILLA
                  CALL U2MESG('F', 'OBSERVATION_3',2,VALK,0,0,0,0.D0)
               ENDIF
 22         CONTINUE
         ENDIF
         IF ( N3 .NE. 0 ) THEN
            NBMA = -N3
            CALL WKVECT ('&&DYOBS1.LIST_MAIL','V V K8',NBMA,JMAI)
            CALL GETVTX (MOTFAC,'MAILLE',IOCC,1,NBMA,
     &                   ZK8(JMAI),N3)
            DO 24 IMA = 0 , NBMA-1
               CALL JEEXIN ( JEXNOM(NOMMAI,ZK8(JMAI+IMA)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  VALK (1) = ZK8(JMAI+IMA)
                  VALK (2) = MAILLA
                  CALL U2MESG('F', 'OBSERVATION_4',2,VALK,0,0,0,0.D0)
               ENDIF
 24         CONTINUE
         ENDIF
         IF ( LSUIVI(IOCC) .AND. N6 .NE. 0 ) THEN
            NBGM = -N6
            CALL WKVECT ('&&DYOBS1.LIST_GRMA','V V K8',NBGM,JGRM)
            CALL GETVTX (MOTFAC,'GROUP_MA',IOCC,1,NBGM,
     &                   ZK8(JGRM),N6)
            DO 25 IGMA = 0 , NBGM-1
               CALL JEEXIN ( JEXNOM(GRPMA,ZK8(JGRM+IGMA)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  VALK (1) = ZK8(JGRM+IGMA)
                  VALK (2) = MAILLA
                  CALL U2MESG('F', 'OBSERVATION_5',2,VALK,0,0,0,0.D0)
               ENDIF
 25         CONTINUE
         ENDIF
       
C
         NBPT = 0
         DO 16 I = 1 , NCHP
            IF ( NOCHP(I)(1:4) .EQ. 'DEPL' .OR.
     &           NOCHP(I)(1:4) .EQ. 'TEMP' .OR.            
     &           NOCHP(I)(1:4) .EQ. 'VITE' .OR.
     &           NOCHP(I)(1:4) .EQ. 'ACCE' .OR.
     &           NOCHP(I)(1:9) .EQ. 'VALE_CONT'.OR.
     &         (LSUIVI(IOCC).AND.NOCHP(I)(1:9).EQ.'FORC_NODA'))THEN
               IF ( N1 .NE. 0 ) THEN
                 NBPT=NBNO
               ELSEIF ( N2 .NE. 0 ) THEN
                  NN = 0
                  DO 18 IGNO = 0 , NBGN-1
                     CALL JELIRA (JEXNOM(GRPNO,ZK8(JGRN+IGNO)),
     &                            'LONMAX',NBN,K8B )
                    NN = NN + NBN
 18               CONTINUE
                  NBPT = NN
               ELSE IF (LSUIVI(IOCC))THEN
                  IF( (N7.NE.0) .OR. (N8.NE.0) ) NBPT=NBPT+1
               ELSE
                  VALK (1) = NOCHP(I)(1:4)
                  VALK (2) = 'NOEUD'
                  VALK (3) = 'GROUP_NO'
                  CALL U2MESG('F', 'OBSERVATION_6',3,VALK,0,0,0,0.D0)
               ENDIF
C
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' .OR.
     &               NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN

               LSTOP=.FALSE.
               IF (LSUIVI(IOCC))THEN

                 IF( N6 .NE. 0 ) THEN
                   NBM=0
                   DO 17 IGMA = 0 , NBGM-1
                     CALL JELIRA (JEXNOM(GRPMA,ZK8(JGRM+IGMA)),
     &                            'LONMAX',NBM,K8B )
                     NBMA = NBMA + NBM
 17                CONTINUE
                 ENDIF

                  IF( (N7.NE.0) .OR. (N8.NE.0) )THEN
                     NBPT=NBPT+1
                  ELSEIF( (N3*N4 .NE. 0) .OR. (N6*N4 .NE. 0)) THEN
                     NBPT = NBPT + ABS(NBMA*N4)
                  ELSE
                     LSTOP=.TRUE.
                  ENDIF

               ELSE
                  IF( N3*N4 .NE. 0)THEN
                     NBPT = NBPT + ABS( N3*N4 )
                  ELSE
                     LSTOP=.TRUE.
                  ENDIF
               ENDIF


               IF ( LSTOP ) THEN
                  VALK (1) = NOCHP(I)
                  VALK (2) = 'MAILLE'
                  VALK (3) = 'POINT'
                  CALL U2MESG('F', 'OBSERVATION_7',3,VALK,0,0,0,0.D0)
               ENDIF

            ENDIF
C
 16      CONTINUE
C
C --- VERIFICATION QUE LES NOEUDS SUPPORTENT LES COMPOSANTES FOURNIES
C
       IF (CHAMNO) THEN
         CALL GETVID(' ','MODELE',0,1,1,MO,IRET)

         IF (N2.NE.0) THEN
           CALL CHKCMP(MO,MAILLA,
     &                 NCMP,ZK8(JCMP),NBGN,ZK8(JGRN),'GROUP_NO', 'F')
         ENDIF

         IF (N1.NE.0) THEN
           CALL CHKCMP(MO,MAILLA,
     &                 NCMP,ZK8(JCMP),NBNO,ZK8(JNOE),'NOEUD   ', 'F')
         ENDIF
       ENDIF
C
C --- NETTOYAGE
C
      CALL JEDETR('&&DYOBS1.LIST_CMP' )
      CALL JEDETR('&&DYOBS1.LIST_NOEU')
      CALL JEDETR('&&DYOBS1.LIST_GRNO')
      CALL JEDETR('&&DYOBS1.LIST_MAIL')
      CALL JEDETR('&&DYOBS1.LIST_GRMA')
C
C --- NOMBRE DE FONCTIONS CREEES
C
      IF ( LSUIVI(IOCC))THEN
        NBSUIV = NBSUIV + ( NCHP * NCMP * NBPT )
      ELSE
        NTOBS  = NTOBS + ( NCHP * NCMP * NBPT )
      ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END

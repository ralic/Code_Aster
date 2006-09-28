      SUBROUTINE OP0154 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C
C   OPERATEUR: MODI_MAILLAGE
C
C     ------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES  JEVEUX -----------------------
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
C     --- FIN  DECLARATIONS  NORMALISEES  JEVEUX -----------------------
C
      INTEGER       N1, N2, NBOCC, NBOC1, NBOC2,NOP,I,
     &              DIM, IADCOO, NBNO, NDDL
      LOGICAL       BIDIM
      CHARACTER*8   MA, MA2, DEPLA, K8BID, COUTUR
      CHARACTER*16  KBI1, KBI2, OPTION
      CHARACTER*19  GEOMI, GEOMF,GEOM1
      CHARACTER*24  COORJV
      REAL*8        LTCHAR, PT(3), PT2(3), DIR(3), ANGL

      REAL*8        AXE1(3),AXE2(3),PERP(3)

C -DEB------------------------------------------------------------------
C
      CALL INFMAJ
C
      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, MA, N1 )
C
      CALL GETRES ( MA2, KBI1, KBI2 )
C
      IF (MA.NE.MA2) CALL U2MESS('F','SOUSTRUC_15')
C
C
C     --- TRAITEMENT DU MOT CLEF  "ORIE_FISSURE" :
C     ---------------------------------------------
      CALL GETFAC ( 'ORIE_FISSURE', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         CALL CONORI ( MA )
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "MODI_MAILLE" :
C     ---------------------------------------------
      CALL GETFAC ( 'MODI_MAILLE', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         CALL GETVTX ( 'MODI_MAILLE', 'OPTION', 1,1,1, OPTION, N1 )
         IF ( OPTION .EQ.  'NOEUD_QUART' ) THEN
            CALL MOMABA ( MA )
         ENDIF
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "DEFORME" :
C     ---------------------------------------
      CALL GETFAC ( 'DEFORME', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         CALL GETVTX ('DEFORME','OPTION',1,1,1,OPTION,NOP)
         CALL GETVID ( 'DEFORME', 'DEPL', 1,1,1, DEPLA, N1 )
         CALL CHPVER('F',DEPLA,'NOEU','DEPL_R',IER)
         GEOMI = MA//'.COORDO'
         GEOMF = MA//'.COORD2'
         CALL VTGPLD ( GEOMI, 1.D0, DEPLA, 'V', GEOMF )
         CALL DETRSD ( 'CHAMP_GD', GEOMI )
         IF (OPTION.EQ.'TRAN_APPUI') THEN
            CALL DEFAPP ( MA, GEOMF, 1.D0, DEPLA, 'G', GEOMI )
         ELSE
            CALL COPISD ( 'CHAMP_GD', 'G', GEOMF, GEOMI )
         ENDIF
         CALL DETRSD ( 'CHAMP_GD', GEOMF )
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "TRANSLATION" :
C     ---------------------------------------
      CALL GETVID ( ' ', 'TRANSLATION', 1,1,0, K8BID, N1 )
      IF ( N1 .NE. 0 ) THEN
         GEOMI = MA//'.COORDO'
         BIDIM = .FALSE.
         CALL GETVR8 ( ' ', 'TRANSLATION', 1, 1, 0, LTCHAR, DIM )
         DIM = (-1.D0) * DIM
         IF ( DIM .EQ. 2 ) THEN
           CALL GETVR8 ( ' ', 'TRANSLATION', 1, 1, 2, DIR, N1 )
           DIR(3) = 0.D0
           BIDIM = .TRUE.
         ELSE
            CALL GETVR8 ( ' ', 'TRANSLATION', 1, 1, 3, DIR, N1 )
         ENDIF
         CALL TRANMA ( GEOMI,  DIR, BIDIM )
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "MODI_BASE" :
C     ---------------------------------------
      CALL GETFAC ( 'MODI_BASE', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         GEOMI = MA//'.COORDO'
         BIDIM = .FALSE.
         CALL GETVR8 ( 'MODI_BASE', 'VECT_X', 1, 1, 0, LTCHAR, DIM )
         DIM = (-1.D0) * DIM
         IF ( DIM .EQ. 2 ) THEN
            CALL GETVR8 ( 'MODI_BASE', 'VECT_X', 1, 1, 2, PT, N1 )
            PT(3) = 0.D0
            CALL INITVE(3,PT2)
            BIDIM = .TRUE.
         ELSE
            CALL GETVR8 ( 'MODI_BASE', 'VECT_X', 1, 1, 3, PT, N1 )
            CALL GETVR8 ( 'MODI_BASE', 'VECT_Y', 1, 1, 3, PT2, N1 )
         ENDIF
         CALL CHGREF ( GEOMI, PT, PT2, BIDIM )
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "ROTATION" :
C     ---------------------------------------
      CALL GETFAC ( 'ROTATION', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         GEOMI = MA//'.COORDO'
         BIDIM = .FALSE.
         DO 10 I = 1 , NBOCC
            CALL GETVR8 ( 'ROTATION', 'POIN_1', I, 1, 0, LTCHAR, DIM )
            CALL GETVR8 ( 'ROTATION', 'ANGL', I, 1, 1, ANGL, N1 )
            CALL GETVID ( 'ROTATION', 'POIN_2', I, 1, 0, K8BID, N2 )
            DIM = (-1.D0) * DIM
            IF ( DIM .EQ. 2 ) THEN
               CALL GETVR8 ( 'ROTATION', 'POIN_1', I, 1, 2, PT, N1 )
               PT(3) = 0.D0
               CALL INITVE(3,PT2)
               CALL INITVE(3,DIR)
               BIDIM = .TRUE.
            ELSE
               CALL GETVR8 ( 'ROTATION', 'POIN_1', I, 1, 3, PT, N1 )
               IF ( N2 .NE. 0 ) THEN
                  CALL GETVR8 ( 'ROTATION', 'POIN_2', I, 1, 3, PT2, N1 )
                  CALL VDIFF(3,PT2,PT,DIR)
               ELSE
                  CALL GETVR8 ( 'ROTATION', 'DIR', I, 1, 3, DIR, N1 )
               ENDIF
            ENDIF
            CALL ROTAMA ( GEOMI, PT, DIR, ANGL, BIDIM )
 10      CONTINUE
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "SYMETRIE" :
C     ---------------------------------------
      CALL GETFAC ( 'SYMETRIE', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         GEOMI = MA//'.COORDO'
         DO 20 I = 1 , NBOCC
            CALL GETVR8 ( 'SYMETRIE', 'POINT', I, 1, 0, PT,   DIM )
            CALL GETVR8 ( 'SYMETRIE', 'AXE_1', I, 1, 0, AXE1, N1 )
            CALL GETVR8 ( 'SYMETRIE', 'AXE_2', I, 1, 0, AXE2, N2 )

C           DIM, N1, N2 = 2 OU 3 ==> IMPOSE PAR LES CATALOGUES
C           EN 2D : DIM=N1=2    , AXE_2 N'EXISTE PAS N2=0
C           EN 3D : DIM=N1=N2=3
            IF ( DIM .EQ. -2) THEN
               IF ( N1 .NE. DIM ) THEN
                  CALL U2MESS('F','ALGORITH9_62')
               ENDIF
               IF ( N2 .NE. 0 ) THEN
                  CALL U2MESS('A','ALGORITH9_63')
               ENDIF
               CALL GETVR8 ( 'SYMETRIE', 'POINT', I, 1, 2, PT,   DIM )
               CALL GETVR8 ( 'SYMETRIE', 'AXE_1', I, 1, 2, AXE1, N1 )
C              CONSTRUCTION DU VECTEUR PERPENDICULAIRE A Z ET AXE1
               PERP(1) = -AXE1(2)
               PERP(2) =  AXE1(1)
               PERP(3) =  0.0D0
            ELSE
               IF ( N1 .NE. DIM ) THEN
                 CALL U2MESS('F','ALGORITH9_62')
               ENDIF
               IF ( N2 .NE. DIM ) THEN
                 CALL U2MESS('F','ALGORITH9_64')
               ENDIF
               CALL GETVR8 ( 'SYMETRIE', 'POINT', I, 1, 3, PT,   DIM )
               CALL GETVR8 ( 'SYMETRIE', 'AXE_1', I, 1, 3, AXE1, N1 )
               CALL GETVR8 ( 'SYMETRIE', 'AXE_2', I, 1, 3, AXE2, N2 )
C              CONSTRUCTION DU VECTEUR PERPENDICULAIRE A AXE1 ET AXE2
               PERP(1) = AXE1(2)*AXE2(3) - AXE1(3)*AXE2(2)
               PERP(2) = AXE1(3)*AXE2(1) - AXE1(1)*AXE2(3)
               PERP(3) = AXE1(1)*AXE2(2) - AXE1(2)*AXE2(1)
            ENDIF
            CALL SYMEMA( GEOMI, PERP, PT)
 20      CONTINUE
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "ECHELLE" :
C     ---------------------------------------
      CALL GETVID ( ' ', 'ECHELLE', 1,1,0, K8BID, N1 )
      IF ( N1 .NE. 0 ) THEN
         GEOMI = MA//'.COORDO'
         CALL GETVR8 ( ' ', 'ECHELLE', 1, 1, 1, LTCHAR, N2 )
         CALL ECHELL ( GEOMI,  LTCHAR )
      ENDIF
C
C
C     --- TRAITEMENT DU MOT CLEF  "EQUE_PIQUA" :
C     ------------------------------------------
      CALL GETFAC ( 'EQUE_PIQUA', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         CALL PIQINI ( MA )
         CALL PIQELI ( MA )
      ENDIF
C
C
C     --- TRAITEMENT DES MOTS CLES  "ORIE_PEAU_2D" , "ORIE_PEAU_3D"
C                               ET  "ORIE_NORM_COQUE" :
C     ---------------------------------------------------------------
        CALL ORILGM(MA)
C
C     --- TRAITEMENT DU MOT CLEF  "ORIE_SHB8" :
C     ------------------------------------------
      CALL GETFAC ( 'ORIE_SHB8', NBOCC )
      IF ( NBOCC .NE. 0 ) THEN
         CALL ORISHB ( MA )
      ENDIF
C
C     --- TRAITEMENT DES MOT CLEF  "PLAQ_TUBE" ET "TUBE_COUDE":
C     --------------------------------------------------------
      CALL GETFAC ( 'PLAQ_TUBE', NBOC1 )
      IF ( NBOC1 .NE. 0 ) THEN
         CALL GETVR8 ( 'PLAQ_TUBE', 'L_TUBE_P1' , 1,1,1, LTCHAR , N1 )
         CALL GETVTX ( 'PLAQ_TUBE', 'COUTURE'   , 1,1,1, COUTUR , N1 )
         IF (COUTUR.EQ.'OUI') CALL ASCELI ( MA )
         CALL ASCTUB ( MA )
      ENDIF
      CALL GETFAC ( 'TUBE_COUDE', NBOC2 )
      IF ( NBOC2 .NE. 0 ) THEN
        CALL GETVR8 ( 'TUBE_COUDE', 'L_TUBE_P1' , 1,1,1, LTCHAR , N1 )
        CALL ASCCOU ( MA )
      ENDIF
      IF (NBOC1.NE.0 .OR. NBOC2.NE.0) THEN
        CALL ASCREP ( MA, LTCHAR )
      END IF
C
C
      CALL CARGEO ( MA )
C
C
      END

      SUBROUTINE PJEFTE (RESU1, RESU2 )
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/11/2004   AUTEUR VABHHTS J.PELLET 
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
C     COMMANDE:  PROJ_CHAMP  METHODE:'ELEM'
C ----------------------------------------------------------------------
C
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*(*) RESU1, RESU2
C
C 0.2. ==> COMMUNS
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'PJEFTE' )

      CHARACTER*4 CDIM1,CDIM2,EXIVOL
      CHARACTER*8 NOMA1,NOMA2,MODEL1,MODEL2
      CHARACTER*16 CORRES,CORRE1,CORRE2,CORRE3
      CHARACTER*16 TYMOCL(5),MOTCLE(5)
      INTEGER NDIM,NCAS,N1,NBOCC,IOCC,IE,IBID,NBNO2,NBMA1
      INTEGER IAGNO2,IAGMA1,K,JTYPM1,TYPM1
      INTEGER KK,LTMVOL(9),INDIIS

      LOGICAL LDMAX
      REAL*8  DISTMA,R8MAEM
C----------------------------------------------------------------------
C       DESCRIPTION DE LA SD CORRESP_2_MAILLA : CORRES
C       --------------------------------------------------------
C          (CORRESPONDANCE ENTRE LES 2 MODELES MODEL1 ET MODEL2)
C  DESCRIPTION DE LA SD CORRESP_2_MAILLA :
C  SOIT NNO2= NB_NO(MAILLAGE(MODEL2))

C  CORRESP_2_MAILLA (K16)  ::= RECORD
C     '.PJEF_NO'  : S V K8 LONG=2
C     '.PJEF_NB'  : S V I  LONG=NNO2
C     '.PJEF_M1'  : S V I  LONG=NNO2
C     '.PJEF_NU'  : S V I  LONG=LONT
C     '.PJEF_CF'  : S V R  LONG=LONT

C     '.PJEF_NO' (1) : NOM DU MAILLAGE 1 : M1
C     '.PJEF_NO' (2) : NOM DU MAILLAGE 2 : M2


C     '.PJEF_NB' (INO2) : NOMBRE DE NOEUDS DE M1 QUI DOIVENT SERVIR
C                          A L'INTERPOLATION DU NOEUD INO2 DE M2

C     '.PJEF_M1' (INO2) : NUMERO DE LA MAILLE DE M1 QUI DOIT SERVIR
C                         A L'INTERPOLATION DU NOEUD INO2 DE M2

C     '.PJEF_NU' : CONTIENT LES NUMEROS DES NOEUDS DE M1 SERVANT A
C                  L'INTERPOLATION DES NOEUDS DE M2 (MIS BOUT A BOUT)
C     '.PJEF_CF' : CONTIENT LES COEFFICIENTS POUR LES NOEUDS DE
C                  M1 SERVANT A L'INTERPOLATION DES NOEUDS DE M2
C                  (MIS BOUT A BOUT)

C    EXEMPLE D'UTILISATION :
C      ON VEUT SAVOIR COMMENT INTERPOLER INO2 A PARTIR DU MAILLAGE M1
C      SOIT NBNO1='.PJEF_NB'(INO2)
C      SOIT DECAL= SOMME POUR INO<INO2 DE '.PJEF_NB'(INO)
C      VAL2(INO2)=0
C      DO I=1,NBNO1
C        NUNO1='.PJEF_NU' (DECAL+I)
C        COEFR='.PJEF_CF' (DECAL+I)
C        VAL2(INO2)=VAL2(INO2)+COEFR*VAL1(UNO1)
C      END DO
C----------------------------------------------------------------------
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C               12   345678   9012345678901234
      CORRES = '&&'//NOMPRO//'.CORRESP'
      CORRE1 = '&&'//NOMPRO//'.CORRES1'
      CORRE2 = '&&'//NOMPRO//'.CORRES2'
      CORRE3 = '&&'//NOMPRO//'.CORRES3'

      CALL GETVID(' ','MODELE_1',1,1,1,MODEL1,N1)
      CALL GETVID(' ','MODELE_2',1,1,1,MODEL2,N1)

      CALL DISMOI('F','NOM_MAILLA',MODEL1,'MODELE',IBID,NOMA1,IE)
      CALL JEVEUO(NOMA1//'.TYPMAIL','L',JTYPM1)
      CALL DISMOI('F','NOM_MAILLA',MODEL2,'MODELE',IBID,NOMA2,IE)

      LDMAX = .FALSE.
      DISTMA = R8MAEM()
      CALL GETVR8(' ','DISTANCE_MAX',1,0,0,DISTMA,N1)
      IF ( ABS(N1) .EQ. 1 ) THEN
        CALL GETVR8(' ','DISTANCE_MAX',1,0,1,DISTMA,N1)
        LDMAX = .TRUE.
      ENDIF

C     DETERMINATION DE LA DIMENSION DE L'ESPACE (NDIM) :
C     --------------------------------------------------------
      CALL DISMOI('F','Z_CST',NOMA1,'MAILLAGE',IBID,CDIM1,IE)
      CALL DISMOI('F','Z_CST',NOMA2,'MAILLAGE',IBID,CDIM2,IE)
      IF ( CDIM1 .EQ. CDIM2 ) THEN
         IF (CDIM1.EQ.'OUI') THEN
            NDIM = 2
         ELSE
            NDIM = 3
         END IF
      ELSE
         IF ( CDIM1.EQ.'OUI' .AND. CDIM2.NE.'OUI') THEN
            CALL UTMESS('F',NOMPRO,'ON NE PEUT PAS PROJETER UN '//
     +              'CHAMP D''UN MAILLAGE "2D" SUR UN MAILLAGE "3D".')
         ELSEIF ( CDIM1.NE.'OUI' .AND. CDIM2.EQ.'OUI') THEN
            NDIM = 3
         END IF
      END IF

C     REMPLISSAGE DE LTMVOL : LISTE DES NUMEROS DES TYPE_MAILLE
C     VOLUMIQUES :
C     --------------------------------------------------------
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','HEXA8')  ,LTMVOL(1))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','HEXA20') ,LTMVOL(2))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','HEXA27') ,LTMVOL(3))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TETRA4') ,LTMVOL(4))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TETRA10'),LTMVOL(5))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PENTA6') ,LTMVOL(6))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PENTA15'),LTMVOL(7))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PYRAM5') ,LTMVOL(8))
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PYRAM13'),LTMVOL(9))


      CALL GETFAC('VIS_A_VIS',NBOCC)
      IF (NBOCC.EQ.0) THEN


C       -- CAS : TOUT:'OUI'
C       ------------------------

C     DETERMINATION DU CAS DE FIGURE : 2D, 3D OU 2.5D : NCAS
C     --------------------------------------------------------
        IF (NDIM.EQ.2) THEN
          NCAS = 2
        ELSE IF (NDIM.EQ.3) THEN
          CALL DISMOI('F','EXI_ELTVOL',MODEL1,'MODELE',IBID,EXIVOL,IE)
          IF (EXIVOL.EQ.'OUI') THEN
            NCAS = 3
          ELSE
            NCAS = 4
          END IF
        END IF


        IF (NCAS.EQ.2) THEN
          CALL PJ2DCO('TOUT',MODEL1,MODEL2,0,0,0,0,' ',' ',CORRES,
     +                 LDMAX,DISTMA)
        ELSE IF (NCAS.EQ.3) THEN
          CALL PJ3DCO('TOUT',MODEL1,MODEL2,0,0,0,0,' ',' ',CORRES,
     +                 LDMAX,DISTMA)
        ELSE IF (NCAS.EQ.4) THEN
          CALL PJ4DCO('TOUT',MODEL1,MODEL2,0,0,0,0,' ',' ',CORRES,
     +                 LDMAX,DISTMA)
        ELSE
          CALL UTMESS('F',NOMPRO,'STOP 4')
        END IF

      ELSE


C       -- CAS : VIS_A_VIS
C       ------------------------
        DO 30 IOCC = 1,NBOCC

C        -- RECUPERATION DE LA LISTE DE MAILLES LMA1 :
C        ----------------------------------------------
          MOTCLE(1) = 'MAILLE_1'
          TYMOCL(1) = 'MAILLE'
          MOTCLE(2) = 'GROUP_MA_1'
          TYMOCL(2) = 'GROUP_MA'
          MOTCLE(3) = 'TOUT_1'
          TYMOCL(3) = 'TOUT'
          CALL RELIEM(MODEL1,NOMA1,'NU_MAILLE','VIS_A_VIS',IOCC,3,
     +                MOTCLE,TYMOCL,'&&'//NOMPRO//'.LIMANU1',NBMA1)
          CALL JEVEUO('&&'//NOMPRO//'.LIMANU1','L',IAGMA1)


C        -- RECUPERATION DE LA LISTE DE NOEUDS LNO2 :
C        ----------------------------------------------
          MOTCLE(1) = 'NOEUD_2'
          TYMOCL(1) = 'NOEUD'
          MOTCLE(2) = 'GROUP_NO_2'
          TYMOCL(2) = 'GROUP_NO'
          MOTCLE(3) = 'MAILLE_2'
          TYMOCL(3) = 'MAILLE'
          MOTCLE(4) = 'GROUP_MA_2'
          TYMOCL(4) = 'GROUP_MA'
          MOTCLE(5) = 'TOUT_2'
          TYMOCL(5) = 'TOUT'
          CALL RELIEM(MODEL2,NOMA2,'NU_NOEUD','VIS_A_VIS',IOCC,5,MOTCLE,
     +                TYMOCL,'&&'//NOMPRO//'.LINONU2',NBNO2)
          CALL JEVEUO('&&'//NOMPRO//'.LINONU2','L',IAGNO2)



C         DETERMINATION DU CAS DE FIGURE : 2D, 3D OU 2.5D : NCAS
C         --------------------------------------------------------
          IF (NDIM.EQ.2) THEN
            NCAS = 2
          ELSE IF (NDIM.EQ.3) THEN
C            -- SI UNE MAILLE EST DE TYPE "VOLUMIQUE" : NCAS=3
C               SINON NCAS=4
            NCAS = 4
            DO 10,K = 1,NBMA1
              TYPM1 = ZI(JTYPM1-1+ZI(IAGMA1-1+K))
              KK = INDIIS(LTMVOL,TYPM1,1,9)
              IF (KK.GT.0) THEN
                NCAS = 3
                GO TO 20
              END IF
   10       CONTINUE
   20       CONTINUE
          END IF


C        -- CALCUL DU CORRESP_2_MAILLA POUR IOCC :
C        ----------------------------------------------
          CALL DETRSD('CORRESP_2_MAILLA',CORRE1)
          IF (NCAS.EQ.2) THEN
            CALL PJ2DCO('PARTIE',MODEL1,MODEL2,NBMA1,ZI(IAGMA1),NBNO2,
     +                  ZI(IAGNO2),' ',' ',CORRE1,LDMAX,DISTMA)
          ELSE IF (NCAS.EQ.3) THEN
            CALL PJ3DCO('PARTIE',MODEL1,MODEL2,NBMA1,ZI(IAGMA1),NBNO2,
     +                  ZI(IAGNO2),' ',' ',CORRE1,LDMAX,DISTMA)
          ELSE IF (NCAS.EQ.4) THEN
            CALL PJ4DCO('PARTIE',MODEL1,MODEL2,NBMA1,ZI(IAGMA1),NBNO2,
     +                  ZI(IAGNO2),' ',' ',CORRE1,LDMAX,DISTMA)
          ELSE
            CALL UTMESS('F',NOMPRO,'STOP 5')
          END IF


C        -- SURCHARGE DU CORRESP_2_MAILLA :
C        ----------------------------------------------
          IF (IOCC.EQ.1) THEN
            CALL COPISD('CORRESP_2_MAILLA','V',CORRE1,CORRE2)
          ELSE
            CALL PJFUCO(CORRE2,CORRE1,'V',CORRE3)
            CALL DETRSD('CORRESP_2_MAILLA',CORRE2)
            CALL COPISD('CORRESP_2_MAILLA','V',CORRE3,CORRE2)
            CALL DETRSD('CORRESP_2_MAILLA',CORRE3)
          END IF

          CALL JEDETR('&&'//NOMPRO//'.LIMANU1')
          CALL JEDETR('&&'//NOMPRO//'.LINONU2')
   30   CONTINUE
        CALL COPISD('CORRESP_2_MAILLA','V',CORRE2,CORRES)
        CALL DETRSD('CORRESP_2_MAILLA',CORRE1)
        CALL DETRSD('CORRESP_2_MAILLA',CORRE2)

      END IF




C       3 -- PROJECTION DES CHAMPS DE RESU1 SUR MODEL2
C          SUIVANT LA CORRESPONDANCE CORRES
C       --------------------------------------------------------
      CALL PJEFPR(RESU1(1:8),RESU2(1:8),MODEL2,CORRES)

      CALL DETRSD('CORRESP_2_MAILLA',CORRES)
      CALL JEDETC('V',RESU2(1:8),1)


      CALL JEDEMA()
      END

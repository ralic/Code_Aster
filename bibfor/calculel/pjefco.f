      SUBROUTINE PJEFCO (MOA1,MOA2,CORRES,BASE)
      IMPLICIT   NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C     COMMANDE:  PROJ_CHAMP  METHODE:'ELEM'
C BUT : CALCULER LA STRUCTURE DE DONNEE CORRESP_2_MAILLA
C ----------------------------------------------------------------------
C
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*8 MOA1,MOA2
      CHARACTER*16 CORRES
      CHARACTER*1 BASE
C
C 0.2. ==> COMMUNS
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C 0.3. ==> VARIABLES LOCALES
C

      CHARACTER*8  NOMA1,NOMA2,NOMO1,NOMO2,NCAS
      CHARACTER*16 CORRE1,CORRE2,CORRE3
      CHARACTER*16 TYMOCL(5),MOTCLE(5)
      CHARACTER*24 GEOM2,GEOM1
      INTEGER      N1,NBOCC,IOCC,IE,IBID,NBNO2,NBMA1
      INTEGER      IAGNO2,IAGMA1,IEXI

      LOGICAL      LDMAX
      REAL*8       DISTMA,R8MAEM
C----------------------------------------------------------------------
      CALL JEMARQ()
      CALL ASSERT(BASE.EQ.'V')

      CORRE1 = '&&PJEFCO.CORRES1'
      CORRE2 = '&&PJEFCO.CORRES2'
      CORRE3 = '&&PJEFCO.CORRES3'

      CALL JEEXIN(MOA1//'.MODELE    .REPE',IEXI)
      IF (IEXI.GT.0) THEN
        NOMO1=MOA1
        CALL DISMOI('F','NOM_MAILLA',NOMO1,'MODELE',IBID,NOMA1,IE)
      ELSE
        NOMO1=' '
        NOMA1=MOA1
      ENDIF

      CALL JEEXIN(MOA2//'.MODELE    .REPE',IEXI)
      IF (IEXI.GT.0) THEN
        NOMO2=MOA2
        CALL DISMOI('F','NOM_MAILLA',NOMO2,'MODELE',IBID,NOMA2,IE)
      ELSE
        NOMO2=' '
        NOMA2=MOA2
      ENDIF


C     DETERMINATION DE DISTMA ET LDMAX:
C     --------------------------------------------------------
      LDMAX = .FALSE.
      DISTMA = R8MAEM()
      CALL GETVR8(' ','DISTANCE_MAX',1,0,1,DISTMA,N1)
      IF ( N1.EQ. 1 )  LDMAX = .TRUE.


      CALL GETFAC('VIS_A_VIS',NBOCC)
      IF (NBOCC.EQ.0) THEN
C        -- CAS : TOUT:'OUI'
C        ------------------------
         CALL PJEFCA (MOA1,' ',0,NCAS )

C        PRISE EN COMPTE DU MOT-CLE TRANSF_GEOM_[1|2]
C        --------------------------------------------
         CALL PJEFTG(1,GEOM1,NOMA1,' ',1)
         CALL PJEFTG(2,GEOM2,NOMA2,' ',1)

         IF (NCAS.EQ.'2D') THEN
            CALL PJ2DCO('TOUT',MOA1,MOA2,0,0,0,0,GEOM1,GEOM2,
     &                 CORRES,LDMAX,DISTMA)
         ELSE IF (NCAS.EQ.'3D') THEN
            CALL PJ3DCO('TOUT',MOA1,MOA2,0,0,0,0,GEOM1,GEOM2,
     &                 CORRES,LDMAX,DISTMA)
         ELSE IF (NCAS.EQ.'2.5D') THEN
            CALL PJ4DCO('TOUT',MOA1,MOA2,0,0,0,0,GEOM1,GEOM2,
     &                 CORRES,LDMAX,DISTMA,' ')
         ELSE IF (NCAS.EQ.'1.5D') THEN
            CALL PJ6DCO('TOUT',MOA1,MOA2,0,0,0,0,GEOM1,GEOM2,
     &                 CORRES,LDMAX,DISTMA)
         ELSE
            CALL ASSERT(.FALSE.)
         END IF

      ELSE

C        -- CAS : VIS_A_VIS
C        ------------------------
         DO 30 IOCC = 1,NBOCC

C           -- RECUPERATION DE LA LISTE DE MAILLES LMA1 :
C           ----------------------------------------------
            MOTCLE(1) = 'MAILLE_1'
            TYMOCL(1) = 'MAILLE'
            MOTCLE(2) = 'GROUP_MA_1'
            TYMOCL(2) = 'GROUP_MA'
            MOTCLE(3) = 'TOUT_1'
            TYMOCL(3) = 'TOUT'
            CALL RELIEM(NOMO1,NOMA1,'NU_MAILLE','VIS_A_VIS',IOCC,3,
     &                  MOTCLE,TYMOCL,'&&PJEFCO.LIMANU1',NBMA1)
            CALL JEVEUO('&&PJEFCO.LIMANU1','L',IAGMA1)

C           -- RECUPERATION DE LA LISTE DE NOEUDS LNO2 :
C           ----------------------------------------------
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
            CALL RELIEM(' ',NOMA2,'NU_NOEUD','VIS_A_VIS',IOCC,5,
     &                  MOTCLE,TYMOCL,'&&PJEFCO.LINONU2',NBNO2)
            CALL JEVEUO('&&PJEFCO.LINONU2','L',IAGNO2)

C           PRISE EN COMPTE DU MOT-CLE TRANSF_GEOM_[1|2]
C           --------------------------------------------
            CALL PJEFTG(1,GEOM1,NOMA1,'VIS_A_VIS',IOCC)
            CALL PJEFTG(2,GEOM2,NOMA2,'VIS_A_VIS',IOCC)

C           -- CALCUL DU CORRESP_2_MAILLA POUR IOCC :
C           ----------------------------------------------
            CALL PJEFCA (MOA1,'&&PJEFCO.LIMANU1',IOCC,NCAS)

            CALL DETRSD('CORRESP_2_MAILLA',CORRE1)
            IF (NCAS.EQ.'2D') THEN
               CALL PJ2DCO('PARTIE',MOA1,MOA2,NBMA1,ZI(IAGMA1),
     &               NBNO2,ZI(IAGNO2),GEOM1,GEOM2,CORRE1,LDMAX,DISTMA)
            ELSE IF (NCAS.EQ.'3D') THEN
               CALL PJ3DCO('PARTIE',MOA1,MOA2,NBMA1,ZI(IAGMA1),
     &               NBNO2,ZI(IAGNO2),GEOM1,GEOM2,CORRE1,LDMAX,DISTMA)
            ELSE IF (NCAS.EQ.'2.5D') THEN
               CALL PJ4DCO('PARTIE',MOA1,MOA2,NBMA1,ZI(IAGMA1),NBNO2,
     &               ZI(IAGNO2),GEOM1,GEOM2,CORRE1,LDMAX,DISTMA,' ')
            ELSE IF (NCAS.EQ.'1.5D') THEN
               CALL PJ6DCO('PARTIE',MOA1,MOA2,NBMA1,ZI(IAGMA1),
     &               NBNO2,ZI(IAGNO2),GEOM1,GEOM2,CORRE1,LDMAX,DISTMA)
            ELSE
               CALL ASSERT(.FALSE.)
            END IF


C           -- SURCHARGE DU CORRESP_2_MAILLA :
C           ----------------------------------------------
            IF (IOCC.EQ.1) THEN
               CALL COPISD('CORRESP_2_MAILLA','V',CORRE1,CORRE2)
            ELSE
               CALL PJFUCO(CORRE2,CORRE1,'V',CORRE3)
               CALL DETRSD('CORRESP_2_MAILLA',CORRE2)
               CALL COPISD('CORRESP_2_MAILLA','V',CORRE3,CORRE2)
               CALL DETRSD('CORRESP_2_MAILLA',CORRE3)
            END IF

            CALL JEDETR('&&PJEFCO.LIMANU1')
            CALL JEDETR('&&PJEFCO.LINONU2')
   30    CONTINUE
         CALL COPISD('CORRESP_2_MAILLA','V',CORRE2,CORRES)
         CALL DETRSD('CORRESP_2_MAILLA',CORRE1)
         CALL DETRSD('CORRESP_2_MAILLA',CORRE2)
      END IF


      CALL JEDEMA()
      END

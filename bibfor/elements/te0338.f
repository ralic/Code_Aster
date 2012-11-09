      SUBROUTINE TE0338(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*) OPTION,NOMTE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     -----------------------------------------------------------------
C     FONCTION REALISEE :

C         CALCUL DU CHAM_ELEM DE WEIBULL
C         COMPORTEMENT NON-LINEAIRE.
C         ELEMENTS ISOPARAMETRIQUES 3D.

C         OPTION : 'WEIBULL'

C ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
C         --->  NOMTE  : NOM DU TYPE D'ELEMENT


C-DEL CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8

      INTEGER ICODRE(4)
      INTEGER CODRES
      CHARACTER*4 FAMI
      CHARACTER*8 NOMRES(4)
      CHARACTER*16 OPTCAL(12),PHENOM

      REAL*8 SIGM(6),SIG1,SIGWK,VALRES(4),EPSG(6),EPS1
      REAL*8 M,VREF,SREF,SEUIL,DVPG,POIDS,VKP,DFDBID(30)
      REAL*8 EQUI(6),PP,PPT,VKPACT
      REAL*8 SIGOLD,SIGNEW,TG,TMOY

      INTEGER I,KP,NDIM,ICOMPO,NBVARI,IPOPP,IPOPPT
      INTEGER JGANO,IPOIDS,IVF,IDFDE,NPG,NNO,NNOS
      INTEGER IMATE,IGEOM,ICONG,IVARIG,ISSOPT,IWEIB,IDEFG,NBVP
      INTEGER ISIGIE,ISIGIS,JTAB(7),IRET

C======================== CORPS DU PROGRAMME ===========================


      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      NBVP = 3

C     1.2 CHAMPS IN
C     -------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTRG','L',ICONG)
      CALL JEVECH('PVARIPG','L',IVARIG)
      CALL JEVECH('PSOUSOP','L',ISSOPT)
      CALL JEVECH('PDOMMAG','L',ISIGIE)
      CALL TECACH('OON','PVARIPG',7,JTAB,IRET)
      NBVARI = MAX(JTAB(6),1)*JTAB(7)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C     READ (ZK16(ICOMPO+1),'(I16)') NBVARI

      CALL PSVARI (ZK16(ICOMPO),NBVARI,'3D',IPOPP,IPOPPT)

C     1.3 CHAMPS OUT
C     --------------
      CALL JEVECH('PWEIBUL','E',IWEIB)
      CALL JEVECH('PSIGISG','E',ISIGIS)

C     1.4 OPTIONS DE CALCUL
C     ---------------------
      OPTCAL(1) = ZK24(ISSOPT) (1:16)
      OPTCAL(2) = ZK24(ISSOPT) (17:19)

C     1.5 DONNES DE LA RC DU MATERIAU
C     -------------------------------
      NOMRES(1) = 'M'
      NOMRES(2) = 'VOLU_REFE'
      NOMRES(3) = 'SEUIL_EPSP_CUMU'
      NOMRES(4) = 'SIGM_REFE'

      CALL RCCOMA(ZI(IMATE),'WEIBULL',PHENOM,CODRES)

C     --- S'IL N Y A PAS DE CHAMP DE TEMPERATURE
C     ARRET

      IF (OPTCAL(1).EQ.'SIGM_ELMOY') THEN
        TMOY = 0.D0
      END IF

      CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',0.D0,
     &            3,NOMRES,VALRES,ICODRE,1)
      CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',0.D0,
     &            1,NOMRES(3),VALRES(3),ICODRE(3),1)
      IF (ICODRE(3).NE.0) VALRES(3) = 1.0D-6
      M = VALRES(1)
      VREF = VALRES(2)
      SEUIL = VALRES(3)

C     1.6 INITIALISATION
C     ------------------
      POIDS = 0.D0
      DVPG = 0.D0
      VKP = 0.D0
      VKPACT = 0.D0
      SIGWK = 0.D0
      DO 10,I = 1,6,1
        SIGM(I) = 0.D0
        EPSG(I) = 0.D0
   10 CONTINUE
C -CRITERE PLASTIQUE
      PPT = 0.D0
      PP = 0.D0


C     2. BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL
C     -------------------------------------------------------
C     2.1 SIGM_W A PARTIR DE SIGM MOYENNE SANS CORRECTION PLASTIQUE
C     -------------------------------------------------------------
      IF ((OPTCAL(1).EQ.'SIGM_ELMOY') .AND. (OPTCAL(2).EQ.'NON')) THEN
C        2.1.1 INTEGRATION DE SIGM SUR LA PARTIE PLASTIFIEE
C        --------------------------------------------------
        DO 40,KP = 1,NPG,1
C VOLUME PLASTIFIE
          PP = ZR(IVARIG+NBVARI* (KP-1)+IPOPP-1)
          IF (PP.GE.SEUIL) THEN
            CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                    ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG = POIDS
            VKP = VKP + DVPG
            DO 20,I = 1,6,1
              SIGM(I) = SIGM(I) + DVPG*ZR(ICONG+6*KP+I-7)
   20       CONTINUE
C           --- TEMPERATURE MOYENNE
            CALL RCVARC(' ','TEMP','+','RIGI',KP,1,TG,IRET)
            IF (IRET.EQ.1) CALL U2MESS('F','CALCULEL_31')
            TMOY = TG * DVPG
          END IF
C VOLUME PLASTIQUE ACTIF
          IF ((ZK16(ICOMPO).EQ.'LEMAITRE').AND.(PP.GE.SEUIL)) THEN
             PPT = 1.D0
          ELSE
             PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
          END IF
          IF (PPT.GE. (1.D0)) THEN
            DVPG = POIDS
            VKPACT = VKP + DVPG
          END IF

   40   CONTINUE

        SIG1 = 0.D0
        IF ((VKP.NE.0.0D0) .AND. (VKPACT.NE.0.D0)) THEN
C           2.1.2 CALCUL DE LA VALEUR MOYENNE DE SIGM SUR LA  MAILLE
C           --------------------------------------------------------
          DO 50,I = 1,6,1
            SIGM(I) = SIGM(I)/VKP
   50     CONTINUE

          TMOY = TMOY/VKP
          CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &                1,'TEMP',TMOY,1,NOMRES(4),
     &                VALRES(4),ICODRE(4),1)
          SREF = VALRES(4)

C           2.1.3 CALCUL DE SIGM_W
C           ----------------------
          CALL FGEQUI(SIGM,'SIGM',NBVP,EQUI)
          SIG1 = MAX(EQUI(3),EQUI(4),EQUI(5))
          SIG1 = SIG1/SREF
        END IF

        SIGOLD = ZR(ISIGIE)
        IF (SIG1.GT.SIGOLD) THEN
          ZR(ISIGIS) = SIG1
        ELSE
          ZR(ISIGIS) = ZR(ISIGIE)
        END IF
        SIG1 = ZR(ISIGIS)

        SIGWK = (VKP/VREF)* (SIG1**M)

C     2.2 SIGM_W A PARTIR DE SIGM MOYENNE AVEC CORRECTION PLASTIQUE
C     -------------------------------------------------------------
      ELSE IF ((OPTCAL(1).EQ.'SIGM_ELMOY') .AND.
     &         (OPTCAL(2).EQ.'OUI')) THEN
C        2.2.1 INTEGRATION DE SIGM SUR LA PARTIE PLASTIFIEE
C        --------------------------------------------------
        CALL JEVECH('PDEFORR','L',IDEFG)
        DO 80,KP = 1,NPG,1
C VOLUME PLASTIFIE
          PP = ZR(IVARIG+NBVARI* (KP-1)+IPOPP-1)
          IF (PP.GE.SEUIL) THEN
            CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                    ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG = POIDS
            VKP = VKP + DVPG
            DO 60,I = 1,6,1
              SIGM(I) = SIGM(I) + DVPG*ZR(ICONG+6*KP+I-7)
              EPSG(I) = EPSG(I) + DVPG*ZR(IDEFG+6*KP+I-7)
   60       CONTINUE
C           --- TEMPERATURE AU PG
            CALL RCVARC(' ','TEMP','+','RIGI',KP,1,TG,IRET)
            IF (IRET.EQ.1) CALL U2MESS('F','CALCULEL_31')
            TMOY = TG * DVPG
          ENDIF
C VOLUME PLASTIQUE ACTIF
          IF ((ZK16(ICOMPO).EQ.'LEMAITRE').AND.(PP.GE.SEUIL)) THEN
             PPT = 1.D0
          ELSE
             PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
          END IF
          IF (PPT.GE. (1.D0)) THEN
            DVPG = POIDS
            VKPACT = VKPACT + DVPG
          END IF
   80   CONTINUE

        SIGNEW = 0.D0
        IF ((VKP.NE.0.0D0) .AND. (VKPACT.NE.0.D0)) THEN
C           2.2.2 CALCUL DE LA VALEUR MOYENNE DE SIGM SUR LA  MAILLE
C           --------------------------------------------------------
          DO 90,I = 1,6,1
            SIGM(I) = SIGM(I)/VKP
            EPSG(I) = EPSG(I)/VKP
   90     CONTINUE

          TMOY = TMOY/VKP
          CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &                1,'TEMP',TMOY,1,NOMRES(4),
     &                VALRES(4),ICODRE(4),1)
          SREF = VALRES(4)

C           2.2.3 CALCUL DE SIGM_W
C           ----------------------
          CALL EPDCP(SIGM,EPSG,SIG1,EPS1)
          SIGNEW = (SIG1/SREF)*EXP(-EPS1*0.5D0)
        END IF

        SIGOLD = ZR(ISIGIE)
        IF (SIGNEW.GT.SIGOLD) THEN
          ZR(ISIGIS) = SIGNEW
        ELSE
          ZR(ISIGIS) = ZR(ISIGIE)
        END IF
        SIGNEW = ZR(ISIGIS)

        SIGWK = (VKP/VREF)* (SIGNEW**M)

C     2.3 SIGM_W A PARTIR DE SIGM ORIGINAL AVEC CORRECTION PLASTIQUE
C     -------------------------------------------------------------
      ELSE IF ((OPTCAL(1).EQ.'SIGM_ELGA') .AND.
     &         (OPTCAL(2).EQ.'OUI')) THEN
        CALL JEVECH('PDEFORR','L',IDEFG)
        DO 120,KP = 1,NPG,1
          PP = ZR(IVARIG+NBVARI* (KP-1)+IPOPP-1)
          SIGNEW = 0.D0
          IF (PP.GE.SEUIL) THEN
            CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                    ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG = POIDS
            IF ((ZK16(ICOMPO).EQ.'LEMAITRE').AND.(PP.GE.SEUIL)) THEN
             PPT = 1.D0
            ELSE
             PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            END IF
            IF (PPT.GE. (1.D0)) THEN
              DO 100,I = 1,6,1
                SIGM(I) = ZR(ICONG+6*KP+I-7)
                EPSG(I) = ZR(IDEFG+6*KP+I-7)
  100         CONTINUE
              CALL EPDCP(SIGM,EPSG,SIG1,EPS1)
C           --- TEMPERATURE AU PG
              CALL RCVALB(FAMI,KP,1,'+',ZI(IMATE),' ',PHENOM,0,' ',0.D0,
     &                  1,NOMRES(4),VALRES(4),ICODRE(4),1)
              SREF = VALRES(4)
              SIGNEW = (SIG1/SREF)*EXP(-EPS1*0.5D0)
            END IF
          END IF
          SIGOLD = ZR(ISIGIE+KP-1)
          IF (SIGNEW.GT.SIGOLD) THEN
            ZR(ISIGIS+KP-1) = SIGNEW
          ELSE
            ZR(ISIGIS+KP-1) = ZR(ISIGIE+KP-1)
          END IF
          SIGNEW = ZR(ISIGIS+KP-1)
          SIGWK = SIGWK + (DVPG/VREF)* (SIGNEW**M)
  120   CONTINUE

C     2.4 SIGM_W A PARTIR DE SIGM ORIGINAL SANS CORRECTION
C     ----------------------------------------------------
      ELSE IF ((OPTCAL(1).EQ.'SIGM_ELGA') .AND.
     &         (OPTCAL(2).EQ.'NON')) THEN
        DO 150,KP = 1,NPG,1
          PP = ZR(IVARIG+NBVARI* (KP-1)+IPOPP-1)
          IF (PP.GE.SEUIL) THEN
            CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                    ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
            DVPG = POIDS
            IF ((ZK16(ICOMPO).EQ.'LEMAITRE').AND.(PP.GE.SEUIL)) THEN
             PPT = 1.D0
            ELSE
             PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            END IF
            SIG1 = 0.D0
            IF (PPT.GE. (1.D0)) THEN
              DO 130,I = 1,6,1
                SIGM(I) = ZR(ICONG+6*KP+I-7)
  130         CONTINUE
              CALL FGEQUI(SIGM,'SIGM',NBVP,EQUI)
              SIG1 = MAX(EQUI(3),EQUI(4),EQUI(5))
              CALL RCVALB(FAMI,KP,1,'+',ZI(IMATE),' ',PHENOM,0,' ',
     &                    0.D0,1,NOMRES(4),VALRES(4),ICODRE(4),1)
              SREF = VALRES(4)
              SIG1 = SIG1/SREF
            END IF
          ELSE
            SIG1=0.D0
          END IF
          SIGOLD = ZR(ISIGIE+KP-1)
          IF (SIG1.GT.SIGOLD) THEN
            ZR(ISIGIS+KP-1) = SIG1
          ELSE
            ZR(ISIGIS+KP-1) = ZR(ISIGIE+KP-1)
          END IF
          SIG1 = ZR(ISIGIS+KP-1)

          SIGWK = SIGWK + (DVPG/VREF)* (SIG1**M)
  150   CONTINUE

C     2.5 TRAITEMENT DES OPTIONS INVALIDES
C     ------------------------------------
      ELSE
C       OPTION DE CALCUL NON VALIDE
        CALL ASSERT(.FALSE.)
      END IF


C     3. ECRITURE DU CHAM_ELEM DE WEIBULL
C     -----------------------------------
      ZR(IWEIB) = SIGWK

      END

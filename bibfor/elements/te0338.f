      SUBROUTINE TE0338 (OPTION,NOMTE)
      IMPLICIT NONE        
      CHARACTER*(*)     OPTION,NOMTE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/11/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     -----------------------------------------------------------------
C     FONCTION REALISEE :
C
C         CALCUL DU CHAM_ELEM DE WEIBULL
C         COMPORTEMENT NON-LINEAIRE.
C         ELEMENTS ISOPARAMETRIQUES 3D.
C
C         OPTION : 'WEIBULL'
C
C ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
C         --->  NOMTE  : NOM DU TYPE D'ELEMENT
C
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C-DEL CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*2  CODRET(4), CODRES
      CHARACTER*8  NOMRES(4),ELREFE
      CHARACTER*24 CARAC,FF
      CHARACTER*16 OPTCAL(12), PHENOM

      REAL*8       SIGM(6),SIG1,SIGWK,VALRES(4),EPSG(6),EPS1
      REAL*8       M,VREF,SREF,SEUIL,DVPG,POIDS,VKP,DFDBID(30)
      REAL*8       EQUI(6),PP,PPT,VKPACT
      REAL*8       SIGOLD,SIGNEW, TPG, TMOY

      INTEGER      I,K,KP,KT,NDIM,ICOMPO,NBVARI,IPOPP,IPOPPT
      INTEGER      ICARAC,IPOIDS,IFF,IVF,IDFDN,IDFDE,IDFDK,NPG,NNO
      INTEGER      IMATE,IGEOM,ICONG,IVARIG,ISSOPT,IWEIB,IDEFG,NBVP
      INTEGER      ISIGIE,ISIGIS, ITEMPE,INO,JTAB(7)
C
C======================== CORPS DU PROGRAMME ===========================
C
      CALL ELREF1(ELREFE)
      CARAC='&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CARAC,'L',ICARAC)
      NDIM = ZI(ICARAC)
      NNO  = ZI(ICARAC+1)
      NPG  = ZI(ICARAC+3)
      NBVP = 3
C
      FF ='&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
C     1.2 CHAMPS IN
C     -------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTRG','L',ICONG)
      CALL JEVECH('PVARIPG','L',IVARIG)
      CALL JEVECH('PSOUSOP','L',ISSOPT)
      CALL JEVECH('PDOMMAG','L',ISIGIE)
      CALL TECACH(.TRUE.,.TRUE.,'PVARIPG',7,JTAB)     
      NBVARI = MAX(JTAB(6),1)*JTAB(7)      
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
C
      IF (     (ZK16(ICOMPO).EQ.'VMIS_ISOT_TRAC')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ISOT_LINE')
     &     .OR.(ZK16(ICOMPO).EQ.'LEMAITRE')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ECMI_TRAC')
     &     .OR.(ZK16(ICOMPO).EQ.'VMIS_ECMI_LINE')
     &     .OR.(ZK16(ICOMPO).EQ.'VISC_CIN1_CHAB')
     &     .OR.(ZK16(ICOMPO).EQ.'VISC_CIN2_CHAB') ) THEN
        IPOPP  = 1
        IPOPPT = 2        
      ELSE IF  (ZK16(ICOMPO).EQ.'ROUSS_PR')  THEN
        IPOPP  = 1
        IPOPPT = NBVARI
      ELSE IF  (ZK16(ICOMPO).EQ.'ROUSSELIER')  THEN
        IPOPP  = 1
        IPOPPT = 9
      ELSE IF  (ZK16(ICOMPO).EQ.'ROUSS_VISC')  THEN
        IPOPP  = 1
        IPOPPT = NBVARI
      ELSE IF  (ZK16(ICOMPO).EQ.'CHABOCHE') THEN
        IPOPP  = 13
        IPOPPT = 14
      ELSE 
        CALL UTMESS('F','TE0338','POUR L''OPTION '//
     +          '"WEIBULL", LA RELATION "'//ZK16(ICOMPO)//
     +          '" N''EST PAS ADMISE') 
      END IF
C
C     1.3 CHAMPS OUT
C     --------------
      CALL JEVECH('PWEIBUL','E',IWEIB)
      CALL JEVECH('PSIGISG','E',ISIGIS)
C
C     1.4 OPTIONS DE CALCUL
C     ---------------------
      OPTCAL(1) = ZK24(ISSOPT)(1:16)
      OPTCAL(2) = ZK24(ISSOPT)(17:19)
C
C     1.5 DONNES DE LA RC DU MATERIAU
C     -------------------------------
      NOMRES(1) = 'M'
      NOMRES(2) = 'VOLU_REFE'
      NOMRES(3) = 'SEUIL_EPSP_CUMU'
      NOMRES(4) = 'SIGM_REFE'

      CALL RCCOMA(ZI(IMATE),'WEIBULL',PHENOM,CODRES)
C
C     --- S'IL N Y A PAS DE CHAMP DE TEMPERATURE
C     ARRET
C
      CALL TECACH(.FALSE.,.TRUE.,'PTEMPER',1,ITEMPE)
      IF (ITEMPE.EQ.0) THEN
            CALL UTMESS('F','PEWEIB','ERREUR: WEIBULL '//
     +                      'PAS DE CHAMP THERMIQUE')
      ENDIF
      IF (OPTCAL(1).EQ.'SIGM_ELMOY') THEN
            TMOY = 0.D0
      ENDIF

      CALL RCVALA(ZI(IMATE),PHENOM,0,' ',DFDBID(1),3,
     &               NOMRES, VALRES, CODRET, 'FM' )
      CALL RCVALA(ZI(IMATE),PHENOM,0,' ',DFDBID(1),1,
     &               NOMRES(3), VALRES(3), CODRET(3), 'FM' )
      IF (CODRET(3).NE.'OK') VALRES(3) = 1.0D-6
      M    = VALRES(1)
      VREF = VALRES(2)
      SEUIL = VALRES(3)
C
C     1.6 INITIALISATION
C     ------------------
      POIDS = 0.D0
      DVPG  = 0.D0
      VKP   = 0.D0
      VKPACT = 0.D0
      SIGWK = 0.D0
      DO 10, I = 1, 6, 1
         SIGM(I) = 0.D0
         EPSG(I) = 0.D0
10    CONTINUE
C -CRITERE PLASTIQUE
      PPT = 0.D0
      PP = 0.D0
C
C
C     2. BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL
C     -------------------------------------------------------
C     2.1 SIGM_W A PARTIR DE SIGM MOYENNE SANS CORRECTION PLASTIQUE
C     -------------------------------------------------------------
      IF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'NON')) THEN
C        2.1.1 INTEGRATION DE SIGM SUR LA PARTIE PLASTIFIEE
C        --------------------------------------------------
         DO 100, KP = 1, NPG, 1
C VOLUME PLASTIFIE
            PP = ZR(IVARIG+NBVARI*(KP-1)+IPOPP-1)
            IF (PP.GE.SEUIL) THEN
               KT = (KP - 1)*NNO
               K = (KP - 1)*NNO*3
               CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
               DVPG = POIDS
               VKP  = VKP + DVPG
               DO 110, I = 1, 6, 1
                  SIGM(I) = SIGM(I)+DVPG*ZR(ICONG+6*KP+I-7)
110            CONTINUE
C           --- TEMPERATURE MOYENNE
               DO 112 INO = 1,NNO
                   TMOY = TMOY + ZR(ITEMPE+INO-1)
     &                     *ZR(IVF+KT+INO-1)*DVPG
112            CONTINUE
            ENDIF
C VOLUME PLASTIQUE ACTIF
            PPT = ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            IF (PPT. EQ. (1.D0)) THEN
               DVPG = POIDS
               VKPACT  = VKP + DVPG
            ENDIF

100      CONTINUE
C
         SIG1 = 0.D0
         IF ((VKP .NE. 0.0D0).AND.(VKPACT.NE.0.D0)) THEN
C           2.1.2 CALCUL DE LA VALEUR MOYENNE DE SIGM SUR LA  MAILLE
C           --------------------------------------------------------
            DO 120, I = 1, 6, 1
               SIGM(I) = SIGM(I)/VKP
120         CONTINUE
C
            TMOY = TMOY/VKP
            CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TMOY,1,
     &              NOMRES(4),VALRES(4), CODRET(4), 'FM' )
            SREF = VALRES(4)
C
C           2.1.3 CALCUL DE SIGM_W
C           ----------------------
            CALL FGEQUI ( SIGM, 'SIGM', NBVP, EQUI )
            SIG1 = MAX(ABS(EQUI(3)),ABS(EQUI(4)),ABS(EQUI(5)))
            SIG1 = SIG1/SREF
         ENDIF
C
         SIGOLD=ZR(ISIGIE)
         IF (SIG1.GT.SIGOLD) THEN
            ZR(ISIGIS)=SIG1
         ELSE
            ZR(ISIGIS)=ZR(ISIGIE)
         ENDIF
         SIG1=ZR(ISIGIS)
C
        SIGWK = (VKP/VREF)*(SIG1**M)
C
C     2.2 SIGM_W A PARTIR DE SIGM MOYENNE AVEC CORRECTION PLASTIQUE
C     -------------------------------------------------------------
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'OUI')) THEN
C        2.2.1 INTEGRATION DE SIGM SUR LA PARTIE PLASTIFIEE
C        --------------------------------------------------
         CALL JEVECH('PDEFORR','L',IDEFG)
         DO 200, KP = 1, NPG, 1
C VOLUME PLASTIFIE
            PP = ZR(IVARIG+NBVARI*(KP-1)+IPOPP-1)
            IF (PP.GE.SEUIL) THEN
               KT = (KP - 1)*NNO
               K = (KP - 1)*NNO*3
               CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
               DVPG = POIDS
               VKP  = VKP + DVPG
               DO 210, I = 1, 6, 1
                  SIGM(I)=SIGM(I)+DVPG*ZR(ICONG  +6*KP+I-7)
                  EPSG(I)=EPSG(I)+DVPG*ZR(IDEFG +6*KP+I-7)
210            CONTINUE
C           --- TEMPERATURE AU PG
               DO 212 INO = 1,NNO
                  TMOY = TMOY + ZR(ITEMPE+INO-1)
     &                          *ZR(IVF+KT+INO-1)*DVPG
212            CONTINUE
            ENDIF
C VOLUME PLASTIQUE ACTIF
            PPT = ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            IF (PPT.EQ.(1.D0)) THEN
               DVPG = POIDS
               VKPACT  = VKPACT + DVPG
            ENDIF
200      CONTINUE
C
         SIGNEW = 0.D0
         IF ((VKP .NE. 0.0D0).AND.(VKPACT.NE.0.D0)) THEN
C           2.2.2 CALCUL DE LA VALEUR MOYENNE DE SIGM SUR LA  MAILLE
C           --------------------------------------------------------
            DO 220, I = 1, 6, 1
               SIGM(I) = SIGM(I)/VKP
               EPSG(I) = EPSG(I)/VKP
220         CONTINUE

            TMOY = TMOY/VKP
            CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TMOY,1,
     &                  NOMRES(4),VALRES(4), CODRET(4), 'FM' )
            SREF = VALRES(4)

C           2.2.3 CALCUL DE SIGM_W
C           ----------------------
            CALL EPDCP(SIGM,EPSG,SIG1,EPS1)
            SIGNEW=(SIG1/SREF)*EXP(-EPS1*0.5D0)
         ENDIF
C
         SIGOLD=ZR(ISIGIE)
         IF (SIGNEW.GT.SIGOLD) THEN
             ZR(ISIGIS)=SIGNEW
         ELSE
             ZR(ISIGIS)=ZR(ISIGIE)
         ENDIF
         SIGNEW= ZR(ISIGIS)
C
         SIGWK = (VKP/VREF)*(SIGNEW**M)
C
C     2.3 SIGM_W A PARTIR DE SIGM ORIGINAL AVEC CORRECTION PLASTIQUE
C     -------------------------------------------------------------
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'OUI')) THEN
         CALL JEVECH('PDEFORR','L',IDEFG)
         DO 300, KP = 1, NPG, 1
            PP = ZR(IVARIG+NBVARI*(KP-1)+IPOPP-1)
            SIGNEW = 0.D0
            IF (PP .GE. SEUIL) THEN
               K = (KP - 1)*NNO*3
               KT = (KP - 1)*NNO
               CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
               DVPG = POIDS
               PPT = ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
               IF (PPT . EQ. (1.D0)) THEN
                 DO 310, I = 1, 6, 1
                    SIGM(I)=ZR(ICONG  +6*KP+I-7)
                    EPSG(I)=ZR(IDEFG+ 6*KP+I-7)
310              CONTINUE
                 CALL EPDCP(SIGM,EPSG,SIG1,EPS1)
C           --- TEMPERATURE AU PG
                 TPG = 0.D0
                 DO 312 INO = 1,NNO
                    TPG = TPG + ZR(ITEMPE+INO-1)*ZR(IVF+KT+INO-1)
312              CONTINUE
                 CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,
     &              NOMRES(4),VALRES(4), CODRET(4), 'FM' )
                 SREF = VALRES(4)
                 SIGNEW=(SIG1/SREF)*EXP(-EPS1*0.5D0)
               ENDIF
            ENDIF
            SIGOLD=ZR(ISIGIE+KP-1)
            IF (SIGNEW.GT.SIGOLD) THEN
               ZR(ISIGIS+KP-1)=SIGNEW
            ELSE 
               ZR(ISIGIS+KP-1)=ZR(ISIGIE+KP-1)
            ENDIF
            SIGNEW=ZR(ISIGIS+KP-1)
            SIGWK = SIGWK + (DVPG/VREF)*(SIGNEW**M)
300      CONTINUE
C
C     2.4 SIGM_W A PARTIR DE SIGM ORIGINAL SANS CORRECTION
C     ----------------------------------------------------
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'NON')) THEN
         DO 400, KP = 1, NPG, 1
            PP = ZR(IVARIG+NBVARI*(KP-1)+IPOPP-1)
            IF (PP .GE. SEUIL) THEN
               K = (KP - 1)*NNO*3
               KT = (KP - 1)*NNO
               CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDBID,DFDBID,DFDBID,POIDS)
               DVPG = POIDS
               PPT = ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
               SIG1 = 0.D0
               IF (PPT . EQ. (1.D0)) THEN
                 DO 410, I = 1, 6, 1
                    SIGM(I)=ZR(ICONG  +6*KP+I-7)
410              CONTINUE
                 CALL FGEQUI ( SIGM, 'SIGM', NBVP, EQUI )
                 SIG1 = MAX(ABS(EQUI(3)),ABS(EQUI(4)),ABS(EQUI(5)))
C           --- TEMPERATURE AU PG
                 TPG = 0.D0
                 DO 412 INO = 1,NNO
                      TPG = TPG + ZR(ITEMPE+INO-1)*ZR(IVF+KT+INO-1)
412              CONTINUE
                 CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,
     &              NOMRES(4),VALRES(4), CODRET(4), 'FM' )
                 SREF = VALRES(4)
                 SIG1 = SIG1/SREF
               ENDIF
            ENDIF
            SIGOLD=ZR(ISIGIE+KP-1)
            IF (SIG1.GT.SIGOLD) THEN
               ZR(ISIGIS+KP-1)=SIG1
            ELSE
               ZR(ISIGIS+KP-1)=ZR(ISIGIE+KP-1)
            ENDIF
            SIG1=ZR(ISIGIS+KP-1)
C
            SIGWK = SIGWK + (DVPG/VREF)*(SIG1**M)
400      CONTINUE
C
C     2.5 TRAITEMENT DES OPTIONS INVALIDES
C     ------------------------------------
      ELSE
         CALL UTMESS('F','TE0338','OPTION DE CALCUL NON VALIDE')
      ENDIF
C
C
C     3. ECRITURE DU CHAM_ELEM DE WEIBULL
C     -----------------------------------
      ZR(IWEIB) = SIGWK
C
      END

      SUBROUTINE TE0331 (OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION II
C MODIF ELEMENTS  DATE 17/09/2002   AUTEUR T2BAXJM R.MASSON 
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
C     ------------------------------------------------------------------
C     FONCTION REALISEE :
C
C         CALCUL DE LA CONTRAINTE DE WEIBULL D'UNE STRUCTURE
C         EN COMPORTEMENT NON-LINEAIRE.
C         ELEMENTS ISOPARAMETRIQUES 2D.
C
C         OPTION : 'WEIBULL'
C
C ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
C         --->  NOMTE  : NOM DU TYPE D'ELEMENT
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*2        CODRET(4), CODRES
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(4),ELREFE
      CHARACTER*16       OPTCAL(12), PHENOM
      REAL*8             SIG(6),SIGI,DSIGWB,VALRES(4),EPSGI,R8BID
      REAL*8             POIDS,R,VOLUME,VOLACT,DVOL,SEUIL,M,V0
      REAL*8             CONG(4),EPSQ(4),DFDX(9),DFDY(9),PP,PPT
      REAL*8             TC(6),TDP(6),SIGOLD,SIGNEW,SREF,TPG,TMOY

      INTEGER            NNO,KP,NPG,K,II,IWEIB,JTAB(7)
      INTEGER            IDEFG,ISSOPT,INO,IPOPP,IPOPPT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IMATE
      INTEGER            IGEOM,ICONG,IVARIG
      INTEGER            ISIGIE,ISIGIS,ITEMPE,ICOMPO,NBVARI
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF   ,'L',IFF   )
      IPOIDS=IFF
      IVF   = IPOIDS+NPG
      IDFDE = IVF   +NPG*NNO
      IDFDK = IDFDE +NPG*NNO
C
      POIDS=0.D0
      DSIGWB=0.D0
      VOLUME=0.D0
      VOLACT=0.D0
      DVOL=0.D0
C
      NOMRES(1) = 'M'
      NOMRES(2) = 'VOLU_REFE'
      NOMRES(3) = 'SEUIL_EPSP_CUMU'
      NOMRES(4) = 'SIGM_REFE'

C
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTRG','L',ICONG)
      CALL JEVECH('PVARIPG','L',IVARIG)
      CALL JEVECH('PSOUSOP','L',ISSOPT)
      CALL JEVECH('PDOMMAG','L',ISIGIE)
      CALL JEVECH('PWEIBUL','E',IWEIB)
      CALL JEVECH('PSIGISG','E',ISIGIS)
C
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
        IPOPP  = 9
        IPOPPT = 10
      ELSE
        CALL UTMESS('F','TE0331','POUR L''OPTION '//
     +          '"WEIBULL", LA RELATION "'//ZK16(ICOMPO)//
     +          '" N''EST PAS ADMISE')
      END IF
C
      OPTCAL(1) = ZK24(ISSOPT)(1:16)
      OPTCAL(2) = ZK24(ISSOPT)(17:19)
C
C
      DO 150 II=1,4
         CONG(II)=0.D0
         EPSQ(II)=0.D0
 150  CONTINUE
C -FONCTION SEUIL
      PPT = 0.D0
      PP = 0.D0

C
C     --- CAS SIGU DEPEND DE LA TEMPERATURE (WEIBULL_FO)?
C     SI OUI ET QU IL N Y A PAS DE CHAMP DE TEMPERATURE
C     ARRET
C
      CALL RCCOMA(ZI(IMATE),'WEIBULL',PHENOM,CODRES)
      CALL TECACH(.FALSE.,.TRUE.,'PTEMPER',1,ITEMPE)
      IF (ITEMPE.EQ.0) THEN
         CALL UTMESS('F','PEWEIB','ERREUR: WEIBULL '//
     +                      'PAS DE CHAMP THERMIQUE')
      ENDIF

      IF (OPTCAL(1).EQ.'SIGM_ELMOY') THEN
            TMOY = 0.D0
      ENDIF

C
C     --- RECUPERATION DES DONNEES MATERIAU ---
C
      CALL RCVALA ( ZI(IMATE),PHENOM,0,' ',R8BID,3,NOMRES,
     &              VALRES, CODRET, 'FM' )
      CALL RCVALA ( ZI(IMATE),PHENOM,0,' ',R8BID,1,NOMRES(3),
     &              VALRES(3), CODRET(3),'FM')
      IF (CODRET(3).NE.'OK') VALRES(3) = 1.D-6
      M     = VALRES(1)
      V0    = VALRES(2)
      SEUIL = VALRES(3)
C
C     --- BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL ---
C
C=================================================================
C=================================================================
      IF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'NON')) THEN
         DO 200 KP=1,NPG
            K=(KP-1)*NNO
            R=0.D0
            CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                   ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 160 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 160           CONTINUE
               POIDS=POIDS*R
            ENDIF
C VOLUME PLASTIFIE
            PP =ZR(IVARIG+NBVARI*(KP-1)+IPOPP-1)
            IF (PP.GE.SEUIL) THEN
               DVOL=POIDS
               VOLUME=VOLUME+DVOL
               DO 165 II=1,4
                  CONG(II)=CONG(II)+DVOL*ZR(ICONG+4*KP+II-5)
 165           CONTINUE
C           --- TEMPERATURE MOYENNE
               DO 168 INO = 1,NNO
                    TMOY = TMOY + ZR(ITEMPE+INO-1)
     &                     *ZR(IVF+K+INO-1)*DVOL
 168           CONTINUE
            ENDIF
C VOLUME PLASTIQUE ACTIF
            PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            IF (PPT.EQ.(1.0D0)) THEN
               DVOL=POIDS
               VOLACT=VOLACT+DVOL
            ENDIF
 200     CONTINUE
C
         SIGI = 0.D0
         IF ((VOLACT.NE.0.D0).AND.(VOLUME.NE.0.D0)) THEN
            SIG(1)  =CONG(1)/VOLUME
            SIG(2)  =CONG(2)/VOLUME
            SIG(3)  =CONG(3)/VOLUME
            SIG(4)  =CONG(4)/VOLUME
            CALL VPRI2D(SIG,SIGI)
C
            TMOY = TMOY/VOLUME
            CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TMOY,1,
     &              NOMRES(4),VALRES(4), CODRET(4), 'FM' )
            SREF = VALRES(4)
            SIGI = SIGI/SREF
         ENDIF
         SIGOLD=ZR(ISIGIE)
         IF (SIGI.GT.SIGOLD) THEN
            ZR(ISIGIS)=SIGI
         ELSE
            ZR(ISIGIS)=ZR(ISIGIE)
         ENDIF
         SIGI=ZR(ISIGIS)
C
         DSIGWB=VOLUME/V0*(SIGI**M)
C=================================================================
C=================================================================
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'OUI'))
     &       THEN
         DO 300 KP=1,NPG
            R=0.D0
            K=(KP-1)*NNO
            CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                   ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 170 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 170           CONTINUE
               POIDS=POIDS*R
            ENDIF
            VOLUME=POIDS
            CALL JEVECH('PDEFORR','L',IDEFG)
            DO 180 II=1,4
               CONG(II)=ZR(ICONG+4*KP+II-5)
               EPSQ(II)=ZR(IDEFG+4*KP+II-5)
 180        CONTINUE
            PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
C
            SIGNEW = 0.D0
            IF (PPT.EQ.(1.D0)) THEN
C
C              ------CALCUL DE SIGI ET EPSI---------
C
              TC(1) = CONG(1)
              TC(2) = CONG(2)
              TC(3) = CONG(3)
              TC(4) = CONG(4)
              TC(5) = 0.D0
              TC(6) = 0.D0
C
              TDP(1) = EPSQ(1)
              TDP(2) = EPSQ(2)
              TDP(3) = EPSQ(3)
              TDP(4) = EPSQ(4)
              TDP(5) = 0.D0
              TDP(6) = 0.D0
              CALL EPDCP(TC,TDP,SIGI,EPSGI)
C           --- TEMPERATURE AU PG SI 'WEIBULL_FO'
              TPG = 0.D0
              DO 190 INO = 1,NNO
                    TPG = TPG + ZR(ITEMPE+INO-1)*ZR(IVF+K+INO-1)
 190          CONTINUE
              CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,
     &              NOMRES(4),VALRES(4), CODRET(4), 'FM' )
              SREF = VALRES(4)

              SIGNEW=EXP((-EPSGI/2.D0))*SIGI/SREF
            ENDIF
            SIGOLD=ZR(ISIGIE+KP-1)
            IF (SIGNEW.GT.SIGOLD) THEN
                 ZR(ISIGIS+KP-1)=SIGNEW
            ELSE
                 ZR(ISIGIS+KP-1)=ZR(ISIGIE+KP-1)
            ENDIF
            SIGNEW=ZR(ISIGIS+KP-1)
            DSIGWB=DSIGWB+VOLUME*(SIGNEW**M)/V0
C
 300     CONTINUE
C=================================================================
C=================================================================
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELMOY').AND.(OPTCAL(2).EQ.'OUI'))
     &       THEN

         DO 400 KP=1,NPG
            R=0.D0
            K=(KP-1)*NNO
            CALL DFDM2D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                   ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 210 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 210           CONTINUE
               POIDS=POIDS*R
            ENDIF
C VOL PLASTIFIE
            PP =ZR(IVARIG+NBVARI*(KP-1)+IPOPP-1)
            CALL JEVECH('PDEFORR','L',IDEFG)
            IF (PP.GE.SEUIL) THEN
               DVOL=POIDS
               VOLUME=VOLUME+DVOL
               DO 220 II=1,4
                  CONG(II)=CONG(II)+DVOL*ZR(ICONG+4*KP+II-5)
                  EPSQ(II)=EPSQ(II)+DVOL*ZR(IDEFG+4*KP+II-5)
220            CONTINUE
C           --- TEMPERATURE MOYENNE
               DO 225 INO = 1,NNO
                    TMOY = TMOY + ZR(ITEMPE+INO-1)*ZR(IVF+K+INO-1)
     &                            *DVOL
225            CONTINUE
            ENDIF
C VOL PLASTIQUE ACTIF
            PPT =ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            IF (PPT.EQ.(1.0D0)) THEN
               DVOL=POIDS
               VOLACT=VOLACT+DVOL
            ENDIF

400      CONTINUE
C
         SIGNEW = 0.D0
         IF ((VOLACT.NE.(0.D0)).AND.(VOLUME.NE.0.D0)) THEN
                TC(1) = CONG(1)/VOLUME
                TC(2) = CONG(2)/VOLUME
                TC(3) = CONG(3)/VOLUME
                TC(4) = CONG(4)/VOLUME
                TC(5) = 0.D0
                TC(6) = 0.D0
C
                TDP(1) = EPSQ(1)/VOLUME
                TDP(2) = EPSQ(2)/VOLUME
                TDP(3) = EPSQ(3)/VOLUME
                TDP(4) = EPSQ(4)/VOLUME
                TDP(5) = 0.D0
                TDP(6) = 0.D0
                CALL EPDCP(TC,TDP,SIGI,EPSGI)
                TMOY = TMOY/VOLUME
                CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TMOY,1,
     &                  NOMRES(4),VALRES(4), CODRET(4), 'FM' )
                SREF = VALRES(4)
                SIGNEW=EXP((-EPSGI/2.D0))*SIGI/SREF
          ENDIF
          SIGOLD=ZR(ISIGIE)
          IF (SIGNEW.GT.SIGOLD) THEN
             ZR(ISIGIS)=SIGNEW
          ELSE
             ZR(ISIGIS)=ZR(ISIGIE)
          ENDIF
          SIGNEW= ZR(ISIGIS)
          DSIGWB=VOLUME*(SIGNEW**M)/V0
C=================================================================
C=================================================================
      ELSEIF ((OPTCAL(1).EQ.'SIGM_ELGA').AND.(OPTCAL(2).EQ.'NON'))
     &       THEN
         DO 100 KP=1,NPG
            K=(KP-1)*NNO
            R=0.D0
            DO 175 II=1,4
               CONG(II)=ZR(ICONG+(4*KP)-5+II)
 175        CONTINUE
            CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                    ZR(IGEOM),DFDX,DFDY,POIDS )
            IF (NOMTE(3:4).EQ.'AX') THEN
               DO 240 II=1,NNO
                  R=R+ZR(IGEOM+2*II-2)*ZR(IVF+K+II-1)
 240           CONTINUE
               POIDS=POIDS*R
            ENDIF
            VOLUME=POIDS
C
            SIGI = 0.D0
            PPT=ZR(IVARIG+NBVARI*(KP-1)+IPOPPT-1)
            IF (PPT.EQ.(1.D0)) THEN
C CALCUL DE SIGI
               CALL VPRI2D(CONG,SIGI)
C TEMPERATURE AU PG
               TPG = 0.D0
               DO 250 INO = 1,NNO
                    TPG = TPG + ZR(ITEMPE+INO-1)*ZR(IVF+K+INO-1)
250            CONTINUE
               CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,
     &              NOMRES(4),VALRES(4), CODRET(4), 'FM' )
               SREF = VALRES(4)
               SIGI = SIGI/SREF
           ENDIF
           SIGOLD=ZR(ISIGIE+KP-1)
           IF (SIGI.GT.SIGOLD) THEN
              ZR(ISIGIS+KP-1)=SIGI
           ELSE
              ZR(ISIGIS+KP-1)=ZR(ISIGIE+KP-1)
           ENDIF
           SIGI=ZR(ISIGIS+KP-1)
           DSIGWB=DSIGWB+VOLUME*(SIGI**M)/V0
C
 100     CONTINUE
C
      ELSE
      CALL UTMESS('F','TE0331','OPTION DE CALCUL NON VALIDE')
      ENDIF
C=================================================================
C=================================================================
      ZR(IWEIB)=DSIGWB
C
C     DESTRUCTION DES OBJETS CREES DANS LA BASE
C
      END

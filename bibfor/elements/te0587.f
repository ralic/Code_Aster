      SUBROUTINE TE0587(OPTION,NOMTE)
      IMPLICIT NONE
C MODIF ELEMENTS  DATE 02/10/2002   AUTEUR ASSIRE A.ASSIRE 
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
C TOLE CRP_20
      CHARACTER*16 OPTION,NOMTE
C ......................................................................

C    - FONCTION REALISEE:  CALCUL DES OPTIONS VARI_ELNO_ELGA ET
C      SIEF_ELNO_ELGA POUR U
C                          ELEMENT: METUSEG3

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NBCOUM,NBSECM,JNBSPI
      CHARACTER*24 CARAC,FF,CHMAT,NOMCHA,NOMCMP
      CHARACTER*16 ELREFL
      CHARACTER*8  ELREFE
      REAL*8 H,A
      PARAMETER (NBSECM=32,NBCOUM=10)
      REAL*8 POICOU(2*NBCOUM+1),POISEC(2*NBSECM+1)
      REAL*8 PI,DEUXPI,SIG(6),FNO(4,6)
      REAL*8 EFG(6),ALPHAF,BETAF,ALPHAM,BETAM,XA,XB,XC,XD
      REAL*8 PGL(3,3),PGL4(3,3),VNO(4),VPG(4)
      REAL*8 COSFI,SINFI,HK(4,4)
      REAL*8 FI,POIDS,R,R8PI,OMEGA
      REAL*8 PGL1(3,3),PGL2(3,3),PGL3(3,3),RAYON,THETA,L
      REAL*8 CP(2,2),CV(2,2),CO(4,4),SI(4,4),TK(4),XPG(4)
      REAL*8 VEQG(6),VALMIN,VALMAX,R8MAEM,VAL   
      INTEGER NNO,NPG,NBCOU,NBSEC,ICARAC,LORIEN,NUMCMP
      INTEGER IPOIDS,IVF,ICOUDE,JMAT,IC,KP,NNOS,NBCMP
      INTEGER ICAGEP,IGEOM,I1,I2,IH,NLIG,NCOL
      INTEGER IGAU,ICOU,ISECT,I,JIN,JOUT,INO,NPSEMI,NPSEMA
      INTEGER IFF,INDICE,K,IP,INVAR,K2,ICOUD2,MMT,JNOM
      INTEGER J1,J2,J3,KPGS,LGPG,NBVARI,ICOMPO,JTAB(7)
      INTEGER NPG1,IXF,NCOUMI,NCOUMA,NPCOMI,NPCOMA,NSECMI,NSECMA

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      CALL ELREF1(ELREFE)

      ELREFL = ELREFE

      PI = R8PI()
      DEUXPI = 2.D0*PI

C=====RECUPERATION NOMBRE DE COUCHES ET DE SECTEURS ANGULAIRES

      CALL TECACH(.FALSE.,.FALSE.,'PCOMPOR',1,ICOMPO)
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
      NBSEC=ZI(JNBSPI-1+2)
      IF (NBCOU*NBSEC.LE.0) THEN
        CALL UTMESS('F','TE0587','LE NOMBRE DE COUCHES ET DE '//
     +              'SECTEURS DOIVENT ETRE SUPERIEURS A 0')
      END IF
      IF (NBCOU.GT.NBCOUM) THEN
        CALL UTDEBM('F','TE0587','TUYAU : LE NOMBRE DE COUCHES')
        CALL UTIMPI('L',' EST LIMITE A ',1,NBCOUM)
        CALL UTFINM
      END IF
      IF (NBSEC.GT.NBSECM) THEN
        CALL UTDEBM('F','TE0587','TUYAU : LE NOMBRE DE SECTEURS')
        CALL UTIMPI('L',' EST LIMITE A ',1,NBSECM)
        CALL UTFINM
      END IF

      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)

      NNO = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)

C     PREMIERE FAMILLE DE POINTS DE GAUSS POUR LES CHAMPS

      NPG = NPG1
      IXF= IFF
      IPOIDS = IXF+NPG
      IVF = IPOIDS + NPG
      CHMAT = '&INEL.'//ELREFL//'M1'
      CALL JEVETE(CHMAT,'L',JMAT)

      DO 55 I=1,NPG
         XPG(I)=ZR(IXF-1+I)
55    CONTINUE

C  LES POIDS POUR L'INTEGRATION DANS L'EPAISSEUR

      POICOU(1) = 1.D0/3.D0
      DO 10 I = 1,NBCOU - 1
        POICOU(2*I) = 4.D0/3.D0
        POICOU(2*I+1) = 2.D0/3.D0
   10 CONTINUE
      POICOU(2*NBCOU) = 4.D0/3.D0
      POICOU(2*NBCOU+1) = 1.D0/3.D0

C  LES POIDS POUR L'INTEGRATION SUR LA CIRCONFERENCE

      POISEC(1) = 1.D0/3.D0
      DO 20 I = 1,NBSEC - 1
        POISEC(2*I) = 4.D0/3.D0
        POISEC(2*I+1) = 2.D0/3.D0
   20 CONTINUE
      POISEC(2*NBSEC) = 4.D0/3.D0
      POISEC(2*NBSEC+1) = 1.D0/3.D0

C   FIN DES POIDS D'INTEGRATION


C  CONTRUCTION DE LA MATRICE H(I,J) = MATRICE DES VALEURS DES
C  FONCTIONS DE FORMES AUX POINT DE GAUSS

      DO 40,K = 1,NNO
        DO 30,IGAU = 1,NPG
          HK(K,IGAU) = ZR(IVF-1+NNO* (IGAU-1)+K)
   30   CONTINUE
   40 CONTINUE


      IF (OPTION(1:9).EQ.'VARI_ELNO') THEN
C     -------------------------------------
        CALL JEVECH('PVARIGR','L',JIN)

        READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
        CALL TECACH(.TRUE.,.TRUE.,'PVARIGR',7,JTAB)
        LGPG = MAX(JTAB(6),1)*JTAB(7)


C -- RECUPERATION DES VARIABLES INTERNES
C -- NBVARI = NOMBRES DE VARIABLES INTERNES
C -- STOCKAGE DANS PVARIGR : PAR POINT DE GAUSS DU PREMIER
C -- AU DERNIER

        CALL JEVECH('PVARINR','E',JOUT)

       IF ((NNO.EQ.3).AND.(NPG.EQ.3)) THEN

        DO 90 ICOU = 1,2*NBCOU + 1

C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE

          DO 80 ISECT = 1,2*NBSEC + 1
            KPGS = ((2*NBSEC+1)* (ICOU-1)+ (ISECT-1))*NBVARI
            DO 70 INO = 1,NNO
              IF (INO.EQ.1) THEN
                IH = 2
                I1 = 1
                I2 = 3
                J1 = KPGS
                J2 = 2*LGPG + KPGS
                J3 = LGPG + KPGS
              ELSE IF (INO.EQ.2) THEN
                IH = 1
                I1 = 3
                I2 = 1
                J1 = 2*LGPG + KPGS
                J2 = KPGS
                J3 = LGPG + KPGS
              ELSE
                K2 = LGPG + KPGS
                DO 50 INVAR = 1,NBVARI
                  ZR(JOUT-1+K2+LGPG+INVAR) = ZR(JIN-1+K2+INVAR)
   50           CONTINUE
                GO TO 70
              END IF
              K2 = LGPG* (INO-1) + KPGS
              DO 60 INVAR = 1,NBVARI
                ZR(JOUT-1+K2+INVAR) = (HK(IH,I2)*ZR(JIN-1+J1+INVAR)-
     +                                HK(IH,I1)*ZR(JIN-1+J2+INVAR)-
     +                                ZR(JIN-1+J3+INVAR)*
     +                                (HK(3,I1)*HK(IH,I2)-HK(3,
     +                                I2)*HK(IH,I1)))/
     +                                (HK(1,1)*HK(2,3)-HK(1,3)*HK(2,1))
   60         CONTINUE
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE

       ELSE

          DO 213 IC=1,LGPG
             DO 214 KP=1,NPG
                VPG(KP) = ZR(JIN+LGPG*(KP-1)+IC-1)
214          CONTINUE
             NNOS=2
             NLIG=NINT(ZR(JMAT))
             NCOL=NINT(ZR(JMAT+1))
             CALL TUGANO (ZR(JMAT+2),NLIG,NCOL,NNOS,NNO,NPG,VPG,VNO)

             DO 215 I=1,NNO
                ZR(JOUT+LGPG*(I-1)+IC-1) = VNO(I)
215          CONTINUE

213       CONTINUE
        ENDIF

C  FIN STOCKAGE


      ELSE IF (OPTION(1:9).EQ.'SIEF_ELNO') THEN
C     ------------------------------------------

        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PCAGEPO','L',ICAGEP)
        H = ZR(ICAGEP+1)
        A = ZR(ICAGEP) - H/2.D0

C A= RMOY, H = EPAISSEUR
C     --- RECUPERATION DES ORIENTATIONS ---

        CALL JEVECH('PCAORIE','L',LORIEN)
        CALL CARCOU(ZR(LORIEN),L,PGL,RAYON,THETA,PGL1,PGL2,PGL3,PGL4,NNO
     &             ,OMEGA, ICOUD2)
      IF (ICOUD2.GE.10) THEN
         ICOUDE = ICOUD2 - 10
         MMT=0
      ELSE
         ICOUDE = ICOUD2
         MMT=1
      ENDIF

       IF (NNO.EQ.3) THEN
          TK(1)  = 0.D0
          TK(2)  = THETA
          TK(3)  = THETA/2.D0
       ELSEIF (NNO.EQ.4) THEN
          TK(1)  = 0.D0
          TK(2)  = THETA
          TK(3)  = THETA/3.D0
          TK(4)  = 2.D0*THETA/3.D0
       ENDIF

C ======== RAPPEL DES CONTRAINTES ====================

        CALL JEVECH('PCONTRR','L',JIN)

        CALL JEVECH('PSIEFNOR','E',JOUT)

C BOUCLE SUR LES POINTS DE GAUSS

C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR

        KPGS = 0
        DO 140 IGAU = 1,NPG

C INIALISATION DE EFG

          DO 100,I = 1,6
            EFG(I) = 0.D0
  100     CONTINUE

          DO 120 ICOU = 1,2*NBCOU + 1
          IF (MMT.EQ.0) THEN
              R      = A
          ELSE
              R = A + (ICOU-1)*H/ (2.D0*NBCOU) - H/2.D0
          ENDIF

C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE

            DO 110 ISECT = 1,2*NBSEC + 1

              KPGS = KPGS + 1
              FI = (ISECT-1)*DEUXPI/ (2.D0*NBSEC)
              COSFI = COS(FI)
              SINFI = SIN(FI)

              INDICE = JIN - 1 + 6* (KPGS-1)
              SIG(1) = ZR(INDICE+1)
              SIG(2) = ZR(INDICE+2)
              SIG(3) = ZR(INDICE+4)
              SIG(4) = ZR(INDICE+5)

              POIDS = POICOU(ICOU)*POISEC(ISECT)*H*DEUXPI/
     +                (4.D0*NBCOU*NBSEC)*R


              EFG(1) = EFG(1) + POIDS*SIG(1)
              EFG(2) = EFG(2) - POIDS* (SINFI*SIG(4)+COSFI*SIG(3))
              EFG(3) = EFG(3) + POIDS* (SINFI*SIG(3)-COSFI*SIG(4))

              EFG(4) = EFG(4) - POIDS*SIG(3)*R
              EFG(5) = EFG(5) - POIDS*SIG(1)*R*COSFI
              EFG(6) = EFG(6) + POIDS*SIG(1)*R*SINFI


  110       CONTINUE
  120     CONTINUE

          DO 130,I = 1,6
            FNO(IGAU,I) = EFG(I)
  130     CONTINUE

  140   CONTINUE
C STOCKAGE DES EFG

       IF ((NNO.EQ.3).AND.(NPG.EQ.3)) THEN

        DO 160 IGAU = 1,NPG
          DO 150 INO = 1,NNO
            IF (ICOUDE.EQ.0) THEN
              CO(IGAU,INO) = 1.D0
              SI(IGAU,INO) = 0.D0
            ELSE
              CO(IGAU,INO) = COS((1.D0+XPG(IGAU))*THETA/2.D0-TK(INO))
              SI(IGAU,INO) = SIN((1.D0+XPG(IGAU))*THETA/2.D0-TK(INO))
            END IF
  150     CONTINUE
  160   CONTINUE
        DO 200,INO = 1,NNO
          IF (INO.EQ.1) THEN
            IH = 2
            IP = 1
            I1 = 1
            I2 = 3
          ELSE IF (INO.EQ.2) THEN
            IH = 1
            IP = 2
            I1 = 3
            I2 = 1
          ELSE
            DO 170,I = 1,6
              EFG(I) = FNO(2,I)
  170       CONTINUE
            GO TO 180
          END IF

          CP(1,1) = CO(1,IH)*CO(1,3) + SI(1,IH)*SI(1,3)
          CP(1,2) = -CO(1,IH)*SI(1,3) + SI(1,IH)*CO(1,3)
          CP(2,1) = -CP(1,2)
          CP(2,2) = CP(1,1)
          CV(1,1) = CO(3,IH)*CO(3,3) + SI(3,IH)*SI(3,3)
          CV(1,2) = -CO(3,IH)*SI(3,3) + SI(3,IH)*CO(3,3)
          CV(2,1) = -CP(1,2)
          CV(2,2) = CP(1,1)

          ALPHAF = HK(IH,3)* (CO(1,IH)*FNO(1,1)+SI(1,IH)*FNO(1,2)) -
     +             HK(IH,3)*HK(3,1)* (CP(1,1)*FNO(2,1)+
     +             CP(1,2)*FNO(2,2)) - HK(IH,1)*
     +             (CO(3,IH)*FNO(3,1)+SI(3,IH)*FNO(3,2)) +
     +             HK(IH,1)*HK(3,3)* (CV(1,1)*FNO(2,1)+CV(1,2)*FNO(2,2))

          BETAF = HK(IH,3)* (-SI(1,IH)*FNO(1,1)+CO(1,IH)*FNO(1,2)) -
     +            HK(IH,3)*HK(3,1)* (CP(2,1)*FNO(2,1)+
     +            CP(2,2)*FNO(2,2)) - HK(IH,1)*
     +            (-SI(3,IH)*FNO(3,1)+CO(3,IH)*FNO(3,2)) +
     +            HK(IH,1)*HK(3,3)* (CV(2,1)*FNO(2,1)+CV(2,2)*FNO(2,2))

          ALPHAM = HK(IH,3)* (CO(1,IH)*FNO(1,4)+SI(1,IH)*FNO(1,5)) -
     +             HK(IH,3)*HK(3,1)* (CP(1,1)*FNO(2,4)+
     +             CP(1,2)*FNO(2,5)) - HK(IH,1)*
     +             (CO(3,IH)*FNO(3,4)+SI(3,IH)*FNO(3,5)) +
     +             HK(IH,1)*HK(3,3)* (CV(1,1)*FNO(2,4)+CV(1,2)*FNO(2,5))

          BETAM = HK(IH,3)* (-SI(1,IH)*FNO(1,4)+CO(1,IH)*FNO(1,5)) -
     +            HK(IH,3)*HK(3,1)* (CP(2,1)*FNO(2,4)+
     +            CP(2,2)*FNO(2,5)) - HK(IH,1)*
     +            (-SI(3,IH)*FNO(3,4)+CO(3,IH)*FNO(3,5)) +
     +            HK(IH,1)*HK(3,3)* (CV(2,1)*FNO(2,4)+CV(2,2)*FNO(2,5))

          CP(1,1) = CO(1,IH)*CO(1,IP) + SI(1,IH)*SI(1,IP)
          CP(1,2) = -CO(1,IH)*SI(1,IP) + SI(1,IH)*CO(1,IP)
          CP(2,1) = -CP(1,2)
          CP(2,2) = CP(1,1)
          CV(1,1) = CO(3,IH)*CO(3,IP) + SI(3,IH)*SI(3,IP)
          CV(1,2) = -CO(3,IH)*SI(3,IP) + SI(3,IH)*CO(3,IP)
          CV(2,1) = -CP(1,2)
          CV(2,2) = CP(1,1)

          XA = HK(IP,1)*HK(IH,3)*CP(1,1) - HK(IP,3)*HK(IH,1)*CV(1,1)
          XB = HK(IP,1)*HK(IH,3)*CP(1,2) - HK(IP,3)*HK(IH,1)*CV(1,2)
          XC = HK(IP,1)*HK(IH,3)*CP(2,1) - HK(IP,3)*HK(IH,1)*CV(2,1)
          XD = HK(IP,1)*HK(IH,3)*CP(2,2) - HK(IP,3)*HK(IH,1)*CV(2,2)

          EFG(1) = (XD*ALPHAF-XB*BETAF)/ (XA*XD-XB*XC)
          EFG(2) = (-XC*ALPHAF+XA*BETAF)/ (XA*XD-XB*XC)
          EFG(3) = (HK(IH,I2)*FNO(I1,3)-HK(IH,I1)*FNO(I2,3)-
     +             FNO(2,3)* (HK(3,I1)*HK(IH,I2)-HK(3,I2)*HK(IH,I1)))/
     +             (HK(1,1)*HK(2,3)-HK(1,3)*HK(2,1))
          EFG(4) = (XD*ALPHAM-XB*BETAM)/ (XA*XD-XB*XC)
          EFG(5) = (-XC*ALPHAM+XA*BETAM)/ (XA*XD-XB*XC)
          EFG(6) = (HK(IH,I2)*FNO(I1,6)-HK(IH,I1)*FNO(I2,6)-
     +             FNO(2,6)* (HK(3,I1)*HK(IH,I2)-HK(3,I2)*HK(IH,I1)))/
     +             (HK(1,1)*HK(2,3)-HK(1,3)*HK(2,1))

  180     CONTINUE

          DO 190,I = 1,6
            ZR(JOUT-1+6* (INO-1)+I) = EFG(I)
  190     CONTINUE
  200   CONTINUE

       ELSE
          DO 201 IC=1,6

             DO 211 KP=1,NPG
                VPG(KP) = FNO(KP,IC)
211          CONTINUE
             NNOS=2
             NLIG=NINT(ZR(JMAT))
             NCOL=NINT(ZR(JMAT+1))
             CALL TUGANO (ZR(JMAT+2),NLIG,NCOL,NNOS,NNO,NPG,VPG,VNO)
             DO 212 INO=1,NNO
                ZR(JOUT+6*(INO-1)+IC-1) = VNO(INO)
212          CONTINUE

201         CONTINUE

        ENDIF

C  =========================================




C=======================================================================
      ELSE IF (OPTION(1:9).EQ.'EQUI_ELGA') THEN
C=======================================================================


C ======== RAPPEL DES CONTRAINTES ====================

        IF (OPTION.EQ.'EQUI_ELGA_SIGM') THEN
          CALL JEVECH('PCONTRR','L',JIN)
          CALL JEVECH('PCONTEQ','E',JOUT)
        ELSE
          CALL JEVECH('PDEFORR','L',JIN)
          CALL JEVECH('PDEFOEQ','E',JOUT)
        ENDIF

C BOUCLE SUR LES POINTS DE GAUSS

C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR

        KPGS = 0
        DO 340 IGAU = 1,NPG

C INIALISATION DE EFG

          DO 300,I = 1,6
            VEQG(I) = 0.D0
  300     CONTINUE

          DO 320 ICOU = 1,2*NBCOU + 1

C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE

            DO 310 ISECT = 1,2*NBSEC + 1

              KPGS = KPGS + 1

              INDICE = JIN - 1 + 6* (KPGS-1)
              SIG(1) = ZR(INDICE+1)
              SIG(2) = ZR(INDICE+2)
              SIG(3) = ZR(INDICE+3)
              SIG(4) = ZR(INDICE+4)
              SIG(5) = ZR(INDICE+5)
              SIG(6) = ZR(INDICE+6)

              IF (OPTION.EQ.'EQUI_ELGA_SIGM') THEN
                CALL FGEQUI(SIG,'SIGM',3,VEQG)
              ELSE
                CALL FGEQUI(SIG,'EPSI',3,VEQG)
              ENDIF

              ZR(JOUT-1+KPGS) = VEQG(1)

  310       CONTINUE
  320     CONTINUE
  340   CONTINUE

C  =========================================


C=======================================================================
      ELSE IF (OPTION.EQ.'VALE_NCOU_MAXI') THEN
C=======================================================================

C ======== RAPPEL DES CONTRAINTES ====================

        CALL JEVECH('PMINMAX','E',JOUT)
        CALL JEVECH('PNOMCMP','L',JNOM)
        NOMCHA=ZK24(JNOM)
        NOMCMP=ZK24(JNOM+1)
        IF (NOMCHA(1:9).EQ.'SIEF_ELGA') THEN
          CALL JEVECH('PCONTRR','L',JIN)
          NBCMP=6
          IF (NOMCMP(1:4).EQ.'SIXX') THEN
                NUMCMP=1
          ELSEIF (NOMCMP(1:4).EQ.'SIYY') THEN
                NUMCMP=2
          ELSEIF (NOMCMP(1:4).EQ.'SIZZ') THEN
                NUMCMP=3
          ELSEIF (NOMCMP(1:4).EQ.'SIXY') THEN
                NUMCMP=4
          ELSEIF (NOMCMP(1:4).EQ.'SIXZ') THEN
                NUMCMP=5
          ELSEIF (NOMCMP(1:4).EQ.'SIYZ') THEN
                NUMCMP=6
          ELSE
           CALL UTMESS('A','VALE_NCOU_MAXI','CMP '//NOMCMP//
     &                ' NON TRAITEE, ON ABANDONNE')
           GOTO 210
          ENDIF
        ELSEIF (NOMCHA.EQ.'EQUI_ELGA_SIGM') THEN
          CALL JEVECH('PCONTEQ','L',JIN)
          NBCMP=1
          IF (NOMCMP(1:4).EQ.'VMIS') THEN
                NUMCMP=1
          ELSE
           CALL UTMESS('A','VALE_NCOU_MAXI','CMP '//NOMCMP//
     &                ' NON TRAITEE, ON ABANDONNE')
           GOTO 210
          ENDIF
        ELSEIF (NOMCHA.EQ.'EPSI_ELGA_DEPL') THEN
          CALL JEVECH('PDEFORR','L',JIN)
          NBCMP=6
          IF (NOMCMP(1:4).EQ.'EPXX') THEN
                NUMCMP=1
          ELSEIF (NOMCMP(1:4).EQ.'EPYY') THEN
                NUMCMP=2
          ELSEIF (NOMCMP(1:4).EQ.'EPZZ') THEN
                NUMCMP=3
          ELSEIF (NOMCMP(1:4).EQ.'EPXY') THEN
                NUMCMP=4
          ELSEIF (NOMCMP(1:4).EQ.'EPXZ') THEN
                NUMCMP=5
          ELSEIF (NOMCMP(1:4).EQ.'EPYZ') THEN
                NUMCMP=6
          ELSE
           CALL UTMESS('A','VALE_NCOU_MAXI','CMP '//NOMCMP//
     &                ' NON TRAITEE, ON ABANDONNE')
           GOTO 210
          ENDIF
        ELSEIF (NOMCHA.EQ.'EQUI_ELGA_EPSI') THEN
          CALL JEVECH('PDEFOEQ','L',JIN)
          NBCMP=1
          IF (NOMCMP(1:6).EQ.'INVA_2') THEN
                NUMCMP=1
          ELSE
           CALL UTMESS('A','VALE_NCOU_MAXI','CMP '//NOMCMP//
     &                ' NON TRAITEE, ON ABANDONNE')
           GOTO 210
          ENDIF
        ELSE
           CALL UTMESS('A','VALE_NCOU_MAXI','CHAMP '//NOMCHA//
     &                ' NON TRAITE, ON ABANDONNE')
           GOTO 210
        ENDIF

C BOUCLE SUR LES POINTS DE GAUSS

        KPGS = 0
        
C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR

        DO 440 IGAU = 1,NPG
           VALMAX=-R8MAEM()
           VALMIN=R8MAEM()
           NCOUMA=0
           NCOUMI=0
           NPCOMA=0
           NPCOMI=0
           NSECMA=0
           NSECMI=0
           NPSEMA=0
           NPSEMI=0

C INIALISATION DE EFG

          DO 420 ICOU = 1,2*NBCOU + 1

C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE

            DO 410 ISECT = 1,2*NBSEC + 1

              KPGS = KPGS + 1

              INDICE = JIN - 1 + NBCMP* (KPGS-1)
              VAL = ZR(INDICE+NUMCMP)
              IF (VAL.GT.VALMAX) THEN
                 VALMAX=VAL
                 IF (ICOU.GT.1) THEN
                    NCOUMA=ICOU/2
                    NPCOMA=ICOU-2*NCOUMA+2
                 ELSE
                    NCOUMA=1
                    NPCOMA=1
                 ENDIF
                 IF (ISECT.GT.1) THEN
                    NSECMA=ISECT/2
                    NPSEMA=ISECT-2*NSECMA+2
                 ELSE
                    NSECMA=1
                    NPSEMA=1
                 ENDIF
               ENDIF
              IF (VAL.LT.VALMIN) THEN
                 VALMIN=VAL
                 IF (ICOU.GT.1) THEN
                    NCOUMI=ICOU/2
                    NPCOMI=ICOU-2*NCOUMI+2
                 ELSE
                    NCOUMI=1
                    NPCOMI=1
                 ENDIF
                 IF (ISECT.GT.1) THEN
                    NSECMI=ISECT/2
                    NPSEMI=ISECT-2*NSECMI+2
                 ELSE
                    NSECMI=1
                    NPSEMI=1
                 ENDIF
               ENDIF
  410       CONTINUE
  420     CONTINUE
C   MIN      MAX      NCOUMIN  NCOUMAX  NSECMIN  NSECMAX
C   NPCOMIN NPCOMAX NPSECMIN NPSECMAX        
            ZR(JOUT+10*(IGAU-1)+0) = VALMIN
            ZR(JOUT+10*(IGAU-1)+1) = VALMAX
            ZR(JOUT+10*(IGAU-1)+2) = NCOUMI
            ZR(JOUT+10*(IGAU-1)+3) = NCOUMA
            ZR(JOUT+10*(IGAU-1)+4) = NSECMI
            ZR(JOUT+10*(IGAU-1)+5) = NSECMA
            ZR(JOUT+10*(IGAU-1)+6) = NPCOMI
            ZR(JOUT+10*(IGAU-1)+7) = NPCOMA
            ZR(JOUT+10*(IGAU-1)+8) = NPSEMI
            ZR(JOUT+10*(IGAU-1)+9) = NPSEMA
  440   CONTINUE


C  =========================================


      ELSE
        CALL UTMESS('F','TE0587','L''OPTION "'//OPTION//
     +              '" EST NON PREVUE')
      END IF

  210 CONTINUE
      END

      SUBROUTINE TE0377 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C
C    - FONCTION REALISEE:  CALCUL DE L'ERREUR SUR UN ELEMENT AVEC LA
C                          METHODE DES RESIDUS
C                          OPTION : 'ERRE_ELGA_NORE  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER            NPG1, I, K, KP, NNO, TY, TYV
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM
      INTEGER            IAD, IFOR, IERR, IPES, IROT, IMATE
      INTEGER            IREF, IVOIS, IAREPE, JCELD, JCELV, IAVAL1
      INTEGER            JAD, JADV, IGREL, IEL, IGD
      INTEGER            NBS, NBNA, JNO, NBSV, NBNV, NCHER
      INTEGER            INOV, JNOV, IMAV, MNO, MNOV, JKP
      REAL*8             A, B, C, D, E, F, G, J, H1, H2
      REAL*8             DFDX(9), DFDY(9), H, POIDS, XC, YC
      REAL*8             FORX, FORY, FPX, FPY, FRX(9), FRY(9), RHO
      REAL*8             SIG11,SIG22,SIG12,DSIG11,DSIG12,DSIG21,DSIG22
      REAL*8             SPG11,SPG22,SPG33,SPG12,SIG33,DSX,DSY,R
      REAL*8             TER, ERREST, NOR, NORSIG, SIGCAL, NUEST, COEFF
      REAL*8             TER2, TER3, HF, NORM, NORM2
      REAL*8             SG11(3), SG22(3), SG12(3), JAC(3), X(2), Y(2)
      REAL*8             XN(3), YN(3), XT(3), YT(3)
      REAL*8             PR, CI, FX, FY, VALPAR(3), PRC(3), CIC(3)
      REAL*8             FXC(3), FYC(3), X3, Y3, INST
      REAL*8             DXDE,DXDK,DYDE,DYDK,XP,YP,JACOB,DFRDE,DFRDK
      CHARACTER*2        CODRET, FORM, FORMV, NOEU, NOEUV
      CHARACTER*4        NOMPAR(3)
      CHARACTER*8        MA, TYPEMA, TYPMAV
      CHARACTER*8        PRF, CIF, FXF, FYF
      CHARACTER*8        ELREFE
      CHARACTER*16        PHENOM
      CHARACTER*19       MO, SIGMA, CARTE1, CARTE2, NOMGD1, KBID
      CHARACTER*19       NOMGD2
      CHARACTER*24       CARAC,FF
C
C --------- CALCUL DU PREMIER TERME DE L'ERREUR ------------------------
C
      CALL ELREF1(ELREFE)
      CALL JEMARQ()
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,' ',IFF)
      IPOIDS = IFF
      IVF  = IPOIDS+NPG1
      IDFDE = IVF  +NNO*NPG1
      IDFDK = IDFDE +NNO*NPG1
C
C
      CALL JEVECH ('PGEOMER', 'L', IGEOM )
      CALL JEVECH ('PCONTNO', 'L', IAD   )
      CALL JEVECH ('PFRVOLU', 'L', IFOR  )
      CALL JEVECH ('PERREUR', 'E', IERR  )
      CALL JEVECH ('PTEMPSR', 'L', JTIME )
      INST = ZR(JTIME-1+1)
C
C -------- CALCUL DU DIAMETRE H ---------------------------------------
C
      IF (NOMTE(5:6) .EQ. 'TR') THEN
        I = 1
        A = ZR(IGEOM+2*I-2) + ZR(IGEOM+2*(I+1)-2)
        B = ZR(IGEOM+2*(I+2)-1) + ZR(IGEOM+2*I-1)
        C = ZR(IGEOM+2*(I+1)-1) - ZR(IGEOM+2*I-1)
        D = ZR(IGEOM+2*I-2) - ZR(IGEOM+2*(I+1)-2)
        E = ZR(IGEOM+2*(I+2)-1) - ZR(IGEOM+2*I-1)
        F = ZR(IGEOM+2*I-2) - ZR(IGEOM+2*(I+2)-2)
        G = ZR(IGEOM+2*(I+2)-2) - ZR(IGEOM+2*(I+1)-2)
        J = ZR(IGEOM+2*(I+1)-1) + ZR(IGEOM+2*I-1)
        IF (D .EQ. 0.D0) THEN
          YC = 0.5D0 * J
          XC = 0.5D0 *(A + G -(E/F)*(B - 2.0D0*YC))
        ELSE IF (F .EQ. 0.D0) THEN
          YC = 0.5D0 * B
          XC = 0.5D0*(A -(C/D)*(J -2.0D0*YC))
        ELSE
          YC = 0.5D0*(1.0D0/((C/D)-(E/F)))*(G -(E*B/F)+(C*J/D))
          XC = 0.5D0*(A -(C/D)*(J -2.0D0*YC))
        ENDIF
        H = 2.D0 * SQRT((ZR(IGEOM+2*I-2)-XC)**2+
     &                          (ZR(IGEOM+2*I-1)-YC)**2)
      ELSE IF ((NOMTE(5:6).EQ.'QU').OR.(NOMTE(5:6).EQ.'QS')) THEN
        I = 1
        H1 = SQRT((ZR(IGEOM+2*I-2) - ZR(IGEOM+2*(I+2)-2))**2 +
     &          (ZR(IGEOM+2*I-1) - ZR(IGEOM+2*(I+2)-1))**2)
        H2 = SQRT((ZR(IGEOM+2*(I+1)-2) - ZR(IGEOM+2*(I+3)-2))**2 +
     &          (ZR(IGEOM+2*(I+1)-1) - ZR(IGEOM+2*(I+3)-1))**2)
        IF (H1.GT.H2) THEN
          H = H1
        ELSE
          H = H2
        ENDIF
      ELSE
        CALL UTMESS ('F','TE0377','TYPE ELEMENT INCONNU')
      ENDIF
C
C ------ INITIALISATION DES FORCES --------------------------------
C
      FPX = 0.D0
      FPY = 0.D0
      DO 50 JKP =1, 9
        FRX(JKP) = 0.D0
        FRY(JKP) = 0.D0
   50 CONTINUE
C
C ------ TEST D'EXISTENCE DES CARTES DE PESA ET ROTA -----------------
C
      CALL TECACH(.TRUE.,.FALSE.,'PPESANR',1,IP)
      CALL TECACH(.TRUE.,.FALSE.,'PROTATR',1,IR)
      IF (IP .NE. 0 .OR. IR .NE.0) THEN
         CALL JEVECH ('PMATERC','L',IMATE)
         CALL RCCOMA ( ZI(IMATE),'ELAS',PHENOM,CODRET)
         CALL RCVALA ( ZI(IMATE),PHENOM,1,' ',R8BID,1,'RHO',
     &                 RHO, CODRET, 'FM' )
C
C -----------CALCUL DE LA FORCE DE PESANTEUR --------------------------
C
         IF (IP .NE. 0) THEN
           CALL JEVECH('PPESANR','L',IPES)
           FPX = RHO * ZR(IPES) * ZR(IPES+1)
           FPY = RHO * ZR(IPES) * ZR(IPES+2)
         ENDIF
C
C -----------CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS--------
C
         IF (IR .NE. 0)THEN
           CALL JEVECH('PROTATR','L',IROT)
           CALL RESROT ( ZR(IROT), ZR(IGEOM), ZR(IVF), RHO, NNO, NPG1,
     &              FRX, FRY)
         ENDIF
      ENDIF
C
C -----------CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS--------
C
      TER = 0.D0
      NORSIG = 0.D0
      DO 200 KP=1,NPG1
        K = (KP-1)*NNO
        CALL DFDM2D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &              ZR(IGEOM),DFDX,DFDY,POIDS)
C CALCUL L'ORIENTATION DE LA MAILLE
        DXDE=0.D0
        DXDK=0.D0
        DYDE=0.D0
        DYDK=0.D0
        DO 111 I=1,NNO
          I1 = I - 1
          IJ = IGEOM+2*I1
          XP = ZR(IJ)
          YP = ZR(IJ+1)
          DFRDE = ZR(IDFDE+K+I1)
          DFRDK = ZR(IDFDK+K+I1)
          DXDE=DXDE+XP*DFRDE
          DXDK=DXDK+XP*DFRDK
          DYDE=DYDE+YP*DFRDE
          DYDK=DYDK+YP*DFRDK
  111   CONTINUE
        JACOB=DXDE*DYDK-DXDK*DYDE
        JACOB = SIGN(1.D0,JACOB)
C ----------CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA -----------
C
        DSIG11 = 0.D0
        DSIG12 = 0.D0
        DSIG21 = 0.D0
        DSIG22 = 0.D0
C
        SPG11 = 0.D0
        SPG22 = 0.D0
        SPG33 = 0.D0
        SPG12 = 0.D0
C
        IF ( NOMTE(3:4) .EQ. 'AX') THEN
          R = 0.D0
          DO 100 I=1,NNO
            R = R + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
C
            SIG11 = ZR(IAD-1+4*(I-1)+1)
            SIG22 = ZR(IAD-1+4*(I-1)+2)
            SIG33 = ZR(IAD-1+4*(I-1)+3)
            SIG12 = ZR(IAD-1+4*(I-1)+4)
C
            DSIG11 = DSIG11+SIG11*DFDX(I)
            DSIG12 = DSIG12+SIG12*DFDY(I)
            DSIG22 = DSIG22+SIG22*DFDY(I)
            DSIG21 = DSIG21+SIG12*DFDX(I)
C
            SPG11 = SPG11 + SIG11*ZR(IVF+K+I-1)
            SPG22 = SPG22 + SIG22*ZR(IVF+K+I-1)
            SPG33 = SPG33 + SIG33*ZR(IVF+K+I-1)
            SPG12 = SPG12 + SIG12*ZR(IVF+K+I-1)
  100     CONTINUE
          IF(ABS(R).LE.R8PREM()) THEN
             CALL UTMESS('F','TE0377','AXI : R=0')
          ENDIF
          DSX = DSIG11 + DSIG12 + (1.D0/R)*(SPG11 - SPG33)
          DSY = DSIG21 + DSIG22 + (1.D0/R)*SPG12
          POIDS = POIDS*R
        ELSE
          DO 150 I=1,NNO
            SIG11 = ZR(IAD-1+4*(I-1)+1)
            SIG22 = ZR(IAD-1+4*(I-1)+2)
            SIG12 = ZR(IAD-1+4*(I-1)+4)
C
            DSIG11 = DSIG11+SIG11*DFDX(I)
            DSIG12 = DSIG12+SIG12*DFDY(I)
            DSIG22 = DSIG22+SIG22*DFDY(I)
            DSIG21 = DSIG21+SIG12*DFDX(I)
C
            SPG11 = SPG11 + SIG11*ZR(IVF+K+I-1)
            SPG22 = SPG22 + SIG22*ZR(IVF+K+I-1)
            SPG33 = SPG33 + ZR(IAD-1+4*(I-1)+3)*ZR(IVF+K+I-1)
            SPG12 = SPG12 + SIG12*ZR(IVF+K+I-1)
  150     CONTINUE
          DSX = DSIG11+DSIG12
          DSY = DSIG21+DSIG22
        ENDIF
C
        FORX = ZR(IFOR+2*KP-2) + FPX + FRX(KP)
        FORY = ZR(IFOR+2*KP-1) + FPY + FRY(KP)
        TER = TER + ((FORX+DSX)**2 + (FORY+DSY)**2)*POIDS
        NOR = SPG11**2 + SPG22**2 + SPG33**2 + SPG12**2
        NORSIG = NORSIG + NOR * POIDS
  200 CONTINUE
C
C ----------- CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR ------
C
      CALL JEVECH('PFORCE','L',IREF)
      CALL JEVECH('PPRESS','L',IREF2)
      CALL JEVECH('PVOISIN','L',IVOIS)
C
      MO = ZK24(IREF+1)
      SIGMA = ZK24(IREF+2)
      CARTE1 = ZK24(IREF+3)
      CARTE2 = ZK24(IREF2+3)
C
C -------- RECHERCHE DES ADRESSES POUR OBTENIR SIGMA SUR LES VOISINS ---
C
      CALL JEVEUO(MO//'.REPE','L',IAREPE)
      CALL JEVEUO(SIGMA//'.CELD','L',JCELD)
      CALL JEVEUO(SIGMA//'.CELV','L',JCELV)
C
C ------- RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES SEGMENTS ----
C
      IF (CARTE1 .NE. ' ') THEN
        CALL JEVEUO (CARTE1//'.DESC','L',IADE1)
        CALL JEVEUO (CARTE1//'.VALE','L',IAVA1)
        IGD = ZI(IADE1)
        CALL JELIRA (JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',NCMPM1,KBID)
      ENDIF

C
      IF (CARTE2 .NE. ' ') THEN
        CALL JEVEUO (CARTE2//'.DESC','L',IADE2)
        CALL JEVEUO (CARTE2//'.VALE','L',IAVA2)
        IGD = ZI(IADE2)
        CALL JELIRA (JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',NCMPM2,KBID)
      ENDIF
C
C --------- TEST SUR LE TYPE DE LA MAILLE COURANTE --------------------
C
      TY = ZI(IVOIS + 7)

      CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',TY),TYPEMA)

      FORM = TYPEMA(1:2)
      IF (FORM .EQ. 'TR') THEN
        NBS = 3
        ELSE
        NBS = 4
      ENDIF
      NOEU = TYPEMA(5:5)
      IF (NOEU .EQ. '6' .OR. NOEU .EQ. '8' .OR. NOEU .EQ. '9') THEN
        NBNA = 3
        ELSE
        NBNA = 2
      ENDIF
C
C -------BOUCLE SUR LES ARETES -----------------------------------------
C
      TER2 = 0.D0
      TER3 = 0.D0
      DO 300 INO = 1,NBS
C
CALCUL DE HF ---------------
        IF (INO .EQ. NBS) THEN
          JNO = 1
          ELSE
          JNO = INO+1
        ENDIF
        HF = SQRT((ZR(IGEOM+2*INO-2) - ZR(IGEOM+2*JNO-2))**2 +
     &          (ZR(IGEOM+2*INO-1) - ZR(IGEOM+2*JNO-1))**2)
C
CALCUL D'UN POINT MILIEU
        IF (NBNA .EQ. 3) THEN
          MNO = NBS+INO
          X3 = ZR(IGEOM+2*MNO-2)
          Y3 = ZR(IGEOM+2*MNO-1)
        ELSE
          X3 = (ZR(IGEOM+2*INO-2)+ZR(IGEOM+2*JNO-2))/2.D0
          Y3 = (ZR(IGEOM+2*INO-1) + ZR(IGEOM+2*JNO-1))/2.D0
        ENDIF
C
CALCUL NORMALE, TANGENTE ET JACOBIEN PREMIER POINT D'INTEGRATION ----
C
        X(1) = -( (ZR(IGEOM+2*JNO-1) - ZR(IGEOM+2*INO-1))/2.D0
     &        -(ZR(IGEOM+2*INO-1) + ZR(IGEOM+2*JNO-1) -
     &        2.D0 * Y3) )
        Y(1) = (ZR(IGEOM+2*JNO-2)-ZR(IGEOM+2*INO-2))/2.D0 -
     &   (ZR(IGEOM+2*INO-2)+ZR(IGEOM+2*JNO-2)-2.D0*X3)
C
        JAC(1) = SQRT(Y(1)**2 + X(1)**2)
        IF ( NOMTE(3:4) .EQ. 'AX') THEN
           JAC(1) = JAC(1) * ZR(IGEOM+2*INO-2)
        ENDIF
        XN(1) = (X(1)*JACOB) / (SQRT(X(1)**2 + Y(1)**2))
        YN(1) = (Y(1)*JACOB) / (SQRT(X(1)**2 + Y(1)**2))
        XT(1) = YN(1)
        YT(1) = -XN(1)
C
CALCUL NORMALE, TANGENTE ET JACOBIEN DEUXIEME POINT D'INTEGRATION ----
C
        X(2) = -( (ZR(IGEOM+2*JNO-1)-ZR(IGEOM+2*INO-1))/2.D0+
     &           (ZR(IGEOM+2*INO-1) + ZR(IGEOM+2*JNO-1) -
     &            2.D0 * Y3) )
        Y(2) = (ZR(IGEOM+2*JNO-2)-ZR(IGEOM+2*INO-2))/2.D0 +
     &   (ZR(IGEOM+2*INO-2)+ZR(IGEOM+2*JNO-2)-2.D0*X3)
C
        JAC(2) = SQRT(Y(2)**2 + X(2)**2)
        IF ( NOMTE(3:4) .EQ. 'AX') THEN
           JAC(2) = JAC(2) * ZR(IGEOM+2*JNO-2)
        ENDIF
        XN(2) = (X(2)*JACOB) / (SQRT(X(2)**2 + Y(2)**2))
        YN(2) = (Y(2)*JACOB) / (SQRT(X(2)**2 + Y(2)**2))
        XT(2) = YN(2)
        YT(2) = -XN(2)
C
        IF (NBNA .EQ. 3) THEN
C
CALCUL NORMALE, TANGENTE ET JACOBIEN TROISIEME POINT D'INTEGRATION ----
C
          XN(3) = JACOB*(ZR(IGEOM+2*INO-1) - ZR(IGEOM+2*JNO-1))/HF
          YN(3) = JACOB*(ZR(IGEOM+2*JNO-2) - ZR(IGEOM+2*INO-2))/HF
          XT(3) = YN(3)
          YT(3) = -XN(3)
          JAC(3) = HF/2.D0
          IF ( NOMTE(3:4) .EQ. 'AX') THEN
            JAC(3) = JAC(3)*(ZR(IGEOM+2*JNO-2)+ZR(IGEOM+2*INO-2))*0.5D0
          ENDIF
        ENDIF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        TYV = ZI(IVOIS+7+INO)
        IF (TYV .NE. 0) THEN

          CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',TYV),TYPMAV)

          FORMV = TYPMAV(1:2)
          NOEUV = TYPMAV(5:5)
          IF ( FORMV .EQ. 'SE') THEN
C
C ----------CALCUL DU 3 IEME TERME D'ERREUR ----------------------------
C
          IF (CARTE1 .NE. ' ') THEN

            IMAV = ZI(IVOIS+INO)
            CALL JEEXIN (CARTE1//'.PTMA',IRET)
            IF (IRET .EQ. 0) THEN
C              CARTE CONSTANTE
            ELSE
C            LA CARTE A ETE ETENDUE
               CALL JEVEUO (CARTE1//'.PTMA','L',IAPTMA)
               IENT1 = ZI(IAPTMA -1 +IMAV)
            ENDIF
            NOMGD1 = ZK24(IREF+4)
          ENDIF
C
          IF (CARTE2 .NE. ' ') THEN

            IMAV = ZI(IVOIS+INO)
            CALL JEEXIN (CARTE2//'.PTMA',IRET)
            IF (IRET .EQ. 0) THEN
C              CARTE CONSTANTE
               IENT2 = 1
            ELSE
C            LA CARTE A ETE ETENDUE
               CALL JEVEUO (CARTE2//'.PTMA','L',IAPTMA)
               IENT2 = ZI(IAPTMA -1 +IMAV)
            ENDIF
            NOMGD2 = ZK24(IREF2+4)
          ENDIF
C
C
          IF (NOMGD2(1:6) .EQ. 'PRES_R') THEN
               PR = -ZR(IAVA2-1+(IENT2-1)*NCMPM2+1)
               CI = ZR(IAVA2-1+(IENT2-1)*NCMPM2+2)
C
           IF ( ABS(PR) .GT. 1.D-15 .OR. ABS(CI) .GT. 1.D-15) THEN
C
             NORM = JAC(1) * ( (PR*XN(1)+CI*XT(1)-ZR(IAD+4*INO-4)*XN(1)
     &              -ZR(IAD+4*INO-1)*YN(1))**2 + (PR*YN(1)+CI*YT(1)-
     &             ZR(IAD+4*INO-1)*XN(1) -ZR(IAD+4*INO-3)*YN(1))**2 )
     &         +   JAC(2) * ( (PR*XN(2)+CI*XT(2)-ZR(IAD+4*JNO-4)*XN(2)
     &              -ZR(IAD+4*JNO-1)*YN(2))**2+ (PR*YN(2)+CI*YT(2)-
     &             ZR(IAD+4*JNO-1)*XN(2) -ZR(IAD+4*JNO-3)*YN(2))**2 )
C
             IF (NBNA .EQ. 3) THEN
              NORM2 = JAC(3)*( (PR*XN(3)+CI*XT(3)-ZR(IAD+4*MNO-4)*XN(3)
     &               - ZR(IAD+4*MNO-1)*YN(3))**2+ (PR*YN(3)+CI*YT(3)
     &           -ZR(IAD+4*MNO-1)*XN(3) -ZR(IAD+4*MNO-3)*YN(3))**2 )
             TER3 = TER3 + SQRT(HF) * SQRT((1.D0/3.D0)*NORM +
     &                                           (4.D0/3.D0)*NORM2)
             ELSE
              TER3 = TER3 + SQRT(HF) * SQRT(NORM)
             ENDIF
           ENDIF
C
        ELSE IF (NOMGD2(1:6) .EQ. 'PRES_F') THEN
             PRF = ZK8(IAVA2-1+(IENT2-1)*NCMPM2+1)
             CIF = ZK8(IAVA2-1+(IENT2-1)*NCMPM2+2)
C
           IF (PRF .NE. '&FOZERO' .OR. CIF .NE. '&FOZERO') THEN
C
             NOMPAR(1) = 'X'
             NOMPAR(2) = 'Y'
             NOMPAR(3) = 'INST'
             VALPAR(1) = ZR(IGEOM+2*INO-2)
             VALPAR(2) = ZR(IGEOM+2*INO-1)
             VALPAR(3) = INST
             CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRC(1),IER1)
             CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIC(1),IER2)
C
             VALPAR(1) = ZR(IGEOM+2*JNO-2)
             VALPAR(2) = ZR(IGEOM+2*JNO-1)
             CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRC(2),IER3)
             CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIC(2),IER4)
C
             NORM = JAC(1) * ( (-PRC(1)*XN(1)+CIC(1)*XT(1)-
     &           ZR(IAD+4*INO-4)*XN(1)-ZR(IAD+4*INO-1)*YN(1))**2
     &           + (-PRC(1)*YN(1)+CIC(1)*YT(1)-ZR(IAD+4*INO-1)*XN(1)
     &           -ZR(IAD+4*INO-3)*YN(1))**2 )+ JAC(2)*((-PRC(2)*XN(2)
     &           +CIC(2)*XT(2)-ZR(IAD+4*JNO-4)*XN(2) -ZR(IAD+4*JNO-1)
     &           *YN(2))**2+ (-PRC(2)*YN(2)+CIC(2)*YT(2)-
     &           ZR(IAD+4*JNO-1)*XN(2) -ZR(IAD+4*JNO-3)*YN(2))**2 )
C
             IF (NBNA .EQ. 3) THEN
              VALPAR(1) = ZR(IGEOM+2*MNO-2)
              VALPAR(2) = ZR(IGEOM+2*MNO-1)
              VALPAR(3) = INST
              CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRC(3),IER5)
              CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIC(3),IER6)
              NORM2 = JAC(3)*( (-PRC(3)*XN(3) + CIC(3)*XT(3)-
     &              ZR(IAD+4*MNO-4)*XN(3) - ZR(IAD+4*MNO-1)*YN(3))**2
     &              +(-PRC(3)*YN(3)+CIC(3)*YT(3)-ZR(IAD+4*MNO-1)*XN(3)
     &              -ZR(IAD+4*MNO-3)*YN(3))**2 )
C
              TER3 = TER3 + SQRT(HF)*SQRT((1.D0/3.D0)*NORM +
     &               (4.D0/3.D0)*NORM2)
             ELSE
              TER3 = TER3 + SQRT(HF)*SQRT(NORM)
             ENDIF
           ENDIF
C
         ELSE IF (NOMGD1(1:6) .EQ. 'FORC_R') THEN
             FX = ZR(IAVA1-1+(IENT1-1)*NCMPM1+1)
             FY = ZR(IAVA1-1+(IENT1-1)*NCMPM1+2)
C
           IF (ABS(FX) .GT. 1.D-15 .OR. ABS(FY) .GT. 1.D-15) THEN
C
             NORM = JAC(1) * ( ( FX + ZR(IAD+4*INO-4) * XN(1)
     &              + ZR(IAD+4*INO-1) * YN(1))**2 + ( FY +
     &             ZR(IAD+4*INO-1)*XN(1) +ZR(IAD+4*INO-3)*YN(1))**2)
     &         +   JAC(2) * ( ( FX + ZR(IAD+4*JNO-4) * XN(2)
     &              +ZR(IAD+4*JNO-1)*YN(2))**2+ ( FY +
     &             ZR(IAD+4*JNO-1)*XN(2) +ZR(IAD+4*JNO-3)*YN(2))**2)
C
             IF (NBNA .EQ. 3) THEN
              NORM2 = JAC(3)*( ( FX + ZR(IAD+4*MNO-4) * XN(3)
     &               + ZR(IAD+4*MNO-1) * YN(3))**2+ ( FY
     &             +ZR(IAD+4*MNO-1)*XN(3) +ZR(IAD+4*MNO-3)*YN(3))**2)
C
              TER3 = TER3 + SQRT(HF)*SQRT((1.D0/3.D0)*NORM +
     &                                        (4.D0/3.D0)*NORM2)
             ELSE
              TER3 = TER3 + SQRT(HF)*SQRT(NORM)
             ENDIF
           ENDIF
C
         ELSE IF (NOMGD1(1:6) .EQ. 'FORC_F') THEN
             FXF = ZK8(IAVA1-1+(IENT1-1)*NCMPM1+1)
             FYF = ZK8(IAVA1-1+(IENT1-1)*NCMPM1+2)
C
           IF (FXF .NE. '&FOZERO' .OR. FYF .NE. '&FOZERO') THEN
C
             NOMPAR(1) = 'X'
             NOMPAR(2) = 'Y'
             NOMPAR(3) = 'INST'
             VALPAR(1) = ZR(IGEOM+2*INO-2)
             VALPAR(2) = ZR(IGEOM+2*INO-1)
             VALPAR(3) = INST
             CALL FOINTE('FM',FXF,3,NOMPAR,VALPAR,FXC(1),IER1)
             CALL FOINTE('FM',FYF,3,NOMPAR,VALPAR,FYC(1),IER2)
C
             VALPAR(1) = ZR(IGEOM+2*JNO-2)
             VALPAR(2) = ZR(IGEOM+2*JNO-1)
             VALPAR(3) = INST
             CALL FOINTE('FM',FXF,3,NOMPAR,VALPAR,FXC(2),IER3)
             CALL FOINTE('FM',FYF,3,NOMPAR,VALPAR,FYC(2),IER4)
C
             NORM = JAC(1) * ( ( FXC(1)+ZR(IAD+4*INO-4)*XN(1)+
     &            ZR(IAD+4*INO-1)*YN(1))**2+( FYC(1)+ZR(IAD+4*INO-1)
     &            *XN(1)+ZR(IAD+4*INO-3)*YN(1))**2 )+JAC(2)*((FXC(2)
     &            +ZR(IAD+4*JNO-4)*XN(2) +ZR(IAD+4*JNO-1)*YN(2))**2+
     &            ( FYC(2)+ZR(IAD+4*JNO-1)*XN(2)+ZR(IAD+4*JNO-3)
     &            *YN(2))**2)
C
             IF (NBNA .EQ. 3) THEN
              VALPAR(1) = ZR(IGEOM+2*MNO-2)
              VALPAR(2) = ZR(IGEOM+2*MNO-1)
              VALPAR(3) = INST
              CALL FOINTE('FM',FXF,3,NOMPAR,VALPAR,FXC(3),IER5)
              CALL FOINTE('FM',FYF,3,NOMPAR,VALPAR,FYC(3),IER6)
              NORM2 = JAC(3)*( ( FXC(3) + ZR(IAD+4*MNO-4) * XN(3) +
     &            ZR(IAD+4*MNO-1) * YN(3))**2 + (  FYC(3) +
     &            ZR(IAD+4*MNO-1)*XN(3)+ZR(IAD+4*MNO-3)*YN(3))**2)
C
              TER3 = TER3 + SQRT(HF)*SQRT((1.D0/3.D0)*NORM +
     &            (4.D0/3.D0)*NORM2)
             ELSE
              TER3 = TER3 + SQRT(HF)*SQRT(NORM)
             ENDIF
           ENDIF
      ENDIF
C
C ---------- CALCUL DU DEUXIEME TERME ----------------------------------
C
        ELSE IF ( FORMV .EQ. 'TR' .OR. FORMV .EQ. 'QU') THEN
          IF ( FORMV .EQ. 'TR') THEN
            NBSV = 3
            IF ( NOEUV .EQ. '3') THEN
              NBNV = 3
              ELSE
              NBNV = 6
            ENDIF
          ELSE IF ( FORMV .EQ. 'QU') THEN
            NBSV = 4
            IF ( NOEUV .EQ. '4') THEN
              NBNV = 4
              ELSE IF ( NOEUV .EQ. '8') THEN
              NBNV = 8
              ELSE
              NBNV = 9
            ENDIF
          ENDIF
C
CALCUL DE NUMEROTATION DU VOISIN -------
        MA = ZK24(IREF)
        CALL JEVEUO(JEXNUM(MA//'.CONNEX',ZI(IVOIS)),'L',JAD)
        CALL JEVEUO(JEXNUM(MA//'.CONNEX',ZI(IVOIS+INO)),'L',JADV)

        NCHER = ZI(JAD-1+INO)
        INOV = INDIIS(ZI(JADV),NCHER,1,NBNV)
C       IF (INOV .EQ. 1) THEN
C          JNOV = NBSV
C          ELSE
C          JNOV = INOV-1
C        ENDIF
        NCHER = ZI(JAD-1+JNO)
        JNOV = INDIIS(ZI(JADV),NCHER,1,NBNV)
C
C --- CALCUL DES DIFFERENCES DES CONTRAINTES -----------------
C     AUX POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
C                 2 --> JNO
C                 3 --> MNO NOEUD MILIEU S'IL EXISTE300
C
        IMAV = ZI(IVOIS+INO)
        IGREL = ZI(IAREPE+2*(IMAV-1))
        IEL = ZI(IAREPE+2*(IMAV-1)+1)
        IAVAL1= JCELV -1 + ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)
C
        SG11(1) = ZR(IAD+4*INO-4)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*INOV-4)
        SG22(1) = ZR(IAD+4*INO-3)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*INOV-3)
        SG12(1) = ZR(IAD+4*INO-1)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*INOV-1)
C
        SG11(2) = ZR(IAD+4*JNO-4)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*JNOV-4)
        SG22(2) = ZR(IAD+4*JNO-3)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*JNOV-3)
        SG12(2) = ZR(IAD+4*JNO-1)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*JNOV-1)
C
C
C        ON REALISE UNE INTEGRATION DE NEWTON-COTES AVEC 2 OU 3 POINTS
C        D'INTEGRATION SELON LE NOMBRE DE POINTS DE L'ARETE CONSIDEREE
C        SOIT NBNA = 2 OU 3
C
        NORM = JAC(1) *((SG11(1)*XN(1)+SG12(1)*YN(1))**2+
     &       (SG12(1)*XN(1)
     &       + SG22(1)*YN(1))**2) + JAC(2) * ((SG11(2)*XN(2) +
     &       SG12(2)*YN(2))**2 + (SG12(2)*XN(2)+SG22(2)*
     &       YN(2))**2)
C
        IF (NBNA .EQ. 3) THEN
         MNOV = NBSV+JNOV
         SG11(3) = ZR(IAD+4*MNO-4)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*MNOV-4)
         SG22(3) = ZR(IAD+4*MNO-3)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*MNOV-3)
         SG12(3) = ZR(IAD+4*MNO-1)-ZR(IAVAL1+4*NBNV*(IEL-1)+4*MNOV-1)
C
         NORM2 = JAC(3) * ((SG11(3)*XN(3)+SG12(3)*YN(3))**2 +
     &           (SG12(3)*XN(3)+SG22(3)*YN(3))**2)
         TER2 = TER2 +0.5D0*SQRT(HF)*SQRT((1.D0/3.D0)*NORM +
     &           (4.D0/3.D0)*NORM2)
C
C
        ELSE
         TER2 = TER2 +0.5D0*SQRT(HF)*SQRT(NORM)
        ENDIF
      ENDIF
      ENDIF
  300 CONTINUE
C
C------------MISE EN MEMOIRE DES DIFFERENTS TERMES DE IERR -------------
C
      IF (NBNA .EQ. 3) THEN
        COEFF = SQRT(96.D0)
        ELSE
        COEFF = SQRT(24.D0)
      ENDIF
C
      ERREST =( H * SQRT(TER) + TER2 + TER3)/COEFF
      SIGCAL = SQRT(NORSIG)
      NUEST = 100.D0 * SQRT(ERREST**2/(ERREST**2 + NORSIG))
C
C
      ZR(IERR) = ERREST
      ZR(IERR+1) = NUEST
      ZR(IERR+2) = SIGCAL
      CALL JEDEMA()
      END

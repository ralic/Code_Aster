      SUBROUTINE TE0451(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/09/2011   AUTEUR PELLET J.PELLET 
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
C ======================================================================
C  BUT:  CALCUL DE L'OPTION EFGE_ELGA
C        POUR LES ELEMENTS DE COQUE A "SOUS-POINTS"
C        ON PART DE SIEF_ELGA ET ON INTEGRE DANS L'EPAISSEUR
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
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
C --------- FIN  DECLARATIONS NORMALISEES JEVEUX -----------------------

      INTEGER J1,NBCOU,NPGH,JSIGM,IDEC,JEFF,NPG,ITAB(7),IRET
      INTEGER NBSP,KPG,IBID,NBSIG,NBEFF,ICOU,JMATE,ICODRE
      REAL*8 NXX,NYY,MXX,MYY,NXY,MXY,QX,QY
      REAL*8 R8BID,CB,CM,CH,H,HB,HM,HH
      REAL*8 SIYYB,SIYYM,SIYYH,SIXXB,SIXXM,SIXXH,SIXYB,SIXYM,SIXYH
      REAL*8 SIYZB,SIYZM,SIYZH,SIXZB,SIXZM,SIXZH,EPCOU(100),EPI
      CHARACTER*8 ALIAS8,NOMRES
      CHARACTER*3 CMOD,NUM
      CHARACTER*2 VAL
      LOGICAL LCOQMU
C     ------------------------------------------------------------------
      CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
      CMOD=ALIAS8(3:5)
      IF (CMOD.EQ.'DKT' .OR. CMOD.EQ.'DST' .OR. CMOD.EQ.'Q4G' .OR.
     &    CMOD.EQ.'CQ3') THEN
        NBSIG=6
        NBEFF=8
      ELSEIF (CMOD.EQ.'CQA' .OR. CMOD.EQ.'CQC' .OR. CMOD.EQ.'CQD') THEN
        NBSIG=4
        NBEFF=6
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C     -- EPAISSEUR :
      CALL JEVECH('PCACOQU','L',J1)
      H=ZR(J1)

C     -- NOMBRE DE COUCHES :
      CALL JEVECH('PNBSP_I','L',J1)
      NBCOU=ZI(J1)


C     -- SI LE MATERIAU EST 'ELAS_COQMU', LES COUCHES
C        N'ONT PAS LA MEME EPAISSEUR.
C        ON LES STOCKE DANS EPCOU
C     ------------------------------------------------
      LCOQMU=.FALSE.
      CALL JEVECH('PMATERC','L',JMATE)
      CALL CODENT(1,'G',NUM)
      CALL CODENT(1,'G',VAL)
      NOMRES='C'//NUM//'_V'//VAL
      CALL RCVALA(ZI(JMATE),' ','ELAS_COQMU',0,' ',R8BID,1,NOMRES,EPI,
     &            ICODRE,0)
      IF (ICODRE.EQ.0)LCOQMU=.TRUE.
      IF (LCOQMU) THEN
        CALL ASSERT(NBCOU.LE.100)
        DO 10,ICOU=1,NBCOU
          CALL CODENT(ICOU,'G',NUM)
          NOMRES='C'//NUM//'_V'//VAL
          CALL RCVALA(ZI(JMATE),' ','ELAS_COQMU',0,' ',R8BID,1,NOMRES,
     &                EPI,ICODRE,0)
          CALL ASSERT(ICODRE.EQ.0)
          CALL ASSERT(EPI.GE.0.D0)
          EPCOU(ICOU)=EPI
   10   CONTINUE
      ENDIF


C     -- CONTRAINTES DANS LES COUCHES :
C     ----------------------------------
      CALL TECACH('OOO','PSIEFR',7,ITAB,IRET)
      JSIGM=ITAB(1)
      NPG=ITAB(3)
      NBSP=ITAB(7)
      NPGH=3
      CALL ASSERT(NBSP.EQ.NBCOU*NPGH)
      CALL ASSERT(ITAB(2).EQ.NBSIG*NPG)


C     -- CALCUL DES EFFORTS PAR INTEGRATION DANS L'EPAISSEUR :
C     --------------------------------------------------------
      CALL TECACH('OOO','PEFGER',7,ITAB,IRET)
      JEFF=ITAB(1)
      CALL ASSERT(ITAB(2).EQ.NBEFF*NPG)

C     -- BOUCLE SUR LES POINTS DE GAUSS :
      DO 30,KPG=1,NPG
        NXX=0.D0
        NYY=0.D0
        NXY=0.D0
        MXX=0.D0
        MYY=0.D0
        MXY=0.D0
        QX=0.D0
        QY=0.D0

C       -- BOUCLE SUR LES COUCHES :
        HB=-H/2
        DO 20,ICOU=1,NBCOU
          IDEC=((KPG-1)*NBCOU+(ICOU-1))*NPGH*NBSIG

C         -- HB, HM, HH : "HAUTEUR" DES SOUS-POINTS :
          IF (LCOQMU) THEN
            EPI=EPCOU(ICOU)
          ELSE
            EPI=H/NBCOU
          ENDIF
          HM=HB+EPI/2.D0
          HH=HM+EPI/2.D0

C         -- SIXXB, SIYYB, ... : CONTRAINTES AU BAS DE LA COUCHE
          SIXXB=ZR(JSIGM-1+IDEC+1)
          SIYYB=ZR(JSIGM-1+IDEC+2)
          SIXYB=ZR(JSIGM-1+IDEC+4)
          IF (NBSIG.EQ.6) THEN
            SIXZB=ZR(JSIGM-1+IDEC+5)
            SIYZB=ZR(JSIGM-1+IDEC+6)
          ENDIF
C         -- SIXXM, SIYYM, ... : CONTRAINTES AU MILIEU DE LA COUCHE
          SIXXM=ZR(JSIGM-1+IDEC+1+NBSIG)
          SIYYM=ZR(JSIGM-1+IDEC+2+NBSIG)
          SIXYM=ZR(JSIGM-1+IDEC+4+NBSIG)
          IF (NBSIG.EQ.6) THEN
            SIXZM=ZR(JSIGM-1+IDEC+5+NBSIG)
            SIYZM=ZR(JSIGM-1+IDEC+6+NBSIG)
          ENDIF

C         -- SIXXH, SIYYH, ... : CONTRAINTES EN HAUT DE LA COUCHE
          SIXXH=ZR(JSIGM-1+IDEC+1+2*NBSIG)
          SIYYH=ZR(JSIGM-1+IDEC+2+2*NBSIG)
          SIXYH=ZR(JSIGM-1+IDEC+4+2*NBSIG)
          IF (NBSIG.EQ.6) THEN
            SIXZH=ZR(JSIGM-1+IDEC+5+2*NBSIG)
            SIYZH=ZR(JSIGM-1+IDEC+6+2*NBSIG)
          ENDIF

C         -- ON INTEGRE DANS L'EPAISSEUR DE CHAQUE COUCHE
C            AVEC UNE FORRMULE DE NEWTON-COTES A 3 POINTS
C            LES COEFFICIENTS SONT 1/6, 4/6 ET 1/6
          CB=EPI/6
          CM=4.D0*EPI/6
          CH=EPI/6

C         -- NXX, NYY, NXY = SOMME DE SIXX, SIYY, SIXY :
          NXX=NXX+CB*SIXXB+CM*SIXXM+CH*SIXXH
          NYY=NYY+CB*SIYYB+CM*SIYYM+CH*SIYYH
          NXY=NXY+CB*SIXYB+CM*SIXYM+CH*SIXYH

          IF (NBEFF.EQ.8) THEN
C           -- QX, QY = SOMME DE SIXZ, SIYZ
            QX=QX+CB*SIXZB+CM*SIXZM+CH*SIXZH
            QY=QY+CB*SIYZB+CM*SIYZM+CH*SIYZH
          ENDIF

C         -- MXX, MYY, MXY = MOMENTS DE SIXX, SIYY, SIXY :
          MXX=MXX+CB*SIXXB*HB+CM*SIXXM*HM+CH*SIXXH*HH
          MYY=MYY+CB*SIYYB*HB+CM*SIYYM*HM+CH*SIYYH*HH
          MXY=MXY+CB*SIXYB*HB+CM*SIXYM*HM+CH*SIXYH*HH

C         -- MISE A JOUR DE HB POUR LA COUCHE SUIVANTE :
          HB=HB+EPI
   20   CONTINUE

        ZR(JEFF-1+(KPG-1)*NBEFF+1)=NXX
        ZR(JEFF-1+(KPG-1)*NBEFF+2)=NYY
        ZR(JEFF-1+(KPG-1)*NBEFF+4)=MXX
        ZR(JEFF-1+(KPG-1)*NBEFF+5)=MYY
        IF (NBEFF.EQ.8) THEN
          ZR(JEFF-1+(KPG-1)*NBEFF+3)=NXY
          ZR(JEFF-1+(KPG-1)*NBEFF+6)=MXY
          ZR(JEFF-1+(KPG-1)*NBEFF+7)=QX
          ZR(JEFF-1+(KPG-1)*NBEFF+8)=QY
        ENDIF
   30 CONTINUE
      END

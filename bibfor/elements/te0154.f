      SUBROUTINE TE0154(OPTION,NOMTE)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT  NONE
      CHARACTER*(*)     OPTION,NOMTE
C ----------------------------------------------------------------------
C MODIF ELEMENTS  DATE 11/06/2012   AUTEUR DELMAS J.DELMAS 
C     CALCUL
C       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
C       - DU VECTEUR ELEMENTAIRE CONTRAINTE
C       - DE L'ENERGIE DE DEFORMATION
C       - DE L'ENERGIE CINETIQUE
C     POUR LES ELEMENTS DE BARRE
C ----------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'EFGE_ELNO'   : CALCUL DU VECTEUR EFFORT GENERALISE
C        'SIEF_ELGA'   : CALCUL DU VECTEUR EFFORT GENERALISE
C        'EPSI_ELGA'   : CALCUL DU VECTEUR DEFORMATION
C        'EPOT_ELEM'   : CALCUL DE L'ENERGIE DE DEFORMATION
C        'ECIN_ELEM'   : CALCUL DE L'ENERGIE CINETIQUE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_BARRE'   : BARRE
C        'MECA_2D_BARRE'   : BARRE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      REAL*8       PGL(3,3), KLC(6,6), ENERTH
      REAL*8       UGR(6),ULR(6),FLR(6)
      INTEGER CODRES
      CHARACTER*1  STOPZ(3)
      CHARACTER*4  FAMI
      CHARACTER*8  NOMAIL
      CHARACTER*16 CH16
      LOGICAL      LTEIMP
      REAL*8       A,EPSTH,E,R8BID,RHO,XFL1,XFL4,XL,XMAS,XRIG
      INTEGER      I,IF,ITYPE,J,JDEPL,JEFFO,JENDE,JFREQ,JDEFO,KANL
      INTEGER      LMATER,LORIEN,LSECT,IRET,LX,NC,NNO,IADZI,IAZK24
      INTEGER      JVITE
C     ------------------------------------------------------------------

      LTEIMP = .FALSE.
      NNO = 2
      NC  = 3
      FAMI = 'RIGI'
C
      IF ((NOMTE .NE. 'MECA_BARRE').AND.
     &    (NOMTE .NE. 'MECA_2D_BARRE'))  THEN
         CH16 = NOMTE
         CALL U2MESK('F','ELEMENTS2_42',1,CH16)
      ENDIF
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH ('PMATERC', 'L', LMATER)

      CALL VERIFT(FAMI,1,1,'+',ZI(LMATER),'ELAS',1,EPSTH,IRET)

      CALL RCVALB(FAMI,1,1,'+',ZI(LMATER),' ','ELAS',
     &            0,' ',R8BID,1,'E',E,
     &            CODRES, 1)
      IF (EPSTH.NE.0.D0) LTEIMP =.TRUE.
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
C
      IF (NOMTE.EQ.'MECA_BARRE') THEN
C
        CALL LONELE( ZR(LX),3,XL)
C
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        CALL LONELE( ZR(LX),2,XL)
C
      ENDIF
C
      IF( XL .EQ. 0.D0 ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      IF( OPTION .NE. 'EPSI_ELGA' ) THEN
        CALL JEVECH ('PCAGNBA', 'L',LSECT)
        A = ZR(LSECT)
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C     --- MATRICE DE ROTATION PGL
      CALL MATROT ( ZR(LORIEN) , PGL )
C
C     --- RECUPERATION DES DEPLACEMENTS OU DES VITESSES ----
      DO 19 I=1,6
           UGR(I) =  0.D0
 19   CONTINUE
C
      IF ( OPTION .NE. 'ECIN_ELEM' ) THEN
C
C ON RECUPERE DES DEPLACEMENTS
C
        CALL JEVECH ('PDEPLAR', 'L', JDEPL)
        IF (NOMTE.EQ.'MECA_BARRE') THEN
          DO 21 I = 1,6
            UGR(I) = ZR(JDEPL+I-1)
 21       CONTINUE
        ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          UGR(1) =  ZR(JDEPL+1-1)
          UGR(2) =  ZR(JDEPL+2-1)
          UGR(4) =  ZR(JDEPL+3-1)
          UGR(5) =  ZR(JDEPL+4-1)
        ENDIF
C
      ELSE
C
        STOPZ(1)='O'
        STOPZ(2)='N'
        STOPZ(3)='O'
        CALL TECACH(STOPZ,'PVITESR',1,JVITE,IRET)
C IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        IF (IRET.EQ.0) THEN
C
C ON RECUPERE DES VITESSES
C
          IF (NOMTE.EQ.'MECA_BARRE') THEN
            DO 22 I = 1,6
              UGR(I) = ZR(JVITE+I-1)
 22         CONTINUE
          ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
            UGR(1) =  ZR(JVITE+1-1)
            UGR(2) =  ZR(JVITE+2-1)
            UGR(4) =  ZR(JVITE+3-1)
            UGR(5) =  ZR(JVITE+4-1)
          ENDIF
C
        ELSE
C
C ON RECUPERE DES DEPLACEMENTS
C
          CALL TECACH(STOPZ,'PDEPLAR',1,JDEPL,IRET)
          IF (IRET.EQ.0) THEN
            IF (NOMTE.EQ.'MECA_BARRE') THEN
              DO 23 I = 1,6
                UGR(I) = ZR(JDEPL+I-1)
 23           CONTINUE
            ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
              UGR(1) =  ZR(JDEPL+1-1)
              UGR(2) =  ZR(JDEPL+2-1)
              UGR(4) =  ZR(JDEPL+3-1)
              UGR(5) =  ZR(JDEPL+4-1)
            ENDIF
          ELSE
            CALL U2MESK('F','ELEMENTS2_1',1,OPTION)
          ENDIF
C
        ENDIF
C
      ENDIF
C
C     --- VECTEUR DANS REPERE LOCAL  ULR = PGL * UGR
C
      CALL UTPVGL ( NNO, NC, PGL, UGR, ULR )
C
C     --- RIGIDITE ELEMENTAIRE ---
      DO 30 I =1,6
         DO 32 J =1,6
            KLC(I,J) = 0.D0
 32      CONTINUE
 30   CONTINUE
C
C     --- ENERGIE DE DEFORMATION ----
      IF( OPTION .EQ. 'EPOT_ELEM' ) THEN
         CALL JEVECH ('PENERDR', 'E', JENDE)
         XRIG = E * A / XL
         KLC(1,1) =  XRIG
         KLC(1,4) = -XRIG
         KLC(4,1) = -XRIG
         KLC(4,4) =  XRIG
         IF = 0
         CALL PTENPO(6,ULR,KLC,ZR(JENDE),IF,IF)
C
         IF ( LTEIMP ) THEN
           CALL PTENTH(ULR,XL,EPSTH,6,KLC,IF,ENERTH)
           ZR(JENDE) = ZR(JENDE) - ENERTH
         ENDIF
C
      ELSEIF( OPTION .EQ. 'ECIN_ELEM' ) THEN
         CALL RCVALB(FAMI,1,1,'+', ZI(LMATER),' ','ELAS',
     &                 0,' ',R8BID,1,'RHO',RHO,
     &                 CODRES , 1)
         CALL JEVECH ('PENERCR', 'E', JENDE)
         CALL JEVECH ('POMEGA2' , 'L', JFREQ)
         XMAS = RHO * A * XL / 6.D0
         KLC(1,1) =  XMAS * 2.D0
         KLC(2,2) =  XMAS * 2.D0
         KLC(3,3) =  XMAS * 2.D0
         KLC(4,4) =  XMAS * 2.D0
         KLC(5,5) =  XMAS * 2.D0
         KLC(6,6) =  XMAS * 2.D0
         KLC(1,4) =  XMAS
         KLC(4,1) =  XMAS
         KLC(2,5) =  XMAS
         KLC(5,2) =  XMAS
         KLC(3,6) =  XMAS
         KLC(6,3) =  XMAS
         IF = 0
         ITYPE = 50
         KANL = 1
         CALL PTENCI(6,ULR,KLC,ZR(JFREQ),ZR(JENDE),ITYPE,KANL,IF)

C
      ELSEIF( OPTION .EQ. 'EPSI_ELGA' ) THEN
         CALL JEVECH('PDEFOPG','E',JDEFO)
         ZR(JDEFO-1+1)=(ULR(4)-ULR(1))/XL
      ELSE
         XRIG = E * A / XL
         KLC(1,1) =  XRIG
         KLC(1,4) = -XRIG
         KLC(4,1) = -XRIG
         KLC(4,4) =  XRIG

C
C        --- VECTEUR EFFORT LOCAL  FLR = KLC * ULR
         CALL PMAVEC('ZERO',6,KLC,ULR,FLR)
C
C        --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
         IF ( LTEIMP ) THEN
C
C              --- CALCUL DES FORCES INDUITES ---
               XFL1 = -EPSTH * E * A
               XFL4 = -XFL1
               FLR(1) = FLR(1) - XFL1
               FLR(4) = FLR(4) - XFL4
         ENDIF
C
         IF ( OPTION .EQ. 'SIEF_ELGA' ) THEN
            CALL JEVECH('PCONTRR','E',JEFFO)
            ZR(JEFFO  ) = -FLR(1)
C
         ELSEIF (OPTION .EQ. 'EFGE_ELNO') THEN
            CALL JEVECH('PEFFORR','E',JEFFO)
            ZR(JEFFO  ) = -FLR(1)
            ZR(JEFFO+1) = FLR(4)
C
         ELSE
C OPTION NON PROGRAMMEE
            CALL ASSERT(.FALSE.)
         ENDIF
      ENDIF
C
      END

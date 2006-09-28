      SUBROUTINE TE0344(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C INSPI TE0144
C     CALCUL
C       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
C     POUR LES ELEMENTS DE POUTRE DE TIMOSHENKO AVEC GAUCHISSEMENT.
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C       'EFGE_ELNO_DEPL'
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_TG': POUTRE DROITE DE TIMOSHENKO AVEC GAUCHISSEMENT
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      PARAMETER   (       NBRES=3)
      REAL*8       VALRES(NBRES)
      CHARACTER*2  CODRES(NBRES)
      CHARACTER*8  NOMPAR,NOMRES(NBRES)
      CHARACTER*16 CH16
      REAL*8       NU,ULI(14)
      REAL*8       ULR(14), UGR(14), PGL(14,14), KLC(14,14)
      REAL*8       PGL1(3,3), PGL2(3,3)
      REAL*8       FLR(14), KLV(105)
      REAL*8       FE(12), FI(12)
C     ------------------------------------------------------------------
      DATA NOMRES/'E','NU','ALPHA'/
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH('PMATERC','L',LMATER)
C
      NBPAR = 0
      NOMPAR = '  '
      VALPAR = 0.D0
      ZERO   = 0.D0
      ANGS2  = ZERO
      RAD    = ZERO
      DO 10 I = 1,NBRES
          VALRES(I) = ZERO
   10 CONTINUE
C
      DO 11 I = 1,3
      DO 11 J = 1,3
          PGL1(I,J) = ZERO
          PGL2(I,J) = ZERO
   11 CONTINUE
C
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      IF ( ITEMPE .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = ZERO
      ELSE
         NBPAR  = 1
         NOMPAR = 'TEMP'
         VALPAR = 0.5D0*(ZR(ITEMPE) + ZR(ITEMPE+1))
      ENDIF
C
      CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,2,NOMRES,
     &            VALRES, CODRES, 'FM' )
      CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,1,NOMRES(3),
     &            VALRES(3), CODRES(3), '  ' )
      IF ( CODRES(3) .NE. 'OK' )  VALRES(3) = 0.D0
C
      E     = VALRES(1)
      NU    = VALRES(2)
      ALPHA = VALRES(3)
      G = E / ( 2.D0 * ( 1.D0 + NU ) )
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH('PCAGNPO','L',LSECT)
      LSECT = LSECT - 1
      A   = ZR(LSECT+1)
      XIY = ZR(LSECT+2)
      XIZ = ZR(LSECT+3)
      ALFAY = ZR(LSECT+4)
      ALFAZ = ZR(LSECT+5)
      EY  = -ZR(LSECT+6)
      EZ  = -ZR(LSECT+7)
      XJX = ZR(LSECT+8)
      XJG = ZR(LSECT+12)
      ITYPE =  NINT(ZR(LSECT+23))
      NNO = 2
      NC  = 7
      NCC = 6
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT((ZR(LX+4)-ZR(LX+1))**2+ (ZR(LX+5)-ZR(LX+2))**2+
     &     (ZR(LX+6)-ZR(LX+3))**2)
      IF (XL.EQ.0.D0) THEN
          CH16 = ' ?????????'
          CALL U2MESK('F','ELEMENTS2_43',1,CH16(:8))
      END IF
C
C     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL MATROT ( ZR(LORIEN) , PGL )
C
      IF (OPTION.EQ.'EFGE_ELNO_DEPL') THEN
          CALL JEVECH('PEFFORR','E',JEFFO)
      ELSE
          CH16 = OPTION
          CALL U2MESK('F','ELEMENTS3_27',1,CH16)
      ENDIF
C
C     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
      CALL PTKA21(KLV,E,A,XL,XIY,XIZ,XJX,XJG,G,ALFAY,ALFAZ,EY,EZ)
C
C     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
      CALL VECMA(KLV,105,KLC,14)
C
      CALL JEVECH('PDEPLAR','L',JDEPL)
      DO 510 I = 1,14
          UGR(I) = ZR(JDEPL+I-1)
 510  CONTINUE
C      --- VECTEUR DEPLACEMENT LOCAL  ULR = PGL * UGR
      CALL UTPVGL ( NNO, NC, PGL, UGR, ULR )
C     --- VECTEUR EFFORT       LOCAL  FLR = KLC * ULR
      CALL PMAVEC('ZERO',14,KLC,ULR,FLR)
C
C     --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
      IF (ALPHA.NE.0.D0) THEN
          DO 20 I = 1,14
              UGR(I) = 0.D0
   20     CONTINUE
C         - CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
C           TEMPERATURE DE REFERENCE
          CALL JEVECH('PTEREF','L',LTREF)
C
C           TEMPERATURE EFFECTIVE
          CALL JEVECH('PTEMPER','L',LTEMP)
C
          TEMP = 0.5D0*(ZR(LTEMP)+ZR(LTEMP+1)) - ZR(LTREF)
C
          IF (TEMP.NE.0.D0) THEN
              F = ALPHA*TEMP
              UGR(1) = -F*XL
              UGR(8) = -UGR(1)
C
C              --- CALCUL DES FORCES INDUITES ---
              DO 35 I = 1,7
                  FLR(I)   = FLR(I)   - KLC(I,1)*UGR(1)
                  FLR(I+7) = FLR(I+7) - KLC(I+7,1+7)*UGR(1+7)
   35         CONTINUE
          END IF
      END IF
C
C --- PRISE EN COMPTE DES EFFORTS REPARTIS :
C     ------------------------------------
      CALL TECACH('ONN','PFR1D1D',1,LFORCR,IRET)
      IF (LFORCR.NE.0) THEN
         CALL PTFORP(ITYPE,'CHAR_MECA_FR1D1D',NOMTE,A,A,XL,RAD,
     &               ANGS2,1,NNO,NCC,PGL,PGL1,PGL2, FE, FI )
         DO 100 I = 1, 6
            FLR(I)   = FLR(I)   - FE(I)
            FLR(I+7) = FLR(I+7) - FE(I+6)
 100     CONTINUE
      ENDIF
C
C --- PRISE EN COMPTE DES EFFORTS REPARTIS (SOUS FORME DE FONCTION) :
C     -------------------------------------------------------------
      CALL TECACH('ONN','PFF1D1D',1,LFORCF,IRET)
      IF (LFORCF.NE.0) THEN
         CALL PTFORP(ITYPE,'CHAR_MECA_FF1D1D',NOMTE,A,A,XL,RAD,
     &               ANGS2,1,NNO,NCC,PGL,PGL1,PGL2, FE, FI )
         DO 110 I = 1, 6
            FLR(I)   = FLR(I)   - FE(I)
            FLR(I+7) = FLR(I+7) - FE(I+6)
 110     CONTINUE
      ENDIF
C
C     ENDIF
C
C
C       --- ARCHIVAGE ---
C       --- NOTER L INVERSION DU SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C           (CONVENTION ADOPTEE/AL95-205)
      DO 700 I = 1,7
          ZR(JEFFO-1+I)   = - FLR(I)
          ZR(JEFFO-1+I+7) =   FLR(I+7)
  700 CONTINUE
C
      END

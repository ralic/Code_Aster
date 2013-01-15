      SUBROUTINE TE0156 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C-----------------------------------------------------------------------
C REALISE LES OPTIONS :
C     SIEF_ELNO
C                POUR  LES CONTRAINTES DE L'ELEMENT MECA_BARRE
C     FORC_NODA      : FORCES NODALE DE L'ELEMENT MECA_BARRE
C
C ----------------------------------------------------------------------
C IN OPTION    : K16 :  OPTION DE CALCUL
C                       'FORC_NODA' OU  'SIEF_ELNO'
C                       OU 'REFE_FORC_NODA'
C IN NOMTE     : K16 : NOM DU TYPE ELEMENT
C                      'MECA_BARRE'
C                      'MECA_2D_BARRE'

C
C
      INTEGER IVECTU,ICONTG,LORIEN,NNO,NC,INO,I
      INTEGER ICOMPO,IDEPLM,IDEPLP,IGEOM,IRETC
      REAL*8  FS(6),PGL(3,3),VECT(6),FORREF
      REAL*8  W(6),ANG1(3),XD(3)
      LOGICAL REACTU
C
C     ------------------------------------------------------------------
C
      IF ( OPTION .EQ. 'REFE_FORC_NODA' ) THEN
         NNO = 2
         IF (NOMTE .EQ. 'MECA_2D_BARRE') THEN
            NC = 2
         ELSEIF(NOMTE .EQ. 'MECA_BARRE' )THEN
            NC = 3
         ENDIF
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL TEREFE('EFFORT_REFE','MECA_BARRE',FORREF)
         DO 101 INO=1,NNO
            DO 102  I=1,NC
               ZR(IVECTU+(INO-1)*NC+I-1)=FORREF
102         CONTINUE
101      CONTINUE

      ELSEIF ( OPTION .EQ. 'FORC_NODA' ) THEN
         CALL JEVECH ( 'PCONTMR', 'L', ICONTG )
         CALL JEVECH ( 'PCAORIE', 'L', LORIEN )
         CALL TECACH('ONN','PCOMPOR','L',1,ICOMPO,IRETC)
         REACTU = .FALSE.
         IF (IRETC.EQ.0) REACTU = (ZK16(ICOMPO+2).EQ.'PETIT_REAC')

C        PARAMETRES EN SORTIE
         CALL JEVECH('PVECTUR','E',IVECTU)
         NNO=2
         NC=3
         DO 13 I=1,NNO*NC
            FS(I)=0.D0
13       CONTINUE
         FS(1) = -ZR(ICONTG)
         FS(4) =  ZR(ICONTG)


         IF (REACTU) THEN
           CALL JEVECH('PGEOMER','L',IGEOM)
           CALL JEVECH('PDEPLMR','L',IDEPLM)
           CALL JEVECH('PDEPLPR','L',IDEPLP)
           IF (NOMTE.EQ.'MECA_BARRE') THEN
             DO 10 I = 1,3
               W(I) = ZR(IGEOM-1+I) + ZR(IDEPLM-1+I) + ZR(IDEPLP-1+I)
               W(I+3) = ZR(IGEOM+2+I) + ZR(IDEPLM+2+I) + ZR(IDEPLP+2+I)
               XD(I) = W(I+3) - W(I)
   10        CONTINUE
           ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
             W(1) = ZR(IGEOM-1+1) + ZR(IDEPLM-1+1) + ZR(IDEPLP-1+1)
             W(2) = ZR(IGEOM-1+2) + ZR(IDEPLM-1+2) + ZR(IDEPLP-1+2)
             W(3) = 0.D0
             W(4) = ZR(IGEOM-1+3) + ZR(IDEPLM-1+3) + ZR(IDEPLP-1+3)
             W(5) = ZR(IGEOM-1+4) + ZR(IDEPLM-1+4) + ZR(IDEPLP-1+4)
             W(6) = 0.D0
             XD(1) = W(4) - W(1)
             XD(2) = W(5) - W(2)
             XD(3) = 0.D0
           END IF
           CALL ANGVX(XD,ANG1(1),ANG1(2))
           ANG1(3) = ZR(LORIEN+2)
           CALL MATROT(ANG1,PGL)
         ELSE
           CALL MATROT ( ZR(LORIEN) , PGL )
         ENDIF
         CALL UTPVLG ( NNO, NC, PGL, FS, VECT )
C
C        ECRITURE DANS LE VECTEUR VECTU SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
            DO 30 I=1,6
               ZR(IVECTU+I-1)=VECT(I)
30          CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
            ZR(IVECTU)      = VECT(1)
            ZR(IVECTU +1)   = VECT(2)
            ZR(IVECTU +2)   = VECT(4)
            ZR(IVECTU +3)   = VECT(5)
         ENDIF
      ENDIF
      END

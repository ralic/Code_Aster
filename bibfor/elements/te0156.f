      SUBROUTINE TE0156 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/10/2011   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C-----------------------------------------------------------------------
C REALISE LES OPTIONS :
C     EFGE_ELNO (OU SIEF_ELNO): PASSAGE DU POINT DE GAUSS AUX NOEUD
C                POUR  LES CONTRAINTES DE L'ELEMENT MECA_BARRE
C     FORC_NODA      : FORCES NODALE DE L'ELEMENT MECA_BARRE
C
C ----------------------------------------------------------------------
C IN OPTION    : K16 :  OPTION DE CALCUL
C                       'EFGE_ELNO' OU 'FORC_NODA' OU  'SIEF_ELNO'
C                       OU 'REFE_FORC_NODA'
C IN NOMTE     : K16 : NOM DU TYPE ELEMENT
C                      'MECA_BARRE'
C                      'MECA_2D_BARRE'

C
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER IVECTU,ICONTG,LORIEN,NNO,NC,IREFCO,INO,I
      REAL*8  FS(6),PGL(3,3),VECT(6)
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
         CALL JEVECH('PREFCO', 'L',IREFCO)
         CALL JEVECH('PVECTUR','E',IVECTU)
         DO 101 INO=1,NNO
            DO 102  I=1,NC
               ZR(IVECTU+(INO-1)*NC+I-1)=ZR(IREFCO)
102         CONTINUE
101      CONTINUE

      ELSEIF (OPTION.EQ.'EFGE_ELNO'.OR.OPTION.EQ.'SIEF_ELNO') THEN
         IF (OPTION.EQ.'EFGE_ELNO') THEN
            CALL JEVECH('PEFFORR','E',IVECTU)
         ELSE
            CALL JEVECH('PSIEFNOR','E',IVECTU)
         ENDIF
         CALL JEVECH('PCONTRR', 'L',ICONTG)
         ZR(IVECTU)   = ZR(ICONTG)
         ZR(IVECTU+1) = ZR(ICONTG)
C
      ELSEIF ( OPTION .EQ. 'FORC_NODA' ) THEN
         CALL JEVECH ( 'PCONTMR', 'L', ICONTG )
         CALL JEVECH ( 'PCAORIE', 'L', LORIEN )
C        PARAMETRES EN SORTIE
         CALL JEVECH('PVECTUR','E',IVECTU)
         NNO=2
         NC=3
         DO 13 I=1,NNO*NC
            FS(I)=0.D0
13       CONTINUE
         FS(1) = -ZR(ICONTG)
         FS(4) =  ZR(ICONTG)
         CALL MATROT ( ZR(LORIEN) , PGL )
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

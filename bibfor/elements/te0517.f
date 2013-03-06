      SUBROUTINE TE0517(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/03/2013   AUTEUR CHEIGNON E.CHEIGNON 
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
C
C ======================================================================
C     CALCUL DE L'OPTION FORC_NODA POUR LES ELEMENTS :
C
C                         POU_D_EM (MULTI-FIBRES)
C                         POU_D_TGM (MULTI-FIBRES)
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C
      INTEGER NC,NNO
      INTEGER CODRES(2)
      CHARACTER*2 NOMRES(2)

      REAL*8 PGL(3,3),FL(14),XIY,XIZ
      REAL*8 NX,TY,TZ,MX,MY,MZ

      INTEGER NBFIB,KP,NCOMP,I,JACF
      INTEGER ICOMPO,IORIEN,IVECTU
      INTEGER JTAB(7),INO,ISTRXM,NBSP
C
      INTEGER IGEOM,IRET,ISECT,IMATE,K,NPG,IFGM,IRETC
      REAL*8  XD(3),DDOT,EY,EZ,TEMP
      REAL*8  XL,XL2,GAMMA,VALRES(2)
      REAL*8  XLS2,D1B(7,14),CO(3),AA,E,NU,G,ALFAY,ALFAZ,PHIY,PHIZ
      REAL*8  FORREF,MOMREF
      LOGICAL REACTU

C ----------------------------------------------------------------------
      NNO = 2

C     NOMBRE DE COMPOSANTES DES CHAMPS PSTRX? PAR POINTS DE GAUSS
      NCOMP  = 18
      IF(NOMTE.EQ.'MECA_POU_D_EM')THEN
        NC  = 6
      ELSEIF(NOMTE.EQ.'MECA_POU_D_TGM')THEN
        NC  = 7
        NPG = 3
      ENDIF

      IF ( OPTION .EQ. 'REFE_FORC_NODA  ' ) THEN

         CALL JEVECH ( 'PVECTUR','E',IVECTU)

         CALL TEREFE('EFFORT_REFE','MECA_POUTRE',FORREF)
         CALL TEREFE('MOMENT_REFE','MECA_POUTRE',MOMREF)

         DO 501 INO=1,NNO
            DO 503  I=1,3
               ZR(IVECTU+(INO-1)*NC+I-1)=FORREF
503         CONTINUE
            DO 502 I=4,NC
               ZR(IVECTU+(INO-1)*NC+I-1)=MOMREF
502         CONTINUE
501      CONTINUE
      ELSEIF ( OPTION .EQ. 'FORC_NODA') THEN

C       --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
         CALL JEVECH('PNBSP_I','L',I)
         NBFIB = ZI(I)
         CALL JEVECH('PFIBRES','L',JACF)

         CALL JEVECH('PCAORIE','L',IORIEN)
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL TECACH('OON','PCONTMR','L',7,JTAB,IRET)
         NBSP=JTAB(7)
         IF (NBSP.NE.NBFIB) CALL U2MESS('F','ELEMENTS_4')
         CALL JEVECH ( 'PSTRXMR','L',ISTRXM)


         REACTU = .FALSE.
         IF (NOMTE.EQ.'MECA_POU_D_TGM')THEN
           CALL TECACH('ONN','PCOMPOR','L',1,ICOMPO,IRETC)
           IF (IRETC.EQ.0) REACTU = (ZK16(ICOMPO+2).EQ.'GROT_GDEP')
         ENDIF

         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL R8INIR(2*NC,0.D0,FL,1)

C        CALCUL DE LA MATRICE DE PASSAGE GLOBAL/LOCAL
         IF ( REACTU) THEN
           GAMMA = ZR(ISTRXM+18-1)
           CALL POREA2(NNO,NC,ZR(IGEOM),GAMMA,PGL,XL)

         ELSE
           CALL VDIFF(3,ZR(IGEOM-1+4),ZR(IGEOM),XD)
           XL2=DDOT(3,XD,1,XD,1)
           XL = SQRT(XL2)
           CALL MATROT ( ZR(IORIEN) , PGL )

         END IF

         IF(NOMTE.EQ.'MECA_POU_D_EM') THEN

            XLS2 = XL/2.D0

            NX=ZR(ISTRXM-1+1)
            TY=ZR(ISTRXM-1+2)
            TZ=ZR(ISTRXM-1+3)
            MX=ZR(ISTRXM-1+4)
            MY=ZR(ISTRXM-1+5)
            MZ=ZR(ISTRXM-1+6)

C ---       ET ENFIN LE VECTEUR NODAL

            FL(7) = NX
            FL(8) = TY
            FL(9) = TZ
            FL(10) = MX
            DO 10 I = 1,4
               FL(I) = -FL(I+6)
10          CONTINUE
            FL(5) = -MY + TZ*XLS2
            FL(6) = -MZ - TY*XLS2
            FL(11) = MY + TZ*XLS2
            FL(12) = MZ - TY*XLS2
C
         ELSEIF (NOMTE.EQ.'MECA_POU_D_TGM') THEN

            CALL JEVECH('PCAGNPO','L',ISECT)

C           -- CARACTERISTIQUES DE LA SECTION
            AA    = ZR(ISECT)
            XIY   = ZR(ISECT + 1)
            XIZ   = ZR(ISECT + 2)
            ALFAY = ZR(ISECT + 3)
            ALFAZ = ZR(ISECT + 4)
C           -- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
            EY = -ZR(ISECT + 5)
            EZ = -ZR(ISECT + 6)

            CALL JEVECH('PMATERC','L',IMATE)
            CALL MOYTEM('RIGI',NPG,1,'+',TEMP,IRET)
            NOMRES(1) = 'E'
            NOMRES(2) = 'NU'
            CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',
     &              0,'TEMP',TEMP,2,
     &              NOMRES, VALRES, CODRES, 1)
            E = VALRES(1)
            NU = VALRES(2)
            G = E / (2.D0*(1.D0+NU))
            PHIY = E*XIZ*12.D0*ALFAY/ (XL*XL*G*AA)
            PHIZ = E*XIY*12.D0*ALFAZ/ (XL*XL*G*AA)
            XLS2 = 0.5D0 * XL
C           POIDS DES POINTS DE GAUSS
            CO(1) = 5.D0/9.D0
            CO(2) = 8.D0/9.D0
            CO(3) = 5.D0/9.D0

            DO 400 KP = 1,3
               CALL JSD1FF(KP,XL,PHIY,PHIZ,D1B)
               IFGM=NCOMP*(KP-1)-1
               DO 410 K = 1,2*NC
                  DO 420 I = 1,NC
                   FL(K)=FL(K) + XLS2*ZR(ISTRXM+IFGM+I)*D1B(I,K)*CO(KP)
420               CONTINUE
410            CONTINUE
400         CONTINUE
            DO 430 I = 1,2
               FL(7*(I-1)+4) = FL(7*(I-1)+4) -
     &                      EZ*FL(7*(I-1)+2) + EY*FL(7*(I-1)+3)
430         CONTINUE
         ELSE
           CALL ASSERT(.FALSE.)
         ENDIF

C        PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
         CALL UTPVLG(NNO,NC,PGL,FL,ZR(IVECTU))
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      END

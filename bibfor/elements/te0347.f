      SUBROUTINE TE0347(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/11/2012   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C --- ------------------------------------------------------------------
C
C     ELEMENTS :
C        POU_D_TG
C        POU_D_T
C        POU_D_E
C
C     CALCUL DES OPTIONS :
C        SIEF_ELNO
C        FORC_NODA
C        REFE_FORC_NODA
C        VARI_ELNO
C
C     POUR LES CONTRAINTES ET LES FORC_NODA
C       RECOPIE DES POINTS 1 ET 2 SI NPG=2
C       RECOPIE DES POINTS 1 ET 3 SI NPG=3
C     QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C
C --- ------------------------------------------------------------------
      INTEGER     JTAB(7),NNO,NC,ICHG,ICOMPO,ICHN,LGPG,NBVAR,I,K,NPG
      INTEGER     LORIEN,ICGP,ICONTN,ICONTG,IVECTU,IN,IRET,IPLOUF
      INTEGER     ISECT,IGEOM,KP,KK,IMATE
C
      LOGICAL     LEFGNO,OKELEM
C
      REAL*8      PGL(3,3),FS(14),D1B3(2,3),KSI1,FORREF,MOMREF
      REAL*8      SIGP(7),D1B(7,14),XL,CO(3),XD(3),EY,EZ,XL2,TEMP
      REAL*8      VALRES(2),E,NU,G,DDOT,AA,XIY,XIZ,ALFAY,ALFAZ
      REAL*8      PHIY,PHIZ
C
      CHARACTER*2 NOMRES(2)
      CHARACTER*4 FAMI
      CHARACTER*8 PEFFOR
C --- ------------------------------------------------------------------
C
      OKELEM = (NOMTE.EQ.'MECA_POU_D_TG') .OR.
     &         (NOMTE.EQ.'MECA_POU_D_T') .OR.
     &         (NOMTE.EQ.'MECA_POU_D_E')
      CALL ASSERT( OKELEM )
C
      NNO   = 2
      FAMI  = 'RIGI'
C --- NOMBRE DE POINTS DE GAUSS
      CALL ELREF4(' ',FAMI,IPLOUF,IPLOUF,IPLOUF,
     &            NPG,IPLOUF,IPLOUF,IPLOUF,IPLOUF)
      CALL ASSERT( (NPG.EQ.2).OR.(NPG.EQ.3) )
C
      IF (NOMTE .EQ. 'MECA_POU_D_TG') THEN
         NC  = 7
      ELSE
         NC  = 6
      ENDIF

      LEFGNO=(OPTION.EQ.'SIEF_ELNO')
      IF (LEFGNO) PEFFOR='PSIEFNOR'
C
C --- ------------------------------------------------------------------
      IF ( OPTION .EQ. 'REFE_FORC_NODA  ' ) THEN
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL TEREFE('EFFORT_REFE','MECA_POUTRE',FORREF)
         CALL TEREFE('MOMENT_REFE','MECA_POUTRE',MOMREF)
         DO 301 IN=1,NNO
            DO 305  I=1,3
               ZR(IVECTU+(IN-1)*NC+I-1)=FORREF
305         CONTINUE
            DO 302 I=4,NC
               ZR(IVECTU+(IN-1)*NC+I-1)=MOMREF
302         CONTINUE
301      CONTINUE
C
C --- ------------------------------------------------------------------
      ELSEIF ( OPTION .EQ. 'VARI_ELNO  ' ) THEN
         CALL JEVECH ( 'PVARIGR', 'L', ICHG   )
         CALL JEVECH ( 'PCOMPOR', 'L', ICOMPO )
         CALL JEVECH ( 'PVARINR', 'E', ICHN   )

         CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
         LGPG = MAX(JTAB(6),1)*JTAB(7)
         READ (ZK16(ICOMPO+1),'(I16)') NBVAR
C        POUR LES VARIABLES INTERNES, ON PROJETTE AVEC LES FONCTIONS
C        DE FORME SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
C        POUR LE POINT 1
         KSI1 = -SQRT( 5.D0 / 3.D0 )
         D1B3(1,1) = KSI1*(KSI1-1.D0)/2.0D0
         D1B3(1,2) = 1.D0-KSI1*KSI1
         D1B3(1,3) = KSI1*(KSI1+1.D0)/2.0D0
C        POUR LE POINT 2
         KSI1 = SQRT( 5.D0 / 3.D0 )
         D1B3(2,1) = KSI1*(KSI1-1.D0)/2.0D0
         D1B3(2,2) = 1.D0-KSI1*KSI1
         D1B3(2,3) = KSI1*(KSI1+1.D0)/2.0D0
         DO 11 I = 1,NBVAR
           DO 12 K = 1 , 3
             ZR(ICHN     +I-1) = ZR(ICHN     +I-1) +
     &                           ZR(ICHG + LGPG*(K-1) + I-1)*D1B3(1,K)
             ZR(ICHN+LGPG+I-1) = ZR(ICHN+LGPG+I-1) +
     &                           ZR(ICHG + LGPG*(K-1) + I-1)*D1B3(2,K)
12         CONTINUE
11       CONTINUE

C
C --- ------------------------------------------------------------------
      ELSEIF ( LEFGNO .OR. OPTION.EQ.'FORC_NODA' ) THEN
C        RECOPIE DES VALEURS AU POINT GAUSS 1 ET [2|3]
C        QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
C           NPG=2 : RECOPIE DES POINTS 1 ET 2
C           NPG=3 : RECOPIE DES POINTS 1 ET 3
         IF (LEFGNO) THEN
            CALL JEVECH('PCONTRR' , 'L', ICGP   )
            CALL JEVECH(PEFFOR, 'E', ICONTN )
            IF ( NPG .EQ. 2 ) THEN
               DO 100 I = 1 , NC
                  ZR(ICONTN-1+I)    = ZR(ICGP-1+I)
                  ZR(ICONTN-1+I+NC) = ZR(ICGP-1+I+NC)
100            CONTINUE
            ELSE
               DO 110 I = 1 , NC
                  ZR(ICONTN-1+I)    = ZR(ICGP-1+I)
                  ZR(ICONTN-1+I+NC) = ZR(ICGP-1+I+NC+NC)
110            CONTINUE
            ENDIF
         ELSE IF ( OPTION .EQ. 'FORC_NODA' ) THEN
            CALL JEVECH('PCONTMR','L',ICONTG)
            CALL JEVECH('PCAORIE','L',LORIEN)
            CALL JEVECH('PVECTUR','E',IVECTU)

            IF (NOMTE .EQ. 'MECA_POU_D_TG') THEN
               CALL JEVECH('PGEOMER','L',IGEOM)
               CALL JEVECH('PCAGNPO','L',ISECT)
               CALL JEVECH('PMATERC','L',IMATE)
               CALL VDIFF(3,ZR(IGEOM-1+4),ZR(IGEOM),XD)
               XL2=DDOT(3,XD,1,XD,1)
               XL   = SQRT(XL2)

               CALL R8INIR(2*NC,0.D0,FS,1)
               CO(1) = 5.D0/9.D0
               CO(2) = 8.D0/9.D0
               CO(3) = 5.D0/9.D0

C              THERMIQUE A T+
               CALL MOYTEM(FAMI,NPG,1,'+',TEMP,IRET)
               NOMRES(1) = 'E'
               NOMRES(2) = 'NU'
               CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ','ELAS',1,'TEMP',
     &               TEMP,2,NOMRES, VALRES, IRET, 1)
               E  = VALRES(1)
               NU = VALRES(2)
               G  = E / (2.D0*(1.D0+NU))

               AA     = ZR(ISECT-1+1)
               XIY    = ZR(ISECT-1+2)
               XIZ    = ZR(ISECT-1+3)
               ALFAY = ZR(ISECT-1+4)
               ALFAZ = ZR(ISECT-1+5)
               PHIY = E*XIZ*12.D0*ALFAY/ (XL*XL*G*AA)
               PHIZ = E*XIY*12.D0*ALFAZ/ (XL*XL*G*AA)

               DO 400 KP = 1, NPG
                  CALL JSD1FF(KP,XL,PHIY,PHIZ,D1B)
                  DO 405 I = 1 ,NC
                     SIGP(I) = ZR(ICONTG-1+NC*(KP-1)+I)
405               CONTINUE

                  DO 410 K = 1,2*NC
                     DO 420 KK = 1,NC
                        FS(K)=FS(K)+XL*SIGP(KK)*D1B(KK,K)*CO(KP)*0.50D0
420                  CONTINUE
410               CONTINUE
400            CONTINUE
C              PRENDRE EN COMPTE CENTRE DE TORSION
               EY = -ZR(ISECT-1+6)
               EZ = -ZR(ISECT-1+7)
               FS( 4)=FS( 4)-EZ*FS(2)+EY*FS( 3)
               FS(11)=FS(11)-EZ*FS(9)+EY*FS(10)
            ELSE
               IF ( NPG .EQ. 2 ) THEN
                  DO 222 IN = 1,NC
                     FS(IN)    = -ZR(ICONTG+IN-1)
                     FS(IN+NC) =  ZR(ICONTG+IN+NC-1)
222               CONTINUE
               ELSE
                  DO 225 IN = 1,NC
                     FS(IN)    = -ZR(ICONTG+IN-1)
                     FS(IN+NC) =  ZR(ICONTG+IN+NC+NC-1)
225               CONTINUE
               ENDIF
            ENDIF

            CALL MATROT ( ZR(LORIEN) , PGL )
            CALL UTPVLG ( NNO, NC, PGL, FS, ZR(IVECTU) )
         ENDIF
C
      ENDIF
      END

      SUBROUTINE TE0140(OPTION,NOMTE)
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
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
C
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C     ------------------------------------------------------------------
C     CALCULE LA MATRICE DE RIGIDITE ELEMENTAIRE DES ELEMENTS DE POUTRE
C     D'EULER ET DE TIMOSHENKO
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C      'RIGI_MECA '     : CALCUL DE LA MATRICE DE RIGIDITE
C      'RIGI_FLUI_STRU' : CALCUL DE LA MATRICE DE RIGIDITE (ABS_CURV)
C      'RIGI_MECA_SENSI': CALCUL DU VECTEUR -DK/DP*U
C         POUR L'INSTANT : EULER UNIQUEMENT
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C      'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C      'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C      'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C      'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C      'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C                         MULTI-FIBRES SECTION CONSTANTE
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER LVECT,LVAPR,NDDL,NL,IADZI,IAZK24
      INTEGER I, IMATE, J, LMAT, LORIEN, LRCOU
      INTEGER LX, NBPAR, NBRES, NC, NNO, LSECT,IRET
      PARAMETER (NDDL=12,NL=NDDL*(NDDL+1)/2)
      PARAMETER (NBRES=2)
      REAL*8 VALRES(NBRES),VALTOR
      REAL*8 ANGARC, ANGS2, DEUX , E, RAD, TRIGOM
      REAL*8 VALPAR, XL, XNU, G, UN, ZERO
      REAL*8 R8PREM
      CHARACTER*2 CODRES(NBRES),DERIVE
      CHARACTER*4 FAMI
      CHARACTER*8 NOMPAR,NOMRES(NBRES),NOMAIL
      CHARACTER*16 OPTI, CH16
      REAL*8 PGL(3,3),PGL1(3,3),PGL2(3,3),KLV(78),KGV(NL),WK(NDDL,NDDL)
      REAL*8       A,XIY,XIZ,ALFAY,ALFAZ,XJX,XJG,EZ,EY,MAT(105)
C     ------------------------------------------------------------------
      DATA NOMRES/'E','NU'/
C     ------------------------------------------------------------------
      ZERO = 0.D0
      UN   = 1.D0
      DEUX = 2.D0
      DERIVE = '  '
C     ------------------------------------------------------------------

C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---

      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
         OPTI = 'ELAS'
      ELSE IF (OPTION(1:14).EQ.'RIGI_FLUI_STRU') THEN
         OPTI = 'ELAS_FLUI'
      ELSE
C OPTION NON PROGRAMMEE
         CALL ASSERT(.FALSE.)
      END IF
      IF ( NOMTE.NE.'MECA_POU_D_TG' .AND.
     &     NOMTE.NE.'MECA_POU_D_TGM') THEN
         CALL MOYTEM('NOEU',2,1,'+',VALPAR,IRET)
      ELSE
         CALL MOYTEM('RIGI',3,1,'+',VALPAR,IRET)
      ENDIF
      NBPAR = 1
      NOMPAR = 'TEMP'

      IF (OPTION(11:14).EQ.'SENS') THEN
C ON SE LIMITE POUR L'INSTANT AUX : POU_D_E
         IF (NOMTE.NE.'MECA_POU_D_E') THEN
            CALL U2MESS ('F','SENSIBILITE_52')
         ENDIF
         CALL JEVECH('PMATSEN','L',IMATE)
         CALL RCVALA(ZI(IMATE),' ',OPTI,NBPAR,NOMPAR,VALPAR,NBRES,
     &               NOMRES,VALRES,CODRES,'FM')
         E = VALRES(1)
         XNU = VALRES(2)
C A CE NIVEAU : LA PROCEDURE HABITUELLE DE CALCUL DE SENSIBILITE DONNE :
C   SI : DERIVATION PAR RAPPORT A E ALORS : E = 1 ET XNU = 0
C   SI : DERIVATION PAR RAPPORT A NU ALORS : E = 0 ET XNU = 1
C ICI, LA FORMULATION DE LA DERIVEE EST PLUS COMPLEXE
         CALL JEVECH('PMATERC','L',IMATE)
         CALL RCVALA(ZI(IMATE),' ',OPTI,NBPAR,NOMPAR,VALPAR,NBRES,
     &               NOMRES,VALRES,CODRES,'FM')
         IF(ABS(XNU).LT.R8PREM()) THEN
            DERIVE = 'E'
            XNU = VALRES(2)
         ELSE IF(ABS(E).LT.R8PREM()) THEN
            DERIVE = 'NU'
            E = VALRES(1)
            XNU = VALRES(2)
C ET ON NE CONSIDERE QUE LES DDL DE TORSION (VOIR PLUS BAS)
         END IF

      ELSE
         CALL JEVECH('PMATERC','L',IMATE)
         IF(NOMTE(1:13).NE.'MECA_POU_D_EM')THEN
            CALL RCVALA(ZI(IMATE),' ',OPTI,NBPAR,NOMPAR,VALPAR,NBRES,
     &                  NOMRES,VALRES,CODRES,'FM')
            E = VALRES(1)
            XNU = VALRES(2)
            G = E/ (DEUX* (UN+XNU))
         ENDIF
      END IF

C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH('PCAORIE','L',LORIEN)

C     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
      IF (NOMTE(1:13).EQ.'MECA_POU_D_EM') THEN
         CALL PMFRIG(NOMTE,ZI(IMATE),KLV)
      ELSEIF (NOMTE.NE.'MECA_POU_D_TG' .AND.
     &        NOMTE.NE.'MECA_POU_D_TGM') THEN
         CALL PORIGI(NOMTE,E,XNU,KLV)
      END IF

      IF (OPTION(11:14).EQ.'SENS') THEN
         IF (OPTION(15:16).EQ.'_C') THEN
            CALL JEVECH('PVECTUC','E',LVECT)
         ELSE
            CALL JEVECH('PVECTUR','E',LVECT)
         END IF
         CALL JEVECH('PVAPRIN','L',LVAPR)
      ELSE
         CALL JEVECH('PMATUUR','E',LMAT)
      END IF

      IF (NOMTE.EQ.'MECA_POU_D_EM' .OR.
     &    NOMTE.EQ.'MECA_POU_D_E' .OR.
     &    NOMTE.EQ.'MECA_POU_D_T') THEN
         NNO = 2
         NC = 6
         CALL MATROT(ZR(LORIEN),PGL)
         IF (OPTION(11:14).EQ.'SENS') THEN
            IF(DERIVE(1:2).EQ.'NU') THEN
C VALEUR NULLE SAUF POUR LES DDL DE TORSION
               VALTOR = -KLV(10)/(1.D0 + XNU)
               DO 300 I = 1,NL
                  KLV(I) = 0.D0
300            CONTINUE
               KLV(10) = VALTOR
               KLV(49) = -VALTOR
               KLV(55) = VALTOR
            ENDIF
            CALL UTPSLG(NNO,NC,PGL,KLV,KGV)
            CALL VECMA(KGV,NL,WK,NDDL)
            DO 100 I = 1,NDDL
               IF (OPTION(15:16).EQ.'_C') THEN
                  ZC(LVECT-1+I) = DCMPLX(0.D0,0.D0)
               ELSE
                  ZR(LVECT-1+I) = 0.D0
               END IF
               DO 110 J = 1,NDDL
                  IF (OPTION(15:16).EQ.'_C') THEN
                     ZC(LVECT-1+I) = ZC(LVECT-1+I)
     &                           - WK(I,J)*ZC(LVAPR-1+J)
                  ELSE
                     ZR(LVECT-1+I) = ZR(LVECT-1+I)
     &                           - WK(I,J)*ZR(LVAPR-1+J)
                  END IF
110            CONTINUE
100         CONTINUE
         ELSE
            CALL UTPSLG(NNO,NC,PGL,KLV,ZR(LMAT))
         END IF

      ELSE IF (NOMTE(1:12).EQ.'MECA_POU_C_T') THEN
         CALL JEVECH('PGEOMER','L',LX)
         LX = LX - 1
         XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &              (ZR(LX+5)-ZR(LX+2))**2 +
     &              (ZR(LX+6)-ZR(LX+3))**2 )
         CALL JEVECH('PCAARPO','L',LRCOU)
         RAD = ZR(LRCOU)
         ANGARC = ZR(LRCOU+1)
         ANGS2 = TRIGOM('ASIN',XL/ (DEUX*RAD))
         CALL MATRO2(ZR(LORIEN),ANGARC,ANGS2,PGL1,PGL2)
         CALL CHGREP('LG',PGL1,PGL2,KLV,ZR(LMAT))

      ELSEIF (NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &        NOMTE.EQ.'MECA_POU_D_TGM') THEN
C     --- CARACTERISTIQUES GENERALES DES SECTIONS ---
         CALL JEVECH('PCAGNPO','L',LSECT)
         LSECT = LSECT - 1
         A = ZR(LSECT+1)
         XIY = ZR(LSECT+2)
         XIZ = ZR(LSECT+3)
         ALFAY = ZR(LSECT+4)
         ALFAZ = ZR(LSECT+5)
         EY = -ZR(LSECT+6)
         EZ = -ZR(LSECT+7)
         XJX = ZR(LSECT+8)
         XJG = ZR(LSECT+12)
         NNO = 2
         NC  = 7
C     --- COORDONNEES DES NOEUDS ---
         CALL JEVECH('PGEOMER','L',LX)
         LX = LX - 1
         XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &              (ZR(LX+5)-ZR(LX+2))**2 +
     &              (ZR(LX+6)-ZR(LX+3))**2 )
         IF (XL.EQ.ZERO) THEN
           CALL TECAEL(IADZI,IAZK24)
           NOMAIL = ZK24(IAZK24-1+3)(1:8)
           CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
         ENDIF
         DO 20 I = 1 , 105
            MAT(I) = 0.D0
20       CONTINUE
C     --- CALCUL DES MATRICES ELEMENTAIRES ----
         IF ( OPTION(1:9) .EQ. 'RIGI_MECA' .OR.
     &        OPTION(1:9) .EQ. 'RIGI_FLUI') THEN
            CALL PTKA21(MAT,E,A,XL,XIY,XIZ,XJX,XJG,G,ALFAY,
     &                  ALFAZ,EY,EZ)
         ENDIF
C        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
         CALL MATROT ( ZR(LORIEN) , PGL )
         CALL JEVECH ( 'PMATUUR', 'E', LMAT )
         CALL UTPSLG ( NNO, NC, PGL, MAT, ZR(LMAT) )
      ELSE
         CALL U2MESK('F','ELEMENTS2_42',1,NOMTE)
      END IF

      END

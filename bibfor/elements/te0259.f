      SUBROUTINE TE0259(OPTION,NOMTE)
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/03/2008   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      CHARACTER*16 OPTION,NOMTE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C       'GYRO_MECA': CALCUL DE LA MATRICE D'AMORTISSEMENT GYROSCOPIQUE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C       'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C       'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C       'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)

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

      INTEGER LVECT,LVAPR,NDDL,NL,NBRES
      PARAMETER (NDDL=12,NL=(NDDL+1)*NDDL/2)
      PARAMETER (NBRES=3)
      REAL*8 VALRES(NBRES),VALTOR
      CHARACTER*2 CODRES(NBRES)
      CHARACTER*8 NOMPAR,NOMRES(NBRES)
      CHARACTER*16 CH16,OPTI
      REAL*8 PGL(3,3),PGL1(3,3),PGL2(3,3),KLV(NL),KLW(NL)
      REAL*8 E, RHO
      REAL*8 ANGARC, ANGS2, DEUX, RAD, TRIGOM, VALPAR, XL, XNU, ZERO
      INTEGER IMATE, LMAT, LORIEN, LRCOU, LSECT, LX
      INTEGER NBPAR, NC, NNO
C     ------------------------------------------------------------------
      DATA NOMRES/'E','RHO','NU'/
C     ------------------------------------------------------------------
      ZERO = 0.D0
      DEUX = 2.D0
C     ------------------------------------------------------------------

C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH('PMATERC','L',IMATE)
      

      NBPAR = 0
      NOMPAR = ' '
      VALPAR = ZERO
      CALL RCVALA(ZI(IMATE),' ','ELAS',NBPAR,NOMPAR,VALPAR,NBRES,
     &            NOMRES,VALRES,CODRES,'FM')
      E = VALRES(1)
      RHO = VALRES(2)
      XNU = VALRES(3)


      CALL JEVECH('PCAGNPO','L',LSECT)

C     --- RECUPERATION DES ORIENTATIONS ---

      CALL JEVECH('PCAORIE','L',LORIEN)

C     --- CALCUL DE LA MATRICE GYROSCOPIQUE LOCALE ---

      CALL POGYRO(NOMTE,E,RHO,XNU,KLV,NL)
             
      CALL JEVECH('PMATUNS','E',LMAT)


      IF (NOMTE(1:12).EQ.'MECA_POU_D_E' .OR.
     &    NOMTE(1:12).EQ.'MECA_POU_D_T') THEN
        NNO = 2
        NC = 6

        CALL MATROT(ZR(LORIEN),PGL)

C     CHANGEMENT DE BASE : LOCAL -> GLOBAL 
        CALL UTPALG (NNO,NC,PGL,KLV,KLW)

C CONSITUER UNE MATRICE PLEINE A PARTIR DE LA TRIANGULAIRE SUPERIEURE
        CALL UPLETR(NDDL,ZR(LMAT),KLW)
       
      ELSE IF (NOMTE(1:12).EQ.'MECA_POU_C_T') THEN
        CALL JEVECH('PGEOMER','L',LX)
        LX = LX - 1
        XL = SQRT((ZR(LX+4)-ZR(LX+1))**2+ (ZR(LX+5)-ZR(LX+2))**2+
     &       (ZR(LX+6)-ZR(LX+3))**2)
        CALL JEVECH('PCAARPO','L',LRCOU)
        RAD = ZR(LRCOU)
        ANGARC = ZR(LRCOU+1)
        ANGS2 = TRIGOM('ASIN',XL/ (DEUX*RAD))
        CALL MATRO2(ZR(LORIEN),ANGARC,ANGS2,PGL1,PGL2)
        CALL CHGREP('LG',PGL1,PGL2,KLV,ZR(LMAT))

      ELSE
        CALL U2MESK('F','ELEMENTS2_42',1,NOMTE)
      END IF

      END

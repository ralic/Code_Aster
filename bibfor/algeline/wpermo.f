      SUBROUTINE WPERMO(LMASSE,LRAIDE,LAMOR,NBPROP,VECP,FR,AM,EXCL,
     +                                                  OMECOR,ERNORM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMASSE,LRAIDE,LAMOR,NBPROP,EXCL(*)
      COMPLEX*16        VECP(*)
      REAL*8            FR(*),AM(*),OMECOR,ERNORM(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     CALCUL DE LA NORME D'ERREUR MODALE
C     ( IE NORME D'ERREUR SUR LES VALEURS ET VECTEURS PROPRES.)
C     ------------------------------------------------------------------
C     PROBLEME QUADRATIQUE:
C
C                   !! LRAIDE * VECP  - VALP * LMASSE * VECP !!
C       ERNORM   =     -------------------------------------
C                           !! LRAIDE * VECP !!
C     ------------------------------------------------------------------
C     REFERENCE:
C     ------------------------------------------------------------------
C IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE"
C IN  LRAIDE : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
C IN  LAMOR  : IS : DESCRIPTEUR MATRICE D'AMORTISSEMENT
C IN  NBPROP : IS : NOMBRE DE VALEURS ET DE VECTEURS PROPRES
C IN  VECP   : R8 : TABLEAU DES VECTEURS PROPRES
C IN  VALP   : R8 : TABLEAU DES VALEURS PROPRES
C IN  EXCL   : IS : TABLEAU DES NON-EXCLUS
C IN  FCORIG : R8 : FREQUENCE MODE DE CORPS RIGIDE
C OUT ERNORM : R8 : TABLEAU DES NORMES D'ERREUR
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*19 RAIDE
      CHARACTER*24 VALK(2)
      CHARACTER*14 NUME
      REAL*8       ANORM1, ANORM2, XSEUIL
      REAL*8 VALR
      REAL*8       DEPI, R8DEPI
      COMPLEX*16   FREQ, FREQ2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      DEPI   = R8DEPI()
      XSEUIL = OMECOR
      NEQ    = ZI(LMASSE+2)
C
      CALL WKVECT('&&WPERMO.TAMPON.PROV_1' ,'V V C',NEQ,IAUX1)
      CALL WKVECT('&&WPERMO.TAMPON.PROV_2' ,'V V C',NEQ,IAUX2)
      CALL WKVECT('&&WPERMO.TAMPON.PROV_3' ,'V V C',NEQ,IAUX3)
      CALL WKVECT('&&WPERMO.TYPEDDL      ' ,'V V I',NEQ,IAUX4)
C
      DO 1 I=1,NBPROP
C
        IVEC=(I-1)*NEQ+1
        DO 10 J = 0, NEQ-1
           VECP(IVEC+J) = VECP(IVEC+J) * EXCL(J+1)
 10     CONTINUE
C
        AMI = AM(I)
        IF ( ABS(AMI) .EQ. 1.D0) THEN
          ERNORM(I)= 1.D+70
          VALK (1) = ' '
          VALK (2) = ' '
          VALR = 1.0D70
          CALL U2MESG('A', 'ALGELINE4_93',2,VALK,0,0,1,VALR)
        ELSE
          FRI = FREQOM(FR(I))*DEPI
          AMI = -ABS(AMI*FRI)/SQRT(1.D0-AMI*AMI)
          FREQ = DCMPLX( AMI, FRI)
          FREQ2 = FREQ*FREQ
          CALL MCMULT('ZERO',LRAIDE,VECP(IVEC),'C',ZC(IAUX1),1)
          CALL MCMULT('ZERO',LMASSE,VECP(IVEC),'C',ZC(IAUX2),1)
          CALL MCMULT('ZERO',LAMOR ,VECP(IVEC),'C',ZC(IAUX3),1)
          DO 2 J = 0, NEQ-1
            ZC(IAUX2+J)=ZC(IAUX1+J)+FREQ*ZC(IAUX3+J)+FREQ2*ZC(IAUX2+J)
 2        CONTINUE
C
C           --- ON PREND LA NORME EUCLIDIENNE ---
          ANORM1 = 0.D0
          ANORM2 = 0.D0
          DO 3 J = 0, NEQ-1
              ANORM1 = ANORM1 +
     +             DCONJG(ZC(IAUX1+J))*ZC(IAUX1+J)*EXCL(J+1)
              ANORM2 = ANORM2 +
     +             DCONJG(ZC(IAUX2+J))*ZC(IAUX2+J)*EXCL(J+1)
 3        CONTINUE
          IF ( ABS(FREQ) .GT. XSEUIL ) THEN
            IF (  ANORM1 .NE. 0.D0 ) THEN
              ERNORM(I)= SQRT( ANORM2 / ANORM1 )
            ELSE
              ERNORM(I)= 1.D+70
            ENDIF
          ELSE
            ERNORM(I) = ABS(FREQ) * SQRT( ANORM2 )
          ENDIF
C
        ENDIF
    1 CONTINUE
C
      CALL JEDETR('&&WPERMO.TAMPON.PROV_1' )
      CALL JEDETR('&&WPERMO.TAMPON.PROV_2' )
      CALL JEDETR('&&WPERMO.TAMPON.PROV_3' )
      CALL JEDETR('&&WPERMO.TYPEDDL      ' )
C
      CALL JEDEMA()
      END

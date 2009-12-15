      SUBROUTINE OP0181(IER)
      IMPLICIT NONE
      INTEGER  IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/12/2009   AUTEUR DEVESA G.DEVESA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     REALISATION N.GREFFET
C     OPERATEUR "REST_SPEC_TEMP"
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
      INTEGER      IEQ,NPARA,NBVA,NVAL,NSENS,NGRAND,I
      REAL*8       EPSI,VALR
      COMPLEX*16   VALC
      CHARACTER*1  K1B
      CHARACTER*4  K4B,GRAND(3)
      CHARACTER*8  K8B
      CHARACTER*16 K16B,TYPE,CMD,SYMETR,METHOD
      CHARACTER*19 K19B,RESIN,RESOU,VECTOT
      CHARACTER*24 TYPRES
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IER = 0
      CALL GETRES(RESOU,TYPE,CMD)
C
C   Recuperation arguments utilisateurs
C
      CALL GETVID(' ','RESULTAT',1,1,1,RESIN,NVAL)
      IF ( NVAL.EQ.0 ) THEN
         CALL GETVID(' ','RESU_GENE',1,1,1,RESIN,NVAL)
      ENDIF
      CALL GETVTX(' ','METHODE',1,1,1,METHOD,NVAL)
      CALL GETVTX(' ','SYMETRIE',1,1,1,SYMETR,NVAL)
C   Evaluation sens de la FFT
      CALL GETTCO(RESIN,TYPRES)
      IF ((TYPRES(1:10).EQ.'DYNA_HARMO').OR.
     &   (TYPRES(1:9).EQ.'HARM_GENE')) THEN
         NSENS = -1
      ELSE
         NSENS = 1
      ENDIF
C
      CALL GETVTX(' ','NOM_CHAM',1,1,3,GRAND,NGRAND)
      IF (NGRAND .EQ. 0) THEN
          NGRAND = 3
          GRAND(1) = 'DEPL'
          GRAND(2) = 'VITE'
          GRAND(3) = 'ACCE'
      ENDIF
      VECTOT = '&&OP0181.VECTOT'
      DO 10 I = 1,NGRAND
C
C  Calcul des FFT
C
         CALL PREFFT(RESIN,METHOD,SYMETR,NSENS,GRAND(I),
     &               VECTOT,NBVA,IER)
C
C   Ecriture du resultat
C
         CALL ECRESU(RESIN,VECTOT,NBVA,GRAND(I),RESOU,IER)
C         CALL ECRESU(RESIN,NPARA,NBVA,GRAND(I),RESOU,IER)
  10  CONTINUE
C
      CALL JEDEMA()
      END

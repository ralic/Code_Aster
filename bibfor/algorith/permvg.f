      SUBROUTINE PERMVG(VG,S,KRL,DKLDS,KRG,DKGDS)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C PERMVG : CALCUL DES PERMEABILITES RELATIVES
C PAR MUALEM-VAN-GENUCHTEN
C AVEC REGULARISATION DROITE ET GAUCHE
C 
      IMPLICIT NONE
C
C IN
      REAL*8        VG(5),S
C OUT      
      REAL*8        KRL,DKLDS,KRG,DKGDS
C       
      REAL*8        N,SR,PR,SMAX
      REAL*8        M,S1,USN,USM,S1MAX
      REAL*8        X0,Y0,Y0P,Y0W,Y0WP,Y0G,Y0GP,Y1,A1,B1,C1
      REAL*8        SMIN,S1MIN,AR,BR
      INTEGER       IADZI,IAZK24,UMESS,IUNIFI
      CHARACTER*8  NOMAIL
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------


      N    = VG(1)
      PR   = VG(2)
      SR   = VG(3)
      SMAX = VG(4)
C      SATUMA = VG(5)
C      
      KRL = 0.0D0
      DKLDS = 0.0D0
      KRG = 0.0D0
      DKGDS = 0.0D0
C      
      M=1.D0-1.D0/N
      USN=1.D0/N
      USM=1.D0/M
      S1=(S-SR)/(1.D0-SR)
      S1MAX=(SMAX-SR)/(1.D0-SR)
      S1MIN=1.D0-SMAX
      SMIN=SR+(1.D0-SR)*S1MIN
      IF (S.GT.1.00001D0) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'PERMVG',' SAT > 1 A LA MAILLE: ',
     +                                                           NOMAIL
      ENDIF
      IF (S1.LE.0.D0) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'PERMVG',' SAT > SR A LA MAILLE: ',
     +                                                           NOMAIL
      ENDIF
C
C NB : SMAX < SMIN PUISQUE SMAX = S(PCMAX) etc ..
C
      IF ( (S.LT.SMAX).AND.(S.GT.SMIN)) THEN
C
        CALL KFOMVG(PR,SR,M,N,USM,USN,S1,KRL,KRG,DKLDS,DKGDS)
C        
      ELSE IF  (S.GE.SMAX) THEN
        X0=SMAX
C
C  REGUL KL(S) A DROITE
C
        CALL KFOMVG(PR,SR,M,N,USM,USN,S1MAX,Y0W,Y0G,Y0WP,Y0GP)
        Y1=1.D0
        CALL REGUP2(X0,Y0W,Y0WP,Y1,A1,B1,C1)
        KRL=A1*S*S+B1*S+C1
        DKLDS=2.D0*A1*S+B1
C
C  REGUL KG(S) A DROITE
        Y1=0.D0
        CALL REGUP2(X0,Y0G,Y0GP,Y1,A1,B1,C1)
        KRG=A1*S*S+B1*S+C1
        DKGDS=2.D0*A1*S+B1
      ELSE IF  (S.LE.SMIN) THEN
        X0=SMIN
C
C  REGUL KL(S) GAUCHE
C
        CALL KFOMVG(PR,SR,M,N,USM,USN,S1MIN,Y0W,Y0G,Y0WP,Y0GP)
        CALL REGUP1(X0,Y0W,Y0WP,AR,BR)
        KRL=AR*S+BR
        DKLDS=AR
C
C  REGUL KG(S)  GAUCHE
        CALL REGUP1(X0,Y0G,Y0GP,AR,BR)
        KRG=AR*S+BR
        DKGDS=AR 
      ENDIF 

C      KRL = KRL + 1.E-6
C      KRG = KRG + 1.E-6
C =====================================================================
 9001 FORMAT (A8,2X,A30,2X,A8)
C ======================================================================
      
      END

      SUBROUTINE VIRHOL(NBVARI,VINTM,VINTP,ADVIHY,VIHRHO,RHO110,
     +            DP1,DP2,DPAD,CLIQ,DT,ALPLIQ,SIGNE,RHO11,RHO11M,RETCOM)
      IMPLICIT      NONE
      INTEGER       NBVARI,RETCOM,ADVIHY,VIHRHO
      REAL*8        VINTM(NBVARI),VINTP(NBVARI),RHO110,DP1,DP2,DPAD,DT
      REAL*8        CLIQ,SIGNE,ALPLIQ,RHO11,RHO11M
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2005   AUTEUR ROMEO R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- CALCUL ET STOCKAGE DE LA VARIABLE INTERNE DE LA MASSE ------------
C --- VOLUMIQUE DE L EAU -----------------------------------------------
C ======================================================================
      INTEGER       IADZI,IAZK24,UMESS,IUNIFI
      REAL*8        VARBIO,EPXMAX
      PARAMETER    (EPXMAX = 5.D0)
      CHARACTER*8   NOMAIL
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
C ======================================================================
C --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
C --- ET VERIFICATION DE LA COHERENCE ----------------------------------
C ======================================================================
      VARBIO = (DP2-SIGNE*DP1-DPAD)*CLIQ - 3.D0*ALPLIQ*DT
      IF (VARBIO.GT.EPXMAX) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'VIRHOL','VARBIO > EXPMAX A LA MAILLE: ',
     &                                                            NOMAIL
         RETCOM = 1
         GO TO 30
      ENDIF
      VINTP(ADVIHY+VIHRHO) =
     +              - RHO110 + (VINTM(ADVIHY+VIHRHO)+RHO110)*EXP(VARBIO)

      RHO11  = VINTP(ADVIHY+VIHRHO) + RHO110
      RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
C ======================================================================
 30   CONTINUE
C ======================================================================
 9001 FORMAT (A8,2X,A30,2X,A8)
C ======================================================================
      END

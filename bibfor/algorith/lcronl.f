      SUBROUTINE LCRONL (NDIM,IMATE,OPTION,COMPOR,TM,TP,TREF,FM,DF,VIM,
     &                   VIP,SIGP,DSIGDF)

                      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ADBHHVV V.CANO

      IMPLICIT NONE
      INTEGER            NDIM, IMATE
      CHARACTER*16      OPTION,COMPOR(3)
      REAL*8             TM,TP,TREF,DF(3,3),FM(3,3)
      REAL*8             VIM(12),VIP(12),SIGP(6),DSIGDF(6,3,3)

C.......................................................................
C       INTEGRATION DE LA LOI DE ROUSSELIER EN NON LOCAL
C   EN GRANDES DEFORMATIONS DE TYPE NOUVELLE FORMULATION DE SIMO-MIEHE
C.......................................................................
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  OPTION  : OPTION DE CALCUL
C IN  COMPOR  : COMPORTEMENT
C IN  TEMP    : TEMPERATURE A L'INSTANT DU CALCUL
C IN  FM      : GRADIENT DE LA TRANSFORMATION A L INSTANT PRECEDENT
C IN  DF      : INCREMENT DU GRADIENT DE LA TRANSFORMATION
C IN  VIM     : VARIABLES INTERNES A L INSTANT DU CALCUL PRECEDENT
C                 VIM(1)   = P (DEFORMATION PLASTIQUE CUMULEE)
C                 VIM(2:4) =  GRADIENT DE P
C                 VIM(5)   = POROSITE
C                 VIM(6:11)= DEFORMATION ELASTIQUE EULERIENNE
C		  EE = (ID-BE)/2
C IN VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C
C OUT SIGP    : CONTRAINTE A L INSTANT ACTUELLE
C OUT DSIGDF  : DERIVEE DE SIGMA PAR RAPPORT A F
C
C          ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C          L'ORDRE :  XX,YY,ZZ,XY,XZ,YZ
C.......................................................................

      INTEGER ITEMAX, JPROLP, JVALEP, NBVALP
      REAL*8  YOUNG,NU,MU,K,SIGY,ALPHA
      REAL*8  SIG1,D,F0,FCR,ACCE
      REAL*8  FONC,EQETR,PM,RPM,PREC
      COMMON /LCROU/ YOUNG,NU,MU,K,SIGY,ALPHA,
     &               SIG1,D,F0,FCR,ACCE,
     &               FONC,EQETR,PM,RPM,PREC,
     &               ITEMAX, JPROLP, JVALEP, NBVALP

      INTEGER     I,J1,K1
      REAL*8      LAMBDA
      REAL*8      JM,DJ,J
      REAL*8      TRE,BP(6),SP(6),TAUP(6),EM(6),ETR(6)
      REAL*8      DETRDF(6,3,3),VIPP(9),VIMM(9),TRETR,DVETR(6)
      REAL*8      KR(6),PDTSCA(6)
      INTEGER    IND(3,3)
      DATA        KR/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA        PDTSCA/1.D0,1.D0,1.D0,2.D0,2.D0,2.D0/
      DATA        IND/1,4,5,
     &                4,2,6,
     &                5,6,3/

C 1 - RECUPERATION DES CARACTERISTIQUES DU MATERIAU
C 1.1 - CARACTERISTIQUE ELASTIQUE
      
      
      PM=VIM(1)
      CALL LCROMA(IMATE,TP)       
      LAMBDA = YOUNG*NU/((1.D0+NU)*(1.D0-2.D0*NU))
               
C 2 - CALCUL DE JM=DET(FM),DJ=DET(DF),J=JM*DJ ET DFB

      JM=FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &  -FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &  +FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))

      DJ=DF(1,1)*(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))
     &  -DF(2,1)*(DF(1,2)*DF(3,3)-DF(1,3)*DF(3,2))
     &  +DF(3,1)*(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))

      J=JM*DJ
         
C 3 - CALCUL DE SIGP

      IF(OPTION(1:9).EQ.'RAPH_MECA'.OR.
     &   OPTION(1:9).EQ.'FULL_MECA') THEN
      
       TRE=VIP(6)+VIP(7)+VIP(8)
       DO 10 I=1,6
        SP(I)=-2.D0*MU*VIP(5+I)-LAMBDA*TRE*KR(I)
        BP(I)=KR(I)-2.D0*VIP(5+I)
 10    CONTINUE
      
       DO 20 I=1,3
        DO 30 J1=1,3
         TAUP(IND(I,J1))=0.D0
         DO 40 K1=1,3
          TAUP(IND(I,J1))=TAUP(IND(I,J1))
     &      +SP(IND(I,K1))*BP(IND(K1,J1))
 40      CONTINUE
 30     CONTINUE
 20    CONTINUE
 
       DO 50 I=1,2*NDIM
        SIGP(I)=TAUP(I)/J
 50    CONTINUE
      ENDIF
      
C 4 - CALCUL DE LA MATRICE TANGENTE CORRESPONDANT AU MODELE LOCAL 

      IF(OPTION(1:14).EQ.'RIGI_MECA_TANG'.OR.
     &  OPTION(1:9) .EQ.'FULL_MECA') THEN
       
       CALL DCOPY(6, VIM(6),1, EM, 1)
       
       IF (OPTION(1:14).EQ.'RIGI_MECA_TANG') THEN
        VIPP(1)=VIM(1)
        VIMM(1)=VIM(1)
        DO 60 I=2,9
         VIPP(I)=VIM(I+3)
         VIMM(I)=VIM(I+3)
 60     CONTINUE        
       ELSE
        VIPP(1)=VIP(1)
        VIMM(1)=VIM(1)
        DO 70 I=2,9
         VIPP(I)=VIP(I+3)
         VIMM(I)=VIM(I+3)
 70     CONTINUE       
       ENDIF       
       VIPP(9)=0.D0
       VIMM(9)=0.D0      
       CALL CLETDR (DF,EM,OPTION,ETR,DETRDF)
       TRETR=ETR(1)+ETR(2)+ETR(3)
       EQETR=0.D0
       DO 80 I=1,6
        DVETR(I)=ETR(I)-KR(I)*TRETR/3.D0
        EQETR=EQETR+PDTSCA(I)*(DVETR(I)**2.D0)
 80    CONTINUE
       EQETR=SQRT(1.5D0*EQETR)
       FONC = D*VIM(5)*EXP(-K*TRETR/SIG1)
       CALL LCROTG (OPTION,IMATE,TM,TP,TREF,J,JM,DF,VIPP,VIMM,
     &              DETRDF,DSIGDF)
      ENDIF
      END

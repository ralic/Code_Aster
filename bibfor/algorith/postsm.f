      SUBROUTINE POSTSM(OPTION,FM,DF,SIGM,SIGP,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
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

      IMPLICIT NONE     
      CHARACTER*16 OPTION
      REAL*8 FM(3,3),DF(3,3),SIGM(6),SIGP(6),DSIDEP(6,3,3)
      
C------------------------------------------------------------
C   IN  OPTION : OPTION DEMANDEE : RIGI_MECA_TANG, FULL_MECA, RAPH_MECA
C   IN  FM : GRADIENT DE LA TRANSFORMATION EN T-
C   IN  DF : GRADIENT DE LA TRANSFORMATION DE T- A T+
C   IN  SIGM : CONTRAINTE EN T-
C   IN/OUT  SIGP : CONTRAINTE CAUCHY EN T+ -> CONTRAINTE KIRCHHOF EN T+
C   IN/OUT  DSIDEP : MATRICE TANGENTE D(SIG)/DF  ->
C                    D(TAU)/D(FD) * (FD)t               
C-----------------------------------------------------------------------
      LOGICAL RESI,RIGI
      INTEGER KL,P,Q,I
      REAL*8 JM,DJ,JP,TAU(6),J,MAT(6,3,3),ID(3,3)

      DATA    ID   /1.D0, 0.D0, 0.D0, 
     &              0.D0, 1.D0, 0.D0,
     &              0.D0, 0.D0, 1.D0/


      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      
      JM=FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &  -FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &  +FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))

      DJ=DF(1,1)*(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))
     &  -DF(2,1)*(DF(1,2)*DF(3,3)-DF(1,3)*DF(3,2))
     &  +DF(3,1)*(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))

      JP=JM*DJ
      
      IF (RESI) THEN
        CALL DSCAL(6,JP,SIGP,1)
        CALL DCOPY(6,SIGP,1,TAU,1)
        J = JP
      ELSE
        CALL DCOPY(6,SIGM,1,TAU,1)
        CALL DSCAL(6,JM,TAU,1)
        J = JM
      END IF
      
      IF (RIGI) THEN
        CALL DCOPY(54,DSIDEP,1,MAT,1)
        CALL DSCAL(54,J,MAT,1)
        DO 100 KL = 1,6
          DO 200 P = 1,3
            DO 300 Q = 1,3
              DSIDEP(KL,P,Q) = TAU(KL)*ID(P,Q)
              DO 400 I = 1,3
                DSIDEP(KL,P,Q) = DSIDEP(KL,P,Q) + MAT(KL,P,I)*DF(Q,I)
 400          CONTINUE           
 300        CONTINUE
 200      CONTINUE
 100    CONTINUE
      END IF

      END 

      SUBROUTINE I2SENS(CHEMIN,NBRMA2,LIMAIL,NBRMA,CONNEX,TYPMAI)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NBRMA,NBRMA2
      INTEGER CHEMIN(NBRMA2),LIMAIL(NBRMA)
      CHARACTER*(*) CONNEX,TYPMAI
C
C-----------------------------------------------------------------------
      INTEGER I ,J ,MI ,MJ ,NID ,NIG ,NJD 
      INTEGER NJG 
C-----------------------------------------------------------------------
      I = CHEMIN(1)
      MI = LIMAIL(I)
      CHEMIN(1) = MI
      CALL I2EXTF(MI,1,CONNEX,TYPMAI,NIG,NID)
      DO 10 I = 2,NBRMA
        J = CHEMIN(I)
        MJ = LIMAIL(J)
        CALL I2EXTF(MJ,1,CONNEX,TYPMAI,NJG,NJD)
C
        IF (MI.GT.0) THEN
          IF (NID.EQ.NJD) THEN
            MJ = -MJ
          END IF

        ELSE IF (MI.LT.0) THEN
          IF (NIG.EQ.NJD) THEN
            MJ = -MJ
          END IF

        END IF

        MI = MJ
        NIG = NJG
        NID = NJD
        CHEMIN(I) = MI
   10 CONTINUE
      END

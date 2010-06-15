         SUBROUTINE CARAND(RANDD,GR)
C
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ROUTINE CARAND : TIRAGE STATISTIQUE SUR LOI UNIFORME
C AVEC FONCTION VRAIEMENT ALEATOIRE KLOKLO SI GR=0
C GR EST LA GRAINE
C
C
      IMPLICIT NONE
C
      INTEGER HVLUE, LVLUE, TESTV, NEXTN,TIME(9)
      REAL*8    RANDD,RALEA,GR
      INTEGER   IND,MPLIER,MODLUS,MOBYMP,MOMDMP
C
      COMMON  /SEED/NEXTN
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      IND   = NINT(GR)
      IF (IND .EQ. 0) THEN
        IF (NEXTN .EQ. 0) THEN
          CALL KLOKLO(TIME)
          NEXTN = TIME(5)+TIME(6)+TIME(7)
        ENDIF
      ELSE
        IF (NEXTN .EQ. 0) THEN
          NEXTN = IND
        ENDIF
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      RANDD = ABS(DBLE(NEXTN)/DBLE(MODLUS))
C      
      END

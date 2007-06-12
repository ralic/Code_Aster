        SUBROUTINE HUJCRI (MATER, SIG , VIN, SEUILI)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2007   AUTEUR KHAM M.KHAM 
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
C       ---------------------------------------------------------------
C       HUJEUX:  SEUIL DU MECANISME ISOTROPE   FI = ABS( I1 ) + R4*D*PC
C       ---------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = (R4,R1,R2,R3,EPSVP...)
C       OUT SEUILI :  SEUIL  ELASTICITE DU MECANISME ISOTROPE
C       ---------------------------------------------------------------
        INTEGER NDT, NDI, I
        REAL*8  MATER(20,2), R4, I1, SIG(6), VIN(*)
        REAL*8  D, PCO, BETA, SEUILI, PC, EPSVPM
        REAL*8  D13, ZERO, AEXP, EXPTOL, R8MAEM

        COMMON /TDIM/ NDT, NDI

        DATA    D13, ZERO  /0.333333333334D0, 0.D0/

        D      = MATER(3,2)
        PCO    = MATER(7,2)
        BETA   = MATER(2,2)
        R4     = VIN(4)
        EPSVPM = VIN(5)
        
        EXPTOL = LOG(R8MAEM())
        EXPTOL = MIN(EXPTOL, 40.D0)
        AEXP   = -BETA*EPSVPM
        IF (AEXP .GE. EXPTOL) CALL U2MESS('F', 'COMPOR1_7')
        
        PC     = PCO*EXP(-BETA*EPSVPM)
                        
        I1 = ZERO
        DO 10 I = 1, NDI
          I1 = I1 + D13*SIG(I)
 10       CONTINUE

        SEUILI = ABS(I1) + R4*D*PC

        END

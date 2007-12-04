        SUBROUTINE INSINV ( SIG , I1, J2, J3, RCOS3T  )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       ----------------------------------------------------------------
C       NADAI_BETON  :
C
C         IN  SIG :  CONTRAINTE
C
C         OUT I1      :  PREMIER INVARIANT DE CONTRAINTE
C             J2      :  SECOND INVARIANT DU DEVIATEUR DE CONTRAINTE
C             J3      :  TROISIEME INVARIANT DU DEVIATEUR DE CONTRAINTE
C             RCOS3T  :  RCOS3T (J2, J3)
C
C       ----------------------------------------------------------------
        REAL*8          I1, J2, J3, RCOS3T, SIG(6)
        REAL*8          SXY, SXZ, SYZ, SSX, SSY, SSZ, DEV(6)
C       ----------------------------------------------------------------
C
        COMMON /TDIM/   NDT , NDI
C
       INTEGER  NDT , NDI
        CALL LCHYDR (SIG, I1)
        CALL LCDEVI (SIG, DEV)
        I1  = 3.D0*I1
        SSX = DEV(1)
        SSY = DEV(2)
        SSZ = DEV(3)
        SXY = DEV(4)/SQRT(2.D0)
        SXZ = 0.D0
        SYZ = 0.D0
         IF(NDT .EQ. 6) THEN
          SXZ = DEV(5)/SQRT(2.D0)
          SYZ = DEV(6)/SQRT(2.D0)
         ENDIF
C
        J2=(SSX*SSX+SSY*SSY+SSZ*SSZ)/2.D0 + SXY*SXY+SXZ*SXZ+SYZ*SYZ
        J3=SSX*SSY*SSZ + 2.D0*SXY*SXZ*SYZ - SSX*SYZ*SYZ-SSY*SXZ*SXZ
        J3=J3 - SSZ*SXY*SXY
        RCOS3T=1.5D0*SQRT(3.D0)*J3/SQRT(J2*J2*J2+1.D-30)
C
        END

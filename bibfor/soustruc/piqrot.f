      SUBROUTINE PIQROT ( X1, Y1, Z1, TETAX,
     +                    NT, RET, RIT, REP, TETAF, EPSI  )
      IMPLICIT   NONE 
      INTEGER             NT
      REAL*8              X1, Y1, Z1, TETAX, RET, RIT, REP, TETAF, EPSI
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 11/11/98   AUTEUR CIBHHME R.MEDDOURI 
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
C     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
C     AUTEUR Y. WADIER
C    
C     CALCULE L'ANGLE DE ROTATION DU PIQUAGE ROND
C    
C-----------------------------------------------------------------------
C
      INTEGER  N   
      REAL*8   PI, R8PI, RMP, RXT, EPT, LMAX, TETAF0
C     ------------------------------------------------------------------
C
      PI = R8PI()
      RMP = SQRT( X1**2 + Y1**2 )
C
      TETAX = TETAF 
      N = NINT( 2.0D0 * NT * TETAF / PI ) + 1      
C
      RXT  = ( RET + RIT ) / 2.0D0
      EPT  = RET - RIT 
      LMAX = PI * RXT
C
      IF ( (REP+EPT).LE.RMP .AND. RMP.LE.(LMAX+EPSI) ) THEN
         TETAF0 = ( N - 1 ) * PI / ( 2 * NT )
         TETAX  = (TETAF*(LMAX-RMP)+TETAF0*(RMP-REP-EPT))/(LMAX-REP-EPT)
      ENDIF
C 
      END

      SUBROUTINE NTHYDR ( HYDRAT )
      IMPLICIT   NONE
      LOGICAL             HYDRAT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
      INTEGER       NBOCC, N1, I
      CHARACTER*16  COMP
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      HYDRAT = .FALSE.
C
      CALL GETFAC ( 'COMP_THER_NL' , NBOCC )
C
      DO 10 I = 1 , NBOCC
C
         CALL GETVTX ( 'COMP_THER_NL' , 'RELATION', I,IARG,1, COMP, N1 )
C
         IF ( COMP(1:9) .EQ. 'THER_HYDR' ) HYDRAT = .TRUE.
C
 10   CONTINUE
C
C FIN ------------------------------------------------------------------
      END

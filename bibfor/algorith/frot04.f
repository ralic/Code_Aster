      SUBROUTINE FROT04 ( II, JAD1, JAD2, JDEC, IND, ATMU, NDIM, 
     +                    NBLIA, COEQ, MU, CONR )
      IMPLICIT   NONE
      INTEGER             II, NDIM, NBLIA, COEQ(*)
      REAL*8              ATMU(*), MU(*), CONR(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/01/2000   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
      INTEGER    JJ, JAD1, JAD2, JDEC, IND, NUM1, NUM2
C     ------------------------------------------------------------------
      DO 10 JJ = 1, JDEC
         NUM1 = COEQ(JAD1+JJ+1)
         NUM2 = COEQ(JAD2+JJ+1)
         ATMU(NUM1) = ATMU(NUM1) + CONR(IND+2*NDIM+JJ)*MU(II)
         ATMU(NUM2) = ATMU(NUM2) + CONR(IND+3*NDIM+JJ)*MU(II)
         IF ( NDIM .EQ. 3 ) THEN
            ATMU(NUM1) = ATMU(NUM1) + CONR(IND+4*NDIM+JJ)*MU(II+NBLIA)
            ATMU(NUM2) = ATMU(NUM2) + CONR(IND+5*NDIM+JJ)*MU(II+NBLIA)
         ENDIF
 10   CONTINUE
C
      END

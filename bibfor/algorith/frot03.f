      SUBROUTINE FROT03 ( II, AJEUFX, AJEUFY, NDIM, COAD, COEQ, DEPDE,  
     +                    LOG1, DELTA, CONR )
      IMPLICIT   NONE
      INTEGER             II, NDIM, COAD(*), COEQ(*)
      REAL*8              AJEUFX, AJEUFY, CONR(*), DEPDE(*), DELTA(*)
      LOGICAL             LOG1
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
      REAL*8     XX1, XX2
C     ------------------------------------------------------------------
      AJEUFX = 0.D0
      AJEUFY = 0.D0
      JAD1 = COAD(2*(II-1)+2)
      JAD2 = COAD(2*(II-1)+3)
      JDEC = JAD2 - JAD1 - 1
      IND  = (2*NDIM**2+4)*(II-1)
      DO 10 JJ = 1, JDEC
         NUM1 = COEQ(JAD1+JJ+1)
         NUM2 = COEQ(JAD2+JJ+1)
         IF ( LOG1 ) THEN
            XX1  = DEPDE(NUM1) + DELTA(NUM1)
            XX2  = DEPDE(NUM2) + DELTA(NUM2)
         ELSE
            XX1  = DEPDE(NUM1)
            XX2  = DEPDE(NUM2)
         ENDIF
         AJEUFX = AJEUFX + CONR(IND+2*NDIM+JJ)*XX1
     +                   + CONR(IND+3*NDIM+JJ)*XX2
         IF ( NDIM .EQ. 3 ) THEN
            AJEUFY = AJEUFY + CONR(IND+4*NDIM+JJ)*XX1
     +                      + CONR(IND+5*NDIM+JJ)*XX2
         ENDIF
 10   CONTINUE
C
      END

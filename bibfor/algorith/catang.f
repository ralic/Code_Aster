      SUBROUTINE CATANG(NDIM,XNORM,XTANG,TANGDF)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/2002   AUTEUR PABHHHH N.TARDIEU 
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
      IMPLICIT   NONE
      INTEGER NDIM,TANGDF
      REAL*8 XNORM(3),XTANG(6),DET,VARTGT(3),PSCAL
      REAL*8 LAMBDA,SPVECT(3),EPSI,TESTTG
C ----------------------------------------------------------------------
C     BUT : DONNE LE VECTEUR TANGENT ASSOCIE A XNORM
C ----------------------------------------------------------------------
      EPSI = 1.0D-6
      IF (TANGDF.EQ.0 .OR. NDIM.EQ.2) THEN
        IF (NDIM.EQ.2) THEN
          XTANG(1) = -XNORM(2)
          XTANG(2) = XNORM(1)
          XTANG(3) = 0.0D0
        ELSE
          DET = SQRT(XNORM(1)**2+XNORM(2)**2)
          IF (DET.NE.0.0D0) THEN
            XTANG(1) = -XNORM(2)/DET
            XTANG(2) = XNORM(1)/DET
            XTANG(3) = 0.0D0
            XTANG(4) = XNORM(1)*XNORM(3)/DET
            XTANG(5) = XNORM(2)*XNORM(3)/DET
            XTANG(6) = -DET
          ELSE
            XTANG(1) = 0.0D0
            XTANG(2) = -1.0D0
            XTANG(3) = 0.0D0
            XTANG(4) = XNORM(3)
            XTANG(5) = 0.0D0
            XTANG(6) = 0.0D0
          END IF
        END IF
      ELSE
        VARTGT(1) = XTANG(1)
        VARTGT(2) = XTANG(2)
        VARTGT(3) = XTANG(3)
        SPVECT(1) = XNORM(2)*XTANG(3) - XNORM(3)*XTANG(2)
        SPVECT(2) = XNORM(3)*XTANG(1) - XNORM(1)*XTANG(3)
        SPVECT(3) = XNORM(1)*XTANG(2) - XNORM(2)*XTANG(1)
        TESTTG = SQRT(SPVECT(1)**2+SPVECT(2)**2+SPVECT(3)**2)
C         WRITE(6,*) 'CATANG'
C        write(6,*) 'XTANG=',XTANG(1),XTANG(2),XTANG(3)
C        write(6,*) 'KTANG=',XNORM(1),XNORM(2),XNORM(3)
        IF (TESTTG.LT.EPSI) THEN
          CALL UTMESS('F','CATANG','LE VECTEUR TANGENT DEFINI'//
     &                ' EST COLINEAIRE AU VECTEUR NORMAL')
        END IF
        PSCAL = VARTGT(1)*XNORM(1) + VARTGT(2)*XNORM(2) +
     &          VARTGT(3)*XNORM(3)
        LAMBDA = XNORM(1)*XNORM(1) + XNORM(2)*XNORM(2) +
     &           XNORM(3)*XNORM(3)
        LAMBDA = -PSCAL/LAMBDA
        XTANG(1) = LAMBDA*XNORM(1) + VARTGT(1)
        XTANG(2) = LAMBDA*XNORM(2) + VARTGT(2)
        XTANG(3) = LAMBDA*XNORM(3) + VARTGT(3)
        DET = SQRT(XTANG(1)**2+XTANG(2)**2+XTANG(3)**2)
        XTANG(1) = XTANG(1)/DET
        XTANG(2) = XTANG(2)/DET
        XTANG(3) = XTANG(3)/DET
        XTANG(4) = XNORM(2)*XTANG(3) - XNORM(3)*XTANG(2)
        XTANG(5) = XNORM(3)*XTANG(1) - XNORM(1)*XTANG(3)
        XTANG(6) = XNORM(1)*XTANG(2) - XNORM(2)*XTANG(1)
        DET = SQRT(XTANG(4)**2+XTANG(5)**2+XTANG(6)**2)
        XTANG(4) = -XTANG(4)/DET
        XTANG(5) = -XTANG(5)/DET
        XTANG(6) = -XTANG(6)/DET

      END IF
      END

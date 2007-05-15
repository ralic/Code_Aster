      SUBROUTINE NMGVDD(NDIM,NNO1,NNO2,IU,IA,IL)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/05/2007   AUTEUR GENIAUT S.GENIAUT 
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

       INTEGER NDIM,NNO1,NNO2,IU(NDIM*NNO1),IA(NNO2),IL(NNO2)
C ----------------------------------------------------------------------
C
C     POSITION DES INDICES POUR LES DEGRES DE LIBERTE
C
C IN  NDIM    : DIMENSION DES ELEMENTS 
C IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
C IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE A,L)
C ----------------------------------------------------------------------
      
      INTEGER N,I,OS

      CHARACTER*16 NOMELT
      COMMON /FFAUTO/ NOMELT
C ----------------------------------------------------------------------

      
C      ELEMENT P1 - CONTINU

        DO 110 N = 1,NNO2
          DO 120 I = 1,NDIM
            IU(NNO1*(I-1)+N) = I + (N-1)*(NDIM+2)
 120      CONTINUE
          IA(N) = 1 + NDIM + (N-1)*(NDIM+2)
          IL(N) = 2 + NDIM + (N-1)*(NDIM+2)
 110    CONTINUE
        OS = (2+NDIM)*NNO2
        DO 140 N = 1,NNO1-NNO2
          DO 150 I = 1,NDIM
            IU(NNO1*(I-1)+N+NNO2) = I + (N-1)*NDIM + OS
 150      CONTINUE
 140    CONTINUE


 
      END

      SUBROUTINE ALLIR8(BASE,NOLIR8,NBR8,LR8)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                  NBR8
      REAL*8                        LR8(*)
      CHARACTER*8       NOLIR8
      CHARACTER*(*)       BASE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     BUT: CREER UN CONCEPT LIST_REEL DE NOM NOLIR8
C          A PARTIR DE LA LISTE LR8.
C
C     IN: BASE   : BASE DE CREATION
C         NOLIR8 : NOM DU CONCEPT LISTR8 A CREER.
C         NBR8   : NOMBRE DE REELS DU LISTR8
C         LR8    : LISTE DES REELS.
C
C     OUT:  NOLIR8 EST CREE.
C
C ----------------------------------------------------------------------
      CHARACTER*1 B
C
C-----------------------------------------------------------------------
      INTEGER I ,JBOR ,JNBP ,JPAS ,JVAL ,NBB 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      B=BASE
      NBB = NBR8 - 1
      CALL WKVECT(NOLIR8//'           .LPAS',B//' V R',MAX(1,NBB),JPAS)
      CALL WKVECT(NOLIR8//'           .NBPA',B//' V I',MAX(1,NBB),JNBP)
      CALL WKVECT(NOLIR8//'           .VALE',B//' V R',NBR8,JVAL)
      CALL WKVECT(NOLIR8//'           .BINT',B//' V R',NBR8,JBOR)
C
      IF (NBR8.EQ.1) THEN
         ZR(JPAS) = 0.D0
         ZI(JNBP) = 0
         ZR(JVAL) = LR8(1)
         ZR(JBOR) = LR8(1)
      ELSE
         DO 10 I = 1,NBB
            ZR(JPAS-1+I) = LR8(I+1) - LR8(I)
            ZI(JNBP-1+I) = 1
            ZR(JVAL-1+I) = LR8(I)
            ZR(JBOR-1+I) = LR8(I)
 10      CONTINUE
         ZR(JVAL-1+NBR8) = LR8(NBR8)
         ZR(JBOR-1+NBR8) = LR8(NBR8)
      ENDIF
C
      CALL JEDEMA()
      END

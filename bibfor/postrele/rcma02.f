      SUBROUTINE RCMA02 ( ETAT, IOCC, VALE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             IOCC
      REAL*8              VALE(*)
      CHARACTER*1         ETAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     RECUPERATION DES CARACTERISTIQUES MATERIAU POUR UN ETAT
C
C IN  : ETAT   : ETAT STABILISE "A" OU "B"
C IN  : IOCC   : NUMERO D'OCCURRENCE DE LA SITUATION
C OUT : VALE   : CARACTERISTIQUES MATERIAU
C                VALE(1) = E
C                VALE(2) = NU
C                VALE(3) = ALPHA
C                VALE(4) = EC
C                VALE(5) = SM
C                VALE(6) = M
C                VALE(7) = N
C     ------------------------------------------------------------------
C
      INTEGER      NBCMP, I, JVALE
      PARAMETER   ( NBCMP = 8 )
C DEB ------------------------------------------------------------------
C
C --- LES CARACTERISTIQUES MATERIAU
C
      CALL JEVEUO ( '&&RC3200.MATERIAU_'//ETAT, 'L', JVALE )
C
      DO 10 I = 1 , NBCMP
         VALE(I) = ZR(JVALE-1+NBCMP*(IOCC-1)+I)
 10   CONTINUE
C
      END

      INTEGER FUNCTION RGCMPG(ICODE,IRGCMP)
      IMPLICIT    NONE
      INTEGER     ICODE,IRGCMP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 22/04/2013   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C --- ------------------------------------------------------------------
C
C        RANG DE LA VALEUR DANS LA CARTE EN DECODANT L'ENTIER CODE
C
C --- ------------------------------------------------------------------
C
C IN
C     ICODE  : ENTIER CODE DE LA ZONE DE LA CARTE
C     IRGCMP : RANG DE LA COMPOSANTE DANS LA CARTE
C
C OUT
C     RGCMPG : RANG DE LA VALEUR DANS L'ENTIER CODE
C --- ------------------------------------------------------------------
      INTEGER  ICMP,IRGVAL
      LOGICAL  EXISDG
C --- ------------------------------------------------------------------
C
      IRGVAL = 0
      DO 100 ICMP = 1,IRGCMP
         IF ( EXISDG(ICODE,ICMP) ) IRGVAL = IRGVAL + 1
100   CONTINUE
C     SORTIE
      RGCMPG = IRGVAL
C
      END

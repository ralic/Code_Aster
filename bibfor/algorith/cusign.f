      SUBROUTINE CUSIGN(JCMPG,ICMP,SIGN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      INTEGER      JCMPG
      INTEGER      ICMP
      REAL*8       SIGN
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CUPREP
C ----------------------------------------------------------------------
C
C CETTE ROUTINE DONNE LE SIGNE A PLACER DEVANT LA COMPOSANTE DDL
C POUR LA THM, LE SIGNE EST POSITIF A CAUSE DE L'ECRITURE DE L'EQUATION
C HYDRAULIQUE.
C POUR LES AUTRES DDLS, IL EST NEGATIF 
C
C IN  JCOEF  : ADRESSE JEVEUX DES COMPOSANTES
C IN  ICMP   : INDICE DE LA COMPOSANTE DU COEFFICIENT
C OUT COEF   : VALEUR DU SIGNE
C
C
C
C
C
      CHARACTER*8  CMP     
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C ----------------------------------------------------------------------
      CMP = ZK8(JCMPG-1+ICMP)

      IF (CMP(1:4).EQ.'PRE1') THEN
        SIGN = +1.D0
      ELSE IF (CMP(1:4).EQ.'PRE2') THEN
        SIGN = +1.D0
      ELSE
        SIGN = +1.D0
      ENDIF

C ----------------------------------------------------------------------
      CALL JEDEMA()
      END

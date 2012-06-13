      SUBROUTINE LISLCO(LISCHA,ICHAR ,CODCHA)
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
      CHARACTER*19 LISCHA
      INTEGER      ICHAR
      INTEGER      CODCHA
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C RETOURNE LE CODE DU GENRE DE LA CHARGE
C
C ----------------------------------------------------------------------
C
C
C IN  LISCHA : SD LISTE DES CHARGES
C IN  ICHAR  : INDICE DE LA CHARGE
C OUT CODCHA : CODE POUR LE GENRE DE LA CHARGE
C
C
C
C
      CHARACTER*24 CODECH
      INTEGER      JCODC
      INTEGER      NBCHAR
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CODCHA = 0
      CALL LISNNB(LISCHA,NBCHAR)
C
      IF (NBCHAR.NE.0) THEN
        CODECH = LISCHA(1:19)//'.CODC'
        CALL JEVEUO(CODECH,'L',JCODC)
        CODCHA = ZI(JCODC-1+ICHAR)
      ENDIF
C
      CALL JEDEMA()
      END

      SUBROUTINE NMTSTM (COMPOR,IMATRI,MATSYM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/01/2005   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16 COMPOR(*)
      LOGICAL      MATSYM
      INTEGER      IMATRI

C ----------------------------------------------------------------------
C    RENVOIE EN FONCTION DU COMPORTEMENT LE TYPE DE MATRICE
C    SYMETRIQUE OU NON SYMETRIQUE SOUHAITE
C ----------------------------------------------------------------------

      MATSYM=.TRUE.
      IF (COMPOR(1)(1:7) . EQ . 'KIT_DDI') THEN
        IF (COMPOR(8)(1:13) . EQ. 'BETON_UMLV_FP') THEN
          MATSYM = .FALSE.
        ENDIF
      ENDIF

      IF (COMPOR(1)(1:16) . EQ . 'ENDO_ORTH_BETON') THEN
        MATSYM = .FALSE.
      ENDIF
      
      IF (COMPOR(1)(1:3) . EQ . 'CJS') THEN
        MATSYM = .FALSE.
      ENDIF

      IF (COMPOR(1)(1:6) . EQ . 'LAIGLE') THEN
        MATSYM = .FALSE.
      ENDIF

      IF (COMPOR(1)(1:6) . EQ . 'MAZARS') THEN
        MATSYM = .FALSE.
      ENDIF
      IF (MATSYM) THEN
        CALL JEVECH('PMATUUR','L',IMATRI)
      ELSE
        CALL JEVECH('PMATUNS','L',IMATRI)
      ENDIF
      END

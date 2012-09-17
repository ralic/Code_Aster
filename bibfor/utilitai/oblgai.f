      SUBROUTINE OBLGAI(SDLIST,ISTRU ,LACTI )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/09/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      INCLUDE       'jeveux.h'
      CHARACTER*24 SDLIST
      INTEGER      ISTRU
      LOGICAL      LACTI
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
C
C DIT SI LE STRUCT EST ACTIF OU PAS - ACCES PAR INDICE
C
C ----------------------------------------------------------------------
C
C
C IN  SDLIST : NOM DE LA LISTE
C IN  ISTRU  : INDICE DU STRUCT DANS LA LISTE
C OUT LACTI  : .TRUE. SI STRUCT ACTIVE
C
C ----------------------------------------------------------------------
C
      CHARACTER*24 LISACT
      INTEGER      JLISAC
      INTEGER      LA
      CHARACTER*24 TYPESD
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- VERIFICATIONS
C
      CALL OBGETT(SDLIST,TYPESD)
      IF (TYPESD.NE.'LISTE_STRUCTS') CALL ASSERT(.FALSE.)
C
C --- ACTIVATION
C
      CALL OBGETO(SDLIST,'ACT_STRUCTS',LISACT)
      CALL JEVEUO(LISACT,'L',JLISAC)
C
C --- ACTIVATION ?
C
      LA = ZI(JLISAC-1+ISTRU)
      IF (LA.EQ.0) THEN
        LACTI  = .FALSE.
      ELSEIF (LA.EQ.1) THEN
        LACTI  = .TRUE.
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END

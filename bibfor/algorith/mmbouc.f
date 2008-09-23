      SUBROUTINE MMBOUC(RESOCO,NOMBCL,TYPOPE,VALBCL)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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
C REPONSABLE
C
      IMPLICIT NONE
      CHARACTER*24  RESOCO
      CHARACTER*4   NOMBCL
      CHARACTER*4   TYPOPE
      INTEGER       VALBCL
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
C
C GESTION DES BOUCLES
C      
C ----------------------------------------------------------------------
C
C
C IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
C IN  NOMBCL : NOM DE LA BOUCLE
C               CONT - CONTRAINTES ACTIVES
C               FROT - SEUILS DE FROTTEMENT
C               GEOM - GEOMETRIE
C IN  TYPOPE : TYPE DE L'OPERATION
C               READ - LECTURE
C               INIT - INITIALISATION A ZERO
C               INCR - INCREMENTATION
C OUT VALBCL : VALEUR DE LA BOUCLE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*24 MBOUCL
      INTEGER      JMBOUC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MBOUCL = RESOCO(1:14)//'.MBOUCL'
      CALL JEVEUO(MBOUCL,'E',JMBOUC) 
C      
      IF (NOMBCL.EQ.'CONT') THEN
        IF (TYPOPE.EQ.'INIT') THEN
          ZI(JMBOUC-1+1) = 0
        ELSEIF (TYPOPE.EQ.'INCR') THEN
          ZI(JMBOUC-1+1) = ZI(JMBOUC-1+1) +1
        ELSEIF (TYPOPE.EQ.'READ') THEN
          VALBCL = ZI(JMBOUC-1+1)          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        VALBCL = ZI(JMBOUC-1+1)
      ELSEIF (NOMBCL.EQ.'FROT') THEN
        IF (TYPOPE.EQ.'INIT') THEN
          ZI(JMBOUC-1+2) = 0
        ELSEIF (TYPOPE.EQ.'INCR') THEN
          ZI(JMBOUC-1+2) = ZI(JMBOUC-1+2) +1
        ELSEIF (TYPOPE.EQ.'READ') THEN
          VALBCL = ZI(JMBOUC-1+2)          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        VALBCL = ZI(JMBOUC-1+2)        
      ELSEIF (NOMBCL.EQ.'GEOM') THEN
        IF (TYPOPE.EQ.'INIT') THEN
          ZI(JMBOUC-1+3) = 0
        ELSEIF (TYPOPE.EQ.'INCR') THEN
          ZI(JMBOUC-1+3) = ZI(JMBOUC-1+3) +1
        ELSEIF (TYPOPE.EQ.'READ') THEN
          VALBCL = ZI(JMBOUC-1+3)          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF   
        VALBCL = ZI(JMBOUC-1+3)        
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF          
C
      CALL JEDEMA()
      END

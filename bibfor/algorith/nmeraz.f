      SUBROUTINE NMERAZ(SDERRO,TYPEVT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/10/2012   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 SDERRO
      CHARACTER*4  TYPEVT
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (SD ERREUR)
C
C REMISE A ZERO DES EVENEMENTS
C
C ----------------------------------------------------------------------
C
C
C IN  SDERRO : SD GESTION DES ERREURS
C IN  TYPEVT : TYPE DE L'EVENEMENT
C              'TOUS' - TOUS LES EVENEMENTS
C              'EVEN' - EVENEMENT SIMPLE
C
C ----------------------------------------------------------------------
C
      INTEGER      IEVEN,ZEVEN
      INTEGER      IRET
      CHARACTER*24 ERRINF
      INTEGER      JEINFO
      CHARACTER*24 ERRAAC,ERRENI
      INTEGER      JEEACT,JEENIV
      CHARACTER*16 TEVEN
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES SD
C
      ERRINF = SDERRO(1:19)//'.INFO'
      CALL JEEXIN(ERRINF,IRET)
      IF (IRET.EQ.0) GOTO 99
      CALL JEVEUO(ERRINF,'L',JEINFO)
      ZEVEN  = ZI(JEINFO-1+1)
      ERRAAC = SDERRO(1:19)//'.EACT'
      ERRENI = SDERRO(1:19)//'.ENIV'
      CALL JEVEUO(ERRAAC,'E',JEEACT)
      CALL JEVEUO(ERRENI,'L',JEENIV)
C
C --- EVENEMENTS DESACTIVES
C
      DO 15 IEVEN = 1,ZEVEN
        TEVEN  = ZK16(JEENIV-1+IEVEN)(1:9)
        IF (TYPEVT.EQ.'TOUS') THEN
          ZI(JEEACT-1+IEVEN) = 0
        ELSEIF (TYPEVT.EQ.'EVEN') THEN
          IF (TEVEN.EQ.'EVEN') ZI(JEEACT-1+IEVEN) = 0
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
 15   CONTINUE
C
 99   CONTINUE
C
      CALL JEDEMA()
      END

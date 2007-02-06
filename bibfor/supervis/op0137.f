      SUBROUTINE OP0137(IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
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
C     OPERATEUR :     DEBUG
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
C ----------------------------------------------------------------------
      CHARACTER*3 REPONS , CBID
      INTEGER L,SDVERI,IER



C     SDVERI=OUI/NON :
C     ----------------
      REPONS=' '
      CALL GETVTX(' ','SDVERI',1,1,1,REPONS,L)
      IF ( REPONS .EQ. 'OUI') THEN
         IF (SDVERI().EQ.1) THEN
           CALL JDCSET('sdveri', 1)
           CALL U2MESS('I','SUPERVIS_24')
         ELSE
           CALL JDCSET('sdveri', 0)
           CALL U2MESS('A','SUPERVIS_42')
         ENDIF
      ELSE IF ( REPONS .EQ. 'NON') THEN
         CALL JDCSET('sdveri', 0)
         CALL U2MESS('I','SUPERVIS_43')
      ENDIF


C     JEVEUX=OUI/NON :
C     ----------------
      REPONS=' '
      CALL GETVTX(' ','JEVEUX',1,1,1,REPONS,L)
      IF ( REPONS .EQ. 'OUI') THEN
         IDEBUG = 1
         CALL U2MESS('I','SUPERVIS_44')
         CALL JDCSET('jeveux', 1)
      ELSE IF ( REPONS .EQ. 'NON') THEN
         IDEBUG = 0
         CALL U2MESS('I','SUPERVIS_45')
         CALL JDCSET('jeveux', 0)
      ENDIF


C     JXVERI=OUI/NON :
C     ----------------
      REPONS=' '
      CALL GETVTX(' ','JXVERI',1,1,1,REPONS,L)
      IF ( REPONS .EQ. 'OUI') THEN
         CALL U2MESS('I','SUPERVIS_46')
         CALL JDCSET('jxveri', 1)
      ELSE  IF ( REPONS .EQ. 'NON') THEN
         CALL U2MESS('I','SUPERVIS_47')
         CALL JDCSET('jxveri', 0)
      ENDIF


C     IMPR_MACRO=OUI/NON :
C     ---------------------
      REPONS=' '
      CALL GETVTX(' ','IMPR_MACRO',1,1,1,REPONS,L)
      IF ( REPONS .EQ. 'OUI') THEN
         CALL U2MESS('I','SUPERVIS_48')
         CALL JDCSET('impr_macro', 1)
      ELSE IF ( REPONS .EQ. 'NON') THEN
         CALL U2MESS('I','SUPERVIS_49')
         CALL JDCSET('impr_macro', 0)
      ENDIF


      IER=0
      END

      SUBROUTINE JAEXIN ( NOMLU , IRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 28/06/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER *(*)      NOMLU
      INTEGER                     IRET
C ----------------------------------------------------------------------
C BUT : TESTE L'EXISTENCE REELLE D'UN OBJET JEVEUX
C
C IN  NOMLU  : NOM DE L'OBJET JEVEUX (EVENTUELLEMENT JEXNUM(NOMCO,IOBJ))
C OUT IRET   : =0 L'OBJET N'EXISTE PAS
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      CHARACTER *32    NOML32
      CHARACTER *8 KBID
      INTEGER IEXI,IADM,IADD,LADM
C DEB ------------------------------------------------------------------
      NOML32 = NOMLU

      CALL JEEXIN(NOML32,IEXI)
      IF (IEXI.EQ.0) GOTO 9998

      CALL JELIRA(NOML32,'IADD',IADD,KBID)
      CALL JELIRA(NOML32,'IADM',IADM,KBID)
      IF (IADM.EQ.0.AND.IADD.EQ.0) GOTO 9998

9997  CONTINUE
      IRET=1
      GOTO 9999

9998  CONTINUE
      IRET=0
      GOTO 9999

9999  CONTINUE
      END

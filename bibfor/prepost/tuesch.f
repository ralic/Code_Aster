      SUBROUTINE TUESCH(NSSCH)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
      INCLUDE 'jeveux.h'
      CHARACTER*19 NSSCH
C
C**********************************************************************
C
C  OPERATION REALISEE :
C  ------------------
C
C     DESTRUCTION DE LA SD D' UN SOUS_CHAMP_GD
C
C**********************************************************************
C
C   -------------------------
C
C
      INTEGER IRET
C
C====================== CORPS DE LA ROUTINE ========================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEDETR(NSSCH//'.VALE')
      CALL JEDETR(NSSCH//'.PADR')
      CALL JEDETR(NSSCH//'.NOMA')
      CALL JEDETR(NSSCH//'.NUGD')
      CALL JEDETR(NSSCH//'.ERRE')
      CALL JEDETR(NSSCH//'.PCMP')
C
      CALL JEEXIN(NSSCH//'.PNBN',IRET)
C
      IF ( IRET .NE. 0 ) THEN
C
         CALL JEDETR(NSSCH//'.PNBN')
         CALL JEDETR(NSSCH//'.PNCO')
         CALL JEDETR(NSSCH//'.PNSP')

C
      ENDIF
C
      END

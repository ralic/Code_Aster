      SUBROUTINE CFTYLI(RESOCO,ILIAC ,TYPE0 )
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
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       ILIAC, TYPE0
      CHARACTER*24  RESOCO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
C
C RETOURNE LE TYPE DE LA LIAISON SELON UN ENTIER 
C
C ----------------------------------------------------------------------
C
C
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  ILIAC  : NUMERO DE LA LIAISON ACTIVE
C OUT TYPE0  : TYPE DE LA LIAISON
C               1  CONTACT
C               2  FROTTEMENT (2D)
C               3  FROTTEMENT - DIRECTION 1 (3D)
C               4  FROTTEMENT - DIRECTION 2 (3D)
C
C
C
C
      INTEGER      JTYPL
      CHARACTER*2  TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*19 TYPL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES SD CONTACT
C
      TYPL   = RESOCO(1:14)//'.TYPL'
      CALL JEVEUO(TYPL  ,'L',JTYPL )
C 
C --- INITIALISATIONS
C 
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
C 
C --- IDENTIFICATION DE LA LIAISON 
C 
      IF ( ZK8(JTYPL-1+ILIAC).EQ.TYPEC0 ) THEN
         TYPE0 = 1
      ELSE IF ( ZK8(JTYPL-1+ILIAC).EQ.TYPEF0 ) THEN
         TYPE0 = 2
      ELSE IF ( ZK8(JTYPL-1+ILIAC).EQ.TYPEF1 ) THEN
         TYPE0 = 3
      ELSE IF ( ZK8(JTYPL-1+ILIAC).EQ.TYPEF2 ) THEN
         TYPE0 = 4
      ELSE
        CALL ASSERT(.FALSE.)   
      ENDIF
      CALL JEDEMA ()
C 
      END

      SUBROUTINE CARACP(CHAR  )
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8  CHAR
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - SD)
C
C CREATION DES SDS DE DEFINITION DU CONTACT DEDIEES AUX
C PARAMETRES GENERAUX (NE DEPENDANT PAS DE LA ZONE DE CONTACT)
C      
C ----------------------------------------------------------------------
C
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C
C
C
C
      CHARACTER*24 DEFICO
      CHARACTER*24 NDIMCO,PARACR,PARACI
      INTEGER      JDIM  ,JPARCR,JPARCI  
      INTEGER      CFMMVD,ZPARR,ZPARI,ZDIME
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DEFICO = CHAR(1:8)//'.CONTACT'
C   
C --- NOMS SDS
C 
      PARACR = DEFICO(1:16)//'.PARACR'    
      PARACI = DEFICO(1:16)//'.PARACI'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
C     
      ZPARR  = CFMMVD('ZPARR') 
      ZPARI  = CFMMVD('ZPARI')  
      ZDIME  = CFMMVD('ZDIME')               
C         
      CALL WKVECT(PARACR,'G V R',ZPARR      ,JPARCR)
      CALL WKVECT(PARACI,'G V I',ZPARI      ,JPARCI) 
      CALL WKVECT(NDIMCO,'G V I',ZDIME      ,JDIM  )
C
      CALL JEDEMA()
C
      END

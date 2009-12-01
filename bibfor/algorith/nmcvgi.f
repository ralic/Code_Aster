      SUBROUTINE NMCVGI(TYPAFF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/04/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*6 TYPAFF           
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE)
C
C ENCAPSULATION DES AFFICHAGES DU TABLEAU DE CONVERGENCE ET INFOS
C
C ----------------------------------------------------------------------
C
C
C IN  TYPAFF : TYPE AFFICHAGE
C               'CVG_OK'  MESSAGE DE CONVERGENCE NORMALE 
C               'CVG_MX'  MESSAGE DE CONVERGENCE SI CRITERE 
C                             RESI_GLOB_RELA ET CHARGEMENT = 0,
C                             ON UTILISE RESI_GLOB_MAXI (MAXREL)
C               'CVG_FO'  MESSAGE DE CONVERGENCE SI CONVERGENCE
C                             FORCEE (ARRET=NON)
C               'CVG_NO'  MESSAGE DE CONVERGENCE SI PAS DE CRITERES DE 
C                             CONVERGENCE 
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C

C 
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C

C
      IF (TYPAFF.EQ.'CVG_OK') THEN
        CALL NMIMPR('IMPR','LIGN_TABL',' ',0.D0,0)
        CALL NMIMPR('IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_RECA',' ',0.D0,0)        
      ELSEIF (TYPAFF.EQ.'CVG_MX') THEN
        CALL NMIMPR('IMPR','LIGN_TABL',' ',0.D0,0)
        CALL NMIMPR('IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_MAXI',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_RECA',' ',0.D0,0) 
      ELSEIF (TYPAFF.EQ.'CVG_NO') THEN
        CALL NMIMPR('IMPR','LIGN_TABL',' ',0.D0,0)
        CALL NMIMPR('IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_NONE',' ',0.D0,0)
      ELSEIF (TYPAFF.EQ.'CVG_FO') THEN
        CALL NMIMPR('IMPR','LIGN_TABL',' ',0.D0,0)
        CALL NMIMPR('IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_FORC',' ',0.D0,0)
        CALL NMIMPR('IMPR','CONV_RECA',' ',0.D0,0)
      ELSE
        CALL ASSERT(.FALSE.)   
      ENDIF     
C     
      CALL JEDEMA()
      END

      SUBROUTINE NMERIM(SDERRO,SDIMPR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24  SDERRO,SDIMPR
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - UTILITAIRE)
C
C GESTION DE LA SD ERREUR - IMPRESSION MESSAGE ERRREUR
C      
C ----------------------------------------------------------------------
C 
C
C IN  SDIMPR : SD AFFICHAGE
C IN  SDERRO : SD ERREUR
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C     
      CHARACTER*24 ERRCOD
      INTEGER      JECOD             
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES SD ERROR
C
      ERRCOD = SDERRO(1:19)//'.CODE'
      CALL JEVEUO(ERRCOD,'E',JECOD)  
C
      IF (ZL(JECOD+1-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','ECHEC_LDC',0.D0,0)
      ELSEIF (ZL(JECOD+2-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','ECHEC_PIL',0.D0,0)
      ELSEIF (ZL(JECOD+3-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','MATR_SING',0.D0,0)
      ELSEIF (ZL(JECOD+4-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','CONT_ERR ',0.D0,0)
      ELSEIF (ZL(JECOD+5-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','CONT_SING',0.D0,0)
      ELSEIF (ZL(JECOD+6-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','EVEN_COLL',0.D0,0)
      ELSEIF (ZL(JECOD+8-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','CPU_NEWT ',0.D0,0) 
      ELSEIF (ZL(JECOD+10-1)) THEN
        CALL NMIMPR(SDIMPR,'IMPR','ERREUR','ITER_MAXI',0.D0,0)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END

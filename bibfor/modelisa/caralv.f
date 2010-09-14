      SUBROUTINE CARALV(CHAR  ,NZOCO ,IFORM )
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  CHAR
      INTEGER      NZOCO,IFORM
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
C
C QUELQUES PARAMETRES GLOBAUX
C      
C ----------------------------------------------------------------------
C
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C IN  IFORM  : TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      IZONE ,MMINFI
      LOGICAL      LMAIL,LGLIS
      LOGICAL      MMINFL,LVERI,LALL,LSANS,LUSUR,LEXIS,LPENA,LNOEU,LXCZM
      CHARACTER*24 DEFICO     
      CHARACTER*24 PARACI
      INTEGER      JPARCI
      LOGICAL      CFDISL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DEFICO = CHAR(1:8)//'.CONTACT' 
      LMAIL  = (IFORM.EQ.1) .OR. (IFORM.EQ.2)
C
C --- ACCES SD CONTACT
C
      PARACI = DEFICO(1:16)//'.PARACI'
      CALL JEVEUO(PARACI,'E',JPARCI)         
C
C --- ALL VERIF ?
C
      IF (LMAIL) THEN
        IZONE  = 1
        LALL   = MMINFL(DEFICO,'VERIF',IZONE)
        DO 18 IZONE = 2,NZOCO   
          LVERI  = MMINFL(DEFICO,'VERIF',IZONE )
          LALL   = LALL.AND.LVERI
 18     CONTINUE
        IF (LALL) THEN
          ZI(JPARCI+8-1) = 1
        ENDIF
C
        LEXIS = .FALSE.
        DO 88 IZONE = 1,NZOCO   
          LVERI  = MMINFL(DEFICO,'VERIF',IZONE )
          LEXIS  = LEXIS.OR.LVERI
 88     CONTINUE         
        IF (LEXIS) THEN
          ZI(JPARCI+23-1) = 1
        ENDIF 
C
C ----- REAC_GEOM= 'SANS' FORCEE SI TOUT EN MODE VERIF
C
        IF (LALL) THEN
          LSANS  = CFDISL(DEFICO,'REAC_GEOM_SANS')
          IF (.NOT. LSANS) THEN
            ZI(JPARCI+1-1) = 0
            CALL U2MESS('I','CONTACT2_3')
          ENDIF
        ENDIF      
      ENDIF
C
C --- Y-A-T IL DE L'USURE ?
C 
      IF (IFORM.EQ.2) THEN
        LEXIS  = .FALSE.
        DO 19 IZONE = 1,NZOCO   
          LUSUR  = MMINFL(DEFICO,'USURE',IZONE )
          LEXIS  = LEXIS.OR.LUSUR
 19     CONTINUE
        IF (LEXIS) THEN
          ZI(JPARCI+9-1) = 1
        ENDIF
      ENDIF 
C
C --- Y-A-T IL DE LA PENALISATION (-> MATRICE NON-SYME) ?
C 
      IF ((IFORM.EQ.2).OR.(IFORM.EQ.3)) THEN
        LEXIS  = .FALSE.
        DO 17 IZONE = 1,NZOCO   
          LPENA  = (MMINFL(DEFICO,'ALGO_CONT_PENA',IZONE ).OR.
     &              MMINFL(DEFICO,'ALGO_FROT_PENA',IZONE ))
          LEXIS  = LEXIS.OR.LPENA
 17     CONTINUE
        IF (LEXIS) THEN
          ZI(JPARCI+22-1) = 1
        ENDIF
      ENDIF
C
C --- TOUT INTEGRE AUX NOEUDS ?
C      
      IF (IFORM.EQ.2) THEN
        IZONE  = 1
        LALL   = (MMINFI(DEFICO,'INTEGRATION'      ,IZONE ).EQ.1)
        DO 58 IZONE = 2,NZOCO   
          LNOEU  = (MMINFI(DEFICO,'INTEGRATION'      ,IZONE ).EQ.1)
          LALL   = LALL.AND.LNOEU
 58     CONTINUE
        IF (LALL) THEN
          ZI(JPARCI+24-1) = 1
        ENDIF
      ELSEIF (IFORM.EQ.1) THEN
        ZI(JPARCI+24-1) = 1
      ELSEIF (IFORM.EQ.3) THEN
        ZI(JPARCI+24-1) = 1        
      ENDIF 
C
C --- Y-A-T IL GLISSIERE ?
C      
      IF (IFORM.EQ.2) THEN        
        LEXIS  = .FALSE.
        DO 27 IZONE = 1,NZOCO   
          LGLIS  = MMINFL(DEFICO,'GLISSIERE_ZONE',IZONE )
          LEXIS  = LEXIS.OR.LGLIS
 27     CONTINUE
        IF (LEXIS) THEN
          ZI(JPARCI+26-1) = 1
        ENDIF
      ENDIF            
C
C --- EXISTE-T-IL AU MOINS UNE ZONE EN XFEM+CZM ?      
C
      IF (IFORM.EQ.3) THEN
        LEXIS  = .FALSE.
        DO 67 IZONE = 1,NZOCO   
          LXCZM  = MMINFL(DEFICO,'CONT_XFEM_CZM',IZONE )
          LEXIS  = LEXIS.OR.LXCZM
 67     CONTINUE
        IF (LEXIS) THEN
          ZI(JPARCI+21-1) = 1
        ENDIF
      ENDIF
C
      CALL JEDEMA()
C
      END

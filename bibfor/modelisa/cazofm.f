      SUBROUTINE CAZOFM(CHAR  ,MOTFAC,IFORM ,NZOCO )
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16 MOTFAC
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
C
C AFFECTATION FORMULATION/METHODE DE CONTACT
C      
C ----------------------------------------------------------------------
C
C
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  METHOD : METHODE DE CONTACT
C IN  IFORM  : TYPE DE FORMULATION DE CONTACT
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
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
      INTEGER      NOC
      CHARACTER*16 ALGOC,ALGOF,FORMUL,FROTT
      INTEGER      ICONT,IFROT,IZONE
      LOGICAL      LFROT
      CHARACTER*24 DEFICO 
      CHARACTER*24 PARACI
      INTEGER      JPARCI 
      CHARACTER*16 VALK(3)      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DEFICO = CHAR(1:8)//'.CONTACT' 
      ICONT  = 0
      IFROT  = 0
      IZONE  = 1
      LFROT  = .FALSE.
      FORMUL = ' '
      ALGOC  = ' '
      ALGOF  = ' '
      IF (IFORM.EQ.1) THEN
        FORMUL  =  'DISCRETE'   
      ELSEIF (IFORM.EQ.2) THEN
        FORMUL  =  'CONTINUE'          
      ELSEIF (IFORM.EQ.3) THEN
        FORMUL  =  'XFEM'  
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF         
C 
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C    
      PARACI = DEFICO(1:16)//'.PARACI'
      CALL JEVEUO(PARACI,'E',JPARCI)           
C
C --- LA FORMULATION (UNIQUE !)
C  
      ZI(JPARCI+4-1) = IFORM
C
C --- FROTTEMENT ?
C
      CALL GETVTX(' ','FROTTEMENT',IZONE,1,1,FROTT ,NOC)
      LFROT  = FROTT.EQ.'COULOMB'   
C
C --- RECUPERATION DES METHODES      
C      
      CALL GETVTX(MOTFAC,'ALGO_CONT',IZONE,1,1,ALGOC ,NOC) 
        
      VALK(1) = FORMUL
      VALK(2) = ALGOC
      IF (LFROT) THEN
        CALL GETVTX(MOTFAC,'ALGO_FROT',IZONE,1,1,ALGOF ,NOC)
        VALK(3) = ALGOF
      ENDIF  
C  
      IF (IFORM.EQ.1) THEN
        CALL CAZOUU(MOTFAC,NZOCO ,'ALGO_CONT')
        
        IF (LFROT) THEN
          CALL CAZOUU(MOTFAC,NZOCO ,'ALGO_FROT')
          IF (ALGOF(1:12) .EQ. 'PENALISATION') THEN 
            IFROT  = 1
         
            IF (ALGOC(1:12) .EQ. 'PENALISATION') THEN
              ICONT  = 4
            
            ELSEIF (ALGOC(1:8) .EQ. 'LAGRANGI') THEN
              ICONT  = 5

            ELSE
              CALL U2MESK('F','CONTACT3_3',3,VALK)
            ENDIF
          
          ELSEIF (ALGOF(1:10) .EQ. 'LAGRANGIEN') THEN 
            IFROT  = 2
         
            IF (ALGOC(1:8) .EQ. 'LAGRANGI') THEN
              ICONT  = 5
          
            ELSE
              CALL U2MESK('F','CONTACT3_3',3,VALK)
            ENDIF          
          ELSE
            CALL ASSERT(.FALSE.)   
          ENDIF 
        ELSE
          IFROT  = 0
          
          IF (ALGOC(1:3) .EQ. 'GCP') THEN
            ICONT  = 2

          ELSEIF (ALGOC(1:8) .EQ. 'CONTRAIN') THEN
            ICONT  = 1
            
          ELSEIF (ALGOC(1:8) .EQ. 'PENALISA') THEN
            ICONT  = 4
            
          ELSEIF (ALGOC(1:8) .EQ. 'LAGRANGI') THEN   
            ICONT  = 5
                            
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        
        ENDIF  

      ELSEIF (IFORM.EQ.2) THEN
        ICONT   = 6
        
        IF (LFROT) THEN
          IFROT  = 6
        ELSE
          IFROT  = 0
        ENDIF  
                 
      ELSEIF (IFORM.EQ.3) THEN
        ICONT  = 7

        IF (LFROT) THEN
          IFROT  = 7
        ELSE
          IFROT  = 0
        ENDIF         

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- SAUVEGARDE METHODE CONTACT ET FROTTEMENT
C
      ZI(JPARCI+17-1) = ICONT
      ZI(JPARCI+18-1) = IFROT
C      
      CALL JEDEMA()     
C
      END

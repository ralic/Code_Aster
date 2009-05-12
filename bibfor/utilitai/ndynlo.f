      LOGICAL FUNCTION NDYNLO(SDDYNA,CHAINZ)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 12/05/2009   AUTEUR GREFFET N.GREFFET 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*19  SDDYNA
      CHARACTER*(*) CHAINZ
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C INTERROGE SDDYNA POUR RENVOYER UN BOOLEEN
C      
C ----------------------------------------------------------------------
C
C
C OUT NDYNLO : TRUE SI <CHAINE> FAIT PARTIE DES PROPRIETES DE LA SD
C               FALSE SINON
C IN  SDDYNA : NOM DE LA SD DEDIEE A LA DYNAMIQUE
C IN  CHAINE : PROPRIETE EVENTUELLE DE LA SD DYNAC
C                    = 'DIFF_CENT','TCHAMWA','NEWMARK','THETA_METHODE',
C                      'HHT_NON_MODIFIE','HHT_MODIFIE',
C                      'IMPLICITE', 'EXPLICITE',
C                       'STATIQUE', 'DYNAMQIUE'
C                      'MAT_AMORT','MASS_DIAG',
C                      'MULTI_APPUI','AMOR_MODAL','PROJ_MODAL',
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
      CHARACTER*24 TSCH ,LOSD
      INTEGER      JTSCH,JLOSD
      INTEGER      NDYNIN
      CHARACTER*24 CHAINE
      CHARACTER*16 TYPSCH
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      NDYNLO = .FALSE.
      CHAINE = CHAINZ
C
C --- ACCES OBJET PRINCIPAL SDDYNA
C      
      IF (SDDYNA.EQ.' ') THEN
        IF (CHAINE(1:8).EQ.'STATIQUE') THEN
          NDYNLO = .TRUE.
        ELSE
          NDYNLO = .FALSE.           
        ENDIF 
        GOTO 9999
      ELSE
        TSCH   = SDDYNA(1:15)//'.TYPE_SCH'
        CALL JEVEUO(TSCH,'L',JTSCH)
      ENDIF
      TYPSCH = ZK16(JTSCH+1-1)  
C  
      IF (TYPSCH(1:8).EQ.'STATIQUE') THEN
        IF (CHAINE(1:8).EQ.'STATIQUE') THEN
          NDYNLO = .TRUE.
        ELSE
          NDYNLO = .FALSE.           
        ENDIF 
        GOTO 9999
      ELSE
        IF (CHAINE(1:9).EQ.'DYNAMIQUE') THEN
          NDYNLO = .TRUE.
          GOTO 9999          
        ENDIF 
      ENDIF
C
      LOSD   = SDDYNA(1:15)//'.INFO_SD' 
      CALL JEVEUO(LOSD,'L',JLOSD)  
C
      IF (CHAINE(1:8).EQ.'STATIQUE')THEN
        NDYNLO = .FALSE.
      ELSEIF (CHAINE(1:9).EQ.'DIFF_CENT') THEN
        IF (ZK16(JTSCH+7-1)(1:12).EQ.'DIFF_CENTREE') THEN
          NDYNLO = .TRUE.
        ENDIF  
      ELSEIF (CHAINE(1:7).EQ.'TCHAMWA') THEN
        IF (ZK16(JTSCH+8-1)(1:7).EQ.'TCHAMWA') THEN
          NDYNLO = .TRUE.
        ENDIF  
      ELSEIF (CHAINE(1:7).EQ.'NEWMARK') THEN
        IF (ZK16(JTSCH+2-1)(1:7).EQ.'NEWMARK') THEN
          NDYNLO = .TRUE.
        ENDIF  
        
      ELSEIF (CHAINE(1:15).EQ.'FAMILLE_NEWMARK') THEN
        IF ((ZK16(JTSCH+2-1)(1:7).EQ.'NEWMARK').OR.
     &      (ZK16(JTSCH+7-1)(1:12).EQ.'DIFF_CENTREE').OR.
     &      (ZK16(JTSCH+8-1)(1:7).EQ.'TCHAMWA').OR.
     &      (ZK16(JTSCH+5-1)(1:11).EQ.'HHT_COMPLET').OR.
     &      (ZK16(JTSCH+3-1)(1:3).EQ.'HHT')) THEN
          NDYNLO = .TRUE.
        ELSE 
          NDYNLO = .FALSE.
        ENDIF         
        
      ELSEIF (CHAINE(1:13).EQ.'THETA_METHODE') THEN
        IF (ZK16(JTSCH+4-1)(1:13).EQ.'THETA_METHODE') THEN
          IF (CHAINE(14:18).EQ.'_DEPL') THEN
            IF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.1) THEN
              NDYNLO = .TRUE.
            ELSEIF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.2) THEN
              NDYNLO = .FALSE.    
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ELSEIF (CHAINE(14:18).EQ.'_VITE') THEN
            IF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.2) THEN
              NDYNLO = .TRUE.
            ELSEIF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.1) THEN
              NDYNLO = .FALSE.    
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF 
          ELSE
            NDYNLO = .TRUE. 
          ENDIF  
        ELSE 
          NDYNLO = .FALSE.
        ENDIF   
  
        
      ELSEIF (CHAINE(1:11).EQ.'HHT_COMPLET') THEN
        IF (ZK16(JTSCH+5-1)(1:11).EQ.'HHT_COMPLET') THEN
          NDYNLO = .TRUE.
        ENDIF  
      ELSEIF (CHAINE(1:3).EQ.'HHT') THEN
        IF (ZK16(JTSCH+3-1)(1:3).EQ.'HHT') THEN
          NDYNLO = .TRUE.
        ENDIF  
      ELSEIF (CHAINE(1:9).EQ.'IMPLICITE') THEN
        IF (ZK16(JTSCH+2-1)(1:7) .EQ.'NEWMARK'       .OR.
     &      ZK16(JTSCH+4-1)(1:13).EQ.'THETA_METHODE' .OR.
     &      ZK16(JTSCH+5-1)(1:11).EQ.'HHT_COMPLET'   .OR.
     &      ZK16(JTSCH+3-1)(1:3).EQ.'HHT') THEN
          NDYNLO = .TRUE.
        ENDIF  
      ELSE IF (CHAINE(1:9).EQ.'EXPLICITE') THEN
        IF (ZK16(JTSCH+7-1)(1:12).EQ.'DIFF_CENTREE' .OR.
     &      ZK16(JTSCH+8-1)(1:7) .EQ.'TCHAMWA') THEN
          NDYNLO = .TRUE.
        ENDIF  

C     
     
      ELSEIF (CHAINE(1:9).EQ.'MAT_AMORT') THEN 
        NDYNLO = ZL(JLOSD+1-1)
      ELSEIF (CHAINE(1:11).EQ.'MULTI_APPUI') THEN
        NDYNLO = ZL(JLOSD+2-1)
      ELSEIF (CHAINE(1:10).EQ.'AMOR_MODAL') THEN
        NDYNLO = ZL(JLOSD+3-1)
      ELSEIF (CHAINE(1:9).EQ.'MASS_DIAG') THEN
        NDYNLO = ZL(JLOSD+4-1)
      ELSEIF (CHAINE(1:10).EQ.'PROJ_MODAL') THEN
        NDYNLO = ZL(JLOSD+5-1)
      ELSEIF (CHAINE(1:9 ).EQ.'IMPE_ABSO') THEN
        NDYNLO = ZL(JLOSD+6-1)
      ELSEIF (CHAINE(1:10).EQ.'ONDE_PLANE') THEN
        NDYNLO = ZL(JLOSD+7-1)
      ELSEIF (CHAINE(1:12).EQ.'FORCE_FLUIDE') THEN
        NDYNLO = ZL(JLOSD+8-1)                                       
      ELSEIF (CHAINE(1:9).EQ.'EXPL_GENE') THEN
        NDYNLO = ZL(JLOSD+9-1)  
      ELSEIF (CHAINE(1:6).EQ.'NREAVI') THEN
        NDYNLO = ZL(JLOSD+12-1)
      ELSEIF (CHAINE(1:13).EQ.'RAYLEIGH_KTAN') THEN
        NDYNLO = ZL(JLOSD+13-1)
      ELSEIF (CHAINE(1:11).EQ.'FORMUL_DEPL') THEN
        IF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.1) THEN
          NDYNLO = .TRUE.
        ELSE
          NDYNLO = .FALSE.
        ENDIF
      ELSEIF (CHAINE(1:11).EQ.'FORMUL_VITE') THEN
        IF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.2) THEN
          NDYNLO = .TRUE.
        ELSE
          NDYNLO = .FALSE.
        ENDIF        
      ELSEIF (CHAINE(1:11).EQ.'FORMUL_ACCE') THEN
        IF (NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.3) THEN
          NDYNLO = .TRUE.
        ELSE
          NDYNLO = .FALSE.
        ENDIF            
      ELSEIF (CHAINE.EQ.'MULTI_PAS') THEN
        IF ((ZK16(JTSCH+5-1)(1:11).EQ.'HHT_COMPLET')) THEN
          NDYNLO = .TRUE.
          GOTO 9999
        ELSE
          NDYNLO = .FALSE.
        ENDIF
        
        IF ((NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.2).AND.
     &      ( ZK16(JTSCH+4-1)(1:13).EQ.'THETA_METHODE')) THEN
          NDYNLO = .TRUE.
        ELSE
          NDYNLO = .FALSE.
        ENDIF          
                                                  
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
9999  CONTINUE
C
      CALL JEDEMA()

      END

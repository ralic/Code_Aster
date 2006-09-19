      SUBROUTINE MMALGO(JTABF,IPC,NTPC,XA,XS,
     &                  LVITES,LGLISS,LCOMPL,
     &                  JEU,JEUVIT,ASPERI,LAMBDC,
     &                  MMCVCA,DECOL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER IPC
      INTEGER XA
      INTEGER XS
      INTEGER NTPC
      INTEGER JTABF
      LOGICAL LVITES
      LOGICAL LGLISS
      LOGICAL LCOMPL
      REAL*8  JEU
      REAL*8  JEUVIT
      REAL*8  ASPERI            
      REAL*8  LAMBDC
      LOGICAL MMCVCA
      LOGICAL DECOL    
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : MMMBCA
C ----------------------------------------------------------------------
C
C ALGORITHME DES CONTRAINTES ACTIVES POUR LA METHODE CONTINUE
C TRAITEMENT DES DIFFERENTS CAS
C
C IN  IPC    : NUMERO DU POINT DE CONTACT
C IN  NTPC   : NOMBRE CUMULE DE POINTS DE CONTACT
C IN  JTABF  : ADRESSE JEVEUX DU VECTEUR DEFICO(1:16)//'.TABFIN'
C IN  XA     : POUR LA COMPLIANCE
C              XA = 0 ON N'EST PAS "DANS" LES ASPERITES
C              SI PAS DE COMPLIANCE ACTIVEE: XA VAUT TOUJOURS 1 
C IN  XS     : INDICATEUR DE CONTACT (XS=0: PAS DE CONTACT)
C IN  LVITES : .TRUE. SI FORMULATION EN VITESSE
C IN  LGLISS : .TRUE. SI CONTACT GLISSIERE
C IN  LCOMPL : .TRUE. SI COMPLIANCE
C IN  JEU    : VALEUR DU JEU
C IN  JEUVIT : VALEUR DU GAP DES VITESSES NORMALES
C IN  ASPERI : COEFFICIENT POUR LES ASPERITES (COMPLIANCE)
C IN  LAMBDC : MULTIPLICATEUR DE CONTACT DU POINT DE CONTACT
C OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES 
C              CONTRAINTES ACTIVES
C               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
C OUT DECOL  : VAUT .TRUE. SI LE NOEUD SE DECOLLE DANS LE MODELE
C              DE COMPLIANCE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER CFMMVD,ZTABF
      REAL*8  R8PREM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      ZTABF = CFMMVD('ZTABF')   
C
      IF ((XA.EQ.0) .AND. (XS.EQ.0)) THEN  
C            
C --- LE PC N'EST PAS CONTACTANT          (XS=0) 
C --- LE PC NE TOUCHE PAS LES ASPERITES   (XA=0)
C               
        IF (JEU.GT.ASPERI) THEN
C        
C --- ON TOUCHE LES ASPERITES (-> XA = 1)
C
          ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+21) = 1.D0
          IF (JEU .LE. R8PREM()) THEN
C                
C --- CONTACTANT SI GLISSIERE UNIQUEMENT (-> XS = 1) 
C
            IF (LGLISS) THEN
              ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 1.D0
            ELSE
              ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 0.D0              
            ENDIF
C
C --- ON REBOUCLE
C                
            MMCVCA = .FALSE.           
          ELSE       
C          
C --- CONTACTANT (-> XS = 1)
C            
            ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 1.D0
C
C --- ON NE REBOUCLE PAS SI GLISSIERE
C                
            IF (LGLISS) THEN 
              MMCVCA = .TRUE.  
            ELSE
              MMCVCA = .FALSE.                 
            ENDIF  
          END IF
        ENDIF
      ELSE IF ((XA.EQ.1) .AND. (XS.EQ.0)) THEN
C            
C --- LE PC N'EST PAS CONTACTANT          (XS=0) 
C --- LE PC TOUCHE LES ASPERITES          (XA=1)
C         
        IF (LGLISS) THEN
C                
C --- ON FORCE LE PC CONTACTANT SI GLISSIERE 
C       
          ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 1.D0
          MMCVCA = .FALSE.  
        ENDIF
                     
        IF (JEU .GT. R8PREM()) THEN
C            
C --- LE PC EST CONTACTANT 
C             
          IF (LVITES) THEN
            IF (JEUVIT .GT. 0.D0) THEN
              ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 1.D0
              MMCVCA = .FALSE.               
            END IF
          ELSE
C 
C --- FORMULATION EN DEPLACEMENT: ON REBOUCLE 
C        
            ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 1.D0
            ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+21) = 1.D0
            MMCVCA = .FALSE.           
          END IF             
          IF (LGLISS) THEN      
            MMCVCA = .TRUE.  
          ENDIF   
        ELSEIF (JEU.LT.ASPERI .AND. LCOMPL) THEN
          ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 0.D0
          ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+21) = 0.D0 
        ENDIF           

C        PC CONTACTANT
      ELSE IF ((XA.EQ.1) .AND. (XS.EQ.1)) THEN
C            
C --- LE PC EST       CONTACTANT          (XS=1) 
C --- LE PC TOUCHE LES ASPERITES          (XA=1)
C 
        DECOL =.TRUE.
        IF ((LAMBDC.GT.0) .AND. (.NOT.LGLISS)) THEN
C --- LE PC SE DECOLLE        
          MMCVCA = .FALSE.  
          ZR(JTABF+ZTABF*NTPC+ZTABF*(IPC-1)+13) = 0.D0
        END IF
      ELSE
        CALL UTMESS('F','MMMBCA','ETAT DE CONTACT INCONNU')
      END IF
C
  999 CONTINUE 
C
      CALL JEDEMA()      
      END

      SUBROUTINE MMINFP(IZ,DEFICO,RESOCO,QUESTI,
     &                  IREP,RREP,KREP,LREP)
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
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      INTEGER      IZ
      CHARACTER*(*)QUESTI
      INTEGER      IREP(*)
      REAL*8       RREP(*)
      CHARACTER*24 KREP(*)
      LOGICAL      LREP(*)
C
C ----------------------------------------------------------------------
C ROUTINE UTILITAIRE (CONTACT METHODE CONTINUE)
C ----------------------------------------------------------------------
C
C REPOND A UNE QUESTION SUR UNE OPTION/CARACTERISTIQUE DU CONTACT
C METHODE CONTINUE
C
C IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
C IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT QU'ON INTERROGE
C                0 CARACTERISTIQUE COMMUNE A TOUTES LES ZONES
C IN  QUESTI : QUESTION POSEE
C               'FOND_FISSURE': DETECTION FOND_FISSURE ACTIVEE
C OUT IREP   : VALEUR SI C'EST UN ENTIER
C OUT RREP   : VALEUR SI C'EST UN REEL
C OUT KREP   : VALEUR SI C'EST UNE CHAINE
C OUT LREP   : VALEUR SI C'EST UN BOOLEEN
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      IRET,IZONE
      INTEGER      CFMMVD,ZCMCF,ZMETH,ZTOLE,ZECPD,ZDIRE
      INTEGER      ZPOUD,ZTGDE      
      CHARACTER*24 CARACF,NORLIS,DIRCO,METHCO,TANDEF,TANPOU,ECPDON
      INTEGER      JCMCF,JNORLI,JDIR,JMETH,JTGDEF,JPOUDI,JECPD   
      CHARACTER*24 TOLECO
      INTEGER      JTOLE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C      
      IF (IZ.EQ.0) THEN
        IZONE = 1
      ELSE
        IZONE = IZ
      ENDIF  
C
      CALL JEEXIN(DEFICO(1:16) // '.CARACF',IRET)
      IF (IRET.EQ.0) THEN
        CALL UTMESS('F','MMINFP',
     &              'SD INTROUVABLE (DVLP)')       
      ENDIF
C      
      CARACF = DEFICO(1:16) // '.CARACF'
      NORLIS = DEFICO(1:16) // '.NORLIS'
      DIRCO  = DEFICO(1:16) // '.DIRCO' 
      METHCO = DEFICO(1:16) // '.METHCO'  
      TANDEF = DEFICO(1:16) // '.TANDEF' 
      TANPOU = DEFICO(1:16) // '.TANPOU' 
      ECPDON = DEFICO(1:16) // '.ECPDON' 
      TOLECO = DEFICO(1:16) // '.TOLECO'                               
C
      ZMETH = CFMMVD('ZMETH')
      ZTOLE = CFMMVD('ZTOLE')
      ZCMCF = CFMMVD('ZCMCF') 
      ZECPD = CFMMVD('ZECPD') 
      ZDIRE = CFMMVD('ZDIRE') 
      ZPOUD = CFMMVD('ZPOUD') 
      ZTGDE = CFMMVD('ZTGDE')         
C                        
      IF (QUESTI(1:12).EQ.'FOND_FISSURE') THEN
        CALL JEVEUO(CARACF,'L',JCMCF)      
        IF (ZR(JCMCF+ZCMCF*(IZONE-1)+11) .EQ. 0.D0) THEN
          LREP(1) = .FALSE.
        ELSE
          LREP(1) = .TRUE.        
        ENDIF
      ELSEIF (QUESTI(1:10).EQ.'COMPLIANCE') THEN
        CALL JEVEUO(CARACF,'L',JCMCF)       
        IF (ZR(JCMCF+ZCMCF*(IZONE-1)+7) .EQ. 0.D0) THEN
          LREP(1) = .FALSE.
        ELSE
          LREP(1) = .TRUE.        
        ENDIF 
      ELSEIF (QUESTI(1:14).EQ.'PROJ_NEWT_ITER') THEN
        IREP(1) = 20
      ELSEIF (QUESTI(1:14).EQ.'PROJ_NEWT_EPSI') THEN
        RREP(1) = 1D-4 
      ELSEIF (QUESTI(1:9).EQ.'RESI_FROT') THEN
        RREP(1) = 1D-4
      ELSEIF (QUESTI(1:9).EQ.'RESI_GEOM') THEN
        RREP(1) = 1D-4                                
      ELSEIF (QUESTI(1:13).EQ.'TOLE_PROJ_EXT') THEN
        CALL JEVEUO(TOLECO,'L',JTOLE)      
        RREP(1) = ZR(JTOLE+ZTOLE*(IZONE-1))
      ELSEIF (QUESTI(1:14).EQ.'FLIP_FLOP_IMAX') THEN
        IREP(1) = 20                                
      ELSEIF (QUESTI(1:12).EQ.'CONTACT_INIT') THEN
        CALL JEVEUO(ECPDON,'L',JECPD)       
        IF (ZI(JECPD+ZECPD*(IZONE-1)+5) .EQ. 1) THEN
          LREP(1) = .TRUE.
        ELSE
          LREP(1) = .FALSE.        
        ENDIF           
      ELSEIF (QUESTI(1:7).EQ.'LISSAGE') THEN
        CALL JEVEUO(NORLIS,'L',JNORLI)      
        IF (ZI(JNORLI+IZONE-1+1).EQ.1) THEN
          LREP(1) = .TRUE.
        ELSE
          LREP(1) = .FALSE.        
        ENDIF      
      ELSEIF (QUESTI(1:10).EQ.'SEUIL_INIT') THEN
        CALL JEVEUO(CARACF,'L',JCMCF)       
        RREP(1) = -ABS(ZR(JCMCF+ZCMCF*(IZONE-1)+6))
      ELSEIF (QUESTI(1:11).EQ.'INTEGRATION') THEN
        CALL JEVEUO(CARACF,'L',JCMCF)       
        IREP(1) = NINT(ZR(JCMCF+ZCMCF*(IZONE-1)+1))
      ELSEIF (QUESTI(1:9).EQ.'DIRE_APPA') THEN
        CALL JEVEUO(DIRCO,'L',JDIR)      
        RREP(1) = ZR(JDIR+ZDIRE*(IZONE-1))
        RREP(2) = ZR(JDIR+ZDIRE*(IZONE-1)+1)
        RREP(3) = ZR(JDIR+ZDIRE*(IZONE-1)+2)
        IF ((RREP(1).EQ.0.D0).AND.
     &      (RREP(2).EQ.0.D0).AND.
     &      (RREP(3).EQ.0.D0)) THEN
          LREP(1) = .FALSE.
        ELSE
          LREP(1) = .TRUE.        
        ENDIF        
      ELSEIF (QUESTI(1:13).EQ.'VECT_ORIE_POU') THEN
        CALL JEVEUO(METHCO,'L',JMETH)        
        IF (ZI(JMETH+ZMETH*(IZONE-1)+2).EQ.2) THEN
          CALL JEVEUO(TANPOU,'L',JPOUDI)         
          LREP(1) = .TRUE.
          RREP(1) = ZR(JPOUDI+ZPOUD*(IZONE-1))
          RREP(2) = ZR(JPOUDI+ZPOUD*(IZONE-1)+1)
          RREP(3) = ZR(JPOUDI+ZPOUD*(IZONE-1)+2)
        ELSE
          LREP(1) = .FALSE.
          RREP(1) = 0.D0
          RREP(2) = 0.D0
          RREP(3) = 0.D0          
        ENDIF      
      ELSEIF (QUESTI(1:6).EQ.'VECT_Y') THEN
        CALL JEVEUO(METHCO,'L',JMETH)      
        IF (ZI(JMETH+ZMETH*(IZONE-1)+2).EQ.1) THEN
          CALL JEVEUO(TANDEF,'L',JTGDEF)         
          LREP(1) = .TRUE.
          RREP(1) = ZR(JTGDEF+ZTGDE*(IZONE-1))
          RREP(2) = ZR(JTGDEF+ZTGDE*(IZONE-1)+1)
          RREP(3) = ZR(JTGDEF+ZTGDE*(IZONE-1)+2)
        ELSE
          LREP(1) = .FALSE.
          RREP(1) = 0.D0
          RREP(2) = 0.D0
          RREP(3) = 0.D0           
        ENDIF 
      ELSEIF (QUESTI(1:6).EQ.'VECT_Z') THEN 
        CALL JEVEUO(TANDEF,'L',JTGDEF)        
        RREP(1) = ZR(JTGDEF+ZTGDE*(IZONE-1)+3)
        RREP(2) = ZR(JTGDEF+ZTGDE*(IZONE-1)+4)
        RREP(3) = ZR(JTGDEF+ZTGDE*(IZONE-1)+5)
      ELSEIF (QUESTI(1:8).EQ.'ASPERITE') THEN
        CALL JEVEUO(CARACF,'L',JCMCF)
        RREP(1) = ZR(JCMCF+ZCMCF*(IZONE-1)+8)
      ELSEIF (QUESTI(1:9).EQ.'GLISSIERE') THEN
        CALL JEVEUO(METHCO,'L',JMETH)
        IF (ZI(JMETH+ZMETH*(IZONE-1)+6) .EQ. 8) THEN
          LREP(1) = .TRUE.
        ELSE
          LREP(1) = .FALSE.           
        ENDIF                        
      ELSEIF (QUESTI(1:14).EQ.'ITER_CONT_MAXI') THEN
        CALL JEVEUO(ECPDON,'L',JECPD)      
        IREP(1) = ZI(JECPD+2)                           
      ELSEIF (QUESTI(1:14).EQ.'ITER_FROT_MAXI') THEN
        CALL JEVEUO(ECPDON,'L',JECPD)      
        IREP(1) = ZI(JECPD+3)                           
      ELSEIF (QUESTI(1:14).EQ.'ITER_GEOM_MAXI') THEN
        CALL JEVEUO(ECPDON,'L',JECPD)      
        IREP(1) = ZI(JECPD+4)      
      ELSEIF (QUESTI(1:11).EQ.'FORMUL_VITE') THEN
        CALL JEVEUO(ECPDON,'L',JECPD)
        IF (ZI(JECPD+ZECPD*(IZONE-1)+6).EQ.2) THEN
          LREP(1) = .TRUE.        
        ELSE
          LREP(1) = .FALSE.        
        ENDIF                             
      ELSEIF (QUESTI(1:10).EQ.'FROTTEMENT') THEN
        CALL JEVEUO(CARACF,'L',JCMCF)       
        IREP(1) = NINT(ZR(JCMCF+5)) 
        IF (IREP(1).EQ.1) THEN
          LREP(1) = .FALSE.
        ELSE
          LREP(1) = .TRUE.
        ENDIF                       
      ELSE
        CALL UTMESS('F','MMINFP',
     &              'QUESTION INCONNUE (DVLP)')  
      ENDIF
C
      CALL JEDEMA()      
      END

      SUBROUTINE FOCAAS (NBFON,LNOMF,SURCHG,INTERP,PROLGD,BASE,SORTIE)
      IMPLICIT NONE
      INTEGER             NBFON 
      CHARACTER*1         BASE
      CHARACTER*8         SURCHG, INTERP, PROLGD     
      CHARACTER*(*)       LNOMF(*), SORTIE 
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 12/04/2000   AUTEUR CIBHHAB N.RAHNI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALC_FONCTION: TRAITEMENT DU MOT CLE FACTEUR "ASSE"
C     
C IN  : NBFON  : NOMBRE DE FONCTIONS A CONCATENER
C IN  : LNOMF  : LISTE DES FONCTIONS A CONCATENER
C IN  : SURCHG : SURCHARGE A DROITE OU A GAUCHE
C IN  : INTERP : INTERPOLATION DE LA FONCTION RESULTAT
C IN  : PROLGD : PROLONGEMENT GAUCHE DROIT DE LA FONCTION RESULTAT
C IN  : BASE   : BASE DE LA CREATION DE LA FONCTION RESULTAT
C OUT : SORTIE : FONCTION RESULTAT
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8              XMING,XMIND,R8MAEM,EPS
      INTEGER             I,J,K,NBVAL,LPRO1,LVAR,NBINST,JVAL,ICLAS
      INTEGER             NBPTS,LORDO,ID,KBOO,K1,K2,JTRAV1,JTRAV2
      INTEGER             LORDC,ISMAEM,MAXI,MINI,JTMP
      INTEGER             LPROS,LVAR1,LVAR2,LRES,NBPT1,LTRAV
      CHARACTER*1         K1BID
      CHARACTER*24        PROL, VALE, VALX, PRES      
      LOGICAL             BOOL
      CALL JEMARQ()
C      
      VALE(20:24)  = '.VALE'
      PROL(20:24)  = '.PROL'
      VALX(20:24)  = '.VALX'
      PRES(20:24)  = '.PRES' 
      NBINST = 0  
      XMING = R8MAEM()
      XMIND = -XMING
C     
C     --- VERIFICATION QUE LES FONCTIONS SONT REELLES ---
C
      DO 20 I = 1 , NBFON
         PROL(1:19)  = LNOMF(I)        
         CALL JEVEUO ( PROL, 'L', LPRO1 )
         IF ( ZK8(LPRO1) .NE. 'FONCTION' ) THEN
            CALL UTMESS('F','FOCAAS','FONCTION NON REELLE')
         ENDIF
 20   CONTINUE
C      
C     --- LECTURE DES ABSCISSES ET REMPLISSAGE DES .VALX---
C
      CALL WKVECT('&&FOCAAS.TRAVAIL1','V V R',NBFON,JTRAV1)
      CALL WKVECT('&&FOCAAS.TRAVAIL2','V V I',NBFON,JTRAV2)
      CALL WKVECT('&&FOCAAS.MAXI','V V R',NBFON,MAXI)
      CALL WKVECT('&&FOCAAS.MINI','V V R',NBFON,MINI)
      CALL WKVECT('&&FOCAAS.TMPO','V V R',NBFON,JTMP)            
      DO 30 I = 1 , NBFON
          VALE(1:19) = LNOMF(I)
          CALL JELIRA ( VALE,'LONMAX',NBVAL,K1BID )
          CALL JEVEUO ( VALE,'L',LVAR)
          NBPTS = NBVAL/2
          VALX(1:19) = LNOMF(I)
          CALL WKVECT ( VALX,'V V R',NBPTS,JVAL )
          DO 40 J = 1 , NBPTS
            ZR(JVAL+J-1)=ZR(LVAR+J-1)           
  40      CONTINUE   
          ZR(MAXI + I -1) = ZR(JVAL+NBPTS-1)
          ZR(MINI + I -1) = ZR(JVAL)
          IF (SURCHG.EQ.'DROITE') THEN
             ZR(JTRAV1+I-1) = ZR(MAXI + I -1)
             ZI(JTRAV2+I-1) = I
             ZR(JTMP +I -1) = ZR(MAXI + I -1)
          ELSE IF(SURCHG.EQ.'GAUCHE') THEN
             ZR(JTRAV1+I-1) = ZR(MINI + I -1)
             ZI(JTRAV2+I-1) = I
             ZR(JTMP +I -1) = ZR(MINI + I -1) 
          ELSE
             CALL UTMESS ('F','FOCAAS','ERREUR DONNEES')
          ENDIF
  30  CONTINUE 
           
C
C     --- CLASSEMENT DES FONCTIONS ---
C      SI SURCHARGE=DROITE, CLASSEMENT SUIVANT 
C      LES XMAX LES PLUS GRANDS
C      SINON, SUIVANT LES XMIN LES PLUS PETITS
C             
      CALL WKVECT ('&&FOCAAS.TRAVAIL3','V V I',NBFON,LORDO)
      CALL WKVECT ('&&FOCAAS.TRAVAIL4','V V I',NBFON,LORDC)
C      
      EPS = 0.D0         
      CALL UTTRIR(NBFON,ZR(JTMP),EPS)
      K = 0             
      DO 50 I = 1 , NBFON 
         DO 60 J = 1,NBFON
            IF(ZR(JTMP + I -1).EQ.ZR(JTRAV1+J-1)) THEN
               K=K+1
               ZI(LORDO + K -1) = ZI(JTRAV2+J -1)
               ZI(LORDC + K -1) = ZI(LORDO+ K -1)
            ENDIF 
  60    CONTINUE                                        
  50  CONTINUE
C
      IF (SURCHG.EQ.'DROITE') THEN
         DO 70 I = 1,NBFON
            ZI(LORDO + I -1) = ZI(LORDC + (NBFON - I + 1) -1) 
  70     CONTINUE
      ENDIF 
C
C     ---  REMPLISSAGE DES OBJETS .PRESS ---
C
      DO 80 I = 1 , NBFON
         ID = ZI(LORDO + I -1)
         PRES(1:19) = LNOMF(ID)
         VALX(1:19) = LNOMF(ID)
         CALL JELIRA ( VALX,'LONMAX',NBVAL,K1BID )
         CALL JEVEUO ( VALX,'L', LVAR )         
         CALL WKVECT ( PRES,'V V L',NBVAL,KBOO )        
         IF (SURCHG.EQ.'DROITE') THEN         
           DO 90 J= 1 , NBVAL 
              K1 =  LVAR + NBVAL - J
              IF( ZR( K1 ).LT.XMING ) THEN
                 XMING  = ZR( K1 )
                 BOOL   = .TRUE. 
                 NBINST = NBINST + 1   
              ELSE
                 BOOL   = .FALSE.                 
              ENDIF
              K2 = KBOO + NBVAL - J
              ZL( K2 ) = BOOL                          
  90       CONTINUE 
         ELSE IF (SURCHG.EQ.'GAUCHE') THEN
           DO 100 J= 1 , NBVAL
              IF( ZR(LVAR+J-1).GT.XMIND ) THEN
                 XMIND  = ZR(LVAR+J-1)
                 BOOL   = .TRUE.
                 NBINST = NBINST + 1
              ELSE
                 BOOL   = .FALSE.
              ENDIF 
              ZL(KBOO+J-1)  = BOOL  
  100      CONTINUE                     
         ELSE
           CALL UTMESS ('F','FOCAAS','ERREUR DONNEES')
         ENDIF                   
  80  CONTINUE                      
C      
C     --- CREATION DU .PROL DE LA FONCTION ---
C
      PROL(1:19)  = LNOMF(1)
      CALL JEVEUO ( PROL, 'L', LPRO1 )
      PROL(1:19)  = SORTIE 
      CALL WKVECT ( PROL, BASE//' V K8', 5, LPROS )
      ZK8(LPROS  ) = 'FONCTION'
      ZK8(LPROS+1) = INTERP
      ZK8(LPROS+2) = ZK8(LPRO1+2)
      ZK8(LPROS+3) = ZK8(LPRO1+3)
      ZK8(LPROS+4) = PROLGD
C      
C     ------ DEBUT DU REMPLISSAGE ------
C
      VALE(1:19) = SORTIE
      CALL WKVECT ( VALE, BASE//' V R', 2*NBINST, LRES )
      K=0
      DO 110 I = 1,NBFON
         ID = ZI(LORDC + I -1)
         PRES(1:19) = LNOMF(ID)
         VALE(1:19) = LNOMF(ID)        
         CALL JELIRA ( VALE,'LONMAX',NBVAL,K1BID )       
         CALL JEVEUO ( PRES,'L', LVAR1 )         
         CALL JEVEUO ( VALE,'L', LVAR2 )
         NBVAL = NBVAL/2
         DO 120 J = 1 , NBVAL
         BOOL = ZL(LVAR1+J-1)         
         IF ( BOOL ) THEN
            K = K + 1 
            ZR(LRES+K-1)        = ZR(LVAR2 + J -1)
            ZR(LRES+NBINST+K-1) = ZR(LVAR2 + NBVAL + J -1)      
         ENDIF 
 120     CONTINUE                     
 110   CONTINUE                 
C
      CALL JEDETR('&&FOCAAS.TRAVAIL1')
      CALL JEDETR('&&FOCAAS.TRAVAIL2')
      CALL JEDETR('&&FOCAAS.TRAVAIL3')
      CALL JEDETR('&&FOCAAS.TRAVAIL4')
      CALL JEDETR('&&FOCAAS.MAXI') 
      CALL JEDETR('&&FOCAAS.MINI')
      CALL JEDETR('&&FOCAAS.TMPO')     
      DO 130 I = 1 , NBFON
         PRES(1:19) = LNOMF(I)
         VALX(1:19) = LNOMF(I)      
         CALL JEDETR(VALX)
         CALL JEDETR(PRES) 
 130  CONTINUE
C                
      CALL JEDEMA()
      END

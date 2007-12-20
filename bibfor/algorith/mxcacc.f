      SUBROUTINE MXCACC(SOLVEU,SECMBR,CNFINT,CNDIRI,CNVCPR,
     &                  ACCPLU,MATRIX,LSSTRU,INSTAP,SDDYNA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE BOYERE E.BOYERE
C TOLE CRP_21
C
      IMPLICIT NONE
      REAL*8       INSTAP
      CHARACTER*19 CNFINT,CNDIRI
      CHARACTER*19 MATRIX(2),SOLVEU,SDDYNA
      CHARACTER*24 SECMBR,ACCPLU
      CHARACTER*24 CNVCPR
      LOGICAL      LSSTRU
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - EXPLICITE)
C
C CALCUL DES ACCELERATIONS
C      
C ----------------------------------------------------------------------
C 
C   {An+1}=[M-1]*({Fext}-{Fint})
C
C IN  SOLVEU : SOLVEUR
C IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
C IN  SECMBR : VECTEURS ASSEMBLES DES CHARGEMENTS
C IN  CNFINT : FINT 
C I/O ACCPLU : CORRECTION DE DEPLACEMENT
C IN  MATRIX : MATRICE ASSEMBLEE
C IN  CNVCPR : CHARGEMENT DU AUX VARIABLES DE COMMANDE
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
      INTEGER      NBDONN
      PARAMETER   (NBDONN = 7)
      REAL*8       CODONN(NBDONN)
      CHARACTER*19 CNDONN(NBDONN)
C  
      REAL*8       DDOT
      INTEGER      JRIGGE,JAMOGE
      INTEGER      JACCGM,JVITGM,JDEPGM
      INTEGER      JACCGP,JVITGP,JDEPGP
      INTEGER      JFONGE,JFORGE,JVALFO
      INTEGER      IRET,N,I,IER,IRE2,J
      CHARACTER*19 CRGC,CNSCD0,CNGC0
      CHARACTER*24 CNFEDO,CNFEPI,CNFSDO,CNFSPI,CNDIDO,CNDIPI
      CHARACTER*24 CNCINE,K24BID,ACCGEN,FMODAL,KPRMO,NDYNKK
      INTEGER      NEQ,NFONC,NBMODE
      INTEGER      JSCD0
      INTEGER      JACCGE,JFMODA,JACCP,JMASGE,JBASMO
      CHARACTER*8  K8BID
      LOGICAL      NDYNLO
      INTEGER      IFM,NIV
C
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ...... CALCUL DES ACCELERATIONS' 
      ENDIF      
C
C --- INITIALISATIONS
C
      CALL DESAGG(SECMBR,CNFEDO,CNFEPI,CNDIDO,CNDIPI,
     &            CNFSDO,CNFSPI,K24BID,CNCINE)
      CNSCD0 = '&&CNPART.CHP1'
      CNGC0  = '&&MXCACC.GC0'
      CRGC   = '&&RESGRA_GCPC'  
      CALL JELIRA(CNFEDO(1:19)//'.VALE','LONMAX',NEQ,K8BID)    
C
C -- PREPARATION DU SECOND MEMBRE
C
      CNDONN(1) = CNFEDO(1:19)
      CNDONN(2) = CNFSDO(1:19)
      CNDONN(3) = CNFINT(1:19)
      CNDONN(4) = CNDIPI(1:19)
      CNDONN(5) = CNDIDO(1:19)
      CNDONN(6) = CNVCPR(1:19)
      CNDONN(7) = CNDIRI(1:19)      
      CODONN(1) =  1
      CODONN(2) =  1
      CODONN(3) = -1
      CODONN(4) =  0
      CODONN(5) =  1
      CODONN(6) =  1 
      CODONN(7) = -1

C
C --- CREATION DES SECONDS MEMBRES (CHARGEMENTS FIXES)
C   
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE><RESO> NB SECONDS MEMBRES DONNES :',
     &                 NBDONN
      ENDIF 
C      
      CALL VTZERO(CNSCD0)
      DO 10 N = 1, NBDONN
        CALL VTAXPY(CODONN(N),CNDONN(N),CNSCD0)
        IF (NIV.GE.2) THEN      
          WRITE (IFM,*) '<MECANONLINE><RESO> * SECOND MEMBRE : ',N
          CALL NMDEBG('VECT',CNDONN(N),6)
        ENDIF 
 10   CONTINUE
      CALL JEVEUO(CNSCD0(1:19)//'.VALE','E',JSCD0)
      IF (NIV.GE.2) THEN 
        WRITE (IFM,*) '<MECANONLINE><RESO> -> SECOND MEMBRE'
        CALL NMDEBG('VECT',CNSCD0,6)
      ENDIF 
C 
      IF (NDYNLO(SDDYNA,'PROJ_MODAL')) THEN
      
        KPRMO  = NDYNKK(SDDYNA,'PROJ_MODAL')
        
        CALL JELIRA(KPRMO(1:19)//'.MASG','LONMAX',NBMODE,K8BID)
        CALL JEVEUO(KPRMO(1:19)//'.MASG','L',JMASGE)
        CALL JEVEUO(KPRMO(1:19)//'.BASM','L',JBASMO)
        IF (NIV.GE.2) THEN 
          WRITE (IFM,*) '<MECANONLINE> ...... TERMES MODAUX (',NBMODE,
     &                  ' MODES)'
        ENDIF 
        
        FMODAL = '&&FMODAL'
        CALL JEEXIN(FMODAL,IRET)
        IF (IRET.EQ.0) THEN
          CALL WKVECT(FMODAL,'V V R',NBMODE,JFMODA)
        ELSE
          CALL JEVEUO(FMODAL,'E',JFMODA)
        ENDIF
        IF (LSSTRU) THEN
          IF (NIV.GE.2) THEN 
            WRITE (IFM,*) '<MECANONLINE> ...... MASSES GENERALISEES '
          ENDIF         
          CALL JEVEUO(KPRMO(1:19)//'.RIGG','L',JRIGGE)          
          CALL JEVEUO(KPRMO(1:19)//'.AMOG','L',JAMOGE)
          CALL JEVEUO(KPRMO(1:19)//'.AGEM','L',JACCGM)          
          CALL JEVEUO(KPRMO(1:19)//'.VGEM','L',JVITGM)
          CALL JEVEUO(KPRMO(1:19)//'.DGEM','L',JDEPGM)
          CALL JEVEUO(KPRMO(1:19)//'.VGEP','L',JVITGP)
          CALL JEVEUO(KPRMO(1:19)//'.DGEP','L',JDEPGP)
          CALL JEVEUO(KPRMO(1:19)//'.AGEP','E',JACCGP)
          CALL JEEXIN(KPRMO(1:19)//'.FONG',IRET)
          IF (IRET.EQ.0) THEN
            NFONC = 0
          ELSE
            CALL JEVEUO(KPRMO(1:19)//'.FONG','L',JFONGE)
            CALL JEVEUO(KPRMO(1:19)//'.FORG','L',JFORGE)
            CALL JELIRA(KPRMO(1:19)//'.FONG','LONMAX',NFONC,K8BID)
            CALL JEEXIN('&&VALFON',IRE2)
            IF (IRE2.EQ.0) THEN
              CALL WKVECT('&&VALFON','V V R',NFONC,JVALFO)
            ELSE
              CALL JEVEUO('&&VALFON','E',JVALFO)
            ENDIF
            DO 14 N=1,NFONC
              CALL FOINTE('F ',ZK24(JFONGE+N-1)(1:8),1,'INST',INSTAP,
     &                         ZR(JVALFO+N-1),IER)
 14         CONTINUE                        
          ENDIF
          DO 11 I=1,NBMODE
            ZR(JFMODA+I-1) = DDOT(NEQ,ZR(JBASMO+(I-1)*NEQ),1,
     &                            ZR(JSCD0),1)
            DO 12 J=1,NBMODE
              ZR(JFMODA+I-1) = ZR(JFMODA+I-1)  
     &        - ZR(JRIGGE+(J-1)*NBMODE+I-1)*ZR(JDEPGP+J-1)
     &        - ZR(JAMOGE+(J-1)*NBMODE+I-1)*ZR(JVITGP+J-1)
 12         CONTINUE
            DO 15 N=1,NFONC
              ZR(JFMODA+I-1) = ZR(JFMODA+I-1)  
     &        + ZR(JFORGE+(N-1)*NBMODE+I-1)*ZR(JVALFO+N-1)
 15         CONTINUE
            ZR(JACCGP+I-1) = ZR(JFMODA+I-1)/ZR(JMASGE+I-1) 
 11       CONTINUE
          CALL JEVEUO(ACCPLU(1:19)//'.VALE','E',JACCP)
          CALL MDGEPH(NEQ,NBMODE,ZR(JBASMO),ZR(JACCGP),ZR(JACCP))
        ELSE
          IF (NIV.GE.2) THEN 
            WRITE (IFM,*) '<MECANONLINE> ...... CALCUL DES FORCES '//
     &                     ' MODALES'
          ENDIF         
          ACCGEN = '&&ACCGEN'
          CALL JEEXIN(ACCGEN,IRET)
          IF (IRET.EQ.0) THEN
            CALL WKVECT(ACCGEN,'V V R',NBMODE,JACCGE)
          ELSE
            CALL JEVEUO(ACCGEN,'E',JACCGE)
          ENDIF
          DO 13 I=1,NBMODE
            ZR(JFMODA+I-1) = DDOT(NEQ,ZR(JBASMO+(I-1)*NEQ),1,
     &                            ZR(JSCD0),1)
 13       CONTINUE
          IF (NIV.GE.2) THEN 
            WRITE (IFM,*) '<MECANONLINE> ...... CALCUL DES ACCELER'//
     &                     'ATIONS GENERALISEES'
          ENDIF  
          DO 74 I=1,NBMODE
            ZR(JACCGE+I-1) = ZR(JFMODA+I-1)/ZR(JMASGE+I-1) 
 74       CONTINUE 
          IF (NIV.GE.2) THEN 
            WRITE (IFM,*) '<MECANONLINE> ...... CALCUL DES ACCELER'//
     &                     'ATIONS PHYSIQUES (CONVERSION)'
          ENDIF  
          CALL JEVEUO(ACCPLU(1:19)//'.VALE','E',JACCP)
          CALL MDGEPH(NEQ,NBMODE,ZR(JBASMO),ZR(JACCGE),ZR(JACCP))
        ENDIF
      ELSE
C
C -- INVERSION DES SYSTEMES LINEAIRES
C
        IF (NIV.GE.2) THEN 
          WRITE (IFM,*) '<MECANONLINE><RESO> -> MATRICE'
          CALL NMDEBG('MATA',MATRIX(1),6)
        ENDIF      
        CALL EXISD('CHAMP_GD',CNGC0,IRET)
        IF (IRET.EQ.0) CALL VTDEFS (CNGC0,CNSCD0,'V',' ')
        CALL RESOUD(MATRIX(1),MATRIX(2),CNSCD0, SOLVEU, CNCINE, 
     &              'V'      ,CNGC0    ,CRGC)
        CALL COPISD('CHAMP_GD','V',CNGC0,ACCPLU)
        IF (NIV.GE.2) THEN
          WRITE (IFM,*) '<MECANONLINE><RESO> -> SOLUTION:'
          CALL NMDEBG('VECT',ACCPLU,6)
        ENDIF        
        CALL JEDETR ( CRGC // '.CRTI' )
        CALL JEDETR ( CRGC // '.CRTR' )
        CALL JEDETR ( CRGC // '.CRDE' )
      ENDIF
       
9999  CONTINUE
      CALL JEDEMA()
      END

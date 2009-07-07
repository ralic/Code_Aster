      SUBROUTINE XPRVIT(NOMA,FISS,CNSVT,CNSVN,CNSVV)
      IMPLICIT NONE
      CHARACTER*8    NOMA,FISS
      CHARACTER*19   CNSVT,CNSVN,CNSVV
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/07/2009   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C
C       XPRVIT   : X-FEM PROPAGATION : EXTENSION DU CHAMP DE VITESSES
C       ------     -     --                                  ---       
C    CALCUL DE LA VITESSE DE PROPAGATION DE FISSURE SUR LE FOND
C    ET EXTENSION DU CHAMP DE VITESSE A TOUS LES NOEUDS DU MAILLAGE
C
C    ENTREE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISS    : NOM DU CONCEPT FISSURE X-FEM
C              (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
C
C    SORTIE
C        CNSVT   : CHAM_NO_S VITESSE TANGENTIELLE DE PROPAGATION
C        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION
C	 CNSVV   : CHAM_NO_S NORME DU VECTEUR VITESSE
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
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CHARACTER*1    K1BID
      INTEGER        I,J,IBID,LONG,JCOOR,VALI,IRET,NBNO,JMIN,NBPTFF,
     &               JFONF,JVTFF,JVNFF,JVTL,JVTV,JVNL,JVNV,IFM,NIV,
     &               JVIT,JVVV,JVVL,JBETA,I2,ADDIM,NDIM
      REAL*8         C,M,R8B,BETA,G,EPS,VALR,XM,YM,ZM,R8MAEM,DMIN,SMIN,
     &               XI1,YI1,ZI1,XJ1,YJ1,ZJ1,XIJ,YIJ,ZIJ,XIM,YIM,ZIM,S,
     &               NORM2,XN,YN,ZN,D,K1,K2,BETAX,BETAY
      CHARACTER*8    TABLE,LOI,LIPACR(1),LIPAC2(1),K8B,CTYPE,VALK
      CHARACTER*16   K16BID
      COMPLEX*16     CBID,VALC

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)
      
C  RECUPERATION DES PARAMETRES D'ENTREE DE L'OPERATEUR PROPA_XFEM
      CALL GETVID(' ','TABLE',1,1,1,TABLE,IBID)
      CALL GETVTX('LOI_PROPA','LOI',1,1,1,LOI,IBID)
      IF (LOI.EQ.'PARIS') THEN
          CALL GETVR8('LOI_PROPA','C',1,1,1,C,IBID)
          CALL GETVR8('LOI_PROPA','M',1,1,1,M,IBID)
      ENDIF
      
C  RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C  RECUPERATION DU FOND DE FISSURE
      CALL JEVEUO(FISS//'.FONDFISS','L',JFONF)
      CALL JELIRA(FISS//'.FONDFISS','LONMAX',LONG,K1BID)
      NBPTFF=LONG/4
   
C  CREATION DES VECTEURS DE VITESSE DE PROPAGATION EN FOND DE FISSURE
      CALL WKVECT('&&XPRVIT.VITESSE','V V R8',NBPTFF,JVIT)
      CALL WKVECT('&&XPRVIT.ANGLE','V V R8',NBPTFF,JBETA)
      CALL WKVECT('&&XPRVIT.VT_PROPA_FF','V V R8',NBPTFF,JVTFF)
      CALL WKVECT('&&XPRVIT.VN_PROPA_FF','V V R8',NBPTFF,JVNFF)

C  CREATION DES CHAM_NO_S CONTENANT VT & VN 
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSVT)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSVN)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSVV)
      CALL JEVEUO(CNSVT//'.CNSV','E',JVTV)
      CALL JEVEUO(CNSVT//'.CNSL','E',JVTL)
      CALL JEVEUO(CNSVN//'.CNSV','E',JVNV)
      CALL JEVEUO(CNSVN//'.CNSL','E',JVNL)
      CALL JEVEUO(CNSVV//'.CNSV','E',JVVV)
      CALL JEVEUO(CNSVV//'.CNSL','E',JVVL)

C  CALCUL ET STOCKAGE DE VT ET VN EN FOND DE FISSURE
               
      DO 100 I=1,NBPTFF
         IF (NDIM.EQ.3) THEN
            LIPACR(1)='NUM_PT'
            CALL TBLIVA(TABLE,1,LIPACR,I,R8B,CBID,K8B,K8B,R8B,
     &      'G_LOCAL',CTYPE,VALI,G,VALC,VALK,IRET)
           CALL TBLIVA(TABLE,1,LIPACR,I,R8B,CBID,K8B,K8B,R8B,
     &      'BETA_LOCAL',CTYPE,VALI,BETA,VALC,VALK,IRET)
         ELSEIF (NDIM.EQ.2) THEN
           LIPAC2(1)='INST'
           CALL TBLIVA(TABLE,1,LIPAC2,IBID,1.D0,CBID,K8B,K8B,R8B,
     &       'K1',CTYPE,VALI,K1,VALC,VALK,IRET)
           CALL TBLIVA(TABLE,1,LIPAC2,IBID,1.D0,CBID,K8B,K8B,R8B,
     &       'K2',CTYPE,VALI,K2,VALC,VALK,IRET)
           CALL TBLIVA(TABLE,1,LIPAC2,IBID,1.D0,CBID,K8B,K8B,R8B,
     &      'G',CTYPE,VALI,G,VALC,VALK,IRET) 
           IF (K2.NE.0.D0) THEN
             BETAY=K1/K2-((ABS(K2))/K2)*((K1/K2)**2+8)**0.5D0
             BETAX=4
             BETA = 2*(ATAN2(BETAY,BETAX))
           ELSE 
             BETA=0.D0           
           ENDIF
           WRITE(*,*)'BETA= ',BETA
         ENDIF                 
         ZR(JVIT-1+I)=C*(G**(2*M))
         ZR(JBETA-1+I)=BETA
         ZR(JVTFF-1+I)=ZR(JVIT-1+I)*COS(ZR(JBETA-1+I))
         ZR(JVNFF-1+I)=ZR(JVIT-1+I)*SIN(ZR(JBETA-1+I))         
 100  CONTINUE


 
C     BOUCLE SUR LES NOEUDS M DU MAILLAGE POUR CALCULER PROJ(V)=V
      EPS = 1.D-12
      
      IF (NDIM.EQ.3) THEN
      
       DO 200 I=1,NBNO
C     COORD DU NOEUD M DU MAILLAGE
         XM=ZR(JCOOR-1+(I-1)*3+1)
         YM=ZR(JCOOR-1+(I-1)*3+2)
         ZM=ZR(JCOOR-1+(I-1)*3+3)
         
C     INITIALISATION
         DMIN = R8MAEM()
         JMIN = 0
         SMIN = 0.D0
C     BOUCLE SUR PT DE FONFIS
         DO 210 J=1,NBPTFF-1
C        COORD PT I, ET J
            XI1 = ZR(JFONF-1+4*(J-1)+1)
            YI1 = ZR(JFONF-1+4*(J-1)+2)
            ZI1 = ZR(JFONF-1+4*(J-1)+3)
            XJ1 = ZR(JFONF-1+4*(J-1+1)+1)
            YJ1 = ZR(JFONF-1+4*(J-1+1)+2)
            ZJ1 = ZR(JFONF-1+4*(J-1+1)+3)
C         VECTEUR IJ ET IM
            XIJ = XJ1-XI1
            YIJ = YJ1-YI1
            ZIJ = ZJ1-ZI1
            XIM = XM-XI1
            YIM = YM-YI1
            ZIM = ZM-ZI1
            
C         PARAM S (PRODUIT SCALAIRE...)
            S   = XIJ*XIM + YIJ*YIM + ZIJ*ZIM
            NORM2 = XIJ*XIJ + YIJ*YIJ + ZIJ*ZIJ
            S     = S/NORM2
C         SI N=P(M) SORT DU SEGMENT
            IF((S-1).GE.EPS) S = 1.D0
            IF(S.LE.EPS)     S = 0.D0
C         COORD DE N
            XN = S*XIJ+XI1
            YN = S*YIJ+YI1
            ZN = S*ZIJ+ZI1
C         DISTANCE MN
            D = SQRT((XN-XM)*(XN-XM)+(YN-YM)*(YN-YM)+
     &               (ZN-ZM)*(ZN-ZM))
     
    
            IF(D.LT.DMIN) THEN
              DMIN = D
              JMIN = J
              SMIN = S
            ENDIF

 210      CONTINUE
       
       ZR(JVTV+I-1) = (1-SMIN)*ZR(JVTFF+JMIN-1)+SMIN*ZR(JVTFF+JMIN-1+1)
       ZR(JVNV+I-1) = (1-SMIN)*ZR(JVNFF+JMIN-1)+SMIN*ZR(JVNFF+JMIN-1+1)
       ZL(JVTL+I-1) = .TRUE.
       ZL(JVNL+I-1) = .TRUE.
       ZR(JVVV+I-1) = ((ZR(JVTV+I-1)**2)+(ZR(JVNV+I-1)**2))**0.5D0
       ZL(JVVL+I-1) = .TRUE.
       
 200   CONTINUE
 
      ELSE
             
       DO 400 I=1,NBNO  

         ZR(JVTV+I-1) = ZR(JVTFF)
         ZR(JVNV+I-1) = ZR(JVNFF)
         ZL(JVTL+I-1) = .TRUE.
         ZL(JVNL+I-1) = .TRUE.
         ZR(JVVV+I-1) = ((ZR(JVTV+I-1)**2)+(ZR(JVNV+I-1)**2))**0.5D0
         ZL(JVVL+I-1) = .TRUE.
       
 400   CONTINUE      
 
      ENDIF 
 
C  IMPRESSION DES VITESSES DE PROPAGATION EN INFO=2
C      IF (NIV.GT.1) THEN
         WRITE(IFM,*) 'VITESSE DE PROPAGATION EN FOND DE FISSURE'
         WRITE(IFM,*)  ' NUM_PT    VITESSE         BETA          VT    '
     &               //'        VN'
         DO 310 I=1,NBPTFF
            WRITE(IFM,311) I,ZR(JVIT-1+I),ZR(JBETA-1+I),ZR(JVTFF+I-1),
     &                     ZR(JVNFF+I-1)
 310      CONTINUE
 311      FORMAT(4X,I2,4X,4(D11.5,3X))
C      ENDIF

      CALL JEDETR('&&XPRVIT.VT_PROPA_FF')
      CALL JEDETR('&&XPRVIT.VN_PROPA_FF')
      CALL JEDETR('&&XPRVIT.VITESSE')
      CALL JEDETR('&&XPRVIT.ANGLE')

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END

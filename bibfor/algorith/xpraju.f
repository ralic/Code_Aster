      SUBROUTINE XPRAJU(NOMA,CNSLT,CNSVT,CNSVN,DELTAT)
      IMPLICIT NONE
      CHARACTER*8    NOMA
      CHARACTER*19   CNSVT,CNSVN,CNSLT
      REAL*8         DELTAT

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/08/2006   AUTEUR MASSIN P.MASSIN 
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
C       XPRAJU   : X-FEM PROPAGATION : AJUSTEMENT DE VT
C       ------     -     --            --- 
C    AJUSTEMENT DU CHAMP DE VITESSE VT :
C          SI  LT >=0 , VN AJUSTEE = HEAVISDE(LST)*(VN*LST)/(VT*DELTAT)
C
C    ENTREE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
C        CNSVT   : CHAM_NO_S VITESSE TANGENTIELLE DE PROPAGATION
C        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION
C        DELTAT  : PAS DE TEMPS CHOISI
C
C    SORTIE
C        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION AJUSTEE
C
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

      CHARACTER*8    K8B
      INTEGER        I,IRET,NBNO,JVTNO,JVNNO,JLTNO,IFM,NIV,CPTZO,CPTAJU
      REAL*8         NORMV,HEAV

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C  RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)

C   RECUPERATION DES ADRESSES DES CHAMPS DE VITESSE AUX NOEUDS
      CALL JEVEUO(CNSVT//'.CNSV','L',JVTNO)
      CALL JEVEUO(CNSVN//'.CNSV','E',JVNNO)
 
C   RECUPERATION DE L'ADRESSE DES VALEURS DE LST
      CALL JEVEUO(CNSLT//'.CNSV','L',JLTNO)

      CPTZO = 0
      CPTAJU = 0
C   BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE
      DO 100 I=1,NBNO

C   CALCUL DE HEAVISIDE(LST)
         IF (ZR(JLTNO+I-1).GT.(0.0D0)) THEN
            HEAV=1.D0
            CPTAJU=CPTAJU+1
         ELSE
            HEAV=0.D0
            CPTZO=CPTZO+1
         ENDIF
C   AJUSTEMENT DE LA VALEUR DE VN
         ZR(JVNNO+I-1) =
     &     HEAV*ZR(JVNNO+I-1)*ZR(JLTNO+I-1)/(ZR(JVTNO+I-1)*DELTAT)
     
 100  CONTINUE
 
C      IF (NIV.GT.1) THEN
         WRITE(IFM,*)'NOMBRE DE NOEUDS DONT VN EST ANNULEE :',CPTZO
         WRITE(IFM,*)'NOMBRE DE NOEUDS DONT VN EST AJUSTEE :',CPTAJU
C      ENDIF

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END   

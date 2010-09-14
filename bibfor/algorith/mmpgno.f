      SUBROUTINE MMPGNO(TYPINT,COORMA,COORPC,NNO   ,NDIMG ,
     &                  IPTM  ,INO   )
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
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
      INTEGER     TYPINT
      INTEGER     IPTM,INO
      INTEGER     NNO,NDIMG
      REAL*8      COORMA(27),COORPC(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (UTILITAIRE)
C
C RETOURNE LE NOEUD LE PLUS PROCHE D'UN POINT D'INTEGRATION DONNE
C      
C ----------------------------------------------------------------------
C
C
C IN  TYPINT : TYPE D'INTEGRATION
C                1 NOEUDS
C                2 GAUSS
C                3 SIMPSON
C IN  COORPC : COORDONNEES DU POINT DE CONTACT
C IN  COORMA : COORDONNEES DE LA MAILLE
C IN  NNO    : NOMBRE DE NOEUDS DE LA MAILLE
C IN  NDIMG  : DIMENSION DE L'ESPACE
C IN  IPTM   : NUMERO DU POINT D'INTEGRATION DANS LA MAILLE
C OUT INO    : NUMERO DU NOEUD LE PLUS PROCHE DANS LA MAILLE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      REAL*8       DIST(3),DISTL,DISTM,R8MAEM
      REAL*8       COORNO(3)
      INTEGER      I,IDIM
C
C ----------------------------------------------------------------------
C 
      CALL JEMARQ()
C
C --- INTEGRATION AUX NOEUDS:DIRECT
C
      IF (TYPINT.EQ.1) THEN
        INO = IPTM
        GOTO 99
      ENDIF      
C
C --- INITIALISATIONS
C
      DISTM  = R8MAEM()
      INO    = 0
      DO 12 IDIM  = 1,3
        DIST(IDIM)   = 0.D0 
        COORNO(IDIM) = 0.D0
   12 CONTINUE
C
C --- CHOIX MINIMUM
C
      DO 10 I = 1,NNO
        DO 130 IDIM = 1,NDIMG
          COORNO(IDIM)  = COORMA(3*(I-1)+IDIM)
  130   CONTINUE              
        DO 140 IDIM = 1,NDIMG
          DIST(IDIM) = COORNO(IDIM) - COORPC(IDIM)
  140   CONTINUE        
        DISTL = SQRT(DIST(1)**2+DIST(2)**2+DIST(3)**2)     
        IF (DISTL.LE.DISTM) THEN
          INO = I
        ENDIF
   10 CONTINUE 
C
      IF ((INO.LE.0).OR.(INO.GT.NNO)) THEN
        CALL ASSERT(.FALSE.)
      ENDIF  
C
  99  CONTINUE        
C
      CALL JEDEMA()     
C
      END

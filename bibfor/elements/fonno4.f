      SUBROUTINE FONNO4 (MACOFO,NOMA,NBMAC,TABLEV,NOE,NBNOFF,INDIC)
      IMPLICIT NONE
      CHARACTER*8         NOMA
      CHARACTER*19        MACOFO
      INTEGER             NBMAC,TABLEV(2),NOE(4,4),NBNOFF,INDIC(4)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/09/2011   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       FILTRE DES FACES LIBRES
C       ----------------------------------------------------
C    ENTREES
C       MACOFO : VECTEUR DES MAILLES (PRINCIPALES) CONNECTEES AU SEGMENT
C                DU FOND DE FISSURE COURANT
C       NOMA   : NOM DU MAILLAGE
C       NBMAC : NOMBRE DE MAILLES CONNECTEES AU SEGMENT DU FOND ET DE
C               DE DIMENSION NDIM
C       TABLEV : VECTEUR CONTNANT LES NUMEROS DES DEUX MAILLES 
C                CONNECTEES AU NOEUD SOMMET COURANT ET AUX LEVRES
C       NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
C                MAILLES CONNECTEES AU NOEUD SOMMET COURANT 
C                ET AUX LEVRES
C       NBNOFF : NOMBRE DE NOEUD EN FOND DE FISSURE
C    SORTIE
C       INDIC  : INDICE DES FACES INTERNES

C       ----------------------------------------------------
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
      CHARACTER*32     JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER     JMACO,IATYMA,IAMASE,ITYP,IRET
      INTEGER     COMP5,IMA,INP,INQ,COMPTE,NN,I,J,COMP,INO1
      CHARACTER*8 K8B, TYPE

C     -----------------------------------------------------------------
C           
      CALL JEMARQ() 

C
C     RECUPERATION DE L'ADRESSE DES TYPFON DE MAILLES
      CALL JEVEUO ( NOMA//'.TYPMAIL','L',IATYMA)
C
C     RECUPERATION DU VECTEUR DES MAILLES CONNECTEES AU SEGMENT DU FOND 
      CALL JEVEUO(MACOFO,'L',JMACO)

      INDIC(1)=0
      INDIC(2)=0
      INDIC(3)=0
      INDIC(4)=0
      COMP5=0
C     ON BALAYE LES MAILLES CONNECTEES AU NOEUD INO
      DO 140 IMA=1,NBMAC
C       POUR CHAQUE FACE RETENUE
        DO 141 INP=1,4
          COMPTE=0
C         ON NE CONSIDERE QUE LES MAILLES INTERNES AFIN D'ELIMINER
C         LES FACES (EN 3D) OU LES SEGMENTS INTERNES (EN 2D) 
          IF ((ZI(JMACO-1 + IMA).NE.TABLEV(1)).AND.
     &        (ZI(JMACO-1 + IMA).NE.TABLEV(2))) THEN
            ITYP = IATYMA-1+ZI(JMACO-1 + IMA)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
            CALL DISMOI('F','NBNO_TYPMAIL',TYPE,'TYPE_MAILLE',
     &                        NN,K8B,IRET)
            CALL JEVEUO(JEXNUM( NOMA//'.CONNEX',
     &                        ZI(JMACO-1 + IMA)),'L',IAMASE)
C           POUR CHAQUE NOEUD DE LA MAILLE INTERNE
            DO 142 I=1,NN
C             ON COMPTE LE NOMBRE DE NOEUDS COMMUN AVEC LA FACE INP
              DO 143 INO1=1,4
                IF (NOE(INP,INO1).NE.0) THEN
                  IF (ZI(IAMASE-1+I).EQ.NOE(INP,INO1)) THEN
                    COMPTE = COMPTE+1
                  ENDIF
                ENDIF
 143          CONTINUE 
 142        CONTINUE
          ENDIF
C         LES FACES A NE PAS PRENDRE EN COMPTE CAR INTERNE
          IF (((NBNOFF.GT.1).AND.(COMPTE.GE.3)).OR.
     &        ((NBNOFF.EQ.1).AND.(COMPTE.GE.2))) THEN
            COMP5 = COMP5 + 1
            INDIC(COMP5) = INP
          ENDIF
 141    CONTINUE 
 140  CONTINUE
C     CAS PARTICULIER OU AUCUNE MAILLE INTERNE N'EST PRESENTE
      IF ((COMP5.EQ.0).AND.(NBMAC.EQ.2)) THEN
        DO 200 INP=1,4
          DO 201 INQ=1,4
          COMPTE = 0
            IF (INP.NE.INQ) THEN
            DO 202 I=1,4
              DO 203 J=1,4
                IF (NOE(INP,I).NE.0) THEN
                  IF (NOE(INP,I).EQ.NOE(INQ,J)) THEN
                   COMPTE = COMPTE+1
                  ENDIF
                ENDIF
 203          CONTINUE        
 202        CONTINUE
            ENDIF      
          IF (COMPTE.GE.3) THEN
            INDIC(INP)=INQ    
          ENDIF          
 201      CONTINUE
 200    CONTINUE        
      ENDIF


      CALL JEDEMA()
      END

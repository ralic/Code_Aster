      SUBROUTINE TE0289(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/06/2010   AUTEUR CARON A.CARON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C    - FONCTIONS REALISEES :  ROUTINE X-FEM  
C
C          PASSAGE DES CONTRAINTES 
C          DES POINTS DE GAUSS DES SOUS-ELEMENTS :
C            * AUX NOEUDS DES ELEMENTS PARENTS
C              (CHAMP 'SIEF_ELNO_ELGA') ;
C            * AUX SOMMETS (NOEUDS) DES SOUS-ELEMENTS
C              (CHAMP 'SIEF_SENO_SEGA') ;
C          -> OPTION 'SIEF_ELNO_ELGA'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*8  ELREFP,ELRESE(3),FAMI(3)
      REAL*8  VPG(15),VNO(27),TMP,SOMSIG(27,6)
      INTEGER IBID,NDIM,NNOP,NNO,NPG,IVF,JGANO
      INTEGER DDLH,NFE,SINGU,DDLC,NBSIGM,NBSIG,NBSECO(27)
      INTEGER JCNSET,JLONCH,JSIGPG
      INTEGER JOUT1,JOUT2,IDECPG
      INTEGER I,J,NIT,CPT,IT,NSE,ISE,IN,INO,NSEMAX(3),KPG,IC

      DATA    ELRESE /'SE2','TR3','TE4'/
      DATA    FAMI   /'BID','XINT','XINT'/
      DATA    NSEMAX / 2 , 3 , 6 /
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------

C     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM ET NNOP
      CALL ELREF1(ELREFP)
      CALL ELREF4(' ','RIGI',NDIM,NNOP,IBID,IBID,IBID,IBID,IBID,IBID)
      CALL ASSERT(NNOP.LE.27)

C
C     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG, IVF ET JGANO
      CALL ELREF4(ELRESE(NDIM),FAMI(NDIM),IBID,NNO,IBID,NPG,
     &                                          IBID,IVF,IBID,JGANO)

      CALL ASSERT(NPG.LE.15)

C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      CALL XTEINI(NOMTE,DDLH,NFE,SINGU,DDLC,IBID,IBID,IBID,IBID)
C
C     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
      NBSIG = NBSIGM()
      CALL ASSERT(NBSIG.LE.6)

C-----------------------------------------------------------------------
C     RECUPERATION DES ENTREES / SORTIE
C-----------------------------------------------------------------------

      CALL JEVECH('PCNSETO','L',JCNSET)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PCONTRR','L',JSIGPG)

      CALL JEVECH('PSIEFSER','E',JOUT1)
      CALL JEVECH('PSIEFNOR','E',JOUT2)

C     RÉCUPÉRATION DE LA SUBDIVISION L'ÉLÉMENT PARENT EN NIT TETRAS 
      NIT=ZI(JLONCH-1+1)

C-----------------------------------------------------------------------
C     CALCUL DES CONTRAINTES PAR SOUS-ELEMENTS AUX NOEUDS (SENO)
C-----------------------------------------------------------------------

      CPT=0
C     BOUCLE SUR LES NIT TETRAS
      DO 100 IT=1,NIT

C       RÉCUPÉRATION DU DÉCOUPAGE EN NSE SOUS-ÉLÉMENTS 
        NSE=ZI(JLONCH-1+1+IT)

C       BOUCLE SUR LES NSE SOUS-ÉLÉMENTS
        DO 110 ISE=1,NSE

          CPT=CPT+1

C         DEBUT DE LA ZONE MEMOIRE DE SIG  CORRESPONDANTE
          IDECPG = NPG   * ( NSEMAX(NDIM)*(IT-1)+ (ISE-1))

C         BOUCLE NCMP DES CONTRAINTES
          DO 120 IC = 1,NBSIG

            DO 121 KPG = 1,NPG
              VPG(KPG) = ZR(JSIGPG+(KPG-1+IDECPG)*NBSIG+IC-1)
  121       CONTINUE

            CALL PPGAN2(JGANO,1,VPG,VNO)
             
            DO 122 IN=1,NNO
              ZR(JOUT1-1+NBSIG*NNO*(CPT-1)+NBSIG*(IN-1)+IC)=VNO(IN)
 122        CONTINUE

 120      CONTINUE

 110    CONTINUE

 100  CONTINUE

C-----------------------------------------------------------------------
C     CALCUL DES CONTRAINTES PAR ELEMENTS AUX NOEUDS (ELNO)
C-----------------------------------------------------------------------

C     TABLEAUX DE LA SOMME DES CONTRAINTES
      DO 444 I=1,NNOP
        DO 445 J=1,NBSIG
          SOMSIG(I,J)=0
 445    CONTINUE
 444  CONTINUE

C     TABLEAUX DU NOMBRE DE SOUS-ELEMENTS CONNECTES AUX NOEUDS
      DO 446 I=1,NNOP
        NBSECO(I)=0
 446  CONTINUE

      CPT=0
C     BOUCLE SUR LES NIT TETRAS
      DO 200 IT=1,NIT

C       RÉCUPÉRATION DU DÉCOUPAGE EN NSE SOUS-ÉLÉMENTS 
        NSE=ZI(JLONCH-1+1+IT)

C       BOUCLE SUR LES NSE SOUS-ÉLÉMENTS
        DO 210 ISE=1,NSE

          CPT=CPT+1

C         BOUCLE SUR LES 4/3 SOMMETS DU SOUS-TETRA/TRIA
          DO 211 IN=1,NNO
            INO=ZI(JCNSET-1+(NDIM+1)*(CPT-1)+IN)
            IF (INO.LT.1000) THEN
              NBSECO(INO)=NBSECO(INO)+1
              DO 212 IC = 1,NBSIG
                TMP =  ZR(JOUT1-1+NBSIG*NNO*(CPT-1)+NBSIG*(IN-1)+IC)
                SOMSIG(INO,IC)=SOMSIG(INO,IC)+TMP
 212          CONTINUE
            ENDIF
 211      CONTINUE

 210    CONTINUE
 200  CONTINUE

C     MOYENNES DES CONTRAINTES AUX NOEUDS DE L'ELEMENT PARENT
      DO 300 INO = 1,NNOP
        CALL ASSERT(NBSECO(INO).GT.0)
        DO 310 IC = 1,NBSIG
          ZR(JOUT2-1+NBSIG*(INO-1)+IC) = SOMSIG(INO,IC) / NBSECO(INO)
 310    CONTINUE
 300  CONTINUE

      CALL JEDEMA()
      END

      SUBROUTINE CFLISN(MATYP ,NORMMA,LAMBDA,ITRIA ,COEFNO,
     &                  PROJ  ,LISSA ,MNORM )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      CHARACTER*4  MATYP
      REAL*8       LAMBDA(3)
      REAL*8       COEFNO(9)
      CHARACTER*19 NORMMA       
      INTEGER      PROJ
      INTEGER      ITRIA,LISSA
      REAL*8       MNORM(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL)
C
C LISSAGE DES NORMALES MAITRES
C
C ----------------------------------------------------------------------
C
C
C IN  MATYP  : TYPE DE LA MAILLE MAITRE
C                -> SEG2,SEG3,TRI3,TRI6,QUA4,QUA8,QUA9
C IN  LAMBDA : COORDONNEES PARAMETRIQUES DE LA "PROJECTION" M APRES
C                RABATTEMENT DANS LA MAILLE SI ON DEPASSE
C IN  COEFNO : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
C               MAITRES
C IN  ITRIA  : TRIANGLE LE PLUS PROCHE DU NOEUD ESCLAVE QUAND MAILLE 
C                MAITRE EST UN QUADRANGLE
C IN  LISSA  : LISSAGE DES NORMALES
C               0 PAS DE LISSAGE
C               1 LISSAGE
C IN  PROJ   : TYPE DE PROJECTION
C               1 PROJECTION LINEAIRE
C               2 PROJECTION QUADRATIQUE
C IN  NORMMA : NOM DE L'OBJET CONTENANT LES NORMALES AUX NOEUDS MAITRES
C OUT MNORM  : NORMALE A LA MAILLE MAITRE
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
      REAL*8       ZERO,DEMI, UN, DEUX
      PARAMETER  ( ZERO   =  0.0D0  )      
      PARAMETER  ( DEMI   =  0.5D0  )
      PARAMETER  ( UN     =  1.0D0  )
      PARAMETER  ( DEUX   =  2.0D0  )
      REAL*8       KSI1,KSI2,KSI3,COEFTR(4)
      REAL*8       VLISSA(9),NORME
      INTEGER      K,JLISSA
C
C ----------------------------------------------------------------------
C   
      CALL JEMARQ()
C 
      IF (LISSA.EQ.1) THEN 
        CALL JEVEUO(NORMMA,'L',JLISSA)
        DO 10 K=1,9
          VLISSA(K) = ZR(JLISSA-1+K)
  10    CONTINUE
      ELSEIF   (LISSA.EQ.0) THEN 
        GOTO 999
      ELSE
        CALL CFIMPA('CFLISN',1) 
      ENDIF   
C    
      IF (MATYP.EQ.'SEG2') THEN
        MNORM(1) = COEFNO(1)*VLISSA(1) + COEFNO(2)*VLISSA(4)
        MNORM(2) = COEFNO(1)*VLISSA(2) + COEFNO(2)*VLISSA(5)
        MNORM(3) = COEFNO(1)*VLISSA(3) + COEFNO(2)*VLISSA(6)
      ELSE IF (MATYP.EQ.'SEG3') THEN
        MNORM(1) = COEFNO(1)*VLISSA(1) + COEFNO(2)*VLISSA(4) +
     &             COEFNO(3)*VLISSA(7)
        MNORM(2) = COEFNO(1)*VLISSA(2) + COEFNO(2)*VLISSA(5) +
     &             COEFNO(3)*VLISSA(8)
        MNORM(3) = COEFNO(1)*VLISSA(3) + COEFNO(2)*VLISSA(6) +
     &             COEFNO(3)*VLISSA(9)
      ELSE IF (MATYP(1:3).EQ.'TRI') THEN
        MNORM(1) = COEFNO(1)*VLISSA(1) + COEFNO(2)*VLISSA(4) +
     &             COEFNO(3)*VLISSA(7)
        MNORM(2) = COEFNO(1)*VLISSA(2) + COEFNO(2)*VLISSA(5) +
     &             COEFNO(3)*VLISSA(8)
        MNORM(3) = COEFNO(1)*VLISSA(3) + COEFNO(2)*VLISSA(6) +
     &             COEFNO(3)*VLISSA(9)
      ELSEIF (MATYP(1:3).EQ.'QUA') THEN
        KSI1 = LAMBDA(1)
        KSI2 = LAMBDA(2)
        KSI3 = LAMBDA(3)        
        IF ((MATYP.EQ.'QUA4').OR.(PROJ.EQ.1)) THEN
          IF (ITRIA.EQ.1) THEN
            COEFTR(1) = KSI1
            COEFTR(2) = KSI2
            COEFTR(3) = KSI3
            COEFTR(4) = ZERO
          ELSEIF (ITRIA.EQ.2) THEN
            COEFTR(1) = KSI1
            COEFTR(2) = ZERO
            COEFTR(3) = KSI2
            COEFTR(4) = KSI3 
          ELSEIF (ITRIA.EQ.3) THEN
            COEFTR(1) = KSI1
            COEFTR(2) = KSI2
            COEFTR(3) = ZERO
            COEFTR(4) = KSI3    
          ELSEIF (ITRIA.EQ.4) THEN
            COEFTR(1) = ZERO
            COEFTR(2) = KSI1
            COEFTR(3) = KSI2
            COEFTR(4) = KSI3                                     
          ELSE
            CALL CFIMPA('CFLISN',2) 
          ENDIF
        ELSE IF ((MATYP.EQ.'QUA8').AND.(PROJ.EQ.2)) THEN
          COEFTR(1) = - DEMI*(UN-KSI1)*(UN-KSI2)*(DEUX-KSI1-KSI2)
          COEFTR(2) = - DEMI*KSI1*(UN-KSI2)*(UN+KSI1-KSI2)
          COEFTR(3) = - DEMI*KSI1*KSI2*(KSI1+KSI2)
          COEFTR(4) = - DEMI*(UN-KSI1)*KSI2*(UN-KSI1+KSI2)
        ELSE
          CALL U2MESS('F','CONTACT_30')
        ENDIF    
        MNORM(1) = COEFTR(1)*VLISSA(1) + COEFTR(2)*VLISSA(4) +
     &             COEFTR(3)*VLISSA(7)
        MNORM(2) = COEFTR(1)*VLISSA(2) + COEFTR(2)*VLISSA(5) +
     &             COEFTR(3)*VLISSA(8)
        MNORM(3) = COEFTR(1)*VLISSA(3) + COEFTR(2)*VLISSA(6) +
     &             COEFTR(3)*VLISSA(9)
      END IF 
C
      CALL NORMEV(MNORM,NORME)                         
C
  999 CONTINUE
C
      CALL JEDEMA()  
C     
      END

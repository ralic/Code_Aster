      SUBROUTINE FROT02 ( CONTAC, CONR, LIAC, COAD, COEQ, LDSCON, 
     &                    NDIM, NEQ, 
     &                    DELT0, DELTA, DEPDE, MU, NBLIAC, NBLIAI )
      IMPLICIT   NONE
      INTEGER             LIAC(*), COAD(*), COEQ(*), LDSCON, NDIM,  
     &                    NEQ, NBLIAC, NBLIAI
      REAL*8              CONR(*),DELT0(*),DELTA(*),DEPDE(*),MU(*)
      CHARACTER*24        CONTAC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/01/2000   AUTEUR CIBHHLV L.VIVAN 
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
C
C --- SECOND MEMBRE : ON MET  - A.(DELTA0+DEPDEL) DANS MU
C
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
      CHARACTER*32       JEXNUM , JEXNOM
C ---------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
      INTEGER       II, LLIAC, JAD1, JAD2,
     +              JDEC, IND, JJ, NUM1, NUM2, NEQMAX, LLJAC, JCM1A
      REAL*8        XX1, XX2
      COMPLEX*16    CBID
      CHARACTER*19  CM1A
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
      CM1A   = CONTAC(1:14)//'.CM1A'
C            
      DO 25 II = 1,NBLIAI*(NDIM-1)
         MU(II) = 0.D0
 25   CONTINUE
      DO 30 II = 1, NBLIAC*(NDIM-1)
         IF ( II.GT.NBLIAC ) THEN
            LLIAC = LIAC(II-NBLIAC)
         ELSE
            LLIAC = LIAC(II)
         ENDIF
         JAD1 = COAD(2*(LLIAC-1)+2)
         JAD2 = COAD(2*(LLIAC-1)+3)
         JDEC = JAD2 - JAD1 -1
         IND  = (2*NDIM**2+4)*(LLIAC-1)
C
         DO 31 JJ = 1, JDEC
            NUM1 = COEQ(JAD1+JJ+1)
            NUM2 = COEQ(JAD2+JJ+1)
            XX1  = DELT0(NUM1) + DEPDE(NUM1)
            XX2  = DELT0(NUM2) + DEPDE(NUM2)
            IF ( II.GT.NBLIAC ) THEN
               MU(II) = MU(II) - CONR(IND+4*NDIM+JJ)*XX1
     +                         - CONR(IND+5*NDIM+JJ)*XX2
            ELSE
               MU(II) = MU(II) - CONR(IND+2*NDIM+JJ)*XX1
     +                         - CONR(IND+3*NDIM+JJ)*XX2
            ENDIF
 31      CONTINUE
 30   CONTINUE
C
C --- RESOLUTION POUR OBTENIR MU :-A.C-1.AT.MU = - A.(DELTA0+DEPDEL)
C
C        ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C        DE 1 A NBLIAC*(NDIM-1) :
C
      NEQMAX = ZI(LDSCON+2)
      ZI(LDSCON+2) = NBLIAC*(NDIM-1)
      CALL RLDLGG ( LDSCON, MU, CBID, 1 )
      ZI(LDSCON+2) = NEQMAX
C
C --- CALCUL DE DELTA = DELTA0 - C-1.AT.MU
C
      DO 40 II = 1,NEQ
         DELTA(II) = DELT0(II)
 40   CONTINUE
C
C --- CALCUL DU NOUVEL INCREMENT DE DEPLACEMENT AVEC PRISE EN
C --- COMPTE DU GLISSEMENT
C
      DO 41 JJ =1, NBLIAC*(NDIM-1)
         IF ( JJ .GT. NBLIAC ) THEN
            LLJAC = LIAC(JJ-NBLIAC)
            CALL JEVEUO(JEXNUM(CM1A,LLJAC+NBLIAI),'L',JCM1A)
            DO 43 II=1,NEQ
               DELTA(II) = DELTA(II) - ZR(JCM1A-1+II)*MU(JJ)
 43         CONTINUE
            CALL JELIBE(JEXNUM(CM1A,LLJAC+NBLIAI))
         ELSE
            LLJAC = LIAC(JJ)
            CALL JEVEUO(JEXNUM(CM1A,LLJAC),'L',JCM1A)
            DO 42 II=1,NEQ
               DELTA(II) = DELTA(II) - ZR(JCM1A-1+II)*MU(JJ)
 42         CONTINUE
            CALL JELIBE(JEXNUM(CM1A,LLJAC))
         ENDIF
 41   CONTINUE
C
      CALL JEDEMA()
      END

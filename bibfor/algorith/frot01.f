      SUBROUTINE FROT01 ( CONTAC, CINE, LMAT, IDEBUT, NDIM,
     &                    NEQ, NBLIAC, NBLIAI, XJVMAX,XJVMIN )
      IMPLICIT   NONE
      INTEGER             LMAT, IDEBUT, NDIM, NEQ,NBLIAC, NBLIAI
      REAL*8              XJVMAX, XJVMIN
      CHARACTER*24        CONTAC, CINE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2001   AUTEUR CIBHHBC R.FERNANDES 
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
C --- CALCUL DE A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE IDEBUT)
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
      INTEGER       JCONR, JCOAD, JCOEQ, JLIAC, LLIAC, JAD1, JAD2,
     +              JDEC, IND, NUM1, NUM2, JCM1A, JVALE, LLKAC, JVA,
     +              JRCINE, II, JJ, KK, LL
      REAL*8        XF1, XF2, R8PREM
      CHARACTER*19  MATR, CM1A, COAD, COEQ, CONR, LIAC
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- LECTURE DE LA STRUCTURE DE DONNEES DE CONTACT
C
C CONR   : NORMALES ET JEU
C CM1A   : C-1.AT AVEC C MATRICE DE RIGIDITE TANGENTE,
C          ET A MATRICE DE CONTACT (AT SA TRANSPOSEE)
C LIAC   : LISTE DES LIAISONS ACTIVES (PAS DE GLISSEMENT)
C 
      CONR   = CONTAC(1:14)//'.CONR'
      CM1A   = CONTAC(1:14)//'.CM1A'
      COAD   = CONTAC(1:14)//'.COAD'
      COEQ   = CONTAC(1:14)//'.COEQ'
      LIAC   = CONTAC(1:14)//'.LIAC'
      MATR   = CONTAC(1:14)//'.MATR'
C
      CALL JEVEUO ( CONR, 'L', JCONR )
      CALL JEVEUO ( COAD, 'L', JCOAD )
      CALL JEVEUO ( COEQ, 'L', JCOEQ )
      CALL JEVEUO ( LIAC, 'E', JLIAC )
C
      CALL JEVEUO(JEXNUM(MATR//'.VALE',1),'E',JVALE)
C
      XJVMAX = 0.D0
      XJVMIN = 1.D0/R8PREM()
C      
      DO 20 II = IDEBUT, NBLIAC * (NDIM - 1)
C
C ------ MISE A ZERO DE LA COLONNE
C
         IF (II .LE. NBLIAC) THEN
            LLIAC = ZI(JLIAC-1+II)
            CALL JEVEUO(JEXNUM(CM1A,LLIAC),'E',JCM1A)
            DO 201 LL = 1, NEQ
               ZR(JCM1A-1+LL) = 0.0D0
 201        CONTINUE
         ELSE
            LLIAC = ZI(JLIAC-1+II-NBLIAC)
            CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'E',JCM1A)
            DO 202 LL = 1, NEQ
               ZR(JCM1A-1+LL) = 0.0D0
 202        CONTINUE
         ENDIF
         JAD1 = ZI(JCOAD+2*(LLIAC-1)+1)
         JAD2 = ZI(JCOAD+2*(LLIAC-1)+2)
         JDEC = JAD2 - JAD1 -1
         IND  = (2*NDIM**2+4)*(LLIAC-1)
C
C ------ REMPLISSAGE DE AT POUR LES LIAISONS ACTIVES
C
         DO 21 JJ = 1, JDEC
            NUM1 = ZI(JCOEQ+JAD1+JJ)
            NUM2 = ZI(JCOEQ+JAD2+JJ)
            IF ( II.GT.NBLIAC ) THEN
               ZR(JCM1A-1+NUM1) = ZR(JCONR-1+IND+JJ+4*NDIM)
               ZR(JCM1A-1+NUM2) = ZR(JCONR-1+IND+JJ+5*NDIM)
            ELSE
               ZR(JCM1A-1+NUM1) = ZR(JCONR-1+IND+JJ+2*NDIM)
               ZR(JCM1A-1+NUM2) = ZR(JCONR-1+IND+JJ+3*NDIM)
            ENDIF
 21      CONTINUE
C
C ------ CALCUL DE C-1.AT (EN TENANT COMPTE DES CHARGES CINEMATIQUES)
C
         CALL JEVEUO ( CINE(1:19)//'.VALE', 'E', JRCINE )
         CALL NMRLDL ( LMAT, ZR(JRCINE), ZR(JCM1A) )
C
C ------ CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES)
C ------ (STOCKAGE DE LA MOITIE PAR SYMETRIE)
C
         DO 23 KK = 1, II
            IF ( KK.GT.NBLIAC ) THEN
               LLKAC=ZI(JLIAC-1+KK-NBLIAC)
            ELSE
               LLKAC = ZI(JLIAC-1+KK)
            ENDIF
            JVA = JVALE-1+(II-1)*II/2+KK
            ZR(JVA) = 0.0D0
            JAD1 = ZI(JCOAD+2*(LLKAC-1)+1)
            JAD2 = ZI(JCOAD+2*(LLKAC-1)+2)
            JDEC = JAD2 - JAD1 -1
            IND  = (2*NDIM**2+4)*(LLKAC-1)
C
            DO 24 JJ = 1, JDEC
               NUM1 = ZI(JCOEQ+JAD1+JJ)
               NUM2 = ZI(JCOEQ+JAD2+JJ)
               XF1 = ZR(JCM1A+NUM1-1)
               XF2 = ZR(JCM1A+NUM2-1)
C
               IF ( KK.GT.NBLIAC ) THEN
                  ZR(JVA) = ZR(JVA) - ZR(JCONR-1+IND+JJ+4*NDIM)*XF1
     +                              - ZR(JCONR-1+IND+JJ+5*NDIM)*XF2
               ELSE
                  ZR(JVA) = ZR(JVA) - ZR(JCONR-1+IND+JJ+2*NDIM)*XF1
     +                              - ZR(JCONR-1+IND+JJ+3*NDIM)*XF2
               ENDIF
               IF(ABS(ZR(JVA)).GT.XJVMAX) THEN 
                  XJVMAX = ABS(ZR(JVA))
               ELSEIF(ABS(ZR(JVA)).LT.XJVMIN) THEN 
                  XJVMIN = ABS(ZR(JVA))
               ENDIF   
 24         CONTINUE
C
 23      CONTINUE
C
         IF ( II .LE. NBLIAC ) THEN
            CALL JELIBE(JEXNUM(CM1A,LLIAC))
         ELSE
            CALL JELIBE(JEXNUM(CM1A,LLIAC+NBLIAI))
         ENDIF
C
 20   CONTINUE
C
      CALL JEDEMA()
      END

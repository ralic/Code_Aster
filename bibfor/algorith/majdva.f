      SUBROUTINE MAJDVA(NEQ,   REAROT, NURO,   COEVIT, COEACC,
     &                  DEPMOI, DDEPLA, DEPDEL, DEPKM1, VITKM1,
     &                  ACCKM1, DEPPLU, VITPLU, ACCPLU, ROMKM1,
     &                  ROMK)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2001   AUTEUR PABHHHH N.TARDIEU 
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
      IMPLICIT NONE
      LOGICAL REAROT
      INTEGER INDRO,NEQ
      REAL*8  COEVIT,COEACC
      CHARACTER*19  NURO
      CHARACTER*24  DEPMOI,DDEPLA,DEPDEL,DEPKM1,
     &VITKM1,ACCKM1,DEPPLU,VITPLU,ACCPLU,ROMKM1,
     &ROMK  

C FONCTION: MET A JOUR LES CHAM_NO: DEPDEL, DEPKM1, VITKM1, ACCKM1,
C                   DEPPLU, VITPLU, ACCPLU, ROMKM1, ROMK
C
C IN  NEQ   : LONGUEUR DES CHAM_NO
C     REAROT  : LOGICAL: TRUE  S'IL Y A DES DDL DE GRDE ROTATION
C                        FALSE SINON
C     INDRO   : VECTEUR DONNANT LE TYPE DES DDL:
C                  0: TRANSLATION OU PETITE ROTATION
C                  1: GRANDE ROTATION
C     COEVIT  : COEFFICIENT DONNE
C     COEACC  : COEFFICIENT DONNE
C     DEPMOI  : CHAM_NO DE DEPLACEMENTS A L'INSTANT PRECEDENT
C     DDEPLA  : CHAM_NO DE L'INCREMENT DE DEPLAC. DEPUIS L'ITER. PRECED.
C
C OUT DEPDEL  : CHAM_NO DE L'INCREMENT DE DEPLAC. DEPUIS L'INST. PRECED.
C     DEPKM1  : CHAM_NO DE DEPLACEMENTS A L'ITERATION PRECEDENTE
C     VITKM1  : CHAM_NO DE VITESSES     A L'ITERATION PRECEDENTE
C     ACCKM1  : CHAM_NO D'ACCELERATIONS A L'ITERATION PRECEDENTE
C     DEPPLU  : CHAM_NO DE DEPLACEMENTS A L'ITERATION ACTUELLE
C     VITPLU  : CHAM_NO DE VITESSES     A L'ITERATION ACTUELLE
C     ACCPLU  : CHAM_NO D'ACCELERATIONS A L'ITERATION ACTUELLE
C     ROMKM1  : VECTEURS-ROTATION ENTRE L'INST. PRECE. ET L'ITER. PRECE.
C     ROMK    : VECTEURS-ROTATION ENTRE L'INST. PRECE. ET L'ITER. ACTUE.
C
C ------------------------------------------------------------------
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32       JEXNUM
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
      INTEGER  JDEPM,JDDEPL,JDEPDE,JDEPKM,
     &         JVITKM,JACCKM,JDEPP,JVITP,JACCP,JROMKM,
     &         JROMK
      
      INTEGER I,IC,ICOMP,IRAN(3)
      REAL*8  DELTET(3),TETA1 (3),TETA2 (3),DELQUA(4),QUATER(4)
      REAL*8  QIM(3),QIKM1(3),QIK(3),OMKM1(3),OMPKM1(3),DELRO(3),
     &        VECT1(3),VECT2(3),VECT3(3),VECT4(3),ROTM(3,3),
     &        ROTKM(3,3),ROTK(3,3),ROTMT(3,3),ROTKMT(3,3)
C ----------------------------------------------------------------------
      CALL JEMARQ()

C -- RECUPERATION DES ADRESSES
         CALL JEVEUO(DEPMOI(1:19)//'.VALE','L',JDEPM )
         CALL JEVEUO(DEPDEL(1:19)//'.VALE','E',JDEPDE)
         CALL JEVEUO(DEPPLU(1:19)//'.VALE','E',JDEPP )
         CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL)
         CALL JEVEUO(VITPLU(1:19)//'.VALE','E',JVITP )
         CALL JEVEUO(ACCPLU(1:19)//'.VALE','E',JACCP )
         CALL JEVEUO(DEPKM1(1:19)//'.VALE','E',JDEPKM)
         CALL JEVEUO(VITKM1(1:19)//'.VALE','E',JVITKM)
         CALL JEVEUO(ACCKM1(1:19)//'.VALE','E',JACCKM)
         CALL JEVEUO(ROMKM1(1:19)//'.VALE','E',JROMKM)
         CALL JEVEUO(ROMK(1:19)  //'.VALE','E',JROMK )
C
         IF (REAROT) CALL JEVEUO (NURO//'.NDRO','L',INDRO)

C -- MISES A JOUR
      IF (.NOT.REAROT) THEN
         DO 10 I=1,NEQ
            ZR(JDEPDE+I-1) = ZR(JDEPDE+I-1) + ZR(JDDEPL+I-1)
            ZR(JDEPP+I-1) = ZR(JDEPP+I-1) + ZR(JDDEPL+I-1)
            ZR(JVITP+I-1) = ZR(JVITP+I-1) + COEVIT*ZR(JDDEPL+I-1)
            ZR(JACCP+I-1) = ZR(JACCP+I-1) + COEACC*ZR(JDDEPL+I-1)
10       CONTINUE
      ELSE
         ICOMP = 0
         DO 20 I=1,NEQ
            IF (ZI(INDRO+I-1).EQ.0) THEN
               ZR(JDEPDE+I-1) = ZR(JDEPDE+I-1) + ZR(JDDEPL+I-1)
               ZR(JDEPP+I-1)  = ZR(JDEPP+I-1) + ZR(JDDEPL+I-1)
               ZR(JVITP+I-1)  = ZR(JVITP+I-1) + COEVIT*ZR(JDDEPL+I-1)
               ZR(JACCP+I-1)  = ZR(JACCP+I-1) + COEACC*ZR(JDDEPL+I-1)
            ELSE IF (ZI(INDRO+I-1).EQ.1) THEN
               ZR(JDEPKM+I-1) = ZR(JDEPP+I-1)
               ZR(JVITKM+I-1) = ZR(JVITP+I-1)
               ZR(JACCKM+I-1) = ZR(JACCP+I-1)
               ZR(JROMKM+I-1) = ZR(JROMK+I-1)
               ICOMP = ICOMP + 1
               IRAN(ICOMP) = I
               DELTET(ICOMP) = ZR(JDDEPL+I-1)
               TETA1 (ICOMP) = ZR(JDEPP+I-1)
               TETA2 (ICOMP) = ZR(JROMK+I-1)
               IF (ICOMP.EQ.3) THEN
                  ICOMP = 0
                  CALL VROQUA (DELTET,DELQUA)
                  CALL VROQUA (TETA1 ,QUATER)
                  CALL PROQUA (DELQUA,QUATER)
                  CALL QUAVRO (TETA1 ,QUATER)
                  CALL VROQUA (TETA2 ,QUATER)
                  CALL PROQUA (DELQUA,QUATER)
                  CALL QUAVRO (TETA2 ,QUATER)
                  DO 15 IC=1,3
                     ZR(JDEPP+IRAN(IC) -1) = TETA1 (IC)
                     ZR(JDEPDE+IRAN(IC)-1) = TETA1 (IC)
                     ZR(JROMK+IRAN(IC) -1) = TETA2 (IC)
15                CONTINUE
                  DO 16 IC=1,3
                     QIM   (IC) = ZR(JDEPM+IRAN(IC) -1)
                     QIKM1 (IC) = ZR(JDEPKM+IRAN(IC)-1)
                     QIK   (IC) = ZR(JDEPP+IRAN(IC) -1)
                     OMKM1 (IC) = ZR(JVITKM+IRAN(IC)-1)
                     OMPKM1(IC) = ZR(JACCKM+IRAN(IC)-1)
                     DELRO (IC) = ZR(JROMK+IRAN(IC) -1) - 
     &                           ZR(JROMKM+IRAN(IC)-1)
16                CONTINUE
                  CALL MAROTA (QIM   ,ROTM  )
                  CALL MAROTA (QIKM1 ,ROTKM )
                  CALL MAROTA (QIK   ,ROTK  )
                  CALL TRANSP (ROTM  ,3,3,3,   ROTMT ,3)
                  CALL TRANSP (ROTKM ,3,3,3,   ROTKMT,3)
C*** VITESSE ANGULAIRE
                  CALL PROMAT (ROTMT ,3,3,3,DELRO ,3,3,1,   VECT3 )
                  CALL PROMAT (ROTK  ,3,3,3,VECT3 ,3,3,1,   VECT2 )
                  CALL PROMAT (ROTKMT,3,3,3,OMKM1 ,3,3,1,   VECT3 )
                  CALL PROMAT (ROTK  ,3,3,3,VECT3 ,3,3,1,   VECT1 )
C*** ACCELERATION ANGULAIRE
                  CALL PROMAT (ROTKMT,3,3,3,OMPKM1,3,3,1,   VECT4 )
                  CALL PROMAT (ROTK  ,3,3,3,VECT4 ,3,3,1,   VECT3 )
                  DO 17 IC=1,3
                     ZR(JVITP+IRAN(IC)-1) = VECT1(IC) + COEVIT*VECT2(IC)
                     ZR(JACCP+IRAN(IC)-1) = VECT3(IC) + COEACC*VECT2(IC)
17                CONTINUE
               ENDIF
            ELSE
               CALL UTMESS ('F','MAJDVA','LE CHAMP NURO CREE PAR NUROTA'
     &                      //' COMPORTE D''AUTRES VALEURS QUE 0 OU 1')
            ENDIF
20       CONTINUE
      ENDIF
      CALL JEDEMA()
      END

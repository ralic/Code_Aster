      SUBROUTINE TE0552(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE

C----------------------------------------------------------------------
C FONCTION REALISEE: CALCUL DE LA DURETE ASSOCIEE A LA METALLURGIE
C                    EN 3D
C OPTION : 'DURT_ELGA_META' ET 'DURT_ELNO_META'
C - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*24       CHCTE,CHVAL,NOMRES(5)
      CHARACTER*2        CODRET(5)
      CHARACTER*8        ELREFE
      REAL*8             PHASE(5),VALRES(5),ZALPHA,DURTPG(27)
      INTEGER            NNO,IPOIDS,MATOS,NBPG(10),IMATE
C
C
      CALL JEMARQ()
      CALL ELREF1(ELREFE)

C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 100 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  100 CONTINUE
      NNOS  = ZI(JIN+3-1+NBFPG+1)
      NPG1= NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
C
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PPHASIN','L',IPHASI)
      CALL JEVECH('PDURT_R','E',IDURT)
      MATOS = ZI(IMATE)
C
      DO 300 KP=1,NPG1
C
C----RECUPERATION DES CARACTERISTIQUES
        NOMRES(1) = 'F1_DURT'
        NOMRES(2) = 'F2_DURT'
        NOMRES(3) = 'F3_DURT'
        NOMRES(4) = 'F4_DURT'
        NOMRES(5) = 'C_DURT'
        CALL RCVALA(MATOS,'DURT_META',1,'TEMP',0.D0,5,NOMRES,
     &              VALRES,CODRET,'F')
C
C
C----RECUPERATION Z POUR CHAQUE PHASE
        ZALPHA = 0.D0
        DO 10 I=1,4
           PHASE(I) = ZR(IPHASI+7*(KP-1)+I-1)
           ZALPHA = ZALPHA + PHASE(I)
10      CONTINUE
           PHASE(5) = 1-ZALPHA
C
C----CALCUL DE LA DURETE ----------------------------------------------
C
        DURTPG(1+(KP-1)) = 0.D0
        DO 400 I=1,5
        DURTPG(1+(KP-1)) = DURTPG(1+(KP-1))+PHASE(I)*VALRES(I)
400     CONTINUE
        IF ( OPTION .EQ. 'DURT_ELGA_META' ) THEN
           ZR(IDURT+(KP-1))=DURTPG(1+(KP-1))
        ENDIF
300   CONTINUE
C
      IF ( OPTION .EQ. 'DURT_ELNO_META' ) THEN
         IF ( NOMTE(6:13).EQ.'TETRA10 '.OR.NOMTE(6:13).EQ.'HEXA20  '
     &     .OR. NOMTE(6:13).EQ.'PENTA15 '  ) THEN
         NPG1 = NBPG(3)
         IPOIDS = IPOIDS + (NBPG(1)+NBPG(2))*(1+(NDIM+1)*NNO)
      ENDIF
C
      NCMP = 1
      CALL PPGANO (NNOS,NPG1,NCMP,DURTPG,ZR(IDURT))
C
      ENDIF
C
9999  CONTINUE
      CALL JEDEMA()
      END

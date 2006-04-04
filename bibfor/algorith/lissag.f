      SUBROUTINE LISSAG(NOMA,DEFICO,NEWGEO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/04/2006   AUTEUR KHAM M.KHAM 
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
      IMPLICIT NONE
      CHARACTER*8 NOMA
      CHARACTER*24 DEFICO,NEWGEO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : OP0070
C ----------------------------------------------------------------------
C
C STOCKAGE DES NORMALES MOYENNEES AUX NOEUDS CINEMATIQUES : CONSTRUCTION
C DES VECTEURS TANGENTS.
C
C IN  NOMA   : NOM DU MAILLAGE
C METHODE PARCOURS DES NOEUDS DE CONTACT, PARCOURS DES ELEMENTS QUI
C ENTOURANT CE NOEUD MAITRE ET CALCUL DES NORMALES MOYENNEES
C
C VAR DEFICO : SD POUR LA DEFINITION DE CONTACT
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER JMACO,INOE,NUNOE,NBMA,JPOMA,JDEC,JMANO,NUMA
      INTEGER NTNO,POSNO,JNOCO,JNOESC,JDIM,JCOOR,IATYMA,NDIM,I,J,ITYP
      INTEGER IMA,INO,POSMA,NUMNO,NUTYP,JDEC1,JPONO,NO(9),NNO
      INTEGER JNOMA,POSNNO(30),OO
      INTEGER JTGDEF,IZONE,JPOUDI,JMETH,ZMETH,JZOCO
      REAL*8 TAU1(3),TAU2(3),NORMAL(3),NORME1
      REAL*8 VNORM(3),COOR(30),XI,YI,JEU,NORMN,TOL
      CHARACTER*8 ALIAS
      CHARACTER*24 COTANO,COTAMA,NOESCL,TANPOU,METHCO
      CHARACTER*24 NDIMCO,PMANO,PNOMA,MANOCO,NOMACO,TANDEF,NOZOCO
      LOGICAL LDIST
      PARAMETER (ZMETH=8)
C
C ----------------------------------------------------------------------
C
C     LE TABLEAU  NOESCL = DEFICO(1:16)//.'NOESCL' CONTIENT LES NUMEROS
C     ABSOLUS DES NOEUDS ESCLAVES
C
      CALL JEMARQ
C
C   RECUPERATION DE QUELQUES DONNEES
C
      COTAMA = DEFICO(1:16) // '.MAILCO'
      COTANO = DEFICO(1:16) // '.NOEUCO'
      NOESCL = DEFICO(1:16) // '.NOESCL'
      NDIMCO = DEFICO(1:16) // '.NDIMCO'
      PMANO  = DEFICO(1:16) // '.PMANOCO'
      PNOMA  = DEFICO(1:16) // '.PNOMACO'
      MANOCO = DEFICO(1:16) // '.MANOCO'
      NOMACO = DEFICO(1:16) // '.NOMACO'
      TANDEF = DEFICO(1:16) // '.TANDEF'
      METHCO = DEFICO(1:16) // '.METHCO'
      TANPOU = DEFICO(1:16) // '.TANPOU'
      NOZOCO = DEFICO(1:16) // '.NOZOCO'
C
      CALL JEVEUO(COTAMA,'L',JMACO)
      CALL JEVEUO(COTANO,'L',JNOCO)
      CALL JEVEUO(NOESCL,'E',JNOESC)
      CALL JEVEUO(PMANO,'L',JPOMA)
      CALL JEVEUO(MANOCO,'L',JMANO)
      CALL JEVEUO(PNOMA,'L',JPONO)
      CALL JEVEUO(NOMACO,'L',JNOMA)
      CALL JEVEUO(NDIMCO,'L',JDIM)
      CALL JEVEUO(METHCO,'L',JMETH)
      CALL JEVEUO(NOZOCO,'L',JZOCO)
C
C=======================================================================
C
C   POUR LA RECUPERATION DE LA GEOMETRIE DU NOEUD
C   ---------------------------------------------
C
      CALL JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
      NDIM = ZI(JDIM)
      DO 1 I = 1,30
        COOR(I) = 0.D0
 1    CONTINUE
C
C      CALL UTIMSD(IFM,2,.TRUE.,.TRUE.,
C     &     DEFICO(1:16)//'.NOESCL',1,' ')
C
C   BOUCLE SUR TOUS LES NOEUDS
C   --------------------------
C
      NTNO = ZI(JDIM+4)
C
      DO 10 INOE = 1,NTNO
        POSNO = INOE
        IZONE = ZI(JZOCO+POSNO-1)
C
C  SI LE NOEUD N'EST PAS MAITRE ON SORT
C  ------------------------------------
        IF (ZR(JNOESC+10*(POSNO-1)+1) .EQ. -1.D0) GOTO 10
        NUNOE = ZI(JNOCO+POSNO-1)
        DO 15 I = 1,3
          COOR(I) = ZR(JCOOR-1+3*(NUNOE-1)+I)
 15     CONTINUE
C
        NBMA = ZI(JPOMA+POSNO) - ZI(JPOMA+POSNO-1)
        JDEC = ZI(JPOMA+POSNO-1)
C
C
C  BOUCLE SUR LES MAILLES QUI L'ENTOUNENT
C  -------------------------------------
C
        NORMN = 0.D0
        DO 16 I = 1,3
          NORMAL(I) = 0.D0
          VNORM(I) = 0.D0
 16     CONTINUE
C
        DO 20 IMA = 1,NBMA
C
          POSMA = ZI(JMANO+JDEC+IMA-1)
          NUMA = ZI(JMACO+POSMA-1)
          ITYP = IATYMA - 1 + NUMA
          NUTYP = ZI(ITYP)
C
          IF (NUTYP .EQ. 2) THEN
            ALIAS(1:3) = 'SG2'
            NNO = 2
          ELSEIF (NUTYP .EQ. 4) THEN
            ALIAS(1:3) = 'SG3'
            NNO = 3
          ELSEIF (NUTYP .EQ. 7) THEN
            ALIAS(1:3) = 'TR3'
            NNO = 3
          ELSEIF (NUTYP .EQ. 9) THEN
            ALIAS(1:3) = 'TR6'
            NNO = 6
          ELSEIF (NUTYP .EQ. 12) THEN
            ALIAS(1:3) = 'QU4'
            NNO = 4
          ELSEIF (NUTYP .EQ. 14) THEN
            ALIAS(1:3) = 'QU8'
            NNO = 8
          ELSEIF (NUTYP .EQ. 16) THEN
            ALIAS(1:3) = 'QU9'
            NNO = 9
          ELSE
            CALL UTMESS('F','LISSAGE','STOP_1')
          END IF
C  RECUPERATION DE LA GEOMETRIE DES NOEUDS
C  ---------------------------------------
C
          JDEC1 = ZI(JPONO+POSMA-1)
          DO 45 OO = 1,NNO
            POSNNO(OO) = ZI(JNOMA+JDEC1+OO-1)
            NO(OO) = ZI(JNOCO+POSNNO(OO)-1)
 45       CONTINUE
C
C  COORDONNEES DU NEOUD ESCLAVE ET NOEUDS DE LA MAILLE MAITRE
C  ----------------------------------------------------------
C
C
          DO 50 INO = 1,NNO
            COOR(3*(INO)+1) = ZR(JCOOR+3*(NO(INO)-1))
            COOR(3*(INO)+2) = ZR(JCOOR+3*(NO(INO)-1)+1)
            COOR(3*(INO)+3) = ZR(JCOOR+3*(NO(INO)-1)+2)
 50       CONTINUE
C
          TOL=0.D0
          CALL MPROJP(ALIAS,XI,YI,COOR,TAU1,TAU2,JEU,LDIST,TOL)
C
C  CALCUL DE LA NORAMLE MOYENNEE
C  -----------------------------
C
          IF (NDIM .EQ. 2) THEN
            VNORM(1) = TAU1(2)
            VNORM(2) = -TAU1(1)
            VNORM(3) = 0.D0
          ELSEIF (NDIM .EQ. 3) THEN
C
C ---- DEFINITION BASE TANGENTE LOCALE SUR LA MAILLE MAITRE
C      DANS LE CAS DES POUTRES: 
C      ATTENTION A L'ORIENTATION DE VECT_Y ou VECT_ORIE_POU = TAU2
C
              IF (ZI(JMETH+ZMETH*(IZONE-1)+2).EQ.1) THEN
                CALL JEVEUO(TANDEF,'L',JTGDEF)
                TAU2(1) = ZR(JTGDEF+3* (IZONE-1))
                TAU2(2) = ZR(JTGDEF+3* (IZONE-1)+1)
                TAU2(3) = ZR(JTGDEF+3* (IZONE-1)+2)
                CALL NORMEV(TAU2,NORME1)
              ELSE IF (ZI(JMETH+ZMETH*(IZONE-1)+2).EQ.2) THEN
                CALL JEVEUO(TANPOU,'L',JPOUDI)
                TAU2(1) = ZR(JPOUDI+3* (IZONE-1))
                TAU2(2) = ZR(JPOUDI+3* (IZONE-1)+1)
                TAU2(3) = ZR(JPOUDI+3* (IZONE-1)+2)
                CALL NORMEV(TAU2,NORME1)
              END IF
            
              CALL PROVEC(TAU2,TAU1,VNORM)
              
          END IF
C
          DO 60 I = 1,3
            NORMAL(I) = NORMAL(I) + VNORM(I)
 60       CONTINUE
 
C
 20     CONTINUE
C
C      MOYENNATION
C      -----------
C
        DO 70 I = 1,3
          NORMAL(I) = NORMAL(I) / NBMA
 70     CONTINUE
        NORMN = SQRT(NORMAL(1)*NORMAL(1)+NORMAL(2)*NORMAL(2)+
     &               NORMAL(3)*NORMAL(3))
C
        DO 71 I = 1,3
          ZR(JNOESC+10*(INOE-1)+I+1) = NORMAL(I) / (NORMN)
 71     CONTINUE
C
C      CONSTRUCTION DES VECTEURS TANGENTS
C      ----------------------------------
C
        IF (NDIM .EQ. 3) THEN
          IF (ZR(JNOESC+10*(INOE-1)+1+1) .NE. 0.D0) THEN
            TAU1(1) =
     &        -ZR(JNOESC+10*(INOE-1)+2+1)/ZR(JNOESC+10*(INOE-1)+1+1)
            TAU1(2) = 1.D0
            TAU1(3) = 0.D0
          ELSEIF (ZR(JNOESC+10*(INOE-1)+2+1) .NE. 0.D0) THEN
            TAU1(1) = 1.D0
            TAU1(2) =
     &        -ZR(JNOESC+10*(INOE-1)+1+1)/ZR(JNOESC+10*(INOE-1)+2+1)
            TAU1(3) = 0.D0
          ELSEIF (ZR(JNOESC+10*(INOE-1)+3+1) .NE. 0.D0) THEN
            TAU1(1) = 0.D0
            TAU1(2) = 1.D0
            TAU1(3) =
     &        -ZR(JNOESC+10*(INOE-1)+2+1)/ZR(JNOESC+10*(INOE-1)+3+1)
          ELSE
            CALL UTMESS('F','LISSAG',
     &                 'LE VECTEUR NORMALE EST VRAIMENT NUL
     &                 ')
          END IF
          CALL PROVEC(TAU1,NORMAL,TAU2)
          DO 80 I = 1,3
            ZR(JNOESC+10*(INOE-1)+3+1+I) = TAU1(I)
 80       CONTINUE
          DO 81 I = 1,3
            ZR(JNOESC+10*(INOE-1)+6+1+I) = TAU2(I)
 81       CONTINUE
        ELSEIF (NDIM .EQ. 2) THEN
C
          TAU1(1) = -ZR(JNOESC+10*(INOE-1)+2+1)
          TAU1(2) = ZR(JNOESC+10*(INOE-1)+1+1)
          TAU1(3) = 0.D0
          TAU2(1) = 0.D0
          TAU2(2) = 0.D0
          TAU2(3) = 0.D0
          DO 90 I = 1,3
            ZR(JNOESC+10*(INOE-1)+3+1+I) = TAU1(I)
 90       CONTINUE
          DO 91 I = 1,3
            ZR(JNOESC+10*(INOE-1)+6+1+I) = TAU2(I)
 91       CONTINUE
        ELSE
          CALL UTMESS('F','LISSAG','PROBLEM DE DIMENSION')
        END IF
 10   CONTINUE
C      CALL UTIMSD(IFM,2,.TRUE.,.TRUE.,
C     &     DEFICO(1:16)//'.NOESCL',1,' ')
      CALL JEDEMA
      END

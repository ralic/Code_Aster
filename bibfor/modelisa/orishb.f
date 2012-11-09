      SUBROUTINE ORISHB(NOMA)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*8 NOMA
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ORISHB  --  LE BUT EST DE REORIENTER, SI C'EST NECESSAIRE,
C                 LES MAILLES DU GROUPES DE MAILLES DONNES POUR
C                 QUE LES ELEMETNS SHB8 FONCTIONNENT
C      ETAPE 1 : LA FACE DU BAS (1234) DOIT AVOIR UNE NORMALE
C                TOURNEE VERS L'INTÈRIEUR
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMA         IN/OUT   K*      NOM DU MAILLAGE

C.========================= DEBUT DES DECLARATIONS ====================
C -----  VARIABLES LOCALES
      INTEGER IOCC,NG,JJJ,NGR,IGR,NIV,IFM,IMA,NUMA,INO,JGRO,JDES,NBMAIL,
     &        NBNO,NUTYMA,IDTYMA,JCOOR,NUNO(20)
      REAL*8 COOR(60),PS
      CHARACTER*8 K8B,GMAT,TYPEL
      CHARACTER*16 MOTFAC
      CHARACTER*24 GRMAMA,CONNEX
      INTEGER      IARG

C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

      GRMAMA = NOMA//'.GROUPEMA'
      CONNEX = NOMA//'.CONNEX'
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IDTYMA)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      MOTFAC='ORIE_SHB'
      IOCC=1
      CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',IOCC,IARG,0,K8B,NG)
      IF (NG.NE.0) THEN
        NG = -NG
        CALL WKVECT('&&ORISHB.WORK','V V K8',NG,JJJ)
        CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',IOCC,IARG,NG,
     &              ZK8(JJJ),NGR)

C      POUR CHAQUE GROUPE DE MAILLES
        DO 60 IGR = 1,NGR

          GMAT = ZK8(JJJ+IGR-1)

          CALL JELIRA(JEXNOM(GRMAMA,GMAT),'LONUTI',NBMAIL,K8B)
          CALL JEVEUO(JEXNOM(GRMAMA,GMAT),'L',JGRO)

C      POUR CHAQUE MAILLE DU GROUPE

          DO 50 IMA = 1,NBMAIL
            NUMA = ZI(JGRO-1+IMA)
            NUTYMA = ZI(IDTYMA+NUMA-1)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)

            IF (TYPEL(1:5).EQ.'HEXA8') THEN

              CALL JELIRA(JEXNUM(CONNEX,NUMA),'LONMAX',NBNO,K8B)

C RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
              CALL PACOOR(NOMA,NUMA,NBNO,COOR)

              CALL ORISH8(COOR,PS)

C RENUMEROTATION SEULEMENT SI PS < 0

              IF (PS.LT.0.D0) THEN

                CALL JEVEUO(JEXNUM(CONNEX,NUMA),'E',JDES)

                DO 10 INO = 1,NBNO
                  NUNO(INO) = ZI(JDES+INO-1)
   10           CONTINUE

C RENUMERATION ELEMENT 1234 DEVIENT 1432 ET 5678 DEVIENT 5876

                ZI(JDES+1-1) = NUNO(1)
                ZI(JDES+2-1) = NUNO(4)
                ZI(JDES+3-1) = NUNO(3)
                ZI(JDES+4-1) = NUNO(2)
                ZI(JDES+5-1) = NUNO(5)
                ZI(JDES+6-1) = NUNO(8)
                ZI(JDES+7-1) = NUNO(7)
                ZI(JDES+8-1) = NUNO(6)

              END IF
            END IF
            IF (TYPEL(1:6).EQ.'PENTA6') THEN

                CALL JELIRA(JEXNUM(CONNEX,NUMA),'LONMAX',NBNO,K8B)

C RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                CALL PACOOR(NOMA,NUMA,NBNO,COOR)

                CALL ORISH6(COOR,PS)

C RENUMEROTATION SEULEMENT SI PS < 0

                IF (PS.LT.0.D0) THEN

                   CALL JEVEUO(JEXNUM(CONNEX,NUMA),'E',JDES)

                   DO 20 INO = 1,NBNO
                      NUNO(INO) = ZI(JDES+INO-1)
   20              CONTINUE

C RENUMERATION ELEMENT 123 DEVIENT 132 ET 456 DEVIENT 465

                   ZI(JDES+1-1) = NUNO(1)
                   ZI(JDES+2-1) = NUNO(3)
                   ZI(JDES+3-1) = NUNO(2)
                   ZI(JDES+4-1) = NUNO(4)
                   ZI(JDES+5-1) = NUNO(6)
                   ZI(JDES+6-1) = NUNO(5)
                END IF
            END IF
            IF (TYPEL(1:7).EQ.'PENTA15') THEN

                CALL JELIRA(JEXNUM(CONNEX,NUMA),'LONMAX',NBNO,K8B)

C RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                CALL PACOOR(NOMA,NUMA,NBNO,COOR)

                CALL ORIS15(COOR,PS)

C RENUMEROTATION SEULEMENT SI PS < 0

                IF (PS.LT.0.D0) THEN

                   CALL JEVEUO(JEXNUM(CONNEX,NUMA),'E',JDES)
                   DO 30 INO = 1,NBNO
                      NUNO(INO) = ZI(JDES+INO-1)
   30              CONTINUE

C RENUMERATION ELEMENT 123456 DEVIENT 165432 ET 789 DEVIENT 798
C ET 10,11,12,13,14,15 devient 10,15,14,13,12,11

                   ZI(JDES+1-1)  = NUNO(1)
                   ZI(JDES+2-1)  = NUNO(3)
                   ZI(JDES+3-1)  = NUNO(2)
                   ZI(JDES+4-1)  = NUNO(4)
                   ZI(JDES+5-1)  = NUNO(6)
                   ZI(JDES+6-1)  = NUNO(5)
                   ZI(JDES+7-1)  = NUNO(9)
                   ZI(JDES+8-1)  = NUNO(8)
                   ZI(JDES+9-1)  = NUNO(7)
                   ZI(JDES+10-1) = NUNO(10)
                   ZI(JDES+11-1) = NUNO(12)
                   ZI(JDES+12-1) = NUNO(11)
                   ZI(JDES+13-1) = NUNO(15)
                   ZI(JDES+14-1) = NUNO(14)
                   ZI(JDES+15-1) = NUNO(13)
                END IF
            END IF
            IF (TYPEL(1:6).EQ.'HEXA20') THEN

               CALL JELIRA(JEXNUM(CONNEX,NUMA),'LONMAX',NBNO,K8B)

C RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                CALL PACOOR(NOMA,NUMA,NBNO,COOR)

                CALL ORIS20(COOR,PS)

C RENUMEROTATION SEULEMENT SI PS < 0

                IF (PS.LT.0.D0) THEN

                   CALL JEVEUO(JEXNUM(CONNEX,NUMA),'E',JDES)

                   DO 40 INO = 1,NBNO
                      NUNO(INO) = ZI(JDES+INO-1)
   40              CONTINUE

C RENUMERATION ELEMENT 12345678 DEVIENT 18765432 ET
C 9,10,11,12 DEVIENT 9,12,11,10 ET 13,14,15,16,17,18,19,20
C DEVIENT 13,20,19,18,17,16,15,14

                   ZI(JDES+1-1)  = NUNO(1)
                   ZI(JDES+2-1)  = NUNO(4)
                   ZI(JDES+3-1)  = NUNO(3)
                   ZI(JDES+4-1)  = NUNO(2)
                   ZI(JDES+5-1)  = NUNO(5)
                   ZI(JDES+6-1)  = NUNO(8)
                   ZI(JDES+7-1)  = NUNO(7)
                   ZI(JDES+8-1)  = NUNO(6)
                   ZI(JDES+9-1)  = NUNO(12)
                   ZI(JDES+10-1) = NUNO(11)
                   ZI(JDES+11-1) = NUNO(10)
                   ZI(JDES+12-1) = NUNO(9)
                   ZI(JDES+13-1) = NUNO(13)
                   ZI(JDES+14-1) = NUNO(16)
                   ZI(JDES+15-1) = NUNO(15)
                   ZI(JDES+16-1) = NUNO(14)
                   ZI(JDES+17-1) = NUNO(20)
                   ZI(JDES+18-1) = NUNO(19)
                   ZI(JDES+19-1) = NUNO(18)
                   ZI(JDES+20-1) = NUNO(17)
                END IF
            END IF
   50     CONTINUE
   60   CONTINUE
      END IF

      CALL JEDEMA()
      END

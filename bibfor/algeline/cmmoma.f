      SUBROUTINE CMMOMA ( MAILLA, MOMANU, NBNO, NBNOAJ )
      IMPLICIT NONE
      INTEGER                             NBNO, NBNOAJ
      CHARACTER*(*)       MAILLA, MOMANU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     OPERATEUR CREA_MAILLAGE   MOT CLE FACTEUR "MODI_MAILLE"
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       JMAIL, IM, NUMA, JTYP, ITYP, INO, JPT
      INTEGER       ITTR6, ITTR7, ITQU8, ITQU9, JVALE, NUNO,IATYMA
      INTEGER       ITSE3,ITSE4,K
      REAL*8        DDOT, VALR(3),W
      CHARACTER*8   MA, NOMAIL, NONO1, NONO2, NONO3
      CHARACTER*24  TYPMAI, CONNEX, COOVAL,CANOMA,CANONO
      CHARACTER*24  VALK(4)

      REAL*8  COO1(3),COO2(3),COO3(3),THETA,EPSI,T13(3),T32(3)
      REAL*8 NORMEN,NORME1,NORME2,N(3),OM(3),OC(3),C2,C6,T2,T6,T12(3)
      REAL*8 N3M(3),MC(3),MP(3),MR(3),X3(3),X4(3),COSTET,DN1N2
      INTEGER ICOUDE,I,NUNO1,NUNO2,NUNO3,IFM,NIV
C     ------------------------------------------------------------------

      CALL JEMARQ ( )

      MA = MAILLA
      TYPMAI = MA//'.TYPMAIL        '
      CONNEX = MA//'.CONNEX         '
      COOVAL = MA//'.COORDO    .VALE'
      CANOMA = MA//'.NOMMAI'
      CANONO = MA//'.NOMNOE'

      CALL JEVEUO ( MOMANU, 'L', JMAIL )
      CALL JEVEUO ( COOVAL ,'E', JVALE )

      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA6'),ITTR6)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA7'),ITTR7)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD8'),ITQU8)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD9'),ITQU9)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG3'),ITSE3)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG4'),ITSE4)

      DO 10 IM = 1 , NBNOAJ

         NUMA = ZI(JMAIL+IM-1)
         CALL JEVEUO ( TYPMAI, 'E',IATYMA)
         JTYP=IATYMA-1+NUMA
         ITYP = ZI(JTYP)

         IF ( ITYP .EQ. ITTR6 ) THEN
            ZI(JTYP) = ITTR7
            INO = 7

         ELSEIF ( ITYP .EQ. ITQU8 ) THEN
            ZI(JTYP) = ITQU9
            INO = 9

         ELSEIF ( ITYP .EQ. ITSE3 ) THEN
            ZI(JTYP) = ITSE4
            INO = 4

         ENDIF

         CALL JEVEUO ( JEXNUM(CONNEX,NUMA), 'E', JPT )

         IF (ITYP.NE.ITSE3) THEN
            NUNO = NBNO + IM
            ZI(JPT+INO-1) = NUNO

            IF (ITYP.EQ.ITTR6) THEN
C             -- TRIA6_7
              DO 777, K=1,3
                W= 0.D0
                W= W + ZR(JVALE+3*(ZI(JPT-1+1)-1)-1+K) * (-1.D0/9.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+2)-1)-1+K) * (-1.D0/9.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+3)-1)-1+K) * (-1.D0/9.D0)

                W= W + ZR(JVALE+3*(ZI(JPT-1+4)-1)-1+K) * (4.D0/9.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+5)-1)-1+K) * (4.D0/9.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+6)-1)-1+K) * (4.D0/9.D0)

                ZR(JVALE+3*(NUNO-1)-1+K) = W
777           CONTINUE

            ELSEIF (ITYP.EQ.ITQU8) THEN
C             -- QUAD8_9
              DO 778, K=1,3
                W= 0.D0
                W= W + ZR(JVALE+3*(ZI(JPT-1+1)-1)-1+K) * (-1.D0/4.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+2)-1)-1+K) * (-1.D0/4.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+3)-1)-1+K) * (-1.D0/4.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+4)-1)-1+K) * (-1.D0/4.D0)

                W= W + ZR(JVALE+3*(ZI(JPT-1+5)-1)-1+K) * (1.D0/2.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+6)-1)-1+K) * (1.D0/2.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+7)-1)-1+K) * (1.D0/2.D0)
                W= W + ZR(JVALE+3*(ZI(JPT-1+8)-1)-1+K) * (1.D0/2.D0)

                ZR(JVALE+3*(NUNO-1)-1+K) = W
778           CONTINUE

            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF

         ELSE

            DO 30 I = 1 , 3
               NUNO1 = ZI(JPT+1-1)
               COO1(I) = ZR(JVALE-1+3*(NUNO1-1)+I  )
               NUNO2 = ZI(JPT+2-1)
               COO2(I) = ZR(JVALE-1+3*(NUNO2-1)+I  )
               NUNO3 = ZI(JPT+3-1)
               COO3(I) = ZR(JVALE-1+3*(NUNO3-1)+I  )
30          CONTINUE

            CALL VDIFF(3,COO3,COO1,T13)
            CALL VDIFF(3,COO2,COO3,T32)
            CALL VDIFF(3,COO2,COO1,T12)
            CALL NORMEV(T13,NORME1)
            CALL NORMEV(T32,NORME2)
            CALL NORMEV(T12,DN1N2)
            CALL PROVEC(T32,T13,N)
            CALL NORMEV(N,NORMEN)
            EPSI=1.D-4*NORME1

C           VERIF QUE LE 3EME NOEUD EST BIEN AU MILIEU

            IF (ABS(NORME2-NORME1).GT.EPSI) THEN
               CALL JENUNO(JEXNUM(CANOMA,NUMA),NOMAIL)
               CALL JENUNO(JEXNUM(CANONO,NUNO1),NONO1)
               CALL JENUNO(JEXNUM(CANONO,NUNO2),NONO2)
               CALL JENUNO(JEXNUM(CANONO,NUNO3),NONO3)
               CALL INFNIV(IFM,NIV)
               VALR(1) = NORME1
               VALR(2) = NORME2
               VALR(3) = EPSI
               VALK(1) = NONO3
               VALK(2) = NONO1
               VALK(3) = NONO2
               VALK(4) = NOMAIL
               CALL U2MESG('F','ALGELINE_23', 4 ,VALK, 0 ,0, 3, VALR)
            ENDIF

            IF(NORMEN.LE.EPSI) THEN
               ICOUDE=0
               THETA = 0.D0
            ELSE
               ICOUDE=1
               COSTET=DDOT(3,T13,1,T32,1)
               THETA=2.D0*ATAN2(NORMEN,COSTET)
            ENDIF

            IF (ICOUDE.EQ.0) THEN

C           CAS DU TUYAU DROIT
C              SEGMENT N1-N2 DIVISE EN 3
               DO 40 I=1,3
                  X3(I)=COO1(I)+T12(I)*DN1N2/3.D0
                  X4(I)=COO1(I)+2.D0*T12(I)*DN1N2/3.D0
40             CONTINUE

            ELSE

C           CAS DU COUDE

               C2=COS(THETA/2.D0)
               C6=COS(THETA/6.D0)
               T2=TAN(THETA/2.D0)
               T6=TAN(THETA/6.D0)

               DO 50 I=1,3
                  OM(I)=(COO1(I)+COO2(I))*0.5D0
                  N3M(I)=OM(I)-COO3(I)
                  MC(I)=N3M(I)*C2/(1.D0-C2)
                  OC(I)=OM(I)+MC(I)
                  MP(I)=(COO1(I)-OM(I))*T6/T2
                  MR(I)=(COO2(I)-OM(I))*T6/T2
                  X3(I)=OC(I)+(MP(I)-MC(I))*C6/C2
                  X4(I)=OC(I)+(MR(I)-MC(I))*C6/C2
50             CONTINUE
            ENDIF


            NUNO = ZI(JPT-1+3)
            ZR(JVALE+3*(NUNO-1)  ) = X3(1)
            ZR(JVALE+3*(NUNO-1)+1) = X3(2)
            ZR(JVALE+3*(NUNO-1)+2) = X3(3)

            NUNO = NBNO + IM
            ZR(JVALE+3*(NUNO-1)  ) = X4(1)
            ZR(JVALE+3*(NUNO-1)+1) = X4(2)
            ZR(JVALE+3*(NUNO-1)+2) = X4(3)

            ZI(JPT+INO-1) = NUNO

         ENDIF

10    CONTINUE

      CALL JEDEMA ( )
      END

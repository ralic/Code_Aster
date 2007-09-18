      SUBROUTINE VTCOP1 ( CHIN, CHOUT, IER )
      IMPLICIT NONE
      CHARACTER*(*)       CHIN,CHOUT
      INTEGER                          IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     APPELLE PAR LA ROUTINE CHAPEAU VTCOPY
C     RECOPIE LES VALEURS DU CHAM_NO CHIN DANS LE CHAM_NO CHOUT
C
C     PRECAUTIONS D'EMPLOI :
C     - LES CHAM_NOS DOIVENT EXISTER.
C     - LES DDLS DE "LAGRANGE" NE SONT PAS TRAITES SI LES NUMEROTATIONS
C       DES 2 CHAMPS SONT DIFFERENTES.
C     - LES CHAMPS A "REPRESENTATION CONSTANTE" SONT MAL TRAITES.

C     SI LES .REFE DES CHAM_NOS SONT IDENTIQUES, ON RECOPIE
C                  LE .VALE DE "CHIN" DANS "CHOUT"
C     SINON
C        LE "CHOUT" DOIT ETRE CONSTRUIT A PARTIR D'UN NUME_DDL.
C
C     ------------------------------------------------------------------
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER        IBID, IRET, IEQ1, IEQ2, NEQ1, JLAGR, JVALE1, JVALE2
      INTEGER      NUNOE,NEQ2,JDESC1,JDESC2,JREFE1,JREFE2,JDEEQ1,JDEEQ2
      INTEGER      NNOMX,NCPMX,JTRAV1,NUNO2,NUCP2,NUNO1,NUCP1
      CHARACTER*1    K1B, TYP1, TYP2
      CHARACTER*24 VALK(4)
      CHARACTER*19   CH1, CH2

C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
      IER = 0
      CH1  = CHIN
      CH2 = CHOUT
C
      CALL VRREFE ( CH1, CH2, IRET )


C     1. SI LES 2 CHAMPS ONT LES MEMES NUMEROTATIONS :
C     -------------------------------------------------
      IF ( IRET .EQ. 0 ) THEN
        CALL JELIRA(CH1//'.VALE','TYPE',IBID,TYP1)
        CALL JELIRA(CH1//'.VALE','LONMAX',NEQ1,K1B)
        CALL JEVEUO(CH1//'.VALE','L',JVALE1)
        CALL JELIRA(CH2//'.VALE','TYPE',IBID,TYP2)
        CALL JEVEUO(CH2//'.VALE','E',JVALE2)
        IF ( TYP1 .EQ. TYP2 ) THEN
           IF ( TYP1 .EQ. 'R' ) THEN
              DO 10 IEQ1 = 0 , NEQ1-1
                 ZR(JVALE2+IEQ1) = ZR(JVALE1+IEQ1)
 10           CONTINUE
           ELSEIF ( TYP1 .EQ. 'C' ) THEN
              DO 12 IEQ1 = 0 , NEQ1-1
                 ZC(JVALE2+IEQ1) = ZC(JVALE1+IEQ1)
 12           CONTINUE
           ELSE
               VALK(1) = CH1
               VALK(2) = CH2
               VALK(3) = TYP1
               CALL U2MESK('F','ALGELINE3_93', 3 ,VALK)
           ENDIF
        ELSE
           IF ( TYP1 .EQ. 'R' .AND.  TYP2 .EQ. 'C' ) THEN
              DO 14 IEQ1 = 0 , NEQ1-1
                 ZC(JVALE2+IEQ1) = ZR(JVALE1+IEQ1)
 14           CONTINUE
           ELSE
               VALK(1) = CH1
               VALK(2) = TYP1
               VALK(3) = CH2
               VALK(4) = TYP2
               CALL U2MESK('F','ALGELINE3_94', 4 ,VALK)
           ENDIF
        ENDIF
        GOTO 9999
      ENDIF
C
C
C     2. SI LES 2 CHAMPS N'ONT PAS LES MEMES NUMEROTATIONS :
C     ------------------------------------------------------
      CALL JELIRA ( CH1//'.VALE', 'TYPE'  , IBID, TYP1 )
      CALL JELIRA ( CH1//'.VALE', 'LONMAX', NEQ1 , K1B  )
      CALL JEVEUO ( CH1//'.VALE', 'L', JVALE1 )
      CALL JELIRA ( CH2//'.VALE', 'TYPE'  , IBID, TYP2 )
      CALL JELIRA ( CH2//'.VALE', 'LONMAX', NEQ2 , K1B  )
      CALL JEVEUO ( CH2//'.VALE', 'E', JVALE2 )

      CALL JEVEUO ( CH1//'.DESC', 'L', JDESC1 )
      CALL JEVEUO ( CH2//'.DESC', 'L', JDESC2 )
      IF ((ZI(JDESC1-1+2).LT.0).OR.(ZI(JDESC2-1+2).LT.0))
     &      CALL U2MESS('F','ALGELINE3_95')

      CALL JEVEUO ( CH1//'.REFE', 'L', JREFE1 )
      CALL JEVEUO ( CH2//'.REFE', 'L', JREFE2 )
      CALL ASSERT(ZK24(JREFE1)(1:8).EQ.ZK24(JREFE2)(1:8))
      CALL JEVEUO ( ZK24(JREFE1-1+2)(1:19)//'.DEEQ', 'L', JDEEQ1 )
      CALL JEVEUO ( ZK24(JREFE2-1+2)(1:19)//'.DEEQ', 'L', JDEEQ2 )


C     2.1 ON CHERCHE LE NUMERO DE CMP LE PLUS GRAND ET
C     LE NUMERO DE NOEUD LE PLUS GRAND DANS CH2->.DEEQ2 :
C     ---------------------------------------------------------------

      NNOMX=0
      NCPMX=0
      DO 16, IEQ2=1,NEQ2
        NNOMX= MAX(NNOMX,ZI(JDEEQ2-1+2*(IEQ2-1)+1))
        NCPMX= MAX(NCPMX,ZI(JDEEQ2-1+2*(IEQ2-1)+2))
 16   CONTINUE


C     2.2 ON REMPLIT UN OBJET DE TRAVAIL :
C     ------------------------------------
      CALL WKVECT ( '&&VTCOPY.TRAV1', 'V V I', NNOMX*NCPMX, JTRAV1 )
      DO 18, IEQ2=1,NEQ2
        NUNO2=ZI(JDEEQ2-1+2*(IEQ2-1)+1)
        NUCP2=ZI(JDEEQ2-1+2*(IEQ2-1)+2)
        IF (NUCP2.GT.0) ZI(JTRAV1-1+(NUNO2-1)*NCPMX+NUCP2)=IEQ2
 18   CONTINUE


C     2.3 ON RECOPIE LES VALEURS DE CH1 DANS CH2 :
C     -------------------------------------------
      IF ( TYP1 .EQ. TYP2 ) THEN
         IF ( TYP1 .EQ. 'R' ) THEN
            DO 20 IEQ1 = 1 , NEQ1
              NUNO1=ZI(JDEEQ1-1+2*(IEQ1-1)+1)
              NUCP1=ZI(JDEEQ1-1+2*(IEQ1-1)+2)
              IF ((NUCP1.GT.0).AND.(NUNO1.LE.NNOMX)
     &                        .AND. (NUCP1.LE.NCPMX)) THEN
                IEQ2=ZI(JTRAV1-1+(NUNO1-1)*NCPMX+NUCP1)
                IF (IEQ2 .GT. 0) THEN
                  ZR(JVALE2-1+IEQ2)=ZR(JVALE1-1+IEQ1)
                ENDIF
              END IF
 20         CONTINUE
         ELSEIF ( TYP1 .EQ. 'C' ) THEN
            DO 22 IEQ1 = 1 , NEQ1
              NUNO1=ZI(JDEEQ1-1+2*(IEQ1-1)+1)
              NUCP1=ZI(JDEEQ1-1+2*(IEQ1-1)+2)
              IF ((NUCP1.GT.0).AND.(NUNO1.LE.NNOMX))THEN
                IEQ2=ZI(JTRAV1-1+(NUNO1-1)*NCPMX+NUCP1)
                IF (IEQ2 .GT. 0) THEN
                  ZC(JVALE2-1+IEQ2)=ZC(JVALE1-1+IEQ1)
                ENDIF
              END IF
 22         CONTINUE
         ELSE
             VALK(1) = CH1
             VALK(2) = CH2
             VALK(3) = TYP1
             CALL U2MESK('F','ALGELINE3_93', 3 ,VALK)
         ENDIF
C
      ELSEIF ( TYP1 .EQ. 'R' .AND.  TYP2 .EQ. 'C' ) THEN
         DO 24 IEQ1 = 1 , NEQ1
           NUNO1=ZI(JDEEQ1-1+2*(IEQ1-1)+1)
           NUCP1=ZI(JDEEQ1-1+2*(IEQ1-1)+2)
           IF ((NUCP1.GT.0).AND.(NUNO1.LE.NNOMX))THEN
             IEQ2=ZI(JTRAV1-1+(NUNO1-1)*NCPMX+NUCP1)
             IF (IEQ2 .GT. 0) THEN
               ZC(JVALE2-1+IEQ2)=ZR(JVALE1-1+IEQ1)
             ENDIF
           END IF
 24      CONTINUE
C
      ELSE
          VALK(1) = CH1
          VALK(2) = TYP1
          VALK(3) = CH2
          VALK(4) = TYP2
          CALL U2MESK('F','ALGELINE3_94', 4 ,VALK)
      ENDIF
      CALL JEDETR ( '&&VTCOPY.TRAV1' )
C
 9999 CONTINUE
      CALL JEDEMA ( )
      END

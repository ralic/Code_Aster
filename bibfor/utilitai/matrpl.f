      SUBROUTINE MATRPL ( MATRIC, ISTOC, HCOL, ADIA , ABLO, NEQ,
     +                    NBBLOC, DDLEXC, MATP, NBDDL )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       MATRIC
      INTEGER             HCOL(*), ADIA(*), ABLO(*), DDLEXC(*)
      REAL*8              MATP(NBDDL,NBDDL)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     PASSAGE D'UNE MATRICE STOCKEE TRIANGULAIRE A UNE MATRICE PLEINE
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
      CHARACTER*32      JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*24      VALE
C     ------------------------------------------------------------------
      DATA  VALE  /'                   .VALE'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      VALE(1:19) = MATRIC
C
C     --- STOCKAGE LIGNE DE CIEL ---
C
      IF ( ISTOC .EQ. 1 ) THEN
         KK = 0
         DO 100 IBLOC = 1 , NBBLOC
            CALL JEVEUO( JEXNUM(VALE,IBLOC),'L',LMAT )
            DO 110 IEQUA = ABLO(IBLOC)+1 ,ABLO(IBLOC+1)
               IF ( DDLEXC(IEQUA) .EQ. 1 ) GOTO 110
               KK = KK + 1
               ILONG = HCOL(IEQUA)
               IADIA = LMAT + ADIA(IEQUA) - 1
               IDE   = IADIA - ILONG + 1
               JJ = KK
               DO 120 I = ILONG , 1 , -1
                 LL = IEQUA - ILONG + I
                 IF ( DDLEXC(LL) .EQ. 1 ) GOTO 120
                 MATP(JJ,KK) = ZR(IDE+I-1)
                 JJ = JJ - 1
 120          CONTINUE
 110        CONTINUE
            CALL JELIBE(JEXNUM(VALE,IBLOC))
 100     CONTINUE
C
C
C     --- STOCKAGE MORSE ---
C
      ELSEIF ( ISTOC .EQ. 2 ) THEN
         KK = 0
         DO 200 IBLOC = 1 , NBBLOC
            CALL JEVEUO( JEXNUM(VALE,IBLOC),'L',JVALE)
            IDEBLI = 1
            DO 210 IEQUA = 1 , NEQ
               IFINLI = ADIA(IEQUA)
               IF ( DDLEXC(IEQUA) .EQ. 0 ) THEN
                 KK = KK + 1
                 NBL = IEQUA - KK
                 INDCO1 = IEQUA
                 DO 220 IND = IFINLI , IDEBLI, -1
                    INDCOL = HCOL(IND)
                    ICOLA = 0
                    DO 222 ICOL = INDCOL, INDCO1
                       IF ( DDLEXC(ICOL) .EQ. 1 ) ICOLA = ICOLA + 1
 222                CONTINUE
                    IF ( DDLEXC(INDCOL) .EQ. 0 ) THEN
                      JJ = INDCOL - NBL + ICOLA
                      MATP(JJ,KK) = ZR(JVALE+IND-1)
                    ENDIF
 220             CONTINUE
               ENDIF
               IDEBLI = ADIA(IEQUA) + 1
 210        CONTINUE
            CALL JELIBE(JEXNUM(VALE,IBLOC))
 200     CONTINUE
C
      ELSE
         CALL UTMESS('F','MATRPL','STOCKAGE INCONNU.')
      ENDIF
C
      DO 10 I = 1 , NBDDL
         DO 12 J = I+1 , NBDDL
            MATP(J,I) = MATP(I,J)
 12      CONTINUE
 10   CONTINUE
C
      CALL JEDEMA()
      END

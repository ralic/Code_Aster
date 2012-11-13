      SUBROUTINE JJALLC ( ICLASI , IDATCI , CEL , IBACOL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      INTEGER             ICLASI , IDATCI ,       IBACOL
      CHARACTER*(*)                         CEL
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADML ,IADYN ,IC ,ICEL ,ID ,IPGCEX ,IS 
      INTEGER IX ,JCARA ,JCTAB ,JDATE ,JDOCU ,JGENR ,JHCOD 
      INTEGER JIADD ,JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ 
      INTEGER JORIG ,JRNOM ,JTYPE ,K ,N 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C ----------------------------------------------------------------------
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
C ----------------------------------------------------------------------
      INTEGER         IDDESO         , IDIADM     ,
     &               IDMARQ     , IDNOM      ,             IDLONG     ,
     &               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    (  IDDESO = 1  , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C     ------------------------------------------------------------------
      CHARACTER*1      GENRI,TYPEI
      INTEGER          COL(1),JCOL,ITAB(1)
      INTEGER          IADMI,IADDI(2),LTYPI,LONOI,ISTA1,ISTA2
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IC     = ICLASI
      ID     = IDATCI
      GENRI  = GENR ( JGENR(IC) + ID )
      TYPEI  = TYPE ( JTYPE(IC) + ID )
      LTYPI  = LTYP ( JLTYP(IC) + ID )
      LONOI  = LONO ( JLONO(IC) + ID ) * LTYPI
      IADMI  = IADM ( JIADM(IC) + 2*ID-1 )
      IADYN  = IADM ( JIADM(IC) + 2*ID   )
      IADDI(1) = IADD ( JIADD(IC) + 2*ID-1 )
      IADDI(2) = IADD ( JIADD(IC) + 2*ID   )
C ------- OBJET CONTENANT LES IDENTIFICATEURS DE LA COLLECTION
      IF ( IADMI .EQ. 0 ) THEN
        IADML = 0
        IF ( IADDI(1) .EQ. 0 ) THEN
          IF ( CEL .EQ. 'E' ) THEN
            CALL JJALLS (LONOI, IC, GENRI, TYPEI, LTYPI, 'INIT',
     &                   COL, JCOL, IADML, IADYN)
          ELSE
            CALL U2MESK('F','JEVEUX_18',1,RNOM(JRNOM(IC)+ID))
          ENDIF
        ELSE
          CALL JJALLS (LONOI, IC, GENRI, TYPEI, LTYPI, 'NOINIT',
     &                 COL, JCOL, IADML, IADYN )
          CALL JXLIRO ( IC , IADML , IADDI , LONOI )
        ENDIF
        IADMI = IADML
        IADM (JIADM(IC)+2*ID-1) = IADML
        IADM (JIADM(IC)+2*ID  ) = IADYN
      ELSE
        ISTA1 = ISZON (JISZON+IADMI - 1)
        IS    = JISZON+ISZON(JISZON+IADMI-4)
        ISTA2 = ISZON (IS - 4)
        ICEL = ISTAT(3)
        IF ( CEL .EQ. 'E' ) ICEL = ISTAT(4)
        IF ( ISTA1 .EQ. ISTAT(2) .AND. ISTA2 .EQ. ICEL ) THEN
          IF ( IMARQ(JMARQ(IC)+2*ID-1) .NE. 0 ) THEN
            IBACOL = IADMI
            GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IBACOL = IADMI
      CALL JJECRS (IADMI, IADYN,IC, ID, 0, CEL, IMARQ(JMARQ(IC)+2*ID-1))
 100  CONTINUE
C
      DO 20 K = 2,IDNUM
C     ----------- OBJETS ATTRIBUTS DE COLLECTION
        IX  = ISZON( JISZON + IBACOL + K )
        IF ( IX .GT. 0 ) THEN
          GENRI  = GENR ( JGENR(IC) + IX )
          TYPEI  = TYPE ( JTYPE(IC) + IX )
          LTYPI  = LTYP ( JLTYP(IC) + IX )
          LONOI  = LONO ( JLONO(IC) + IX ) * LTYPI
          IF ( LONOI .EQ. 0 ) THEN
            CALL U2MESK('F','JEVEUX_26',1,RNOM(JRNOM(IC)+IX))
          ENDIF
          IADMI  = IADM ( JIADM(IC) + 2*IX-1 )
          IADYN  = IADM ( JIADM(IC) + 2*IX  )
          IADDI(1) = IADD ( JIADD(IC) + 2*IX-1 )
          IADDI(2) = IADD ( JIADD(IC) + 2*IX   )
          IF ( IADMI .NE. 0 ) THEN
C --------- IL N'Y A RIEN A FAIRE
C
          ELSE IF ( K .NE. IDIADM .AND. K .NE. IDMARQ ) THEN
C --------- MISE EN MEMOIRE AVEC LECTURE DISQUE
            IADML = 0
            IF ( IADDI(1) .EQ. 0 ) THEN
              IF ( CEL .EQ. 'E' ) THEN
                CALL JJALLS (LONOI, IC, GENRI, TYPEI, LTYPI, 'INIT',
     &                       COL, JCOL, IADML, IADYN)
              ELSE
                CALL U2MESK('F','JEVEUX_18',1,RNOM(JRNOM(IC)+IX))
              ENDIF
            ELSE
              CALL JJALLS (LONOI, IC, GENRI, TYPEI, LTYPI, 'NOINIT',
     &                     COL, JCOL, IADML, IADYN)
              CALL JXLIRO ( IC , IADML , IADDI , LONOI )
            ENDIF
            IADMI = IADML
            IADM(JIADM(IC)+2*IX-1) = IADML
            IADM(JIADM(IC)+2*IX  ) = IADYN
          ELSE
C --------- MISE EN MEMOIRE SANS LECTURE DISQUE
            CALL JJALLS (LONOI, IC, GENRI, TYPEI, LTYPI, 'INIT',
     &                   ITAB, JCTAB, IADMI, IADYN)
            IADM(JIADM(IC)+2*IX-1) = IADMI
            IADM(JIADM(IC)+2*IX  ) = IADYN
          ENDIF
          IF ( (K.EQ.IDNOM  .OR. K.EQ.IDLONG  .OR.
     &          K.EQ.IDLONO .OR. K.EQ.IDLUTI)
     &         .AND.  RNOM(JRNOM(IC)+IX)(25:26) .NE. '$$'    ) THEN
            IPGC   = -1
          ENDIF
          CALL JJECRS (IADMI,IADYN,IC,IX,0,CEL,IMARQ(JMARQ(IC)+2*IX-1))
          IPGC = IPGCEX
        ENDIF
 20   CONTINUE
C
      IX  = ISZON( JISZON + IBACOL + IDDESO )
      LTYPI  = LTYP ( JLTYP(IC) + IX )
      LONOI  = LONO ( JLONO(IC) + IX ) * LTYPI
      IF ( LONOI .GT. 0 ) THEN
        IADMI  = IADM ( JIADM(IC) + 2*IX-1 )
        IADYN  = IADM ( JIADM(IC) + 2*IX  )
        IF ( IADMI .NE. 0 ) THEN
          CALL JJECRS (IADMI,IADYN,IC,IX,0,CEL,IMARQ(JMARQ(IC)+2*IX-1))
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END

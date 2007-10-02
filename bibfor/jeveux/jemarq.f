      SUBROUTINE JEMARQ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 01/10/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18 CRS_508 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C INCREMENTE LA MARQUE COURANTE
C
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LDYN , LGDYN , MXDYN , MCDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , MXDYN , MCDYN , NBDYN , NBFREE
C ----------------------------------------------------------------------
      INTEGER          IADMA,IADRS,LSI,KTEMPO(2)
C ----------------------------------------------------------------------
      IF ( KDESMA(1) .EQ. 0 ) THEN
C
C ----- CREATION DES SEGMENTS DE VALEURS DE GESTION DES MARQUES
C
        LGD = NREMAX(1)+NREMAX(2)+NREMAX(3)+NREMAX(4)+NREMAX(5)
        CALL JJALLS ( LGD*LOIS,'V','I',LOIS,'INIT',IADMA,
     &                IADRS,KDESMA(1),KDESMA(2))
        ISZON(JISZON+KDESMA(1)-1) = ISTAT(2)
        ISZON(JISZON+ISZON(JISZON+KDESMA(1)-4)-4) = ISTAT(4)
        LGDUTI = 0
        LGP = 50
        CALL JJALLS ( LGP*LOIS,'V','I',LOIS,'INIT',IADMA,
     &                IADRS,KPOSMA(1),KPOSMA(2))
        ISZON(JISZON+KPOSMA(1)-1) = ISTAT(2)
        ISZON(JISZON+ISZON(JISZON+KPOSMA(1)-4)-4) = ISTAT(4)
        LGPUTI = 0
      ELSE IF ( LGPUTI .EQ. LGP ) THEN
C
C ------ AGRANDISSEMENT DE L'OBJET DONNANT LES POSITIONS
C
        LSI = LGP
        LGP = 2*LGP
        CALL JJALLS (LGP*LOIS,'V','I',LOIS,'INIT',IADMA,
     &               IADRS,KTEMPO(1),KTEMPO(2))
        ISZON(JISZON+KTEMPO(1)-1) = ISTAT(2)
        ISZON(JISZON+ISZON(JISZON+KTEMPO(1)-4)-4) = ISTAT(4)
        DO 100 K=1,LSI
          ISZON(JISZON+KTEMPO(1)+K-1) = ISZON(JISZON+KPOSMA(1)+K-1)
 100    CONTINUE
        IF ( KPOSMA(2) .NE. 0 ) THEN
          MCDYN = MCDYN - (LGP/2)*LOIS
          CALL HPDEALLC ( KPOSMA(2), NBFREE, IBID )
        ELSE IF ( KPOSMA(1) .NE. 0 ) THEN
          CALL JJLIBP ( KPOSMA(1) )
        ENDIF  
        KPOSMA(1) = KTEMPO(1)
        KPOSMA(2) = KTEMPO(2)
      ENDIF
C
C --- ACTUALISATION DE LA POSITION DES OBJETS MARQUES
C
      ISZON(JISZON + KPOSMA(1) + IPGC ) = LGDUTI
      LGPUTI = LGPUTI + 1
      IPGC = IPGC + 1

C     SI IPGC > 200 C'EST PROBABLEMENT QU'UNE ROUTINE
C     FAIT JEMARQ SANS FAIRE JEDEMA
      CALL ASSERT(IPGC.LT.200)

 300  CONTINUE
C FIN ------------------------------------------------------------------
      END

      SUBROUTINE JXLIRO ( IC , IADMI , IADDI , LSO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 22/06/2009   AUTEUR PELLET J.PELLET 
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_6 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IC , IADMI , IADDI(2) , LSO
C ----------------------------------------------------------------------
C LECTURE D'UN SEGMENT DE VALEUR
C
C IN  IC    : NOM DE LA CLASSE
C IN  IADMI : ADRESSE MEMOIRE DU SEGMENT DE VALEURS
C IN  IADDI : ADRESSE DISQUE DU SEGMENT DE VALEURS
C IN  LSO   : LONGUEUR EN OCTET DU SEGMENT DE VALEURS
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
C ----------------------------------------------------------------------
      CHARACTER*75     CMESS
      INTEGER          IADMO , KADD , LADD , LGBL , LSO2 
      LOGICAL          LPETIT
      PARAMETER      ( NDE = 6)
C ----------------------------------------------------------------------
C REMARQUE : LE PARAMETER NDE EST AUSSI DEFINI DANS JXECRO
C
C DEB ------------------------------------------------------------------
      IADM   = IADMI
      LADM   = ISZON(JISZON + IADM - 3 )
      KADD   = IADDI(1)
      LADD   = IADDI(2)
      LGBL   = 1024*LONGBL(IC)*LOIS
      IADMO  = ( IADM - 1 ) * LOIS + LADM + 1
      LSO2   = LSO
      IF ( MOD(LSO,LOIS) .NE. 0 ) LSO2 = (1 + LSO/LOIS) * LOIS
      LPETIT = ( LSO .LT. LGBL-NDE*LOIS )
C
      IF ( IADDI(1) .EQ. 0 ) THEN
        CMESS = 'OBJET SANS IMAGE DISQUE'
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ELSE
C
C ----- RECHARGEMENTS ULTERIEURS
C
        IF ( LPETIT ) THEN
C
C ------- PETIT OBJET
C
          IF      ( KADD .EQ. IITLEC(IC) ) THEN
            CALL JXDEPS ( KITLEC(IC)+LADD+1 , IADMO , LSO )
          ELSE IF ( KADD .EQ. IITECR(IC) ) THEN
            CALL JXDEPS ( KITECR(IC)+LADD+1 , IADMO , LSO )
          ELSE
            IF ( LITLEC(IC) ) THEN
              CALL JXECRB ( IC , IITLEC(IC),KITLEC(IC)+1,LGBL,0,0)
              IUSADI ( JUSADI(IC) + 3*IITLEC(IC)-2 ) = 0
              IUSADI ( JUSADI(IC) + 3*IITLEC(IC)-1 ) = 0
            ENDIF
            CALL JXLIRB ( IC , KADD , KITLEC(IC)+1 , LGBL )
            CALL JXDEPS ( KITLEC(IC)+1+LADD , IADMO , LSO )
            IITLEC(IC) = KADD
            LITLEC(IC) = .FALSE.
          ENDIF
        ELSE
C
C ------- GROS  OBJET
C
          CALL JXLIRB ( IC , KADD , IADMO , LSO2 )
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END

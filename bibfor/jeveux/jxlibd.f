      SUBROUTINE JXLIBD ( IDCO , IDOS , IC , IADDI , LONOI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 17/10/2005   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      INTEGER             IDCO , IDOS , IC , IADDI(2) , LONOI
C ----------------------------------------------------------------------
C MARQUE LA PLACE DISQUE EN VUE D'UNE RECUPERATION ULTERIEURE
C L'IDENTIFICATEUR EST MIS NEGATIF
C IN  IDCO  : IDENTIFICATEUR DE COLLECTION
C IN  IDOS  : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
C IN  IC     : CLASSE ASSOCIEE A L'OBJET JEVEUX
C IN  IADDI  : ADRESSE DISQUE DE L'OBJET REPERE PAR ID
C IN  LONOI  : LONGUEUR EN OCTETS DE L'OBJET
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    , KINDEF    , KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) , KINDEF(N) , KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      COMMON /JUSADI/  JUSADI(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /INBDET/  NBLIM(N),NBGROS(N),NBPETI(N)
C     ------------------------------------------------------------------
      INTEGER          KADD , LADD , LGBL
      LOGICAL          LPETIT , LRAB
C DEB ------------------------------------------------------------------
      KADD   = IADDI(1)
      LADD   = IADDI(2)
      LPETIT = ( (IUSADI(JUSADI(IC)+3*KADD-2) .EQ. 0  .AND.
     &            IUSADI(JUSADI(IC)+3*KADD-1) .EQ. 0       ) )
      LGBL = 1024*LONGBL(IC)*LOIS
C
      IF ( LPETIT ) THEN
C
C ----- PETIT OBJET
C
        IF ( KADD .EQ. IITLEC(IC) ) THEN
          JIECR = (JK1ZON+KITLEC(IC)+LADD)/LOIS+1
          ISZON(JIECR-3) = -IDCO
          ISZON(JIECR-2) = -IDOS
          LITLEC(IC) = .TRUE.
        ELSE IF ( KADD .EQ. IITECR(IC) ) THEN
          JIECR = (JK1ZON+KITECR(IC)+LADD)/LOIS+1
          ISZON(JIECR-3) = -IDCO
          ISZON(JIECR-2) = -IDOS
        ELSE
          IF ( LITLEC(IC) ) THEN
            CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL,0,0)
          ENDIF
          CALL JXLIRB (IC, KADD, KITLEC(IC)+1, LGBL)
          JIECR = (JK1ZON+KITLEC(IC)+LADD)/LOIS+1
          ISZON(JIECR-3) = -IDCO
          ISZON(JIECR-2) = -IDOS
          IITLEC(IC) = KADD
          LITLEC(IC) = .TRUE.
        ENDIF
        IUSADI(JUSADI(IC)+3*KADD) = IUSADI(JUSADI(IC)+3*KADD) + 1
        NBPETI(IC) = NBPETI(IC) + 1
      ELSE
C
C ----- GROS  OBJET
C
        NBLENT = LONOI / LGBL
        LRAB = ( MOD (LONOI , LGBL) .NE. 0 )
        DO 10 I = 1 , NBLENT
          IUSADI(JUSADI(IC)+3*(KADD+I-1)-2) = -IDCO
          IUSADI(JUSADI(IC)+3*(KADD+I-1)-1) = -IDOS
   10   CONTINUE
        IF ( LRAB ) THEN
          IUSADI(JUSADI(IC)+3*(KADD+NBLENT)-2) = -IDCO
          IUSADI(JUSADI(IC)+3*(KADD+NBLENT)-1) = -IDOS
        ENDIF
      ENDIF
      NBGROS(IC) = NBGROS(IC) + 1
C FIN ------------------------------------------------------------------
      END

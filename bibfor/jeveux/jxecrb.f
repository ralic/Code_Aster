      SUBROUTINE JXECRB ( IC , IADDI , IADMO , LSO , IDCO , IDOS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
      INTEGER             IC , IADDI , IADMO , LSO , IDCO , IDOS
C ----------------------------------------------------------------------
C ECRITURE DISQUE D'UN OU PLUSIEURS ENREGISTREMENTS
C ROUTINE AVEC ADHERENCE SYSTEME    CRAY
C
C IN  IC    : NOM DE LA CLASSE
C IN  IADDI : ADRESSE DISQUE DU SEGMENT DE VALEURS
C IN  IADMO : ADRESSE MEMOIRE DU SEGMENT DE VALEURS EN OCTET
C IN  LSO   : LONGUEUR EN OCTET DU SEGMENT DE VALEURS
C IN  IDCO  : IDENTIFICATEUR DE COLLECTION
C IN  IDOS  : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
C                                              >0 GROS OBJET
C                                              =0 PETITS OBJETS
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      PARAMETER      ( N = 5 )
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C     ------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    ,             KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
C     ------------------------------------------------------------------
      CHARACTER*8      NOM
      LOGICAL          LRAB
      INTEGER          LGBL,VALI(3)
      REAL*8           R8BID
C DEB ------------------------------------------------------------------
      IB = 0
      IERR = 0
      LGBL = 1024*LONGBL(IC)*LOIS
      NBLENT = LSO / LGBL
      LRAB   = ( MOD ( LSO , LGBL ) .NE. 0 )
C     ------------------------------------------------------------------
      IF ( KSTINI(IC) .NE. 'DUMMY   ' ) THEN
        NOM = NOMFIC(IC)(1:4)//'.   '
        DO 10 I = 1 , NBLENT
          NUMEXT = (IADDI+I-2)/NBENRG(IC)
          IADLOC = (IADDI+I-1)-(NUMEXT*NBENRG(IC))
          CALL CODENT(NUMEXT+1,'G',NOM(6:7))
          JIECR = (JK1ZON+IADMO-1+LGBL*(I-1))/LOIS+1
          CALL WRITDR ( NOM , ISZON(JIECR) ,
     +                  LGBL/LOUA , IADLOC , -1 , IB , IERR )
          IF ( IERR .NE. 0 ) THEN
            VALI(1) = IADDI+I-1
            VALI(2) = NUMEXT
            VALI(3) = IERR
            CALL U2MESG('F','JEVEUX_40',1,NOMBAS(IC),3,VALI,0,R8BID)
          ENDIF
          IUSADI(JUSADI(IC)+3*(IADDI+I-1)-2) = IDCO
          IUSADI(JUSADI(IC)+3*(IADDI+I-1)-1) = IDOS
   10   CONTINUE
        IACCE (JIACCE(IC)+IADDI) = IACCE (JIACCE(IC)+IADDI) + 1
        IF ( LRAB ) THEN
          NUMEXT = (IADDI+NBLENT-1)/NBENRG(IC)
          IADLOC = (IADDI+NBLENT)-(NUMEXT*NBENRG(IC))
          CALL CODENT(NUMEXT+1,'G',NOM(6:7))
          JIECR = (JK1ZON+IADMO-1+LSO-LGBL)/LOIS+1
          CALL WRITDR ( NOM , ISZON(JIECR) ,
     +                  LGBL/LOUA , IADLOC ,-1, IB , IERR )
          IF ( IERR .NE. 0 ) THEN
            VALI(1) = IADDI+I-1
            VALI(2) = NUMEXT
            VALI(3) = IERR
            CALL U2MESG('F','JEVEUX_40',1,NOMBAS(IC),3,VALI,0,R8BID)
          ENDIF
          IUSADI(JUSADI(IC)+3*(IADDI+NBLENT)-2) = IDCO
          IUSADI(JUSADI(IC)+3*(IADDI+NBLENT)-1) = IDOS
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END

      SUBROUTINE JXLIRB ( IC , IADDI , IADMO , LSO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE
C MODIF JEVEUX  DATE 14/06/2011   AUTEUR TARDIEU N.TARDIEU 
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_6 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IC , IADDI , IADMO , LSO
C ----------------------------------------------------------------------
C LECTURE D'UN BLOC DU FICHIER D4ACCES DIRECT ASSOCIE A UNE BASE
C   
C
C IN  IC    : CLASSE ASSOCIEE
C IN  IADDI : ADRESSE DISQUE
C IN  IADMO : ADRESSE MEMOIRE EN OCTETS
C IN  LSO   : LONGUEUR EN OCTETS
C ----------------------------------------------------------------------
C
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C     ------------------------------------------------------------------
      PARAMETER      ( N = 5 )
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
      COMMON /JIACCE/  JIACCE(N),NBACCE(2*N)
C     ------------------------------------------------------------------
      LOGICAL          LRAB
      CHARACTER*8      NOM
      INTEGER          LGBL,VALI(3)
C DEB ------------------------------------------------------------------
      IERR = 0
      LGBL = 1024*LONGBL(IC)*LOIS
      NBLENT = LSO / LGBL
      LRAB   = ( MOD (LSO,LGBL) .NE. 0 )
C
      IF ( KSTINI(IC) .NE. 'DUMMY   ' ) THEN
        NOM = NOMFIC(IC)(1:4)//'.   '
        DO 10 I = 1 , NBLENT
          NUMEXT = (IADDI+I-2)/NBENRG(IC)
          IADLOC = (IADDI+I-1)-(NUMEXT*NBENRG(IC))
          CALL CODENT(NUMEXT+1,'G',NOM(6:7))
          JIECR = (JK1ZON+IADMO-1+LGBL*(I-1))/LOIS+1
          CALL READDR ( NOM, ISZON(JIECR), LGBL, IADLOC, IERR )
          IF ( IERR .NE. 0 ) THEN
            VALI(1) = IADDI+I-1
            VALI(2) = NUMEXT
            VALI(3) = IERR
            CALL U2MESG('F','JEVEUX_41',1,NOMBAS(IC),3,VALI,0,R8BID)
          ENDIF
          NBACCE(2*IC-1) = NBACCE(2*IC-1) + 1
   10   CONTINUE
        IACCE (JIACCE(IC)+IADDI)=IACCE(JIACCE(IC)+IADDI) + 1
        IF ( LRAB ) THEN
          NUMEXT = (IADDI+NBLENT-1)/NBENRG(IC)
          IADLOC = (IADDI+NBLENT)-(NUMEXT*NBENRG(IC))
          CALL CODENT(NUMEXT+1,'G',NOM(6:7))
          JIECR = (JK1ZON+IADMO-1+LSO-LGBL)/LOIS+1
          IF ( LSO .LT. LGBL ) THEN
            JIECR = (JK1ZON+IADMO-1)/LOIS+1
          ENDIF 
          CALL READDR ( NOM, ISZON(JIECR), LGBL, IADLOC, IERR )
          IF ( IERR .NE. 0 ) THEN
            VALI(1) = IADDI+I-1
            VALI(2) = NUMEXT
            VALI(3) = IERR
            CALL U2MESG('F','JEVEUX_41',1,NOMBAS(IC),3,VALI,0,R8BID)
          ENDIF
          NBACCE(2*IC-1) = NBACCE(2*IC-1) + 1
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END

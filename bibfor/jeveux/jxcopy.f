      SUBROUTINE JXCOPY ( CLSINZ , NOMINZ,  CLSOUZ , NMOUTZ , NBEXT )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) CLSINZ , NOMINZ,  CLSOUZ , NMOUTZ
      CHARACTER*1         CLASIN , CLASOU
      CHARACTER*8         NOMIN  , NOMOUT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 21/12/98   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C     RECOPIE D'UNE BASE DE DONNEES APRES ELIMINATION DES
C     ENREGISTREMENTS DEVENUS INACCESSIBLES
C     ------------------------------------------------------------------
C IN  CLSINZ : NOM DE CLASSE ASSOCIEE EN ENTREE
C IN  NOMINZ : NOM DE LA BASE EN ENTREE
C IN  CLSOUZ : NOM DE CLASSE ASSOCIEE EN SORTIE
C IN  NMOUTZ : NOM DE LA BASE EN SORTIE
C OUT NBEXT  : NOMBRE D'"EXTENDS" UTILISES APRES RETASSAGE
C     ------------------------------------------------------------------
      CHARACTER*1      K1ZON 
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    , KINDEF    , KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) , KINDEF(N) , KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
      COMMON /KINDJE/  INDEF(1)
      COMMON /JINDJE/  JINDEF(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C     ------------------------------------------------------------------
      CHARACTER*1      KCLAS
      CHARACTER*8      NOMBA1,NOMBA2,NOM
      CHARACTER*75     CMESS
      INTEGER          ITP(1),JITP,IADITP
C DEB ------------------------------------------------------------------
      NOMIN  = NOMINZ
      CLASIN = CLSINZ
      NOMOUT = NMOUTZ
      CLASOU = CLSOUZ
C
      KCLAS = CLASIN
      CALL JEINIF ( 'POURSUITE' , 'DETRUIT', NOMIN , KCLAS, 1 , 1 , 1 )
      ICI = INDEX ( CLASSE , KCLAS)
      KCLAS = CLASOU
      NREP = NREMAX(ICI)
      NBLOC= NBENRG(ICI)
      LBLOC= LONGBL(ICI)/LOIS
      NOM = NOMOUT(1:4)//'.?  '
      CALL LXMINS (NOM)
      CALL RMFILE (NOM)
      CALL JEINIF ( 'DEBUT', 'SAUVE', NOMOUT, KCLAS, NREP, NBLOC, LBLOC)
      ICO = INDEX ( CLASSE , KCLAS)
      NOMBA1 = NOMFIC(ICI)(1:4)//'.   '
      NOMBA2 = NOMFIC(ICO)(1:4)//'.   '
C
      CALL JJALLS(LONGBL(ICI),' ','I',LOIS,'INIT',ITP,JITP,IADITP)
      ISZON(JISZON+IADITP-1) = ISTAT(1)
      ISZON(JISZON+ISZON(JISZON+IADITP-4)-4) = ISTAT(4)
      DO 50  K=1,(NBLUTI(ICI)-1)/NBENRG(ICI)
        LINDEF = JINDEF(ICO)+K*(NBENRG(ICO)/512+1)*512
        CALL JXOUVR(ICO,K+1,INDEF(LINDEF),NBENRG(ICO))
        IEXT(ICO) = IEXT(ICO) + 1
 50   CONTINUE
      DO 100 K=1,NBLUTI(ICI)
        NUMEXT = (K-1)/NBENRG(ICI)
        IADLOC =  K - (NUMEXT*NBENRG(ICI))
        CALL CODENT(NUMEXT+1,'G',NOMBA1(6:7))
        CALL READDR ( NOMBA1, ISZON(JISZON + IADITP),
     &                LONGBL(ICI)/LOUA, IADLOC, IERR )
        IF (IERR .NE. 0 ) THEN
          CMESS = ' ERREUR LORS DE LA RELECTURE D''UN ENREGISTREMENT'
          CALL JVMESS('S','JXCOPY01',CMESS)
        ENDIF
        CALL CODENT(NUMEXT+1,'G',NOMBA2(6:7))
        CALL WRITDR ( NOMBA2, ISZON(JISZON + IADITP),
     &                LONGBL(ICO)/LOUA, IADLOC, -1, IB, IERR )
        IF (IERR .NE. 0 ) THEN
          CMESS = ' ERREUR LORS DE L''ECRITURE D''UN ENREGISTREMENT'
          CALL JVMESS('S','JXCOPY02',CMESS)
        ENDIF
 100  CONTINUE
      NBEXT = NUMEXT+1
      CALL JXFERM (ICI)
      CALL JXFERM (ICO)
      CALL JJLIBP (IADITP)
      CLASSE(ICO:ICO) = ' '
      CLASSE(ICI:ICI) = ' '
      DO 300 K=1,NBEXT
         CALL CODENT(K,'G',NOMBA2(6:7))
         CALL CODENT(K,'G',NOMBA1(6:7))
         CALL CPFILE (NOMBA2,NOMBA1)
 300  CONTINUE
C FIN ------------------------------------------------------------------
      END

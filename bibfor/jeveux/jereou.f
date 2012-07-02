      SUBROUTINE JEREOU (CLAS, PCENT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
      IMPLICIT NONE
      CHARACTER*(*)      CLAS
      REAL*8                   PCENT
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR PERMETTANT DE FERMER UNE BASE, DE DETRUIRE LE OU 
C         LES FICHIERS ASSOCIES ET DE LA RE-OUVRIR 
C IN : CLAS  NOM DE LA CLASSE ASSOCIEE ('G','V', ...)
C IN : PCENT POURCENTAGE EN TERME DE NOMBRE D'ENREGISTREMENTS OCCUPES 
C            AU-DELA DUQUEL ON DECLENCHE LA DESTRUCTION
C
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER N 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C DEB ------------------------------------------------------------------
      CHARACTER*1      KLAS
      CHARACTER*8      KSTIN,KSTOU,NOM,NOMB
      INTEGER          IC, NREP, LBLOC, NBLOC, INFO, VALI(3)
      REAL*8           VALR(2)
C     
      KLAS = CLAS
      CALL ASSERT (KLAS .NE. ' ' ) 
      IC   = INDEX (CLASSE , KLAS)
C
      NOMB  = NOMBAS(IC)
      NOM   = NOMB(1:4)//'.?  '
      NREP  = NREMAX(IC)
      LBLOC = LONGBL(IC)
      NBLOC = NBLMAX(IC)
      KSTIN = KSTINI(IC)
      KSTOU = KSTOUT(IC)      
C     
      IF ( NBLUTI(IC) .GT. PCENT*NBLMAX(IC) ) THEN
        VALR (1) = 100.D0*NBLUTI(IC)/NBLMAX(IC)
        VALR (2) = 100.D0*PCENT
        VALI (1) = NBLUTI(IC)
        VALI (2) = NBLUTI(IC)*LONGBL(IC)*LOIS/1024.D0
        VALI (3) = NBLMAX(IC)
        CALL U2MESG ('I','JEVEUX_63',1,NOMBAS(IC),3,VALI,2,VALR)
        INFO = 0
        CALL JELIBF ('DETRUIT' , KLAS , INFO) 
        CALL LXMINS (NOM)
        CALL RMFILE (NOM,0)
        CALL JEINIF (KSTIN,KSTOU,NOMB,KLAS,NREP,NBLOC,LBLOC)
      ENDIF
C
      END

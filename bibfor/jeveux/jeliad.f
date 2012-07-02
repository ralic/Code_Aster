      SUBROUTINE JELIAD ( CLAS , NUMR , NBOCT )
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
C TOLE CRP_18 CRS_508  
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR RENVOYANT LE NUMERO DE L'ENREGISTREMENT CONTENANT
C         L'OBJET SYSTEME $$RNOM DANS LE FICHIER D'ACCES DIRECT ASSOCIE
C IN  : CLAS NOM DE LA CLASSE ASSOCIEE ('G','V', ...)
C OUT : NUMR NUMERO DE L'ENREGISTREMENT 
C OUT : NBOCT NOMBRE D'OCTETS STOCKES AVANT L'ENREGISTREMENT CONTENANT 
C       $$RNOM
C
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)       CLAS
      INTEGER             NUMR , NBOCT , N 
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER JCARA ,JDATE ,JHCOD ,JIADD ,JIADM ,JLONG ,JLONO 
      INTEGER JLTYP ,JLUTI ,JMARQ 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C ----------------------------------------------------------------------
      CHARACTER*1      KCLAS
      INTEGER          IC, LGBL
C DEB ------------------------------------------------------------------
      KCLAS = CLAS
      IC = INDEX ( CLASSE , KCLAS )
      CALL ASSERT (IC .NE. 0)
C
C     L'OBJET $$RNOM CORRESPOND A L'INDICE 7
C      
      NUMR = IADD(JIADD(IC)+2*7-1) 
      LGBL = 1024*LONGBL(IC)*LOIS
      NBOCT = LGBL*(NUMR-1)
C
C FIN ------------------------------------------------------------------
      END

      SUBROUTINE REMP81(NOMRES,LPAR,BASMOD,NBMOD)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2013   AUTEUR BODEL C.BODEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C
C  BUT : < REMPLISSAGE DES MACRO-ELEMENTS AVEC LES VALEURS GENERALISEES
C          RENSEIGNEES A LA MAIN >
C
C        LA MATRICE RESULTAT EST SYMETRIQUE ET STOCKEE TRIANGLE SUP
C
C-----------------------------------------------------------------------
C
C NOMRES /I/  : NOM K19 DE LA MATRICE CARREE RESULTAT
C LPAR   /I/  : ADRESSE POUR LE STOCKAGE DU PARAMETRE (MASSE, RAID...)
C BASMOD /K8/ : NOM UT DE LA BASE MODALE DE PROJECTION
C NBMOD  /I/  : NOMBRE DE MODES DANS LA BASE
C
C
C
      INCLUDE 'jeveux.h'
      CHARACTER*1  CLASSE,TYP1
      CHARACTER*8  BASMOD,K8BID,BLANC
      CHARACTER*19 NOMMAT
      CHARACTER*14 NUM
      CHARACTER*18 NOMRES
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER LDRES,LDREF,LDDES,NBMOD,IMOD
      INTEGER NEQ,NTAIL,LPAR,I,IAD
      REAL*8 XPROD,DDOT
C-----------------------------------------------------------------------
      DATA BLANC/'        '/
C-----------------------------------------------------------------------
C
C --- CREATION ET REMPLISSAGE DU _REFE
C
      CALL JEMARQ()
      CALL WKVECT(NOMRES//'_REFE','G V K24',2,LDREF)
      ZK24(LDREF) = BASMOD
      ZK24(LDREF+1) = BLANC

C --- CREATION ET REMPLISSAGE DU _VALE
C
      NTAIL=NBMOD*(NBMOD+1)/2
      CALL WKVECT(NOMRES//'_VALE','G V R',NTAIL,LDRES)

      DO 10 I=1,NTAIL
        ZR(LDRES+I-1)=0.0D0
 10   CONTINUE
      DO 20 IMOD=1,NBMOD
        IAD=IMOD*(IMOD+1)/2
        
        ZR(LDRES+IAD-1)=ZR(LPAR+IMOD-1)
 20   CONTINUE
C
C
C --- CREATION ET REMPLISSAGE DU .DESC
C
      CALL WKVECT(NOMRES(1:18)//'_DESC','G V I',3,LDDES)
      ZI(LDDES) = 2
      ZI(LDDES+1) = NBMOD
      ZI(LDDES+2) = 2

      CALL JEDEMA()
      END

      SUBROUTINE IMBAMO ( NOMRES, IFM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C***********************************************************************
C    P. RICHARD     DATE 21/02/1991
C-----------------------------------------------------------------------
C  BUT:  IMPRIMER LES RESULTATS RELATIFS A LA BASE MODALE
      IMPLICIT NONE
C
C-----------------------------------------------------------------------
C
C NOMRES   /I/: NOM DU CONCEPT RESULTAT
C MAILLAGE /I/: NOM DU MAILLAGE
C IFM    /I/: UNITE DU FICHIER MESSAGE
C
C
C
C
      INCLUDE 'jeveux.h'
C-----------------------------------------------------------------------
      INTEGER I ,IBID ,IRET ,NBDEF ,NBMOD ,NBPABM ,NBTOT 

      REAL*8 FREQ ,GENEK ,GENEM 
C-----------------------------------------------------------------------
      PARAMETER    (NBPABM=8)
      CHARACTER*8 NOMRES,INTF,NOMNOE,NOMCMP
      CHARACTER*19 RAID,MASS,TYPEBA
      CHARACTER*14 NUMREF
      INTEGER LDPAR(NBPABM), IFM, LLREF,IER
      CHARACTER*16 BMPARA(NBPABM),TYPDEF      
      CHARACTER*8 RESCYC
      CHARACTER*8 K8BID

C
C-----------------------------------------------------------------------
C
      DATA  BMPARA/
     +  'NUME_MODE  '     , 'FREQ'       , 'NORME'           ,
     +  'NOEUD_CMP'       , 'TYPE_DEFO'          , 'OMEGA2'   ,
     +  'MASS_GENE'      , 'RIGI_GENE'/
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C----------------DETERMINATION DU TYPE DE LA BASE-----------------------
C
C------------------RECUPERATION DES CONCEPT AMONT-----------------------
C
        CALL JEVEUO(NOMRES//'           .REFD','L',LLREF)
        RAID=ZK24(LLREF)
        MASS=ZK24(LLREF+1)
        NUMREF=ZK24(LLREF+3)
        INTF=ZK24(LLREF+4)
        TYPEBA=ZK24(LLREF+6)
C
C--------------------------------ECRITURES------------------------------
C
      WRITE(IFM,*)' '
      WRITE(IFM,*)'----------------------------------------------------'
      WRITE(IFM,*)' '
      WRITE(IFM,*)'                DEF_BASE_MODALE '
      WRITE(IFM,*)' '
      WRITE(IFM,*)'  IMPRESSIONS NIVEAU: 2'
      WRITE(IFM,*)' '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' NOM DE LA BASE MODALE: ',NOMRES
      WRITE(IFM,*) '---------------------- '
C
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
C
C    CAS D'UNE BASE DE TYPE CONNUE
C
      IF(TYPEBA(1:9).EQ.'CLASSIQUE') THEN
C
        CALL DISMOI('F','NB_MODES_TOT',NOMRES,'RESULTAT',NBTOT,
     &              K8BID,IER)
        CALL DISMOI('F','NB_MODES_STA',NOMRES,'RESULTAT',NBDEF,
     &              K8BID,IER)
        CALL DISMOI('F','NB_MODES_DYN',NOMRES,'RESULTAT',NBMOD,
     &              K8BID,IER)
C
C
        WRITE(IFM,*) '                TYPE BASE MODALE: CLASSIQUE'
        WRITE(IFM,*) '                ----------------- '
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '                INTERF_DYNA: ',INTF
        WRITE(IFM,*) '                NUMEROTATION: ',NUMREF
        WRITE(IFM,*) '                MATRICE RAIDEUR: ',RAID
        WRITE(IFM,*) '                MATRICE MASSE: ',MASS
        WRITE(IFM,*) '                NOMBRE DE MODE PROPRES: ',NBMOD
        WRITE(IFM,*) '                NOMBRE DE MODE STATIQUES: ',NBDEF
C
C
      ENDIF
C
C   CAS D'UNE BASE DE TYPE CYCLIQUE
C
      IF(TYPEBA(1:8).EQ.'CYCLIQUE') THEN
        CALL DISMOI('F','NB_MODES_TOT',NOMRES,'RESULTAT',NBTOT,
     &              K8BID,IER)
        CALL DISMOI('F','NOM_MODE_CYCL',INTF,'INTERF_DYNA',IBID,RESCYC,
     &              IRET)
C
C
        WRITE(IFM,*) '                TYPE BASE MODALE: CYCLIQUE'
        WRITE(IFM,*) '                ----------------- '
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '                INTERF_DYNA: ',INTF
        WRITE(IFM,*) '                NUMEROTATION: ',NUMREF
C
C
      ENDIF
C
C CAS D'UNE BASE DE RITZ
C
      IF(TYPEBA(1:4).EQ.'RITZ') THEN
C
        CALL DISMOI('F','NB_MODES_TOT',NOMRES,'RESULTAT',NBTOT,
     &               K8BID,IER)
C
        WRITE(IFM,*) '                TYPE BASE MODALE: RITZ'
        WRITE(IFM,*) '                ----------------- '
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '                NUMEROTATION: ',NUMREF
        WRITE(IFM,*) '                DIMENSION BASE: ',NBTOT
C
      ENDIF
C
C
C
      WRITE(IFM,*)' '
      WRITE(IFM,*)'         DEFINITION DES DEFORMEES DE LA BASE MODALE'
      WRITE(IFM,*)'         ------------------------------------------'
C
      DO 10 I=1,NBTOT
C
        WRITE(IFM,*)' '
        CALL RSADPA(NOMRES,'L',NBPABM,BMPARA,I,0,LDPAR,K8BID)
C
        TYPDEF=ZK16(LDPAR(5))
C
        IF(TYPDEF.EQ.'PROPRE') THEN
          FREQ=ZR(LDPAR(2))
          GENEK=ZR(LDPAR(8))
          GENEM=ZR(LDPAR(7))
          WRITE(IFM,*)'NUME_ORDRE: ',I
          WRITE(IFM,*)'              ','MODE PROPRE     FREQUENCE: ',
     &                 FREQ,' HZ'
          WRITE(IFM,*)'              ','MASS_GENE: ',GENEM,
     &                ' RIGI_GENE: ',GENEK
C
        ELSE
          NOMNOE=ZK16(LDPAR(4))(1:8)
          NOMCMP=ZK16(LDPAR(4))(9:16)
          WRITE(IFM,*)'NUME_ORDRE: ',I
          WRITE(IFM,*)'              ','MODE ',TYPDEF
          WRITE(IFM,*)'              ','NOEUD: ',NOMNOE,
     &                ' COMPOSANTE: ',NOMCMP
C
        ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END

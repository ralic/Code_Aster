      SUBROUTINE LRREFD(RESU,PRCHNO)
      IMPLICIT  NONE
      CHARACTER*8 RESU
      CHARACTER*19 PRCHNO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/10/2010   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT:
C       REMPLIR LE .REFD A PARTIR DES DONNEES UTILISATEUR
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   RESU     : NOM DE LA SD_RESULTAT
C
C      SORTIE :
C-------------
C OUT  PRCHNO   : PROFIL DU CHAMNO
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)

      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER IRET,IRET1,IRET2,IRET3,IBID
      INTEGER JREFD

      CHARACTER*8 MATRA,MATRB
      CHARACTER*14 NUDDL
C
C ----------------------------------------------------------------------
C
C     SI L'UTILISATEUR NE DONNE PAS DE NUME_DDL ON LE DEDUIT DE LA 
C     DE LA MATRICE DE RIGIDITE (MATRB).
C     LE PROFIL EST DEDUIT DU NUME_DDL AINSI OBTENU
C
      CALL JEMARQ()

      PRCHNO = ' '
      MATRA  = ' '
      MATRB  = ' '
      NUDDL  = ' '
      
      IBID = 1

      CALL GETVID(' ','MATR_A',IBID,1,1,MATRA,IRET1)

      CALL GETVID(' ','NUME_DDL',0,1,1,NUDDL,IRET2)
      IF(IRET2.EQ.1)THEN
        CALL DISMOI('F','PROF_CHNO',NUDDL,'NUME_DDL',
     &                   IBID,PRCHNO,IRET)
      ENDIF

      CALL GETVID(' ','MATR_B',IBID,1,1,MATRB,IRET3)

      IF ((IRET3.EQ.1).AND.(IRET2.NE.1)) THEN
        CALL U2MESS('I','PREPOST_14')
        CALL DISMOI('F','NOM_NUME_DDL',MATRB,'MATR_ASSE',
     &                 IBID,NUDDL,IRET) 
        CALL DISMOI('F','PROF_CHNO',NUDDL,'NUME_DDL',
     &                 IBID,PRCHNO,IRET)
      ENDIF
        
      CALL WKVECT(RESU//'           .REFD','G V K24',7,JREFD)
      ZK24(JREFD-1+1) = MATRA
      ZK24(JREFD-1+2) = MATRB
      ZK24(JREFD-1+3) = ' '
      ZK24(JREFD-1+4) = NUDDL
      ZK24(JREFD-1+5) = ' '
      ZK24(JREFD-1+6) = ' '
      ZK24(JREFD-1+7) = ' '
C
      CALL JEDEMA()
C
      END

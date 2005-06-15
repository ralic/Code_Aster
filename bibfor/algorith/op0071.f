      SUBROUTINE OP0071 ( IERR )
      IMPLICIT NONE
      INTEGER             IERR
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C-----------------------------------------------------------------------
C
C     CALCUL PROJECTION MATRICE SUR BASE DE RITZ
C
C-----------------------------------------------------------------------
C      ---- DEBUT DES COMMUNS JEVEUX ----------------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C      ---- FIN DES COMMUNS JEVEUX ------------------------------------
C
      INTEGER      IBID, N1, N2, N3, N4, N5, NBVECT, IER, NBMODE, 
     +             IADRIF, LLREFE, NEQ, LLNEQU, LLDESC
      REAL*8       RBID
      COMPLEX*16   CBID
      CHARACTER*1  TYPMAT
      CHARACTER*8  K8B, NOMRES, BASEMO, MATRAS, NUMGEN
      CHARACTER*14 NU, NUMDD1, NUMDD2
      CHARACTER*16 TYPRES, NOMCOM, TYPBAS, MATRI2
      CHARACTER*19 NOMNUM, NOMSTO
      CHARACTER*24 MATRIC
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
C
C --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETVID ( ' ', 'MATR_ASSE'     , 1,1,1, MATRAS, N1 )
      CALL GETVID ( ' ', 'MATR_ASSE_GENE', 1,1,1, MATRAS, N3 )
      CALL GETVID ( ' ', 'BASE'          , 1,1,1, BASEMO, N4 )
      CALL GETVID ( ' ', 'NUME_DDL_GENE' , 1,1,1, NUMGEN, N2 )
C
      CALL GETTCO ( BASEMO, TYPBAS )
      TYPMAT= TYPRES(16:16)
C
      IF (N2.NE.0) THEN
        NOMNUM = NUMGEN//'      .NUME'
        NOMSTO = NUMGEN//'      .SLCS'
      ENDIF


C==================================================
C
      CALL RSORAC ( BASEMO, 'LONUTI', IBID, RBID, K8B, CBID, RBID,
     +              'ABSOLU', NBMODE, 1, IBID )
C
C RECUPERATION DU NOMBRE DE MODES REDUIT,
C NB_VECT DONNE PAR NUME_DDL_GENE
      CALL JEVEUO(NOMSTO//'.DESC','L',LLDESC)
      NBMODE   = ZI(LLDESC)


      IF (N1.NE.0) THEN
         CALL DISMOI('F', 'NOM_NUME_DDL', MATRAS, 'MATR_ASSE', IBID,
     +                                            NUMDD1,IER)
         CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
         IF ((TYPBAS.EQ.'MODE_MECA').OR.(TYPBAS.EQ.'MODE_GENE')) THEN
            MATRIC = ZK24(IADRIF)
         ELSE
            MATRIC = ZK24(IADRIF+2)
         ENDIF
         IF (MATRIC.NE.' ') THEN
            CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',IBID,
     +                NUMDD2,IER)
         ELSE
            NUMDD2 = ZK24(IADRIF+1)(1:14)
         ENDIF
         IF (NUMDD1.NE.NUMDD2) THEN
           CALL UTMESS('I',NOMCOM,'BASE MODALE ET MATR_ASSE AVEC'//
     +                ' NUMEROTATIONS DIFFERENTES')     
         ENDIF
         NU = NUMDD1(1:14)
      ELSE
         CALL JEVEUO(MATRAS//'           .REFA','L',LLREFE)
         NUMDD1 = ZK24(LLREFE+1)
         CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
         MATRIC = ZK24(IADRIF)
         MATRI2 = MATRIC(1:16)
         CALL JEVEUO(MATRI2//'   .REFA','L',LLREFE)
         NUMDD2 = ZK24(LLREFE+1)
         IF (NUMDD1.NE.NUMDD2) THEN
           CALL UTMESS('F',NOMCOM,'BASE MODALE ET MATR_ASSE_GENE AVEC'//
     +                ' NUMEROTATIONS INCOMPATIBLES')    
         ENDIF
         NU = NUMDD1(1:14)
      ENDIF

C
C ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C
      IF (N1.NE.0) THEN
         CALL DISMOI('F','NB_EQUA',MATRAS,'MATR_ASSE',NEQ,K8B,IER)
      ELSE
         CALL JEVEUO(NUMDD1//'.NUME.NEQU','L',LLNEQU)
         NEQ = ZI(LLNEQU)
      ENDIF
C
      IF (TYPMAT.EQ.'R') THEN
         CALL PROJMR(MATRAS,NOMRES,NOMSTO,BASEMO,NOMNUM,NU,NEQ,NBMODE)
      ELSEIF (TYPMAT.EQ.'C') THEN
         CALL PROJMC(MATRAS,NOMRES,NOMSTO,BASEMO,NOMNUM,NU,NEQ,NBMODE)
      ELSE
         CALL UTMESS('F',NOMCOM,' TYPE DE MATRICE INCONNU: '//TYPMAT)
      ENDIF
C
      CALL JEDEMA()
      END

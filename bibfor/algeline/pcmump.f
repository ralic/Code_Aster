      SUBROUTINE PCMUMP(MATASZ, SOLVEZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/01/2010   AUTEUR TARDIEU N.TARDIEU 
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
      IMPLICIT NONE
      CHARACTER*(*) MATASZ, SOLVEZ
C-----------------------------------------------------------------------
C
C     CREATION D'UNE MATRICE DE PRECONDITIONNEMENT DU GCPC
C     PAR FACTORISATION SIMPLE PRECISION PAR MUMPS
C
C-----------------------------------------------------------------------
C IN  K*  MATASZ    : NOM DE LA MATR_ASSE A PRECONDITIONNER
C IN  K*  SOLVEZ    : NOM DE LA SD SOLVEUR
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      INTEGER ISLVK,ISLVI,REACPR,ITERPR,JREFA,IRET
      REAL*8 RBID
      COMPLEX*16 CBID
      CHARACTER*19 SOLVEU,MATASS,SOLVBD
      CHARACTER*24 PRECON
C----------------------------------------------------------------------
      CALL JEMARQ()

      MATASS = MATASZ
      SOLVEU = SOLVEZ

C --  PARAMETRES DU PRECONDITIONNEUR
      CALL JEVEUO(SOLVEU//'.SLVK','L',ISLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','E',ISLVI)
      PRECON=ZK24(ISLVK-1+2)
      REACPR=ZI(ISLVI-1+1)
      ITERPR=ZI(ISLVI-1+3)
      
      CALL ASSERT(PRECON.EQ.'LDLT_SP')
      
C --  PRISE EN COMPTE DES CHARGEMENTS CINEMATIQUES
      CALL JEVEUO(MATASS//'.REFA','L',JREFA)
      CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIMF')
      IF (ZK24(JREFA-1+3).EQ.'ELIML') CALL MTMCHC(MATASS,'ELIMF')
      CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIML')
      
C --  CREATION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION 
C --  (A DETRUIRE A LA SORTIE)
      SOLVBD=ZK24(ISLVK-1+3)
      CALL CRSMSP(SOLVBD,MATASS)

C --  APPEL AU PRECONDITIONNEUR
      IF (MOD(ITERPR,REACPR).EQ.0) THEN
        CALL AMUMPH('DETR_MAT',SOLVBD,MATASS,RBID,CBID,' ',0,IRET)
        CALL AMUMPH('PRERES',  SOLVBD,MATASS,RBID,CBID,' ',0,IRET)
      ENDIF
      
C --  COMPTEUR DU NOMBRE DE FACTORISATION
      ZI(ISLVI-1+3)=ZI(ISLVI-1+3)+1

C --  DESTRUCTION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION
      CALL DETRSD('SOLVEUR',SOLVBD)

      CALL JEDEMA()
      END

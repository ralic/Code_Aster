      SUBROUTINE PCMUMP(MATASZ, SOLVEZ, IRETZ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) MATASZ, SOLVEZ
      INTEGER       IRETZ
C-----------------------------------------------------------------------
C
C     CREATION D'UNE MATRICE DE PRECONDITIONNEMENT DU GCPC
C     PAR FACTORISATION SIMPLE PRECISION PAR MUMPS
C
C-----------------------------------------------------------------------
C IN  K*  MATASZ    : NOM DE LA MATR_ASSE A PRECONDITIONNER
C IN  K*  SOLVEZ    : NOM DE LA SD SOLVEUR
C IN  I   IRETZ     : CODE RETOUR (!=0 SI ERREUR)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      INTEGER      JSLVK,JSLVI,ITERPR,REACPR,PCPIV ,JREFA,IRET
      REAL*8       RBID
      COMPLEX*16   CBID
      CHARACTER*19 SOLVEU,MATASS
      CHARACTER*24 PRECON,SOLVBD
C----------------------------------------------------------------------
      CALL JEMARQ()

      MATASS = MATASZ
      SOLVEU = SOLVEZ

C --  PARAMETRES DU PRECONDITIONNEUR
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','L',JSLVI)
      PRECON=ZK24(JSLVK-1+2)
      ITERPR=ZI(JSLVI-1+5)
      REACPR=ZI(JSLVI-1+6)
      PCPIV =ZI(JSLVI-1+7)
      
      CALL ASSERT(PRECON.EQ.'LDLT_SP')
      
C --  PRISE EN COMPTE DES CHARGEMENTS CINEMATIQUES
C --  SAUF DANS LE CAS OU LE SOLVEUR EST PETSC
C --  CAR DEJA FAIT DANS APETSC
      IF (ZK24(JSLVK).NE.'PETSC') THEN
        CALL JEVEUO(MATASS//'.REFA','L',JREFA)
        CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIMF')
        IF (ZK24(JREFA-1+3).EQ.'ELIML') CALL MTMCHC(MATASS,'ELIMF')
        CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIML')
      ENDIF
      
C --  CREATION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION 
C --  (A DETRUIRE A LA SORTIE)
      SOLVBD=ZK24(JSLVK-1+3)
      CALL CRSMSP(SOLVBD,MATASS,PCPIV,.TRUE.)

C --  APPEL AU PRECONDITIONNEUR
      IRET = 0
      IF (ITERPR.GT.REACPR.OR.ITERPR.EQ.0) THEN
        CALL AMUMPH('DETR_MAT',SOLVBD,MATASS,RBID,CBID,' ',0,IRET,
     &              .TRUE.)
        CALL AMUMPH('PRERES',  SOLVBD,MATASS,RBID,CBID,' ',0,IRET,
     &              .TRUE.)
      ENDIF
      
C --  DESTRUCTION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION
      CALL DETRSD('SOLVEUR',SOLVBD)

C --  CODE RETOUR
      IRETZ = IRET

      CALL JEDEMA()
      END

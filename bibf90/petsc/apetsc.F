      SUBROUTINE APETSC(ACTION,SOLVEZ,MATASZ,VECASZ,SOLUZ,VCINEZ)
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETSC PETSC  DATE 08/04/2008   AUTEUR LEFEBVRE J-P.LEFEBVRE 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2008  EDF R&D              WWW.CODE-ASTER.ORG 
!                                                                    
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE 
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           
!                                                                    
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     
! ================================================================== 
      IMPLICIT NONE
      CHARACTER*(*) ACTION,MATASZ,VECASZ,SOLUZ,VCINEZ,SOLVEZ
C--------------------------------------------------------------
C BUT : ROUTINE D'INTERFACE ENTRE LE CODE_ASTER ET LA BIBLIOTHEQUE
C       PETSC DE RESOLUTION DE SYSTEMES LINEAIRES.

C IN : ACTION :
C     /'PRERES'  : POUR DEMANDER LA FACTORISATION
C     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
C     /'DETR_MAT': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
C                  ASSOCIEE A UNE MATRICE
C     /'FIN'     : DESTRUCTION DE TOUTES LES INSTANCES MUMPS
C                  + MPI_FINALIZE  (A FAIRE DANS LA COMMANDE FIN ??)

C IN : SOLVEZ   (K19) : NOM DE LA SD SOLVEUR
C             (SI ACTION=PRERES/RESOUD)
C IN : MATAS (K19) : NOM DE LA MATR_ASSE
C             (SI ACTION=PRERES/RESOUD/DETR_MATR)
C IN : VECAS (K19) : NOM DU SECOND MEMBRE
C            (SI ACTION=RESOUD)
C IN : SOLU  (K19) : NOM DE LA SOLUTION
C            (SI ACTION=RESOUD)
C IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C            (SI ACTION=RESOUD)



C---------------------------------------------------------------
      INTEGER I,K,IBID,LMAT,NEQ,TYPVAR,TYPSYM,JREFA,IPREM
      CHARACTER*4 ETAMAT
      CHARACTER*19 MATAS,SOLU,VECAS,VCINE
      CHARACTER*19 SOLVEU
      INTEGER*4 ierr
C================================================================
#include "include/finclude/petsc.h"
C================================================================
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,LQUALI
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8,KCODE
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------
      SAVE IPREM
      DATA  IPREM /0/
C----------------------------------------------------------------
      CALL JEMARQ()
      SOLVEU = SOLVEZ
      MATAS = MATASZ
      VECAS = VECASZ
      SOLU = SOLUZ
      VCINE = VCINEZ

      IF (IPREM.EQ.0) THEN
C     --------------------------------
C         write(6,*) 'toto avant ierr=',ierr
         call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
C         write(6,*) 'toto apres ierr=',ierr
         CALL ASSERT(ierr.EQ.0)
         IPREM=1
      END IF

      CALL ASSERT(MATAS.NE.' ')
      CALL JEVEUO(MATAS(1:19)//'.&INT','L',LMAT)
      NEQ = ZI(LMAT+2)
      TYPVAR = ZI(LMAT+3)
      TYPSYM = ZI(LMAT+4)

C     1. INFORMATIONS SUR LA MATRICE :
C     ----------------------------------------------
      IF (TYPVAR.NE.1) THEN
C        CALL UTMESS('F','APETSC',
C     &              'ON NE TRAITE QUE LES MATRICES REELLES!')
         CALL ASSERT(.FALSE.)
      END IF


C     2. QUELQUES VERIFICATIONS ET PETITES ACTIONS :
C     ----------------------------------------------
      IF (ACTION.EQ.'PRERES') THEN

        CALL ASSERT(SOLVEU.NE.' ')

C        -- LA MATRICE EST-ELLE DEJA FACTORISEE ?
        CALL JEVEUO(MATAS//'.REFA','E',JREFA)
        ETAMAT=ZK24(JREFA-1+8)
        IF (ETAMAT.EQ.'DECT') THEN
C          CALL UTMESS('F','APETSC','PAS DE FACTORISATION CAR '//
C     &                'LA MATRICE EST DEJA FACTORISEE.')
          CALL ASSERT(.FALSE.)
        ELSE
          ZK24(JREFA-1+8)='DECT'
        END IF

C        -- DDLS ELIMINES :
         CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIMF')
         IF (ZK24(JREFA-1+3).EQ.'ELIML') CALL MTMCHC(MATAS,'ELIMF')
         CALL ASSERT(ZK24(JREFA-1+3).NE.'ELIML')

      ELSE IF (ACTION.EQ.'RESOUD') THEN

        CALL ASSERT(SOLVEU.NE.' ')
        CALL ASSERT(VECAS.NE.' ')
        CALL ASSERT(SOLU.NE.' ')

      ELSE IF (ACTION.EQ.'FIN') THEN
        CONTINUE
      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
        CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

C     3. RESOLUTION :
C     ----------------------------------------------
      CALL APETSR(ACTION,SOLVEU,MATAS,VECAS,SOLU,VCINE)


   10 CONTINUE
      CALL JEDEMA()
      END

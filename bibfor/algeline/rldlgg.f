      SUBROUTINE RLDLGG(LMAT,XSOL,CXSOL,NBSOL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER LMAT,NBSOL
      REAL*8 XSOL
      COMPLEX*16 CXSOL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C     ------------------------------------------------------------------



C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER TYPRES,TYPVAR,TYPSYM
C------------------------------------------------------------------
      CALL JEMARQ()
      NEQ = ZI(LMAT+2)
      TYPVAR = ZI(LMAT+3)
      TYPSYM = ZI(LMAT+4)
      TYPRES = ZI(LMAT+6)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',IADIA)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',IABLO)
      NBBLOC = ZI(LMAT+13)

C     ------------------------------------------------------------------

      IF (TYPRES.EQ.1) THEN
        CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',IDREFE)
        CALL JEVEUO(ZK24(IDREFE+2) (1:19)//'.HCOL','L',IHCOL)

        IF (TYPVAR.EQ.1) THEN
C                                                 L. D .LT
C           --- SYSTEME REELLE ---

C- CAS D'UNE MATRICE SYMETRIQUE

          IF (TYPSYM.EQ.1) THEN
            CALL RLDLR8(ZK24(ZI(LMAT+1)),ZI(IHCOL),ZI(IADIA),ZI(IABLO),
     &                  NEQ,NBBLOC,XSOL,NBSOL)

C- CAS D'UNE MATRICE NON_SYMETRIQUE

          ELSE IF (TYPSYM.EQ.0) THEN
            CALL RLDUR8(ZK24(ZI(LMAT+1)),ZI(IHCOL),ZI(IADIA),ZI(IABLO),
     &                  NEQ,NBBLOC,XSOL,NBSOL)
          END IF
        ELSE IF (TYPVAR.EQ.2) THEN

C           --- SYSTEME COMPLEXE (SYMETRIQUE) ---
          CALL RLDLC8(ZK24(ZI(LMAT+1)),ZI(IHCOL),ZI(IADIA),ZI(IABLO),
     &                NEQ,NBBLOC,CXSOL,NBSOL)
        END IF
      ELSE IF (TYPRES.EQ.2) THEN
C                          MULTIFRONTALE SYMETRIQUE OU NON
        IF (TYPVAR.EQ.1) THEN
          CALL RLTFR8(ZK24(ZI(LMAT+1)),NEQ,XSOL,NBSOL,TYPSYM)
        ELSE IF (TYPVAR.EQ.2) THEN
C                        MULTIFRONTALE COMPLEXE SYMETRIQUE OU NON
          CALL RLFC16(ZK24(ZI(LMAT+1)),NEQ,CXSOL,NBSOL,
     &                TYPSYM)
        END IF
      END IF
   10 CONTINUE

      CALL JEDEMA()

      END

      SUBROUTINE WP1DFT(LMAT,IMODE,ZEROPO,Z,DETNOR,DET,IDET,ISTURM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMAT,IMODE,                    IDET
      COMPLEX*16                   ZEROPO(*),Z,DETNOR
      REAL*8                                       DET
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C     -----------------------------------------------------------------
      REAL*8         UN,ZERO ,DIST
      CHARACTER*24   NOMDIA
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      DATA  NOMDIA/'                   .&VDI'/
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CALL JEMARQ()
      UN   = 1.D0
      ZERO = 0.D0
C
C     --- PRELIMINAIRE ---
      NOMDIA(1:19) = ZK24(ZI(LMAT+1))
      NEQ          = ZI(LMAT+2 )
      CALL JEEXIN(NOMDIA, IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F', 'WP1DFT',NOMDIA//' N''EXISTE PAS')
      ENDIF
      CALL JEVEUO( NOMDIA , 'L', LDIAG )
C
C     --- CALCUL DE LA DEFLATION ---
      DETNOR = DCMPLX(UN,ZERO)
      DO 1 I=1,IMODE-1
         DETNOR = DETNOR / ( (Z-ZEROPO(I))*(Z-DCONJG(ZEROPO(I))) )
  1   CONTINUE
C-DEL WRITE(6,*) '      /WP1DFT/   LA DEFLATION VAUT :',DETNOR
C
C     --- CALCUL DU DETERMINANT DE LA MATRICE DEFLATEE ---
      DET    = UN
      IDET   = 0
      ISTURM = 0
      DO 33 I = LDIAG, LDIAG+NEQ-1
         DIST = SQRT(DBLE(ZC(I)*DCONJG(ZC(I))))
         DETNOR = DETNOR * ZC(I) / DIST
         IF(DBLE(ZC(I)).LT.ZERO) ISTURM = ISTURM + 1
         CALL ALMULR('CUMUL',DIST,1,DET,IDET)
 33   CONTINUE
C-DEL WRITE(6,*) '      /WP1DFT/   DETNOR =',DETNOR,'   ISTURM =',ISTURM
C-DEL WRITE(6,'(15X,A,F16.10,A,I8)') 'PRODUIT = ',DET,' E',IDET
      CALL JEDEMA()
      END

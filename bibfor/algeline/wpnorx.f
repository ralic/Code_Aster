      SUBROUTINE WPNORX( NBMODE, NEQ, EXCLUS , VECP, RESUFK)
      IMPLICIT   NONE
      INTEGER            NBMODE, NEQ, EXCLUS(*)
      COMPLEX*16                      VECP(NEQ,NBMODE)
      CHARACTER*(*)                                  RESUFK(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/09/2008   AUTEUR PELLET J.PELLET 
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
C     NORMALISE A LA PLUS GRANDE DES VALEURS SUR UN DDL QUI N'EST PAS
C     EXCLUS
C     ------------------------------------------------------------------
C IN  NBMODE : I : NOMBRE  DE  MODE
C IN  NEQ    : I : TAILLE  DES MODES
C VAR VECP   : C : MATRICE DES MODES
C IN  EXCLUS : I : TABLE   DES DDL EXCLUS (0 <=> EXCLUS)
C     ------------------------------------------------------------------
      INTEGER    IMODE, IEQ
      COMPLEX*16 NORMX, ZERO
      REAL*8     R8MIEM,PREC
C     ------------------------------------------------------------------
C
      PREC=R8MIEM()*10.D0
      ZERO = DCMPLX(0.0D0,0.0D0)
      DO 100 IMODE = 1, NBMODE
          NORMX = ZERO
          DO 110 IEQ = 1, NEQ
             IF ( ABS(VECP(IEQ,IMODE)*EXCLUS(IEQ)).GT.ABS(NORMX) ) THEN
                NORMX = VECP(IEQ,IMODE)
             ENDIF
  110     CONTINUE
          IF ( ABS(NORMX) .GT. PREC ) THEN
             NORMX = 1.D0 / NORMX
             DO 120 IEQ = 1, NEQ
                VECP(IEQ,IMODE) = VECP(IEQ,IMODE) * NORMX
  120        CONTINUE
          ENDIF
          RESUFK(IMODE) = 'SANS_CMP: LAGR'
  100 CONTINUE
C
      END

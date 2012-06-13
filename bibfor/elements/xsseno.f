      SUBROUTINE XSSENO(NNO,NBSIG,NSE,NPG,JGANO,JSIGPG,SISENO)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C    - FONCTIONS REALISEES :  ROUTINE X-FEM
C
C     CALCUL DES CONTRAINTES PAR SOUS-ELEMENTS AUX NOEUDS (SENO)
C
C ......................................................................
C
C
C
C
      INCLUDE 'jeveux.h'
      INTEGER MXVAL
      PARAMETER (MXVAL=6*3*4)
C     EN 2D :
C     MXVAL =  6 (NBSE MAX) * 3 (NBNOSE MAX) * 4 (NBCMP MAX)
C     EN 3D :
C     MXVAL = 32 (NBSE MAX) * 4 (NBNOSE MAX) * 6 (NBCMP MAX)

      INTEGER NNO,NPG,JGANO
      INTEGER NBSIG
      INTEGER JSIGPG
      INTEGER IDECPG
      INTEGER NSE,ISE,IN,KPG,IC

      REAL*8 VPG(15),VNO(27)

      REAL*8 SISENO(MXVAL)

C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

C-----------------------------------------------------------------------
C     CALCUL DES CONTRAINTES PAR SOUS-ELEMENTS AUX NOEUDS (SENO)
C-----------------------------------------------------------------------

C     BOUCLE SUR LES NSE SOUS-ELEMENTS
      DO 110 ISE=1,NSE

C       DEBUT DE LA ZONE MEMOIRE DE SIG  CORRESPONDANTE
        IDECPG=NPG*(ISE-1)

C       BOUCLE NCMP DES CONTRAINTES
        DO 120 IC = 1,NBSIG

          DO 121 KPG = 1,NPG
            VPG(KPG) = ZR(JSIGPG+(KPG-1+IDECPG)*NBSIG+IC-1)
  121     CONTINUE

          CALL PPGAN2(JGANO,1,1,VPG,VNO)

          DO 122 IN=1,NNO
            SISENO(NBSIG*NNO*(ISE-1)+NBSIG*(IN-1)+IC)=VNO(IN)
 122      CONTINUE

 120    CONTINUE

 110  CONTINUE

      CALL JEDEMA()

      END

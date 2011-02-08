      SUBROUTINE IRMIFR(IFMIS,FREQ,IFREQ,NFREQ,IC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/02/2011   AUTEUR DEVESA G.DEVESA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER      IFMIS, IFREQ, NFREQ, IC
      REAL*8       FREQ
C
      CHARACTER*72 TEXTE
      REAL*8 A(3)
C
      REWIND IFMIS
      READ(IFMIS,'(A72)') TEXTE
      READ(IFMIS,'(A72)') TEXTE
      IC = 1
      NFREQ = 0
    1 CONTINUE
      NFREQ = NFREQ + 1
      READ(IFMIS,'(A72)') TEXTE
      IF (TEXTE(1:3).EQ.'CHA') THEN
        NFREQ = NFREQ -1 
      ELSE
        GOTO 1
      ENDIF
      REWIND IFMIS
      READ(IFMIS,'(A72)') TEXTE
      READ(IFMIS,'(A72)') TEXTE
      DO 3 I = 1, NFREQ
        READ(IFMIS,*) (A(J),J=1,3)
        IF (FREQ.LE.(A(1)*1.0001D0)) THEN
          IFREQ = I
          IF (I.GT.1.AND.FREQ.LT.(A(1)*0.9999D0)) THEN
            IFREQ = IFREQ-1
          ENDIF
          IF (FREQ.LE.R8PREM( )) IC = 2
          IF (I.EQ.1.AND.NFREQ.EQ.1) IC = 0
          IF (I.EQ.NFREQ.AND.FREQ.GE.(A(1)*0.9999D0)) THEN
            IC = 0
            IFREQ = NFREQ
          ENDIF
          GOTO 4
        ENDIF
    3 CONTINUE
      IFREQ = NFREQ
      IC = 0
    4 CONTINUE
      END      

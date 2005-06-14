      SUBROUTINE TRJEVE ( IFIC, NOCC )
      IMPLICIT   NONE
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU
C                MOT CLE FACTEUR "OBJET"
C ----------------------------------------------------------------------
      INTEGER      IOCC, REFI, N1, N2
      REAL*8       EPSI, REFR
      CHARACTER*3  SSIGNE
      CHARACTER*8  CRIT
      CHARACTER*24 NOMOBJ
C     ------------------------------------------------------------------
C
      DO 100 IOCC = 1,NOCC
        CALL GETVTX('OBJET','NOM',       IOCC,1,1, NOMOBJ, N1 )
        CALL GETVTX('OBJET','VALE_ABS' , IOCC,1,1, SSIGNE, N1 )
        CALL GETVR8('OBJET','PRECISION', IOCC,1,1, EPSI,   N1 )
        CALL GETVTX('OBJET','CRITERE',   IOCC,1,1, CRIT,   N1 )

        WRITE (IFIC,*) '---- OBJET: '//NOMOBJ
        CALL UTEST3(IFIC,'OBJET',IOCC)

        CALL GETVIS('OBJET','RESUME',IOCC,1,1,REFI,N2)
        IF (N2.EQ.1) THEN
C           -- RESUME:
          CALL UTESTO(NOMOBJ,'RESUME',REFI,REFR,EPSI,CRIT,IFIC,SSIGNE)
        END IF

        CALL GETVIS('OBJET','S_I',IOCC,1,1,REFI,N2)
        IF (N2.EQ.1) THEN
C           -- S_I :
          CALL UTESTO(NOMOBJ,'S_I',REFI,REFR,EPSI,CRIT,IFIC,SSIGNE)
        END IF

        CALL GETVR8('OBJET','S_R',IOCC,1,1,REFR,N2)
        IF (N2.EQ.1) THEN
C           -- S_R :
          CALL UTESTO(NOMOBJ,'S_R',REFI,REFR,EPSI,CRIT,IFIC,SSIGNE)
        END IF

 100  CONTINUE
C
      END

      SUBROUTINE USPAS1 ( NBUSUR, PARUSU, IPAS, PAS )
      IMPLICIT   NONE
      INTEGER             NBUSUR, IPAS(10,*)
      REAL*8              PARUSU(10,*), PAS(10,*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 28/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
      INTEGER  I
      REAL*8   DIFF1, DIFF2
C-----------------------------------------------------------------------
C
      DO 140 I = 1 , NBUSUR
         DIFF1 = PARUSU(I,2) - PARUSU(I,1)
         DIFF2 = PARUSU(I,3) - PARUSU(I,2)
C
         IF     (                          DIFF1.LE.20.D0   ) THEN
            IPAS(I,1) = 40
         ELSEIF ( (DIFF1.GT.20.D0)  .AND. (DIFF1.LE.30.D0)  ) THEN
            IPAS(I,1) = 70
         ELSEIF ( (DIFF1.GT.30.D0)  .AND. (DIFF1.LE.40.D0)  ) THEN
            IPAS(I,1) = 90
         ELSEIF ( (DIFF1.GT.40.D0)  .AND. (DIFF1.LE.50.D0)  ) THEN
            IPAS(I,1) = 110
         ELSEIF ( (DIFF1.GT.50.D0)  .AND. (DIFF1.LE.60.D0)  ) THEN
            IPAS(I,1) = 140
         ELSEIF ( (DIFF1.GT.60.D0)  .AND. (DIFF1.LE.70.D0)  ) THEN
            IPAS(I,1) = 200
         ELSEIF ( (DIFF1.GT.70.D0)  .AND. (DIFF1.LE.100.D0) ) THEN
            IPAS(I,1) = 250
         ELSEIF ( (DIFF1.GT.100.D0) .AND. (DIFF1.LE.150.D0) ) THEN
            IPAS(I,1) = 300
         ELSEIF ( (DIFF1.GT.150.D0) .AND. (DIFF1.LE.250.D0) ) THEN
            IPAS(I,1) = 500
         ELSEIF ( (DIFF1.GT.250.D0) .AND. (DIFF1.LE.360.D0) ) THEN
            IPAS(I,1) = 720
         ENDIF
         PAS(I,1) = DIFF1 / IPAS(I,1)
C           
         IF (                              DIFF2.LE.20.D0   ) THEN
            IPAS(I,2) = 40
         ELSEIF ( (DIFF2.GT.20.D0)  .AND. (DIFF2.LE.30.D0)  ) THEN
            IPAS(I,2) = 70
         ELSEIF ( (DIFF2.GT.30.D0)  .AND. (DIFF2.LE.40.D0)  ) THEN
            IPAS(I,2) = 90
         ELSEIF ( (DIFF2.GT.40.D0)  .AND. (DIFF2.LE.50.D0)  ) THEN
            IPAS(I,2) = 110
         ELSEIF ( (DIFF2.GT.50.D0)  .AND. (DIFF2.LE.60.D0)  ) THEN
            IPAS(I,2) = 140
         ELSEIF ( (DIFF2.GT.60.D0)  .AND. (DIFF2.LE.70.D0)  ) THEN
            IPAS(I,2) = 200
         ELSEIF ( (DIFF2.GT.70.D0)  .AND. (DIFF2.LE.100.D0) ) THEN
            IPAS(I,2) = 250
         ELSEIF ( (DIFF2.GT.100.D0) .AND. (DIFF2.LE.150.D0) ) THEN
            IPAS(I,2) = 300
         ELSEIF ( (DIFF2.GT.150.D0) .AND. (DIFF2.LE.250.D0) ) THEN
            IPAS(I,2) = 500
         ELSEIF ( (DIFF2.GT.250.D0) .AND. (DIFF2.LE.360.D0) ) THEN
            IPAS(I,2) = 720
         ENDIF 
         PAS(I,2) = DIFF2 / IPAS(I,2)
C
 140  CONTINUE
C
      END

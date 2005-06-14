        SUBROUTINE CJSSMI ( MATER,SIG ,VIN, SEUILI )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       ----------------------------------------------------------------
C       CJS        :  SEUIL DU MECANISME ISOTROPE   FI = - I1/3 + Q
C
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = ( Q, R, X, ETAT)
C       OUT SEUILI :  SEUIL  ELASTICITE DU MECANISME ISOTROPE
C       ----------------------------------------------------------------
        INTEGER    NDT, NDI,I
        REAL*8     MATER(14,2),QISO,  I1,   SIG(6),  VIN(*),
     >             SEUILI, TROIS

        COMMON /TDIM/   NDT , NDI

        DATA       TROIS  /3.D0/

C       ----------------------------------------------------------------


        QISO = VIN(1)
        I1=0.D0
        DO 10 I=1,NDI
        I1 = I1 + SIG(I)
 10     CONTINUE

        SEUILI  = - (I1+MATER(13,2))/TROIS + QISO

        END

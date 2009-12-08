        SUBROUTINE LCAFYD ( MATERF, NBCOMM,NMAT,NVI,VIND,YD, CRIT)
C RESPONSABLE PROIX J-M.PROIX
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       CHOIX DES VALEURS DE VIND A AFFECTER A YD
C       CAS PARTICULIER DU  MONOCRISTAL  : 
C       ON GARDE 1 VARIABLE INTERNE PAR SYSTEME DE GLISSEMENT SUR 3
C       ----------------------------------------------------------------
C       IN 
C            MATERF :  COEF MATERIAU
C            NBCOMM :  INCIDES DES COEF MATERIAU
C            NMAT   :  DIMENSION MATER
C            NVI    :  NOMBRE DE VARIABLES INTERNES
C       OUT  YD     :  VECTEUR INITIAL
C       ----------------------------------------------------------------
        INTEGER         NDT,NVI,NMAT,NDI,NS,I,NBCOMM(NMAT,3)
        REAL*8          YD(*),MATERF(NMAT,2),VIND(*),CRIT(*)
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------

C       MONOCRISTAL

C       EXPLICITE=1 OU IMPLICITE=0
        IF (INT(CRIT(6)).EQ.1) THEN
           NS=(NVI-8)/3
C            KOCKS-RAUCH
           IF (MATERF(NBCOMM(1,1),2).EQ.4) THEN
C             NOMBRE DE FAMILLES DE SYST : NBFSYS
              CALL ASSERT(NBCOMM(NMAT,2).EQ.1)
              DO 102 I=1,NS
                 YD(NDT+I)=VIND(6+3*(I-1)+1)
 102          CONTINUE
           ELSE
              DO 103 I=1,NS
                 YD(NDT+I)=VIND(6+3*(I-1)+2)
 103          CONTINUE
           ENDIF
        ELSE

C        CAS GENERAL        
           CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
           
        ENDIF
      END

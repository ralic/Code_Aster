        SUBROUTINE MOFICK(FA,FAV,CONT,TANGE,MAXFA,
     >                          NFACE,NFACEV,NFACEM,
     >                    FLUXK,FLUX1K,FLUX2K,
     >                    FLUXL,FLUX1L,FLUX2L,
     >                    MOYFL,MOYFL1,MOYFL2)
C
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      IMPLICIT NONE
C      
      LOGICAL      CONT,TANGE
      INTEGER      MAXFA
      INTEGER      NFACE,NFACEV,NFACEM
      INTEGER      JFA,JFAV,FA,FAV      
      REAL*8       FLUXK(NFACE),FLUXL(NFACEV)
      REAL*8       FLUX1K(1:MAXFA+1,NFACE),FLUX2K(1:MAXFA+1,NFACE)
      REAL*8       FLUX1L(1:MAXFA+1,NFACEV),FLUX2L(1:MAXFA+1,NFACEV)
      REAL*8       MOYFL(NFACEM)
      REAL*8       MOYFL1(1:MAXFA,1:MAXFA+1,0:1),
     >             MOYFL2(1:MAXFA,1:MAXFA+1,0:1) 
C      
      IF(CONT)THEN
         MOYFL(FA) = 0.5D0 * ( FLUXK(FA) - FLUXL(FAV) ) 
      ENDIF
      IF (TANGE)THEN
         MOYFL1(FA,1,0) = 0.5D0 * FLUX1K(1,FA)
         MOYFL2(FA,1,0) = 0.5D0 * FLUX2K(1,FA)
C        
         MOYFL1(FA,1,1) = -0.5D0 * FLUX1L(1,FAV)
         MOYFL2(FA,1,1) = -0.5D0 * FLUX2L(1,FAV)  
C        
         DO 1 JFA = 1,NFACE
            MOYFL1(FA,1+JFA,0) = 0.5D0 * FLUX1K(1+JFA,FA)
            MOYFL2(FA,1+JFA,0) = 0.5D0 * FLUX2K(1+JFA,FA)
   1     CONTINUE
C      
         DO 2 JFAV = 1,NFACEV            
            MOYFL1(FA,1+JFAV,1) = -0.5D0 * FLUX1L(1+JFAV,FAV)
            MOYFL2(FA,1+JFAV,1) = -0.5D0 * FLUX2L(1+JFAV,FAV)
   2     CONTINUE             
      ENDIF
      END                        

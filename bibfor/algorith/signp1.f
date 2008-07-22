      FUNCTION  SIGNP1(THMC)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/07/2008   AUTEUR PELLET J.PELLET 
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
C ======================================================================
C  ROUTINE SIGNP1
C  DONNE LE SIGNE DONT DOIT ETRE AFFECTE PREA DANS LES
C  CALCULS DE SIP EN THM
C
      IMPLICIT      NONE
      REAL*8        SIGNP1
      CHARACTER*16 THMC
      IF (THMC.EQ.'LIQU_SATU') THEN
           SIGNP1  = -1.0D0
      ELSE IF (THMC.EQ.'GAZ') THEN
          SIGNP1=1.0D0
      ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
          SIGNP1  = -1.0D0
      ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
          SIGNP1  = 1.0D0
      ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
          SIGNP1  = 1.0D0
      ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
          SIGNP1=1
      ELSE IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
          SIGNP1  = 1.0D0
      ENDIF
      END

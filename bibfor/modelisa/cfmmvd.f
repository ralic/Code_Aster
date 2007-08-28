      INTEGER FUNCTION CFMMVD(VECT) 
C    
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/08/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*5 VECT
C      
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE POUR LE CONTACT (TOUTES METHODES)
C
C RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SDCONT
C
C ----------------------------------------------------------------------
C
C
C IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
C
C ----------------------------------------------------------------------
C
      INTEGER   ZNOES,ZMETH,ZTOLE,ZTABF
      PARAMETER (ZNOES=10,ZMETH=8 ,ZTOLE=6 ,ZTABF=30) 
      INTEGER   ZCMCF,ZECPD,ZTGDE,ZDIRE
      PARAMETER (ZCMCF=24,ZECPD=6 ,ZTGDE=6 ,ZDIRE=3 )
      INTEGER   ZPOUD,ZDIME,ZMAES
      PARAMETER (ZPOUD=3 ,ZDIME=9 ,ZMAES=3)    
      INTEGER   ZAPME,ZAPPA,ZREAC
      PARAMETER (ZAPME=4 ,ZAPPA=3 ,ZREAC=4)
      INTEGER   ZCONV,ZRESU,ZCARF
      PARAMETER (ZCONV=9 ,ZRESU=24,ZCARF=4)                  
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IF (VECT.EQ.'ZNOES') THEN
        CFMMVD = ZNOES
      ELSE IF (VECT.EQ.'ZMETH') THEN
        CFMMVD = ZMETH
      ELSE IF (VECT.EQ.'ZTOLE') THEN
        CFMMVD = ZTOLE  
      ELSE IF (VECT.EQ.'ZTABF') THEN
        CFMMVD = ZTABF   
      ELSE IF (VECT.EQ.'ZCMCF') THEN
        CFMMVD = ZCMCF   
      ELSE IF (VECT.EQ.'ZECPD') THEN
        CFMMVD = ZECPD          
      ELSE IF (VECT.EQ.'ZTGDE') THEN
        CFMMVD = ZTGDE   
      ELSE IF (VECT.EQ.'ZDIRE') THEN
        CFMMVD = ZDIRE       
      ELSE IF (VECT.EQ.'ZPOUD') THEN
        CFMMVD = ZPOUD 
      ELSE IF (VECT.EQ.'ZDIME') THEN
        CFMMVD = ZDIME  
      ELSE IF (VECT.EQ.'ZMAES') THEN
        CFMMVD = ZMAES  
      ELSE IF (VECT.EQ.'ZREAC') THEN
        CFMMVD = ZREAC    
      ELSE IF (VECT.EQ.'ZAPME') THEN
        CFMMVD = ZAPME     
      ELSE IF (VECT.EQ.'ZAPPA') THEN
        CFMMVD = ZAPPA
      ELSE IF (VECT.EQ.'ZCONV') THEN
        CFMMVD = ZCONV   
      ELSE IF (VECT.EQ.'ZRESU') THEN
        CFMMVD = ZRESU     
      ELSE IF (VECT.EQ.'ZCARF') THEN
        CFMMVD = ZCARF                 
      ELSE
        CALL CFIMPA('CFMMVD',1) 
      ENDIF  
C
      CALL JEDEMA()
      END

      SUBROUTINE PIQPLA ( X1, Y1, Z1, XP, YP, ZP,
     +                    ZONE7, ZONE8, L4, L6, EPSI )
      IMPLICIT   NONE 
      REAL*8              X1, Y1, Z1, XP, YP, ZP, L4, L6, EPSI
      LOGICAL             ZONE7, ZONE8
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 11/11/98   AUTEUR CIBHHME R.MEDDOURI 
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
C     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
C     AUTEUR Y. WADIER
C
C     REALISE LA TRANSFO : GEOMETRIE DE REF. --> PIQUAGE / PLAQUE CARREE
C     POUR LES ZONES : ZONE7,ZONE8
C IN  : X1, Y1, Z1  : COORD. DU POINT DANS LA GEOMETRIE DE REFERENCE
C OUT : XP, YP, ZP  : COORD. DU POINT DANS LA GEOMETRIE PIQUAGE/PLAQUE
C
C-----------------------------------------------------------------------
C
      REAL*8  ALP, LAN, RMP, PIS2, PIS4, R8PI
      REAL*8  X2, Y2
C     ------------------------------------------------------------------
C
      XP = X1
      YP = Y1
      ZP = Z1
C
      IF ( ZONE7 .OR. ZONE8 ) THEN
C
         ALP = ATAN2( Y1 , X1 )
         PIS2 = R8PI() / 2.0D0
         PIS4 = R8PI() / 4.0D0
C
         IF ( (0.0D0.LE.ALP) .AND. (ALP.LT.PIS4) ) THEN
            X2 = L6
            Y2 = L6 * TAN(ALP)
         ENDIF
C
         IF ( (PIS4.LE.ALP) .AND. (ALP.LT.(PIS2-EPSI)) ) THEN
            X2 = L6 * COS(ALP) / SIN(ALP)
            Y2 = L6
         ENDIF
C
         IF ( ABS(ALP-PIS2) .LE. EPSI ) THEN
            X2 = 0.0D0
            Y2 = L6
         ENDIF
C
         RMP = SQRT( X1**2 + Y1**2 )
         LAN = ( RMP - L4 ) / ( L6 - L4 )
C
         XP = ( 1.0D0-LAN ) * X1 + ( LAN*LAN ) * X2
         YP = ( 1.0D0-LAN )*  Y1 + ( LAN*LAN ) * Y2
         ZP = Z1
C
      ENDIF
C
      END

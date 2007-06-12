      SUBROUTINE GDFONC ( DFDX,DFDY,KP,FFORM,DEPL,THET,FORC,TEMP,NNO,
     &                    DUDM,DTDM,DFDM,TGD)
      IMPLICIT   NONE
      INTEGER           KP, NNO
      REAL*8            DEPL(*),THET(*), TEMP(*), FORC(*), FFORM(*)
      REAL*8            DFDX(*),DFDY(*),DUDM(*),DTDM(*),DFDM(*),TGD(*)
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/08/97   AUTEUR CIBHHLV L.VIVAN 
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
C
C                       CALCUL DES GRADIENTS POUR LE CALCUL DU
C                       TAUX DE RESTITUTION D'ENERGIE EN 2D
C ENTREE :
C           DFDX     ---->  DERIVEES DES FONCTIONS DE FORMES/X
C           DFDY     ---->  DERIVEES DES FONCTIONS DE FORMES/Y
C           KP       ---->  POINT DE GAUSS
C           FFORM    ---->  VALEURS DES FONCTIONS DE FORMES
C           DEPL     ---->  CHAMP DE DEPLACEMENT AUX NOEUDS
C           THET     ---->  CHAMP THETA AUX NOEUDS
C           FORC     ---->  CHAMP D'EFFORT SURFACIQUE AUX NOEUDS
C           TEMP     ---->  CHAMP DE TEMPERATURE AUX NOEUDS
C           NNO      ---->  NOMBRE DE NOEUDS
C SORTIE :
C           DUDM     ---->  DERIVEES ET VALEURS DU DEPLACEMENT
C           DTDM     ---->  DERIVEES ET VALEURS DE THETA
C           DFDM     ---->  DERIVEES ET VALEURS DES EFFORTS SURFACIQUES
C           TGD      ---->  DERIVEES DE LA TEMPERATURE AUX POINTS DE G
C ......................................................................
C
      INTEGER      I
      REAL*8       VF
C.......................................................................
C
      DO 10 I = 1 , 7
         DUDM(I) = 0.0D0
         DTDM(I) = 0.0D0
         DFDM(I) = 0.0D0
 10   CONTINUE
      TGD(1) = 0.0D0
      TGD(2) = 0.0D0
C
      DO 3 I = 1 , NNO
         VF      = FFORM( (KP - 1)*NNO + I )
         DUDM(1) = DUDM(1) + DFDX(I) * DEPL( 2*(I - 1) + 1 )
         DUDM(2) = DUDM(2) + DFDY(I) * DEPL( 2*(I - 1) + 2 )
         DUDM(3) = DUDM(3) + DFDY(I) * DEPL( 2*(I - 1) + 1 )
         DUDM(4) = DUDM(4) + VF      * DEPL( 2*(I - 1) + 1 )
         DUDM(5) = DUDM(5) + DFDX(I) * DEPL( 2*(I - 1) + 2 )
         DUDM(6) = DUDM(4)
         DUDM(7) = DUDM(7) + VF      * DEPL( 2*(I - 1) + 2 )
C
         DTDM(1) = DTDM(1) + DFDX(I) * THET( 2*(I - 1) + 1 )
         DTDM(2) = DTDM(2) + DFDY(I) * THET( 2*(I - 1) + 2 )
         DTDM(3) = DTDM(3) + DFDY(I) * THET( 2*(I - 1) + 1 )
         DTDM(4) = DTDM(4) + VF      * THET( 2*(I - 1) + 1 )
         DTDM(5) = DTDM(5) + DFDX(I) * THET( 2*(I - 1) + 2 )
         DTDM(6) = DTDM(4)
         DTDM(7) = DTDM(7) + VF      * THET( 2*(I - 1) + 2 )
C
         DFDM(1) = DFDM(1) + DFDX(I) * FORC( 2*(I - 1) + 1 )
         DFDM(2) = DFDM(2) + DFDY(I) * FORC( 2*(I - 1) + 2 )
         DFDM(3) = DFDM(3) + DFDY(I) * FORC( 2*(I - 1) + 1 )
         DFDM(4) = DFDM(4) + VF      * FORC( 2*(I - 1) + 1 )
         DFDM(5) = DFDM(5) + DFDX(I) * FORC( 2*(I - 1) + 2 )
         DFDM(6) = DFDM(4)
         DFDM(7) = DFDM(7) + VF      * FORC( 2*(I - 1) + 2 )
C
         TGD(1)  = TGD(1)  + DFDX(I) * TEMP( I )
         TGD(2)  = TGD(2)  + DFDY(I) * TEMP( I )
   3  CONTINUE
C
      END

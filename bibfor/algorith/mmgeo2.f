      SUBROUTINE MMGEO2(NDIM,
     &                  IMA,IMABAR,INDNOB,INDRAC,INDNOQ,
     &                  IDEPL,
     &                  DEPLE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2006   AUTEUR MABBAS M.ABBAS 
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
      IMPLICIT NONE
      INTEGER NDIM
      INTEGER IDEPL
      REAL*8  DEPLE(6)
      INTEGER IMA,IMABAR,INDNOB,INDRAC,INDNOQ    
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0365
C ----------------------------------------------------------------------
C
C MISE A JOUR DES DEPLACEMENTS POINT DE CONTACT/SON PROJETE MODIFIES 
C SI FOND DE FISSURE OU RACCORD SURFACIQUE
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  IMA    : NUMERO DE LA MAILLE ESCLAVE
C IN  IMABAR : NUMERO DE LA MAILLE ESCLAVE DE L'ELEMENT DE BARSOUM
C IN  INDNOB : NUMERO DU NOEUD A EXCLURE DANS LA MAILLE POUR BARSOUM
C IN  INDNOQ : NUMERO DU NOEUD EN FACE DU NOEUD A EXCLURE POUR BARSOUM
C IN  INDRAC : NOEUDS EXCLUS PAR GROUP_NO_RACC
C IN  IDEPL  : ADRESSE JEVEUX POUR CHAMP DE DEPLACEMENTS A L'INSTANT
C              COURANT
C OUT DEPLE  : DEPLACEMENTS MODIFIES
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C

C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- TRAITEMENT FOND DE FISSURE 
C   
      IF (IMA .EQ. IMABAR) THEN
        IF (INDNOB .GT. 0) THEN
          IF (INDNOB .LE. 4) THEN
            DEPLE(NDIM+1) = ZR(IDEPL+(INDNOQ-1)*(2*NDIM)+NDIM+1-1)
            ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
          ELSEIF (INDNOB .GT. 4) THEN  
            IF (INDNOQ .EQ. 2) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(1-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(2-1)*(2*NDIM)+NDIM))
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            ELSEIF (INDNOQ .EQ. 4) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(1-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(4-1)*(2*NDIM)+NDIM))  
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            ELSEIF (INDNOQ .EQ. 6) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(3-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(2-1)*(2*NDIM)+NDIM))
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            ELSEIF (INDNOQ .EQ. 12) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(3-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(4-1)*(2*NDIM)+NDIM))
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            END IF   
          END IF
        END IF
      END IF
C
C --- TRAITEMENT DU RACCORD SURFACIQUE 
C
      IF (INDRAC .GT. 0) THEN
        IF (INDRAC .EQ. 5) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(1-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(2-1)*(2*NDIM)+NDIM))
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        ELSEIF (INDRAC .EQ. 6) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(2-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(3-1)*(2*NDIM)+NDIM))  
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        ELSEIF (INDRAC .EQ. 7) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(3-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(4-1)*(2*NDIM)+NDIM))
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        ELSEIF (INDRAC .EQ. 8) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(4-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(1-1)*(2*NDIM)+NDIM))
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        END IF
      END IF
C
      CALL JEDEMA()
C
      END

      SUBROUTINE AVCDMX( NBVEC, DOMTOT, CUDOMX, VNORMX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/01/2004   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT   NONE
      INTEGER    NBVEC, VNORMX
      REAL*8     DOMTOT(NBVEC), CUDOMX
C ----------------------------------------------------------------------
C BUT: CALCULER LE MAX DES CUMULS DE DOMMAGE ET DETERMINER LE VECTEUR
C      NORMAL ASSOCIE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  DOMTOT   IN   R  : VECTEUR CONTENANT LES DOMMAGES TOTAUX (CUMUL)
C                     DE CHAQUE VECTEUR NORMAL.
C  VNORMX   OUT  I  : NUMERO DU VECTEUR NORMAL ASSOCIE AU MAX DES CUMULS
C                     DE DOMMAGE.
C  CUDOMX   OUT  R  : VALEUR DU MAX DES CUMULS DE DOMMAGE.
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER    IVECT
C     ------------------------------------------------------------------
C234567                                                              012
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CUDOMX = 0.0D0
      VNORMX = 1
C
      DO 10 IVECT=1, NBVEC
         IF ( DOMTOT(IVECT) .GT. CUDOMX ) THEN
            CUDOMX = DOMTOT(IVECT)
            VNORMX = IVECT
         ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
C
      END
